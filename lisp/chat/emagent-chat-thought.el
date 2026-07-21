;;; emagent-chat-thought.el --- thought streaming module  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;; This file is part of emagent.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Thought/reasoning streaming, flushing, and folding helpers extracted from
;; `emagent-chat-render'.

;;; Code:

(require 'cl-lib)

(require 'org)

(require 'map)

(require 'emagent-log)

(require 'emagent-chat-header)

(require 'emagent-chat-markup)

(require 'emagent-chat-reasoning)

(declare-function emagent-chat--send-pending-end "emagent-chat-actions")

(declare-function emagent-chat--ensure-response-markers "emagent-chat-response")

(declare-function emagent-chat--response-body-bounds "emagent-chat-response")

(defun emagent-chat--format-thought-block (text)
  "Return `** Thinking' subsection markup for reasoning TEXT, or \"\" when empty."
  (let ((trimmed (string-trim (or text ""))))
    (if (string-empty-p trimmed)
        ""
      (format "%s\n%s\n\n"
              emagent-chat-thinking-headline
              (emagent-chat--escape-reasoning-text trimmed)))))

(defun emagent-chat--reasoning-block-bounds ()
  "Return (CONTENT-START . CONTENT-END) for the `** Thinking' subsection at point."
  (save-excursion
    (unless (looking-at emagent-chat--thinking-headline-re)
      (re-search-backward emagent-chat--thinking-headline-re nil t))
    (beginning-of-line)
    (when (looking-at emagent-chat--thinking-headline-re)
      (let ((content-start (line-end-position))
            (content-end (emagent-chat--thinking-content-end
                          (line-end-position) (point-max))))
        (when (> content-end content-start)
          (cons content-start content-end))))))

(defun emagent-chat--hide-reasoning-by-region (bounds)
  "Hide Reasoning content between BOUNDS using `outline-flag-region'."
  (when bounds
    (ignore-errors
      (outline-flag-region (car bounds) (cdr bounds) t))))

(defun emagent-chat--hide-reasoning-at-point ()
  "Fold the `** Thinking' subsection at or near point.

Hides the subsection body with `org-fold-hide-subtree', leaving its headline
visible as a collapsed summary.  Falls back to folding the inner region only
so incomplete parses never break the buffer."
  (when-let ((bounds (emagent-chat--reasoning-block-bounds)))
    ;; Fontify the body synchronously before folding.  jit-lock skips text
    ;; that is already invisible, so folding an unfontified subsection (as
    ;; happens on interrupt, where finalize and fold run back-to-back with no
    ;; intervening redisplay) leaves the collapsed Thinking line unrendered
    ;; until a manual fold/unfold.
    (ignore-errors
      (font-lock-ensure (save-excursion (goto-char (car bounds))
                                        (line-beginning-position))
                        (cdr bounds)))
    ;; `(car bounds)' is the line-end-position of the `** Thinking' headline
    ;; (i.e. the \n that ends it).  `beginning-of-line' from there lands on
    ;; the `** Thinking' headline itself — exactly where `org-fold-hide-subtree'
    ;; must be called.  We must NOT use `re-search-backward' here: with multiple
    ;; exchanges each containing `** Thinking', a backward search would jump to
    ;; the previous response's headline and fold the wrong block.
    (condition-case _
        (save-excursion
          (goto-char (car bounds))
          (beginning-of-line)
          (require 'org-fold)
          (org-fold-hide-subtree))
      (error
       (emagent-chat--hide-reasoning-by-region bounds)))))

(defun emagent-chat--hide-reasoning-deferred (&optional pos)
  "Hide the Reasoning block near POS after the next redisplay.

Skips hiding while the ACP session is busy (tool call in progress) so
that non-blocking shell waits — which allow idle timers to fire — do not
fold the block prematurely.  `emagent-chat-finish-assistant' re-schedules
the hide when the response is fully complete and the session is idle."
  (when emagent-chat-fold-reasoning-on-done
    (let ((buffer (current-buffer))
          (at (or pos
                  (save-excursion
                    (unless (looking-at emagent-chat--reasoning-begin-re)
                      (re-search-backward emagent-chat--reasoning-begin-re nil t))
                    (point)))))
      (when (and buffer at)
        (run-with-idle-timer
         0 nil
         (lambda ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (unless (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
                 (save-excursion
                   (goto-char at)
                   (emagent-chat--hide-reasoning-at-point)))))))))))

(defun emagent-chat--cancel-thought-flush ()
  "Flush any pending reasoning content and cancel the flush timer.
Empty-lang fences (reasoning notes) are emitted as plain text; language-tagged
fences are closed as org src blocks so buffered content stays readable."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer)
    (setq emagent-chat--thought-flush-timer nil))
  (let ((fence emagent-chat--fence-state)
        (pending emagent-chat--thought-pending))
    (setq emagent-chat--fence-state nil
          emagent-chat--thought-pending "")
    (let ((to-insert
           (concat
            (when fence
              (let* ((raw-lang (car fence))
                     (body (string-trim-right (cdr fence))))
                (cond
                 ((string-empty-p body) "")
                 ((string-empty-p raw-lang) (concat body "\n"))
                 (t (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n"
                            (emagent-chat--lang-from-src-tag raw-lang) body)))))
            pending)))
      (when (and (not (string-empty-p to-insert))
                 (emagent-chat--open-response-p))
        (emagent-chat--insert-thought-now to-insert)))))

(defvar-local emagent-chat--fence-state nil
  "Streaming code-fence buffer for the open Thinking block.
Nil when not inside a fenced code block.
Non-nil: (lang . accumulated-body-so-far) while waiting for the closing ```.")

(defun emagent-chat--split-fences (text)
  "Convert complete markdown fences in TEXT to org src blocks.
Returns (safe-text . fence-state) where fence-state is nil when all fences
are closed, or (lang . body-so-far) for the last unclosed fence."
  (let ((pos 0)
        (parts nil)
        (incomplete nil))
    (while (and (not incomplete) (string-match "```" text pos))
      (let* ((fence-pos (match-beginning 0))
             (after-fence (match-end 0)))
        (push (substring text pos fence-pos) parts)
        (cond
         ;; No newline after ``` — partial lang tag at end of chunk
         ((not (string-match "\n" text after-fence))
          (setq incomplete (cons (substring text after-fence) ""))
          (setq pos (length text)))
         ;; Newline found — complete lang tag, look for closing ```
         (t
          (let* ((tag-end (match-beginning 0))
                 (body-start (match-end 0))
                 (lang (string-trim (substring text after-fence tag-end))))
            (if (not (string-match "```" text body-start))
                ;; No closing fence — buffer the rest
                (progn
                  (setq incomplete (cons lang (substring text body-start)))
                  (setq pos (length text)))
              ;; Complete fence — emit as org src block when lang is given,
              ;; or as plain text (fence stripped) when lang is empty so
              ;; reasoning notes wrapped in plain ``` don't become src blocks.
              (let* ((body-end (match-beginning 0))
                     (close-end (match-end 0))
                     (body (string-trim-right (substring text body-start body-end))))
                (push (if (string-empty-p lang)
                          body
                        (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                                (emagent-chat--lang-from-src-tag lang)
                                (emagent-chat--escape-src-body body)))
                      parts)
                (setq pos close-end))))))))
    (unless incomplete
      (push (substring text pos) parts))
    (cons (apply #'concat (nreverse parts)) incomplete)))


(defun emagent-chat--feed-fences (text state)
  "Continue fence conversion for TEXT from prior STATE.

STATE is nil or (LANG . BODY) as returned by `emagent-chat--split-fences'.
When BODY is empty, fall back to reconstructing an open fence (handles an
incomplete language tag cheaply).  When BODY is non-empty, append TEXT and
search for a closing fence from a 2-character overlap so open code blocks
stay O(chunk) rather than re-scanning the full buffered body each time."
  (let ((fence (make-string 3 ?`)))
    (cond
     ((null state)
      (emagent-chat--split-fences text))
     ((zerop (length (cdr state)))
      (emagent-chat--split-fences
       (concat fence (car state) "\n" (cdr state) text)))
     (t
      (let* ((lang (car state))
             (body (cdr state))
             (overlap (min 2 (length body)))
             (combined (concat body text))
             (close (cl-search fence combined :start2
                               (max 0 (- (length body) overlap)))))
        (if (not close)
            (cons "" (cons lang combined))
          (let* ((complete (string-trim-right (substring combined 0 close)))
                 (rest (substring combined (+ close 3)))
                 (block (if (string-empty-p lang)
                            complete
                          (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                                  (emagent-chat--lang-from-src-tag lang)
                                  (emagent-chat--escape-src-body complete))))
                 (after (emagent-chat--split-fences rest)))
            (cons (concat block (car after)) (cdr after)))))))))

(defun emagent-chat--count-substring (needle s end)
  "Return the count of non-overlapping NEEDLE occurrences in S before END."
  (let ((n 0) (pos 0) (len (length needle)) hit)
    (while (setq hit (cl-search needle s :start2 pos :end2 end))
      (setq n (1+ n)
            pos (+ hit len)))
    n))

(defun emagent-chat--open-markup-start (text)
  "Return start index of a trailing incomplete markdown span in TEXT.
Return nil when TEXT ends on a complete boundary.  Covers inline code,
bold, and links whose closing delimiter may still arrive later.  Each
pattern is anchored to the end of TEXT and forbids an interior newline."
  (let (starts)
    ;; Inline code: a trailing unmatched opening backtick.
    (when (and (string-match "`[^`\n]*\\'" text)
               (cl-evenp (cl-count ?` text :end (match-beginning 0))))
      (push (match-beginning 0) starts))
    ;; Bold: a trailing `**' opener with no closing `**' yet.
    (when (and (string-match "\\*\\*[^*\n]*\\'" text)
               (cl-evenp (emagent-chat--count-substring "**" text
                                                        (match-beginning 0))))
      (push (match-beginning 0) starts))
    ;; Link, in any partial state: `[text', `[text]', or `[text](url'.
    (dolist (re '("\\[[^][\n]*\\'"
                  "\\[[^][\n]+\\][ \t]*\\'"
                  "\\[[^][\n]+\\][ \t]*([^)\n]*\\'"))
      (when (string-match re text)
        (push (match-beginning 0) starts)))
    (when starts (apply #'min starts))))

(defun emagent-chat--split-open-markup (text)
  "Split TEXT before a trailing, still-incomplete markdown span.
Return (EMIT . HOLD): HOLD begins at an inline code, bold, or link span whose
closing delimiter may arrive in a later chunk, or \"\" when TEXT ends on a
complete boundary.  Holding the partial span keeps markup that a streaming
boundary split from rendering as raw `*', backtick, or bracket characters."
  (if-let ((start (emagent-chat--open-markup-start text)))
      (cons (substring text 0 start) (substring text start))
    (cons text "")))

(defun emagent-chat--schedule-thought-flush ()
  "Debounce reasoning insertion using `emagent-chat-thought-stream-delay'."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer))
  (setq emagent-chat--thought-flush-timer
        (run-with-timer emagent-chat-thought-stream-delay nil
                        (lambda ()
                          (setq emagent-chat--thought-flush-timer nil)
                          (emagent-chat--flush-thought-pending)))))

(defun emagent-chat--end-send-pending-if-active ()
  "End the pre-dispatch phase once agent output begins arriving."
  (when (fboundp 'emagent-chat--send-pending-end)
    (emagent-chat--send-pending-end)))

(defun emagent-chat--insert-thought-now (text)
  "Insert reasoning TEXT at the open Thinking marker."
  (emagent-chat--end-send-pending-if-active)
  (when (and (not (string-empty-p text))
             (emagent-chat--open-response-p))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (emagent-chat--ensure-response-markers)
      (emagent-chat--ensure-thought-stream)
      (when (and emagent-chat--thought-marker
                 (marker-position emagent-chat--thought-marker))
        (let ((inhibit-modification-hooks t))
          (save-excursion
            (goto-char emagent-chat--thought-marker)
            (emagent-chat--insert-reasoning-text text)
            (setq emagent-chat--thought-marker (point-marker)
                  emagent-chat--assistant-marker (point-marker)
                  emagent-chat--reasoning-streamed-p t)))))))

(defun emagent-chat--flush-thought-pending (&optional final)
  "Insert any batched reasoning text into the open Thinking block.
Converts complete markdown code fences to org src blocks; buffers incomplete
fences in `emagent-chat--fence-state' until the closing ``` arrives, and
buffers a trailing partial inline span (code, bold, or link) in
`emagent-chat--thought-pending' until its closing delimiter arrives, so markup
split across streaming chunks never renders raw.  With FINAL non-nil (thought
close or an interrupting tool call) everything buffered is emitted as-is."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer)
    (setq emagent-chat--thought-flush-timer nil))
  (let ((text emagent-chat--thought-pending))
    (setq emagent-chat--thought-pending "")
    (when (not (string-empty-p text))
      (let* ((result (emagent-chat--feed-fences text emagent-chat--fence-state))
             (to-insert (car result))
             (new-fence (cdr result)))
        (setq emagent-chat--fence-state new-fence)
        ;; With no open fence and more chunks still coming, hold back a
        ;; trailing partial markup span (inline code, bold, or link) whose
        ;; closing delimiter may arrive in the next chunk.
        (unless (or final new-fence)
          (let ((split (emagent-chat--split-open-markup to-insert)))
            (setq to-insert (car split)
                  emagent-chat--thought-pending (cdr split))))
        (when (not (string-empty-p to-insert))
          (emagent-chat--with-streaming-view
           (lambda ()
             (emagent-chat--insert-thought-now to-insert))))))))

(defun emagent-chat-begin-thought ()
  "Resume or open the Thinking block in the in-flight emagent response."
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (when (emagent-chat--open-response-p)
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat--ensure-response-markers)
            (emagent-chat--ensure-reasoning-scaffold)))))))

(defun emagent-chat-append-thought (text)
  "Append reasoning TEXT to the open Reasoning block."
  (when (not (string-empty-p text))
    (setq emagent-chat--thought-pending
          (concat emagent-chat--thought-pending text))
    (if (or noninteractive (<= emagent-chat-thought-stream-delay 0))
        (emagent-chat--flush-thought-pending)
      (emagent-chat--schedule-thought-flush))))

(defun emagent-chat-close-thought ()
  "Close the open `** Thinking' subsection, if any, and schedule folding."
  (emagent-chat--flush-thought-pending t)
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (when emagent-chat--thought-open-p
          (let ((inhibit-read-only t)
                (hide-at (emagent-chat--open-reasoning-begin)))
            (emagent-chat--writable)
            (when-let ((tail (emagent-chat--reasoning-block-tail)))
              ;; Only reset assistant-marker to the reasoning tail when response
              ;; text hasn't started yet.  If the Response section already exists
              ;; (streaming started), keep the marker tracking the response content.
              (unless (emagent-chat--response-body-bounds)
                (setq emagent-chat--assistant-marker (copy-marker tail nil))))
            (setq emagent-chat--thought-open-p nil
                  emagent-chat--thought-marker nil)
            (emagent-chat--maybe-font-lock-flush)
            (when hide-at
              (emagent-chat--hide-reasoning-deferred hide-at))))))))

(defun emagent-chat--finish-tool-line-in-reasoning ()
  "Leave `emagent-chat--thought-marker' on a fresh line after a tool line."
  (goto-char (line-end-position))
  (unless (or (eobp) (eq (char-after) ?\n))
    (insert "\n"))
  (setq emagent-chat--thought-marker (copy-marker (point) nil)))

(defun emagent-chat--sync-thought-marker-after-tool (end)
  "Place `emagent-chat--thought-marker' after tool display ending at END."
  (goto-char (marker-position end))
  (unless (bolp)
    (goto-char (line-end-position)))
  (unless (or (eobp) (eq (char-after) ?\n))
    (insert "\n"))
  (setq emagent-chat--thought-marker (copy-marker (point) nil)))

(defun emagent-chat--reasoning-after-tool-artifact-p ()
  "Return non-nil when point follows a tool line or src block, modulo blanks."
  (save-excursion
    (skip-chars-backward " \t\n")
    (beginning-of-line)
    (looking-at-p "\\(?:→ \\|#\\+[Ee][Nn][Dd]_[Ss][Rr][Cc]\\)")))

(defun emagent-chat--thinking-leading-blank-p ()
  "Return non-nil when point sits in the blank run after `** Thinking'."
  (when-let* ((beg (emagent-chat--open-reasoning-begin))
              (pos (point)))
    (save-excursion
      (goto-char beg)
      (forward-line 1)
      (let ((start (point)))
        (and (> pos start)
             (string-match-p "\\`[ \t\n]*\\'"
                             (buffer-substring-no-properties start pos)))))))

(defun emagent-chat--collapse-thinking-leading-blanks ()
  "Delete the blank run after `** Thinking' up to point."
  (when-let ((beg (emagent-chat--open-reasoning-begin)))
    (save-excursion
      (goto-char beg)
      (forward-line 1)
      (let ((start (point)))
        (when (< start (point))
          (delete-region start (point))
          (goto-char start))))))

(defun emagent-chat--newlines-before-point ()
  "Return the count of consecutive newlines immediately before point."
  (save-excursion
    (let ((n 0))
      (while (eq (char-before) ?\n)
        (setq n (1+ n))
        (backward-char))
      n)))

(defun emagent-chat--newlines-after-point ()
  "Return the count of consecutive newlines immediately after point."
  (save-excursion
    (let ((n 0))
      (while (eq (char-after) ?\n)
        (setq n (1+ n))
        (forward-char))
      n)))

(defun emagent-chat--insert-reasoning-text (text)
  "Insert TEXT at `emagent-chat--thought-marker' in the open Thinking content.
Prose that resumes after a tool line or src block is separated from it by
exactly one blank line, so the two never glue onto the same line.  Some
agents stream bare paragraph-break deltas (chunks that are only newlines)
while still composing; a run of those is collapsed to at most one blank
line rather than piling up as a growing blank tail.

The streaming marker re-syncs to the content tail by skipping back over
trailing newlines (see `emagent-chat--reasoning-stream-marker'), which
strands the previous chunk's own trailing newlines after point.  That run is
folded into TEXT's leading separation, so a thought that resumes after a
close/reopen cycle (each assistant chunk closes the thought) neither glues
onto the previous one nor grows the blank tail two lines per cycle.  When
the run separates the content from following text — the `** Response'
headline, or a tool line's trailing newline — it is structural and is
re-inserted after TEXT, leaving the marker at the true content end."
  (cl-block emagent-chat--insert-reasoning-text
    (let* ((safe (replace-regexp-in-string
                "\n\\{3,\\}" "\n\n"
                (emagent-chat--escape-reasoning-text text (not (bolp)))))
         (before (emagent-chat--newlines-before-point))
         (after (emagent-chat--newlines-after-point))
         (tail-sep (when (> after 0)
                     (save-excursion
                       (forward-char after)
                       (unless (eobp) (min after 2))))))
    (when (> after 0)
      (delete-char after)
      (setq safe (replace-regexp-in-string
                  "\\`\n\\{3,\\}" "\n\n"
                  (concat (make-string after ?\n) safe))))
    (when (emagent-chat--thinking-leading-blank-p)
      (setq safe (replace-regexp-in-string "\\`[\n\r]+" "" safe))
      (if (string-empty-p (string-trim safe))
          (progn
            (emagent-chat--collapse-thinking-leading-blanks)
            (cl-return-from emagent-chat--insert-reasoning-text nil))
        (emagent-chat--collapse-thinking-leading-blanks)
        (setq before (emagent-chat--newlines-before-point)
              after (emagent-chat--newlines-after-point)
              tail-sep (when (> after 0)
                         (save-excursion
                           (forward-char after)
                           (unless (eobp) (min after 2)))))))
    (cond
     ;; Resuming after a tool line/block: keep exactly one blank line of
     ;; separation.
     ((emagent-chat--reasoning-after-tool-artifact-p)
      (setq safe (concat (make-string (max 0 (- 2 before)) ?\n)
                         (replace-regexp-in-string "\\`[\n\r]+" "" safe))))
     ;; TEXT itself opens with blank line(s): only trim when that run,
     ;; combined with the newlines already before point, would exceed one
     ;; blank line — a lone paragraph break is left untouched.
     ((string-match "\\`\n+" safe)
      (let ((leading (match-end 0)))
        (when (> (+ before leading) 2)
          (setq safe (concat (make-string (max 0 (- 2 before)) ?\n)
                             (substring safe leading)))))))
    (when (string-empty-p safe)
      (cl-return-from emagent-chat--insert-reasoning-text nil))
    (if (not tail-sep)
        (insert safe)
      ;; Structural separation follows: share it with SAFE's own trailing
      ;; newlines (a paragraph break at the tail and the separation are the
      ;; same blank line) and keep point before it, at the content end.
      (let ((trailing 0))
        (when (string-match "\n+\\'" safe)
          (setq trailing (- (match-end 0) (match-beginning 0))
                safe (substring safe 0 (match-beginning 0))))
        (insert safe)
        (save-excursion
          (insert (make-string (max tail-sep (min trailing 2)) ?\n))))))))

(defun emagent-chat--inject-reasoning-thought (thought-text)
  "Insert THOUGHT-TEXT under `** Thinking' when reasoning was not streamed.

Create the `** Thinking' subsection on demand, since responses no longer
open one eagerly."
  (let ((trimmed (string-trim (or thought-text ""))))
    (when (not (string-empty-p trimmed))
      (unless (emagent-chat--open-reasoning-begin)
        (emagent-chat--insert-reasoning-scaffold))
      (when-let ((beg (emagent-chat--open-reasoning-begin)))
        (save-excursion
          (goto-char beg)
          (forward-line 1)
          (insert (emagent-chat--escape-reasoning-text trimmed))
          (unless (bolp) (insert "\n")))))))

(defun emagent-chat--remove-empty-thinking ()
  "Remove the open `** Thinking' headline when its content is blank.

Scoped to the in-flight response so it never deletes a `** Thinking'
subsection that belongs to an earlier response."
  (when-let ((beg (emagent-chat--open-reasoning-begin)))
    (let ((content-start (save-excursion (goto-char beg) (line-end-position)))
          (content-end (emagent-chat--reasoning-block-tail)))
      (when (and content-end
                 (string-empty-p
                  (string-trim
                   (buffer-substring-no-properties content-start content-end))))
        (save-excursion
          (goto-char beg)
          (delete-region (line-beginning-position) content-end))
        t))))

(provide 'emagent-chat-thought)
;;; emagent-chat-thought.el ends here