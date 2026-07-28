;;; emagent-chat-response.el --- assistant response module  -*- lexical-binding: t; -*-

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
;;
;; Assistant response rendering, thought/reasoning, and compression.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'org)
(require 'emagent-chat-input)
(require 'emagent-chat-ui)
(require 'emagent-log)
(require 'emagent-session)

(defun emagent-chat--open-reasoning-begin ()
  "Return point at the `** Thinking' headline in the open response body.
Read from the owned `emagent-chat--thinking-headline-marker' when set; otherwise
locate the headline by search and cache it."
  (if (and emagent-chat--thinking-headline-marker
           (marker-position emagent-chat--thinking-headline-marker)
           (emagent-chat--open-response-p))
      (save-excursion
        (goto-char emagent-chat--thinking-headline-marker)
        (beginning-of-line)
        (when (looking-at emagent-chat--thinking-headline-re)
          (marker-position emagent-chat--thinking-headline-marker)))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--thinking-headline-re (cdr bounds) t)
          (setq emagent-chat--thinking-headline-marker
                (copy-marker (match-beginning 0) nil))
          (match-beginning 0))))))

(defun emagent-chat--open-switching-begin ()
  "Return point at the `** Switching model' headline, or nil."
  (if (and emagent-chat--thinking-headline-marker
           (marker-position emagent-chat--thinking-headline-marker)
           emagent-chat--switching-model-p
           (emagent-chat--open-response-p))
      (save-excursion
        (goto-char emagent-chat--thinking-headline-marker)
        (beginning-of-line)
        (when (looking-at emagent-chat--switching-headline-re)
          (marker-position emagent-chat--thinking-headline-marker)))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--switching-headline-re (cdr bounds) t)
          (setq emagent-chat--thinking-headline-marker
                (copy-marker (match-beginning 0) nil)
                emagent-chat--switching-model-p t)
          (match-beginning 0))))))

(defun emagent-chat--open-preparing-begin ()
  "Return point at the `** Preparing…' headline, or nil."
  (if (and emagent-chat--thinking-headline-marker
           (marker-position emagent-chat--thinking-headline-marker)
           emagent-chat--preparing-p
           (emagent-chat--open-response-p))
      (save-excursion
        (goto-char emagent-chat--thinking-headline-marker)
        (beginning-of-line)
        (when (looking-at emagent-chat--preparing-headline-re)
          (marker-position emagent-chat--thinking-headline-marker)))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--preparing-headline-re (cdr bounds) t)
          (setq emagent-chat--thinking-headline-marker
                (copy-marker (match-beginning 0) nil)
                emagent-chat--preparing-p t)
          (match-beginning 0))))))

(defun emagent-chat--thinking-headline-text (&optional model-id)
  "Return the `** Thinking' headline, optionally suffixing MODEL-ID link."
  (if model-id
      (concat emagent-chat-thinking-headline " "
              (emagent-chat--model-link model-id))
    emagent-chat-thinking-headline))

(defun emagent-chat--switching-headline-text (model-id)
  "Return the `** Switching model' headline for per-turn MODEL-ID."
  (concat emagent-chat-switching-headline " to "
          (emagent-chat--model-link model-id) "…"))

(defun emagent-chat--insert-switching-scaffold ()
  "Insert a `** Switching model' subsection at the response body start."
  (when (and emagent-chat--turn-model
             emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (setq emagent-chat--thinking-headline-marker (copy-marker (point) nil)
          emagent-chat--switching-model-p t
          emagent-chat--preparing-p nil
          emagent-chat--thought-open-p nil
          emagent-chat--thought-marker nil)
    (insert (emagent-chat--switching-headline-text emagent-chat--turn-model) "\n")
    (setq emagent-chat--assistant-marker (copy-marker (point) nil))))

(defun emagent-chat--insert-preparing-scaffold ()
  "Insert a `** Preparing…' subsection at the response body start."
  (when (and emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (setq emagent-chat--thinking-headline-marker (copy-marker (point) nil)
          emagent-chat--preparing-p t
          emagent-chat--switching-model-p nil
          emagent-chat--thought-open-p nil
          emagent-chat--thought-marker nil)
    (insert emagent-chat-preparing-headline "\n")
    (setq emagent-chat--assistant-marker (copy-marker (point) nil))))

(defun emagent-chat--promote-switching-to-thinking ()
  "Replace a `** Switching model' headline with `** Thinking'."
  (when-let ((beg (emagent-chat--open-switching-begin)))
    (goto-char beg)
    (delete-region (line-beginning-position) (line-end-position))
    (insert (emagent-chat--thinking-headline-text emagent-chat--turn-model))
    (unless (bolp) (insert "\n"))
    (setq emagent-chat--switching-model-p nil
          emagent-chat--thought-open-p t
          emagent-chat--thought-marker (copy-marker (point) nil)
          emagent-chat--assistant-marker (copy-marker (point) nil))))

(defun emagent-chat--promote-preparing-to-thinking ()
  "Replace a `** Preparing…' headline with `** Thinking'."
  (when-let ((beg (emagent-chat--open-preparing-begin)))
    (goto-char beg)
    (delete-region (line-beginning-position) (line-end-position))
    (insert (emagent-chat--thinking-headline-text emagent-chat--turn-model))
    (unless (bolp) (insert "\n"))
    (setq emagent-chat--preparing-p nil
          emagent-chat--thought-open-p t
          emagent-chat--thought-marker (copy-marker (point) nil)
          emagent-chat--assistant-marker (copy-marker (point) nil))))

(defun emagent-chat--promote-transient-to-thinking ()
  "Promote preparing/switching headline to `** Thinking' when a turn begins."
  (when (emagent-chat--open-response-p)
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (when (emagent-chat--open-switching-begin)
        (emagent-chat--promote-switching-to-thinking))
      (when (emagent-chat--open-preparing-begin)
        (emagent-chat--promote-preparing-to-thinking)))))

(defun emagent-chat--clear-switching-scaffold ()
  "Remove a `** Switching model' headline from the open response, if present."
  (when-let ((beg (emagent-chat--open-switching-begin)))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (goto-char beg)
      (delete-region (line-beginning-position)
                     (min (point-max) (1+ (line-end-position)))))
    (setq emagent-chat--switching-model-p nil
          emagent-chat--thinking-headline-marker nil)))

(defun emagent-chat--clear-preparing-scaffold ()
  "Remove a `** Preparing…' headline from the open response, if present."
  (when-let ((beg (emagent-chat--open-preparing-begin)))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (goto-char beg)
      (delete-region (line-beginning-position)
                     (min (point-max) (1+ (line-end-position)))))
    (setq emagent-chat--preparing-p nil
          emagent-chat--thinking-headline-marker nil)))

(defun emagent-chat--clear-transient-reasoning-scaffold ()
  "Remove any transient preparing/switching headline from the open response."
  (emagent-chat--clear-switching-scaffold)
  (emagent-chat--clear-preparing-scaffold))

(defun emagent-chat--insert-reasoning-scaffold ()
  "Insert an empty `** Thinking' subsection at the response body start."
  (when (and emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (setq emagent-chat--thinking-headline-marker (copy-marker (point) nil))
    (insert (emagent-chat--thinking-headline-text emagent-chat--turn-model) "\n")
    (setq emagent-chat--thought-marker (point-marker)
          emagent-chat--thought-open-p t
          emagent-chat--assistant-marker (point-marker))))

(defun emagent-chat--thinking-content-end (begin limit)
  "Return where the Thinking content ends after BEGIN, before LIMIT.

That is the start of the `** Response' headline when present, otherwise LIMIT.
Uses the owned `emagent-chat--response-content-marker' (the Response headline
sits on the line just above it) so it does not scan to LIMIT for a Response
headline that has not been created yet — that scan was O(reasoning^2) while
reasoning streamed with no Response below."
  (if (and emagent-chat--response-content-marker
           (marker-position emagent-chat--response-content-marker)
           (< begin (marker-position emagent-chat--response-content-marker)))
      ;; Response headline exists (owned marker sits on the line below it).
      (save-excursion
        (goto-char emagent-chat--response-content-marker)
        (forward-line -1)
        (line-beginning-position))
    ;; No Response headline: reasoning owns the marker whenever the headline is
    ;; created, so a nil marker in this streaming path means Thinking runs to
    ;; the end of the region.
    limit))

(defun emagent-chat--ensure-reasoning-scaffold ()
  "Ensure the open response has a `** Thinking' subsection ready to stream."
  (when (emagent-chat--open-response-p)
    (when (emagent-chat--open-switching-begin)
      (emagent-chat--promote-switching-to-thinking))
    (when (emagent-chat--open-preparing-begin)
      (emagent-chat--promote-preparing-to-thinking))
    (cond
     (emagent-chat--thought-open-p
      (emagent-chat--sync-thought-marker))
     ((not (emagent-chat--open-reasoning-begin))
      (emagent-chat--insert-reasoning-scaffold))
     (t
      (let ((stream (emagent-chat--reasoning-stream-marker)))
        (setq emagent-chat--thought-marker stream
              emagent-chat--thought-open-p t)
        (unless stream
          (emagent-log "emagent: cannot find reasoning stream marker after ensure")))))))

(defun emagent-chat--reasoning-stream-marker ()
  "Return the insert marker at the end of the open Thinking content.

Streamed reasoning is inserted here, before any `** Response' headline."
  (when-let ((tail (emagent-chat--reasoning-block-tail)))
    (save-excursion
      (goto-char tail)
      (skip-chars-backward "\n" (or (emagent-chat--open-response-begin) (point-min)))
      (point-marker))))

(defun emagent-chat--reasoning-block-tail ()
  "Return point at the end of the open Thinking content, or nil.

This is the start of the `** Response' headline when present, otherwise the
end of the open response body."
  (when-let* ((bounds (emagent-chat--open-response-body-bounds))
              (beg (emagent-chat--open-reasoning-begin)))
    (emagent-chat--thinking-content-end
     (save-excursion (goto-char beg) (line-end-position))
     (cdr bounds))))

(defun emagent-chat--sync-thought-marker ()
  "Realign `emagent-chat--thought-marker' to the true Thinking tail."
  (when emagent-chat--thought-open-p
    (when-let ((stream (emagent-chat--reasoning-stream-marker)))
      (let ((cur (and emagent-chat--thought-marker
                      (marker-position emagent-chat--thought-marker))))
        (when (or (not cur) (< cur (marker-position stream)))
          (setq emagent-chat--thought-marker stream))))))

(defun emagent-chat--ensure-thought-stream ()
  "Open or resume the streaming Thinking subsection in the in-flight response."
  (emagent-chat--ensure-reasoning-scaffold))

(defun emagent-chat--format-thought-block (text)
  "Return `** Thinking' markup for reasoning TEXT, or \"\" when empty."
  (let ((trimmed (string-trim (or text ""))))
    (if (string-empty-p trimmed)
        ""
      (format "%s\n%s\n\n"
              emagent-chat-thinking-headline
              (emagent-chat--escape-reasoning-text trimmed)))))

(defun emagent-chat--reasoning-block-bounds ()
  "Return (CONTENT-START . CONTENT-END) for Thinking at point."
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
                     (body (string-trim-right
                            (substring text body-start body-end))))
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

(defun emagent-chat--end-send-pending-if-active ()
  "End the pre-dispatch phase once agent output begins arriving."
  (emagent-chat--send-pending-end))

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

(defun emagent-chat--schedule-thought-flush ()
  "Debounce reasoning insertion using `emagent-chat-thought-stream-delay'."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer))
  (setq emagent-chat--thought-flush-timer
        (run-with-timer emagent-chat-thought-stream-delay nil
                        (lambda ()
                          (setq emagent-chat--thought-flush-timer nil)
                          (emagent-chat--flush-thought-pending)))))

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

(defvar-local emagent-chat--follow-output nil
  "Non-nil when this buffer should keep the live response in view.

Set when the user sends a prompt or the window sits on the live tail;
cleared when they scroll so the follow position is off-screen or move
point into earlier history.")

(defun emagent-chat--bare-slash-command-p (text)
  "Return non-nil when TEXT is a single-line slash command."
  (let ((trimmed (string-trim text)))
    (and (not (string-empty-p trimmed))
         (string-prefix-p "/" trimmed)
         (not (string-match-p "\n" trimmed))
         (let* ((body (substring trimmed 1))
                (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
                (cmd (if space (substring body 0 space) body)))
           (and (> (length cmd) 0)
                (string-match-p "\\`[-a-z0-9:]+\\'" cmd))))))

(defun emagent-chat--compress-command-p (text)
  "Return non-nil when TEXT is a conversation compression slash command."
  (let ((trimmed (string-trim text)))
    (when (string-prefix-p "/" trimmed)
      (let* ((body (substring trimmed 1))
             (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
             (cmd (if space (substring body 0 space) body)))
        (member cmd '("compress" "compact" "summarize"))))))

(defconst emagent-chat--compress-history-limit 200000
  "Maximum conversation chars included in a /compress request.")

(defun emagent-chat--compress-boundary ()
  "Return point at the user heading before an open response, or nil."
  (save-excursion
    (when-let ((resp (emagent-chat--find-open-response-begin)))
      (goto-char resp)
      (when (re-search-backward (emagent-chat--user-heading-re) nil t)
        (line-beginning-position)))))

(defun emagent-chat--conversation-history-text ()
  "Return prior conversation text for /compress, or \"\"."
  (save-excursion
    (let* ((zone (emagent-session-store-metadata-end))
           (end (or (emagent-chat--compress-boundary) (point))))
      (when (and end (> end zone))
        (string-trim (buffer-substring-no-properties zone end))))))

(defun emagent-chat--compress-prompt-text (history)
  "Return a summarization prompt for compression using HISTORY."
  (let ((body (if (> (length history) emagent-chat--compress-history-limit)
                  (concat (substring history 0 emagent-chat--compress-history-limit)
                          "\n\n[...truncated for compression request...]")
                history)))
    (format "Summarize the conversation below for context compression. Preserve key decisions, file paths, errors, and open tasks. Output only the summary.\n\n<conversation>\n%s\n</conversation>"
            body)))

(defun emagent-chat--begin-response (&optional at)
  "Open a new emagent response at AT or point.

Set up the response body markers but do not insert a `** Thinking'
subsection yet.  Thinking is created lazily, only when reasoning or a tool
line actually arrives, so responses without reasoning never show an empty
Thinking block."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    ;; Pin follow for this turn; cleared if the user scrolls the live end
    ;; off-screen or moves point into earlier history.
    (setq emagent-chat--follow-output t)
    (goto-char (or at (point)))
    (unless (bolp)
      (insert "\n"))
    (insert "\n")
    (setq emagent-chat--response-body-start (copy-marker (point) nil)
          emagent-chat--assistant-marker (copy-marker (point) nil)
          emagent-chat--response-end-marker
          (save-excursion
            (if (re-search-forward (emagent-chat--user-heading-re) nil t)
                (copy-marker (line-beginning-position) nil)
              'point-max))
          ;; The Response headline / Thinking headline don't exist yet; clear
          ;; their owned markers so a stale one from a prior turn can't mislocate
          ;; inserted body text even if a reset was skipped.
          emagent-chat--response-content-marker nil
          emagent-chat--thinking-headline-marker nil
          emagent-chat--switching-model-p nil
          emagent-chat--preparing-p nil
          emagent-chat--thought-marker nil
          emagent-chat--thought-open-p nil
          emagent-chat--reasoning-streamed-p nil)))

(defun emagent-chat-insert-system (message)
  "Append system MESSAGE to `emagent-log-buffer-name'."
  (emagent-log "%s" message))

(defun emagent-chat-start-assistant ()
  "Begin a new emagent response section."
  (with-current-buffer (current-buffer)
    (emagent-chat--begin-response)))

(defun emagent-chat--goto-response-insertion-point ()
  "Go to the tail of the open emagent response at or before point."
  (cond
   ((and emagent-chat--assistant-marker
         (marker-position emagent-chat--assistant-marker))
    (goto-char emagent-chat--assistant-marker))
   (t
    (goto-char (point-max)))))

(defun emagent-chat--finish-response-spacing ()
  "Ensure a trailing blank line after a finalized response body."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (unless (save-excursion (forward-line -1) (looking-at-p "[ \t]*$"))
    (insert "\n")))

(defun emagent-chat--cancel-response-flush ()
  "Cancel the assistant flush timer and drop pending text without inserting."
  (when emagent-chat--response-flush-timer
    (cancel-timer emagent-chat--response-flush-timer)
    (setq emagent-chat--response-flush-timer nil))
  (setq emagent-chat--response-pending ""))

(defun emagent-chat--reset-response-state ()
  "Clear response markers and cancel pending stream flushes."
  (emagent-chat--cancel-thought-flush)
  (emagent-chat--cancel-response-flush)
  (setq emagent-chat--assistant-marker nil
        emagent-chat--response-body-start nil
        emagent-chat--response-content-marker nil
        emagent-chat--response-end-marker nil
        emagent-chat--thought-open-p nil
        emagent-chat--switching-model-p nil
        emagent-chat--preparing-p nil
        emagent-chat--thinking-headline-marker nil
        emagent-chat--thought-marker nil
        emagent-chat--reasoning-streamed-p nil
        emagent-chat--fence-state nil
        emagent-chat--response-fence-state nil
        emagent-chat--response-pending ""
        emagent-chat--permission-pending nil)
  (if emagent-chat--tool-call-lines
      (clrhash emagent-chat--tool-call-lines)
    (setq-local emagent-chat--tool-call-lines (make-hash-table :test 'equal))))

(defvar-local emagent-chat--response-fence-state nil
  "Streaming fence state for the response body.
Nil when outside a fenced block; (lang . body-so-far) while buffering.
Mirrors emagent-chat--fence-state but tracks the `** Response' stream.")

(defun emagent-chat--fail-response-p ()
  "Return non-nil when an emagent response is open and can be closed with error."
  (emagent-chat--open-response-p))

(defun emagent-chat--response-body-text ()
  "Return the current `** Response' body text, or nil when unavailable.

Spans from just after the `** Response' headline (the content marker) to
the end of the open response (the next user heading, or `point-max')."
  (when-let* ((bounds (emagent-chat--response-body-bounds))
              (start (car bounds))
              (end (cdr bounds))
              ((<= start end)))
    (buffer-substring-no-properties start end)))

(defun emagent-chat--ensure-response-headline ()
  "Ensure the open response has a `** Response' headline; return its content start."
  (or (car (emagent-chat--response-body-bounds))
      (let ((tail (emagent-chat--reasoning-block-tail)))
        (if tail
            ;; A `** Thinking' subsection exists: place Response after its content.
            (progn
              (goto-char tail)
              (skip-chars-backward "\n" (or (emagent-chat--open-response-begin)
                                            (point-min)))
              ;; Replace whatever newline run the reasoning stream left at its
              ;; tail with exactly one blank line — inserting the headline
              ;; before the run used to push those stray blank lines into the
              ;; Response body.
              (delete-region (point) tail)
              (insert "\n\n" emagent-chat-response-headline "\n")
              (setq emagent-chat--response-content-marker (copy-marker (point) nil))
              (point))
          ;; No reasoning was rendered: place Response at the response body start.
          (when-let ((beg (emagent-chat--open-response-begin)))
            (goto-char beg)
            (insert emagent-chat-response-headline "\n")
            (setq emagent-chat--response-content-marker (copy-marker (point) nil))
            (point))))))

(defun emagent-chat--schedule-response-flush ()
  "Debounce assistant insertion using `emagent-chat-response-stream-delay'."
  (when emagent-chat--response-flush-timer
    (cancel-timer emagent-chat--response-flush-timer))
  (setq emagent-chat--response-flush-timer
        (run-with-timer emagent-chat-response-stream-delay nil
                        (lambda ()
                          (setq emagent-chat--response-flush-timer nil)
                          (emagent-chat--flush-response-pending)))))

(defun emagent-chat--insert-assistant-now (text)
  "Insert assistant TEXT into the open Response block after fence conversion."
  (emagent-chat--end-send-pending-if-active)
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (when (emagent-chat--open-response-p)
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat-close-thought)
            (let* ((result (emagent-chat--feed-fences
                            text emagent-chat--response-fence-state))
                   (safe (emagent-chat--demote-response-headings
                          (emagent-chat--map-outside-src-blocks
                           (lambda (s)
                             (let ((case-fold-search nil))
                               (emagent-chat--convert-markdown-tables
                                (emagent-chat--convert-markdown-headings
                                 (emagent-chat--convert-inline-code-spans
                                  (replace-regexp-in-string
                                   "\\*\\*\\([^*\n]+\\)\\*\\*" "*\\1*"
                                   (replace-regexp-in-string
                                    "\\[\\([^][\n]+\\)\\](\\([^)\n]+\\))"
                                    "[[\\2][\\1]]"
                                    s)))))))
                           (car result))))
                   (existing (emagent-chat--response-body-bounds))
                   (insert-at
                    (cond
                     ((and existing
                           emagent-chat--assistant-marker
                           (marker-position emagent-chat--assistant-marker)
                           (>= (marker-position emagent-chat--assistant-marker)
                               (car existing)))
                      (marker-position emagent-chat--assistant-marker))
                     (existing (cdr existing))
                     (t (emagent-chat--ensure-response-headline)))))
              (setq emagent-chat--response-fence-state (cdr result))
              (when (and insert-at (not (string-empty-p safe)))
                (save-excursion
                  (goto-char insert-at)
                  (let ((beg (point)))
                    (insert safe)
                    (emagent-chat--demote-headlines-in-region beg (point))
                    (setq emagent-chat--assistant-marker (point-marker))))))
            (emagent-chat--maybe-font-lock-flush)))))))

(defun emagent-chat--flush-response-pending (&optional _final)
  "Insert any batched assistant text into the open Response block.
With FINAL non-nil, emit everything buffered (used before finish/fail)."
  (when emagent-chat--response-flush-timer
    (cancel-timer emagent-chat--response-flush-timer)
    (setq emagent-chat--response-flush-timer nil))
  (let ((text emagent-chat--response-pending))
    (setq emagent-chat--response-pending "")
    (when (not (string-empty-p text))
      (emagent-chat--insert-assistant-now text))))

(defun emagent-chat-append-assistant (text)
  "Append streamed assistant TEXT under the `** Response' subsection.
Each chunk is passed through the streaming markdown->org converter so the
buffer shows formatted org while the response is still arriving.  Chunks
are batched using `emagent-chat-response-stream-delay'."
  ;; Normalize line endings so CRLF/CR output does not leave stray ^M in the
  ;; buffer or defeat the LF-based fence/src-block segmentation below.
  (setq text (replace-regexp-in-string "\r\n?" "\n" text))
  (when (not (string-empty-p text))
    (setq emagent-chat--response-pending
          (concat emagent-chat--response-pending text))
    (if (or noninteractive (<= emagent-chat-response-stream-delay 0))
        (emagent-chat--flush-response-pending)
      (emagent-chat--schedule-response-flush))))

(defun emagent-chat-fail-assistant (message)
  "Close the in-flight emagent response with error MESSAGE under `** Response'."
  (emagent-chat--flush-response-pending t)
  (emagent-chat--send-pending-end)
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t))
          (emagent-chat--writable)
          (when (emagent-chat--fail-response-p)
            (emagent-chat--clear-transient-reasoning-scaffold)
            (emagent-chat-close-thought)
            (emagent-chat--ensure-response-headline)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "\n*Error:* %s\n" message))
            (emagent-chat--finish-response-spacing)
            (emagent-chat--reset-response-state)
            (emagent-chat--sync-user-zone-marker)
            (emagent-chat--maybe-font-lock-flush))))))
  (emagent-chat--flush-deferred-font-lock)
  (emagent-chat--insert-user-heading-stub))

(defun emagent-chat--finalize-streamed-assistant (converted)
  "Replace the `** Response' body with CONVERTED assistant text.

When the body text already streamed into the buffer equals CONVERTED,
trimming trailing whitespace, skip the `delete-region' + reinsert
entirely so a finish that mirrors the stream does not blank and redraw
the whole response body.

Org tables are aligned synchronously here.  An idle timer is unreliable:
ACP/MCP process output keeps resetting Emacs idle, so deferred align
often never ran and left pipe tables unaligned until a manual TAB."
  ;; Streaming fence state is no longer needed — finalization replaces the
  ;; buffer content with the fully-converted text.
  (setq emagent-chat--response-fence-state nil)
  (when-let ((content-start (or (car (emagent-chat--response-body-bounds))
                                (emagent-chat--ensure-response-headline))))
    (let* ((body-end (cdr (emagent-chat--open-response-body-bounds)))
           (existing (emagent-chat--response-body-text))
           (unchanged (and existing
                           (string-equal (string-trim-right existing)
                                         (string-trim-right converted))))
           (end (if unchanged
                    body-end
                  (progn
                    (when (and body-end (< content-start body-end))
                      (delete-region content-start body-end))
                    (goto-char content-start)
                    (insert converted)
                    (point)))))
      (when (and end (string-match-p "|" converted))
        (let ((was-modified (buffer-modified-p)))
          (unwind-protect
              (ignore-errors
                (emagent-chat--align-org-tables-in-region content-start end))
            (set-buffer-modified-p was-modified))))
      (setq emagent-chat--assistant-marker (copy-marker end nil)))))

(defvar emagent-chat--finish-close t
  "When non-nil, `emagent-chat-finish-assistant' resets markers and inserts a stub.

ACP prompt rendering binds this to nil so late agent chunks that arrive
while a debounced finish is in flight can still update the open response;
the render loop closes the response only after assistant text is stable.")

(defvar-local emagent-chat--pending-hide-reasoning nil
  "Buffer position to fold after a deferred finish close, or nil.")

(defun emagent-chat--close-finished-response ()
  "Reset response markers and insert the next user heading stub.

Called once the ACP finish render has settled (no newer assistant text)."
  (emagent-chat--flush-response-pending t)
  (when emagent-chat--pending-hide-reasoning
    (emagent-chat--hide-reasoning-deferred
     emagent-chat--pending-hide-reasoning)
    (setq emagent-chat--pending-hide-reasoning nil))
  (emagent-chat--reset-response-state)
  (emagent-chat--sync-user-zone-marker)
  ;; Font-lock before the stub so settle work cannot move point off the
  ;; prompt input position.
  (emagent-chat--flush-deferred-font-lock)
  (emagent-chat--insert-user-heading-stub))

(defun emagent-chat--demote-headlines-in-region (beg end)
  "Demote org headlines at level 1-2 between BEG and END to level 3.

Used after streaming inserts so a `**' split across chunks cannot leave a
level-2 sibling of `** Response' under the user heading."
  (save-excursion
    (goto-char beg)
    (forward-line 0)
    (while (< (point) end)
      (when (looking-at "^\\*\\{1,2\\} ")
        (let* ((star-beg (match-beginning 0))
               (star-end (1- (match-end 0)))
               (old-n (- star-end star-beg)))
          (delete-region star-beg star-end)
          (insert "***")
          (setq end (+ end (- 3 old-n)))))
      (forward-line 1))))

(defun emagent-chat-finish-assistant (text &optional thought-text)
  "Finalize the latest emagent response with TEXT.

Render the assistant answer under `** Response'.  When reasoning was streamed
keep its `** Thinking' subsection; otherwise build one from THOUGHT-TEXT.
A response without any reasoning has no `** Thinking' subsection at all.

When `emagent-chat--finish-close' is nil, leave the response open (no stub)
so a subsequent finish can replace the body if more assistant text arrives."
  (emagent-chat--flush-response-pending t)
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t)
              (converted (emagent-chat--demote-response-headings
                          (emagent-chat--convert-agent-markup text)))
              (hide-at nil))
          (emagent-chat--writable)
          (when (emagent-chat--open-response-p)
            (unless emagent-chat--reasoning-streamed-p
              (emagent-chat--inject-reasoning-thought thought-text))
            (unless (emagent-chat--open-reasoning-begin)
              (emagent-chat--clear-transient-reasoning-scaffold))
            (setq hide-at (emagent-chat--open-reasoning-begin))
            (emagent-chat-close-thought)
            (when (emagent-chat--remove-empty-thinking)
              (setq hide-at nil))
            (emagent-chat--finalize-streamed-assistant converted)
            (emagent-chat--finish-response-spacing)
            (emagent-chat--maybe-font-lock-flush)
            (if emagent-chat--finish-close
                (progn
                  (when hide-at
                    (emagent-chat--hide-reasoning-deferred hide-at))
                  (emagent-chat--reset-response-state)
                  (emagent-chat--sync-user-zone-marker))
              (setq emagent-chat--pending-hide-reasoning hide-at)))))))
  (when emagent-chat--finish-close
    ;; Font-lock then stub after stable view is restored, so point ends at
    ;; the user prompt input rather than being restored to the entry position.
    (emagent-chat--flush-deferred-font-lock)
    (emagent-chat--insert-user-heading-stub)))

(provide 'emagent-chat-response)
;;; emagent-chat-response.el ends here
