;;; emagent-chat-render.el --- render module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'map)
(require 'emagent-log)
(require 'emagent-chat-header)
(require 'emagent-chat-markup)
(require 'emagent-chat-reasoning)
(require 'emagent-tools)

(defun emagent-chat--begin-response (&optional at)
  "Open a new emagent response at AT or point.

Set up the response body markers but do not insert a `** Thinking'
subsection yet.  Thinking is created lazily, only when reasoning or a tool
line actually arrives, so responses without reasoning never show an empty
Thinking block."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (goto-char (or at (point)))
    (unless (bolp)
      (insert "\n"))
    (insert "\n")
    (setq emagent-chat--response-body-start (copy-marker (point) nil)
          emagent-chat--assistant-marker (copy-marker (point) nil)
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

(declare-function emagent-chat--notify-inactive-update "emagent-chat")

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

(defun emagent-chat--reset-response-state ()
  (emagent-chat--cancel-thought-flush)
  (setq emagent-chat--assistant-marker nil
        emagent-chat--response-body-start nil
        emagent-chat--thought-open-p nil
        emagent-chat--thought-marker nil
        emagent-chat--reasoning-streamed-p nil
        emagent-chat--fence-state nil
        emagent-chat--permission-pending nil)
  (clrhash emagent-chat--tool-call-lines))

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

(defvar-local emagent-chat--permission-pending nil
  "Non-nil while a permission dialog is active in the current buffer.
New tool-call lines are suppressed while a dialog awaits user input so the
thinking block stays stable until the user responds.")

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
                                body))
                      parts)
                (setq pos close-end))))))))
    (unless incomplete
      (push (substring text pos) parts))
    (cons (apply #'concat (nreverse parts)) incomplete)))

(defun emagent-chat--schedule-thought-flush ()
  "Debounce reasoning inserts using `emagent-chat-thought-stream-delay'."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer))
  (setq emagent-chat--thought-flush-timer
        (run-with-timer emagent-chat-thought-stream-delay nil
                        (lambda ()
                          (setq emagent-chat--thought-flush-timer nil)
                          (emagent-chat--flush-thought-pending)))))

(defun emagent-chat--insert-thought-now (text)
  "Insert reasoning TEXT at the open Thinking marker."
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

(defun emagent-chat--flush-thought-pending ()
  "Insert any batched reasoning text into the open Thinking block.
Converts complete markdown code fences to org src blocks; buffers incomplete
fences in `emagent-chat--fence-state' until the closing ``` arrives."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer)
    (setq emagent-chat--thought-flush-timer nil))
  (let ((text emagent-chat--thought-pending))
    (setq emagent-chat--thought-pending "")
    (when (not (string-empty-p text))
      ;; Prepend any previously buffered fence content before processing
      (let* ((fence emagent-chat--fence-state)
             (combined (if fence
                           (concat "```" (car fence) "\n" (cdr fence) text)
                         text))
             (result (emagent-chat--split-fences combined))
             (to-insert (car result))
             (new-fence (cdr result)))
        (setq emagent-chat--fence-state new-fence)
        (when (not (string-empty-p to-insert))
          (emagent-chat--with-streaming-view
           (lambda ()
             (emagent-chat--insert-thought-now to-insert))))))))

(defun emagent-chat--ensure-response-markers ()
  "Set body markers for the open response when they were lost."
  (unless (and emagent-chat--response-body-start
               (marker-position emagent-chat--response-body-start)
               emagent-chat--assistant-marker
               (marker-position emagent-chat--assistant-marker))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (setq emagent-chat--response-body-start (copy-marker (car bounds) nil)
            emagent-chat--assistant-marker (copy-marker (cdr bounds) nil)))))

(defun emagent-chat--fail-response-p ()
  "Return non-nil when an emagent response is open and can be closed with error."
  (emagent-chat--open-response-p))

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
  (emagent-chat--flush-thought-pending)
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
  (insert "\n")
  (setq emagent-chat--thought-marker (copy-marker (point) nil)))

(defun emagent-chat--reasoning-after-tool-artifact-p ()
  "Return non-nil when point follows a tool line or src block, modulo blanks."
  (save-excursion
    (skip-chars-backward " \t\n")
    (beginning-of-line)
    (looking-at-p "\\(?:→ \\|#\\+[Ee][Nn][Dd]_[Ss][Rr][Cc]\\)")))

(defun emagent-chat--newlines-before-point ()
  "Return the count of consecutive newlines immediately before point."
  (save-excursion
    (let ((n 0))
      (while (eq (char-before) ?\n)
        (setq n (1+ n))
        (backward-char))
      n)))

(defun emagent-chat--insert-reasoning-text (text)
  "Insert TEXT at `emagent-chat--thought-marker' in the open Thinking content.
Prose that resumes after a tool line or src block is separated from it by
exactly one blank line, so the two never glue onto the same line."
  (let ((safe (emagent-chat--escape-reasoning-text text)))
    (cond
     ;; Right after the `** Thinking' headline: drop any leading blank lines.
     ((save-excursion
        (beginning-of-line)
        (and (looking-at "[ \t]*$")
             (progn (forward-line -1) t)
             (looking-at emagent-chat--thinking-headline-re)))
      (setq safe (replace-regexp-in-string "\\`[\n\r]+" "" safe)))
     ;; Resuming after a tool line/block: keep one blank line of separation.
     ((emagent-chat--reasoning-after-tool-artifact-p)
      (let ((have (emagent-chat--newlines-before-point)))
        (setq safe (concat (make-string (max 0 (- 2 have)) ?\n)
                           (replace-regexp-in-string "\\`[\n\r]+" "" safe))))))
    (insert safe)))

(defun emagent-chat--org-verbatim-paths (text)
  "Wrap file paths in org =verbatim= to prevent /italic/ and =verbatim= glitches.
Matches any token containing a / that follows whitespace, a colon, or the
start of the string, so both absolute (/Users/...) and relative
(project/src/...) paths are wrapped as a unit."
  (replace-regexp-in-string
   "\\(\\(?:^\\|[ \t:]\\)\\)\\([^ \t\n]+/[^ \t\n]*\\)"
   "\\1=\\2=" text))

(defun emagent-chat--format-tool-line (label)
  "Return a Thinking-block tool line for LABEL, safe in org-mode.
The decision annotation (Allow/Deny/Emacs) is placed before the file path
so it is visible without scrolling on long paths."
  (let* ((annotation (emagent-chat--tool-label-annotation label))
         (base (if annotation
                   (string-trim
                    (replace-regexp-in-string
                     (concat " *" (regexp-quote annotation) "\\'")
                     "" label))
                 label))
         ;; Reorder: insert annotation between operation and first path token.
         (reordered
          (if annotation
              (let* ((parts (split-string base " " t))
                     (path-idx (cl-position-if
                                (lambda (s) (string-match-p "/" s))
                                parts)))
                (if path-idx
                    (let ((pre (string-join (seq-take parts path-idx) " "))
                          (post (string-join (seq-drop parts path-idx) " ")))
                      (concat (if (string-empty-p pre) "" (concat pre " "))
                              annotation " " post))
                  ;; No path: append annotation at end as usual
                  (concat base " " annotation)))
            base)))
    (format "→ %s" (emagent-chat--org-verbatim-paths reordered))))

(defun emagent-chat--combined-arrow-label (label code)
  "Return the arrow-line label for a combined arrow + block display.
Abbreviates to the operation verb when the block already carries the detail."
  (let* ((annotation (emagent-chat--tool-label-annotation label))
         (base (if annotation
                   (string-trim
                    (replace-regexp-in-string
                     (concat " *" (regexp-quote annotation) "\\'")
                     "" label))
                 label))
         (code-trimmed (string-trim-right (or code "")))
         (verb (car (split-string base "[ :/\n]" t)))
         (summary-base
          (cond
           ;; Multi-line code: block shows it in full, arrow just names the tool.
           ((string-match-p "\n" code-trimmed) verb)
           ;; Truncated label (ends with …): label IS the code but cut short.
           ((string-match-p "…\\'" base) verb)
           ;; Single-line code that IS the label (or a suffix of it).
           ((and (not (string-empty-p code-trimmed))
                 (or (string= (string-trim-right base) code-trimmed)
                     (string-prefix-p base code-trimmed)
                     (string-suffix-p code-trimmed base)))
            verb)
           (t base))))
    (if annotation
        (concat summary-base " " annotation)
      summary-base)))

(defconst emagent-chat--tool-annotation-re
  " ?\\((Allow: [^)\n]+)\\|(Allow)\\|(Denied)\\)\\'"
  "Regexp matching a trailing decision / (Emacs) annotation on a tool label.")

(defun emagent-chat--tool-label-annotation (label)
  "Return the trailing decision/(Emacs) annotation in LABEL, or nil."
  (when (and label (string-match emagent-chat--tool-annotation-re label))
    (match-string 1 label)))

(defun emagent-chat--tool-label-title-annotation (label)
  "Return comment text for a text block: tool title plus decision annotation.
Strips the path detail (already visible in the block code) to avoid redundancy."
  (when label
    (let* ((annotation (emagent-chat--tool-label-annotation label))
           (base (if annotation
                     (string-trim
                      (replace-regexp-in-string
                       (concat " *" (regexp-quote annotation) "\\'") "" label))
                   (string-trim label)))
           (title (if (string-match "\\`\\(.*?\\): [/~]" base)
                      (match-string 1 base)
                    base)))
      (if (and annotation (not (string-empty-p annotation)))
          (concat (string-trim title) " " annotation)
        (string-trim title)))))

(defun emagent-chat--src-comment-prefix (lang)
  "Return the line-comment prefix used inside a src block of LANG."
  (if (member lang '("elisp" "emacs-lisp" "lisp" "scheme" "clojure"))
      ";; "
    "# "))

(defun emagent-chat--format-tool-block (code lang annotation)
  "Return an Org src block for CODE in LANG.
A decision / (Emacs) ANNOTATION is rendered as a leading comment line inside
the block (using LANG's comment syntax) so it stays attached to the command
without leaving a dangling line beneath the block."
  (let* ((lang (or lang "text"))
         (note (when (and annotation (not (string-empty-p annotation)))
                 (concat (emagent-chat--src-comment-prefix lang)
                         (string-trim annotation) "\n"))))
    (format "#+begin_src %s\n%s%s\n#+end_src"
            lang
            (or note "")
            (string-trim-right code))))

(defun emagent-chat--format-permission-line (question)
  "Return a permission question line for QUESTION."
  (format "? %s" (emagent-chat--org-verbatim-paths question)))

(defun emagent-chat--permission-content-block (tool-call)
  "Return org subsection markup for TOOL-CALL, or nil."
  (when (and tool-call (fboundp 'emagent-acp--tool-call-content-block))
    (emagent-acp--tool-call-content-block tool-call)))

(defun emagent-chat--insert-permission-newline-if-needed ()
  "Insert a separating newline unless point already starts a fresh line."
  (unless (bolp)
    (insert "\n")))

(defconst emagent-chat--tool-decision-re
  " \\((Allow: [^)\n]+)\\|(Allow)\\|(Denied)\\)"
  "Regexp matching a permission decision or source annotation on a tool-call line.
No end-anchor: the annotation may appear before a path on the same line.")

(defun emagent-chat--repair-tool-line-faces (start end)
  "Re-apply path and decision faces after org font-lock on tool-call lines."
  (when (and start end (< start end))
    (with-silent-modifications
      (save-excursion
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward "\\=/[^ \t\n]+" end t))
          (let ((s (match-beginning 0))
                (e (match-end 0)))
            (remove-list-of-text-properties s e '(face))
            (put-text-property s e 'face 'emagent-tool-detail)))
        (goto-char start)
        (when (re-search-forward emagent-chat--tool-decision-re end t)
          (let ((s (match-beginning 1))
                (e (match-end 1)))
            (remove-list-of-text-properties s e '(face))
            (put-text-property s e 'face 'emagent-tool-permission-decision)))))))

(defconst emagent-chat--tool-line-font-lock-keywords
  `((,(concat "^→ .*?" emagent-chat--tool-decision-re)
     1 'emagent-tool-permission-decision prepend))
  "Font-lock keywords that re-apply the permission decision face.
Org font-lock removes manually applied `face' properties on every
fontification pass, so the grey decision suffix on a single-line tool call
must be reapplied as a keyword rather than set once at insertion time.
Block tool calls carry their decision as an in-block comment, which org
fontifies with the comment face natively.")

(defun emagent-chat--fontify-tool-line (start end)
  "Font-lock tool line START..END and repair org emphasis on paths."
  (when (and start end (<= start end))
    (emagent-chat--maybe-font-lock-flush)
    (emagent-chat--repair-tool-line-faces start end)))

(defun emagent-chat--fontify-tool-block (start end)
  "Fontify an Org src-block tool display between START and END natively."
  (when (and start end (<= start end))
    (emagent-chat--maybe-font-lock-flush)
    (ignore-errors
      (font-lock-ensure start end))))

(defun emagent-chat--ensure-reasoning-for-tool ()
  "Ensure the open response can accept tool annotations in Reasoning."
  (when (emagent-chat--open-response-p)
    (emagent-chat--ensure-reasoning-scaffold)))

(defun emagent-chat--separate-before-tool ()
  "Ensure point starts a fresh line, with a blank line before tool prose.
Consecutive arrow lines stay adjacent; anything following a src block
(#+end_src) gets a blank line so blocks don't run together."
  (unless (bolp) (insert "\n"))
  (unless (or (bobp)
              (save-excursion
                (forward-line -1)
                (or (looking-at-p "[ \t]*$")
                    (looking-at-p "→ ")
                    (looking-at emagent-chat--thinking-headline-re))))
    (insert "\n")))

(defun emagent-chat--append-tool-line (label &optional id lang code)
  "Append tool LABEL to the open Reasoning block.
When ID is non-nil, remember the span for later in-place updates.  When CODE
is non-empty, render it as an Org src block in LANG instead of a single →
line, with LABEL's trailing decision/(Emacs) annotation beneath."
  (when (and label (not (string-empty-p label))
               (emagent-chat--open-response-p)
               (not emagent-chat--permission-pending))
    (emagent-chat--with-stable-view
     (lambda ()
       (with-current-buffer (current-buffer)
         (let ((inhibit-read-only t))
           (emagent-chat--writable)
           ;; Write any buffered reasoning first so the tool line lands after
           ;; the prose received so far, never splitting a pending sentence.
           (emagent-chat--flush-thought-pending)
           (emagent-chat--ensure-response-markers)
           (emagent-chat--ensure-reasoning-for-tool)
           (unless (and id (emagent-chat--update-tool-call-line id label lang code))
             (when (and emagent-chat--thought-open-p
                        emagent-chat--thought-marker
                        (marker-position emagent-chat--thought-marker))
               (save-excursion
                 (goto-char emagent-chat--thought-marker)
                 (emagent-chat--separate-before-tool)
                 (let ((line-start (line-beginning-position))
                       (blockp (and code (not (string-empty-p code)))))
                   (insert (if blockp
                               (if (and (equal lang "text")
                                        (not (string-match-p "\n" (or code ""))))
                                   ;; Text block = file path: arrow with full path, no block.
                                   (let* ((annotation (emagent-chat--tool-label-annotation label))
                                          (base (if annotation
                                                    (string-trim
                                                     (replace-regexp-in-string
                                                      (concat " *" (regexp-quote annotation) "\\'")
                                                      "" label))
                                                  label))
                                          (verb (car (split-string base "[ :/]" t)))
                                          (full-label (concat (or verb base)
                                                              ": " code
                                                              (if annotation (concat " " annotation) ""))))
                                     (emagent-chat--format-tool-line full-label))
                                 ;; Non-text blocks: arrow + block.
                                 (concat (emagent-chat--format-tool-line
                                          (emagent-chat--combined-arrow-label label code))
                                         "\n"
                                         (emagent-chat--format-tool-block code lang nil)))
                             (emagent-chat--format-tool-line label)))
                   (let ((line-end (line-end-position)))
                     (when id
                       (puthash id (cons (copy-marker line-start nil)
                                         (copy-marker line-end nil))
                                emagent-chat--tool-call-lines))
                     (if blockp
                         (emagent-chat--fontify-tool-block line-start line-end)
                       (emagent-chat--fontify-tool-line line-start line-end)))
                   (emagent-chat--finish-tool-line-in-reasoning)))))))))))

(defun emagent-chat--update-tool-call-line (id label &optional lang code)
  "Replace the displayed tool-call span for ID with LABEL.
When CODE is non-empty, render an Org src block in LANG instead of a line.
Return non-nil when a span was updated."
  (let ((entry (gethash id emagent-chat--tool-call-lines)))
    (when (and entry
               (markerp (car entry)) (marker-position (car entry))
               (markerp (cdr entry)) (marker-position (cdr entry)))
      (let* ((start (car entry))
             (end (cdr entry))
             (blockp (and code (not (string-empty-p code))))
             (annotation (emagent-chat--tool-label-annotation label))
             ;; When transitioning from an arrow line to a block, keep the
             ;; arrow line (without annotation) and append the block below.
             ;; The annotation moves into the block comment so it appears once.
             (current (buffer-substring-no-properties start end))
             ;; Arrow-only: single → line with no block appended yet.
             ;; Arrow-with-block: already combined → line + #+begin_src block.
             (was-arrow-only (string-match-p "\\`→ [^\n]*\\'" current))
             (was-arrow-with-block (and (string-match-p "\\`→ " current)
                                        (not was-arrow-only)))
             (display (cond
                       ((and blockp (or was-arrow-only was-arrow-with-block)
                             (equal lang "text")
                             (not (string-match-p "\n" (or code ""))))
                        ;; Text block = file path: show the FULL path on the
                        ;; arrow (no block) by reconstructing the label from
                        ;; the untruncated code.
                        (let* ((base (if annotation
                                         (string-trim
                                          (replace-regexp-in-string
                                           (concat " *" (regexp-quote annotation) "\\'")
                                           "" label))
                                       label))
                               (verb (car (split-string base "[ :/]" t)))
                               (full-label (concat (or verb base)
                                                   ": " code
                                                   (if annotation (concat " " annotation) ""))))
                          (emagent-chat--format-tool-line full-label)))
                       ((and blockp (or was-arrow-only was-arrow-with-block))
                        ;; Arrow carries annotation; abbreviate if label==code.
                        (concat (emagent-chat--format-tool-line
                                 (emagent-chat--combined-arrow-label label code))
                                "\n"
                                (emagent-chat--format-tool-block code lang nil)))
                       (blockp
                        (emagent-chat--format-tool-block
                         code lang
                         (if (equal lang "text")
                             (emagent-chat--tool-label-title-annotation label)
                           annotation)))
                       (t (emagent-chat--format-tool-line label)))))
        (unless (string= (buffer-substring-no-properties start end) display)
          (save-excursion
            (delete-region start end)
            (goto-char start)
            (insert display)
            (set-marker end (point))
            (if blockp
                (emagent-chat--fontify-tool-block (marker-position start)
                                                  (marker-position end))
              (emagent-chat--fontify-tool-line (marker-position start)
                                               (marker-position end)))
            (when emagent-chat--thought-open-p
              (setq emagent-chat--thought-marker
                    (emagent-chat--reasoning-stream-marker)))))
        t))))

(defun emagent-chat-show-tool-call (id label &optional lang code)
  "Show or update a tool-call display for ACP toolCallId ID with LABEL.
When CODE is non-empty, render it as an Org src block in LANG instead of a
single → line."
  (emagent-chat--append-tool-line label id lang code))

(defun emagent-chat-permission-prompt (question choices callback &optional tool-call)
  "Show permission UI at the end of the open `** Thinking' subsection.

When TOOL-CALL carries a shell command or edit payload, inserts that content,
then CHOICES as buttons.  Otherwise inserts a ? question line before the
buttons.

CHOICES is a list of (LABEL . VALUE) pairs.  Non-blocking: inserts the dialog
and returns immediately.  CALLBACK is called with the chosen VALUE when a
button is clicked.

Keyboard shortcuts (via keymap text property on the buttons line):
  y / RET  — Allow once    s — Allow for session
  w        — Allow always  a — Allow all (session)
  n        — Deny"
  (when (emagent-chat--open-response-p)
    (let ((buf (current-buffer))
          (content-block (emagent-chat--permission-content-block tool-call))
          (responded nil)
          btn-keymap
          question-beg question-end
          content-beg content-end
          buttons-beg buttons-end)
      (let ((cleanup
             (lambda ()
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (emagent-chat--writable)
                   (when (and question-beg question-end
                              (marker-buffer question-beg) (marker-buffer question-end))
                     (delete-region (marker-position question-beg) (marker-position question-end)))
                   (when (and buttons-beg buttons-end
                              (marker-buffer buttons-beg) (marker-buffer buttons-end))
                     (delete-region (marker-position buttons-beg) (marker-position buttons-end)))
                   (when (and content-beg content-end
                              (marker-buffer content-beg) (marker-buffer content-end))
                     (delete-region (marker-position content-beg) (marker-position content-end)))
                   (when-let ((stream (emagent-chat--reasoning-stream-marker)))
                     (setq emagent-chat--thought-marker stream))
                   (setq emagent-chat--permission-pending nil))))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat--ensure-response-markers)
            (emagent-chat--ensure-reasoning-scaffold)
            (if-let ((insert-at (emagent-chat--reasoning-block-tail)))
                (progn
                  (goto-char insert-at)
                  ;; Normalize: keep at most 1 blank line before the dialog.
                  ;; reasoning-block-tail may point past trailing \n\n from the
                  ;; response body; strip the excess so the dialog stays tight.
                  (let ((content-end-pos (save-excursion
                                           (skip-chars-backward
                                            "\n"
                                            (or (emagent-chat--open-response-begin)
                                                (point-min)))
                                           (point))))
                    (when (> (- insert-at content-end-pos) 2)
                      (delete-region (+ content-end-pos 2) insert-at)
                      (goto-char (+ content-end-pos 2))))
                  (when content-block
                    (setq content-beg (copy-marker (point) nil))
                    (emagent-chat--insert-permission-newline-if-needed)
                    (insert content-block "\n")
                    (setq content-end (copy-marker (point) nil)))
                  (goto-char (or (and content-end (marker-position content-end))
                                 insert-at))
                  (unless content-block
                    (setq question-beg (copy-marker (point) nil))
                    (emagent-chat--insert-permission-newline-if-needed)
                    (insert (emagent-chat--format-permission-line question))
                    (put-text-property (marker-position question-beg) (point)
                                       'face 'emagent-permission-prompt)
                    (emagent-chat--repair-tool-line-faces (marker-position question-beg) (point))
                    (insert "\n")
                    (setq question-end (copy-marker (point) nil)))
                  (goto-char (or (and question-end (marker-position question-end))
                                 (and content-end (marker-position content-end))
                                 insert-at))
                  (setq buttons-beg (copy-marker (point) nil))
                  (emagent-chat--insert-permission-newline-if-needed)
                  (setq btn-keymap (make-sparse-keymap))
                  (set-keymap-parent btn-keymap button-map)
                  ;; Build key-hints alist and populate btn-keymap first
                  (let* ((allow-once-shown nil) (allow-always-shown nil) (deny-shown nil)
                         (hints
                          (mapcar
                           (lambda (choice)
                             (let* ((val (cdr choice))
                                    (id (and (stringp val) (downcase val)))
                                    (kh (cond
                                         ((eq val :allow-once) (setq allow-once-shown t) "y")
                                         ((eq val :allow-session) "s")
                                         ((eq val :allow-always) (setq allow-always-shown t) "w")
                                         ((eq val :allow-all) "a")
                                         ((eq val :deny) (setq deny-shown t) "n")
                                         ((and (not allow-always-shown) id
                                               (string-match-p "allow_always\\|always" id))
                                          (setq allow-always-shown t) "w")
                                         ((and (not allow-once-shown) id
                                               (string-match-p "allow\\|yes\\|run" id))
                                          (setq allow-once-shown t) "y")
                                         ((and (not deny-shown) id
                                               (string-match-p "deny\\|no\\|reject" id))
                                          (setq deny-shown t) "n")
                                         (t nil))))
                               (when kh
                                 (define-key btn-keymap (kbd kh)
                                             (let ((v val))
                                               (lambda ()
                                                 (interactive)
                                                 (unless responded
                                                   (setq responded t)
                                                   (funcall cleanup)
                                                   (funcall callback v))))))
                               kh))
                           choices)))
                    ;; Now insert buttons with btn-keymap as their keymap
                    (cl-mapc
                     (lambda (choice kh)
                       (let ((val (cdr choice)))
                         (insert-button
                          (concat "[" (car choice) "]")
                          'keymap btn-keymap
                          'action
                          (let ((v val))
                            (lambda (_b)
                              (unless responded
                                (setq responded t)
                                (funcall cleanup)
                                (funcall callback v))))
                          'follow-link t)
                         (when kh
                           (insert (propertize (format " [%s]" kh) 'face 'shadow)))
                         (insert "  ")))
                     choices hints))
                  (insert "\n")
                  (setq buttons-end (copy-marker (point) nil))
                  (setq emagent-chat--permission-pending t))
              (setq question-beg nil content-beg nil buttons-beg nil))))
        (emagent-chat--notify-inactive-update)
        (if (not buttons-beg)
            (let ((content-block (or content-block
                                     (emagent-chat--permission-content-block tool-call)))
                  (preamble (concat
                             "\n** Request permissions\n"
                             (when content-block
                               (concat content-block "\n")))))
              (emagent-tools--buttons-prompt
               (if content-block "" question)
               choices buf callback preamble))
          (when-let ((win (get-buffer-window buf)))
            (with-selected-window win
              (when (and buttons-beg (marker-position buttons-beg))
                (goto-char (marker-position buttons-beg))
                (recenter -3)))))))))

(defun emagent-chat--response-body-bounds ()
  "Return (CONTENT-START . END) for the `** Response' body, or nil."
  (when-let ((bounds (emagent-chat--open-response-body-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (when (re-search-forward emagent-chat--response-headline-re (cdr bounds) t)
        (forward-line 1)
        (cons (point) (cdr bounds))))))

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
              (insert "\n\n" emagent-chat-response-headline "\n")
              (point))
          ;; No reasoning was rendered: place Response at the response body start.
          (when-let ((beg (emagent-chat--open-response-begin)))
            (goto-char beg)
            (insert emagent-chat-response-headline "\n")
            (point))))))

(defun emagent-chat-append-assistant (text)
  "Append streamed assistant TEXT under the `** Response' subsection."
  (when (not (string-empty-p text))
    (emagent-chat--with-stable-view
      (lambda ()
        (with-current-buffer (current-buffer)
          (when (emagent-chat--open-response-p)
            (let ((inhibit-read-only t))
              (emagent-chat--writable)
              (emagent-chat-close-thought)
              (let* ((existing (emagent-chat--response-body-bounds))
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
                (when insert-at
                  (save-excursion
                    (goto-char insert-at)
                    (insert text)
                    (setq emagent-chat--assistant-marker (point-marker)))))
              (emagent-chat--maybe-font-lock-flush))))))))

(defun emagent-chat-fail-assistant (message)
  "Close the in-flight emagent response with error MESSAGE under `** Response'."
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t))
          (emagent-chat--writable)
          (when (emagent-chat--fail-response-p)
            (emagent-chat-close-thought)
            (emagent-chat--ensure-response-headline)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "\n*Error:* %s\n" message))
            (emagent-chat--finish-response-spacing)
            (emagent-chat--reset-response-state)
            (emagent-chat--sync-user-zone-marker)
            (emagent-chat--maybe-font-lock-flush))))))
  (emagent-chat--insert-user-heading-stub))

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

(defun emagent-chat--finalize-streamed-assistant (converted)
  "Replace the `** Response' body with CONVERTED assistant text."
  (when-let ((content-start (or (car (emagent-chat--response-body-bounds))
                                (emagent-chat--ensure-response-headline))))
    (let ((body-end (cdr (emagent-chat--open-response-body-bounds))))
      (when (and body-end (< content-start body-end))
        (delete-region content-start body-end))
      (goto-char content-start)
      (let ((start (point)))
        (insert converted)
        (when (string-match-p "|" converted)
          (ignore-errors
            (emagent-chat--maybe-align-org-tables-in-region start (point))))
        (setq emagent-chat--assistant-marker (point-marker))))))

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

(defun emagent-chat-finish-assistant (text &optional thought-text)
  "Finalize the latest emagent response.

Render the assistant answer under `** Response'.  When reasoning was streamed
keep its `** Thinking' subsection; otherwise build one from THOUGHT-TEXT.
A response without any reasoning has no `** Thinking' subsection at all."
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
            (setq hide-at (emagent-chat--open-reasoning-begin))
            (emagent-chat-close-thought)
            (when (emagent-chat--remove-empty-thinking)
              (setq hide-at nil))
            (emagent-chat--finalize-streamed-assistant converted)
            (emagent-chat--finish-response-spacing)
            (emagent-chat--reset-response-state)
            (emagent-chat--sync-user-zone-marker)
            (emagent-chat--maybe-font-lock-flush)
            (when hide-at
              (emagent-chat--hide-reasoning-deferred hide-at)))))))
  ;; Insert stub after stable view is restored, so point ends up at the
  ;; user prompt heading rather than being restored to the entry position.
  (emagent-chat--insert-user-heading-stub))

(provide 'emagent-chat-render)
;;; emagent-chat-render.el ends here
