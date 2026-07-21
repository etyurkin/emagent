;;; emagent-chat-tools-ui.el --- tool lines and permission UI module  -*- lexical-binding: t; -*-

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

;; Tool-call lines and permission-prompt UI rendered in the Thinking block,
;; extracted from `emagent-chat-render'.

;;; Code:

(require 'cl-lib)

(require 'org)

(require 'map)

(require 'emagent-log)

(require 'emagent-chat-header)

(require 'emagent-session-store)

(require 'emagent-chat-markup)

(require 'emagent-chat-reasoning)

(require 'emagent-chat-thought)

(require 'emagent-chat-response)

(require 'emagent-chat-ui)

(require 'emagent-tools)

(declare-function emagent-chat--notify-inactive-update "emagent-chat")

(defvar-local emagent-chat--permission-pending nil
  "Non-nil while a permission dialog is active in the current buffer.
New tool-call lines are suppressed while a dialog awaits user input so the
thinking block stays stable until the user responds.")

(defun emagent-chat--org-verbatim-paths (text)
  "Wrap file paths in org =verbatim= to prevent /italic/ and =verbatim= glitches.
Matches any token containing a / that follows whitespace, a colon, or the
start of the string.  Paths are shortened via `emagent-chat--display-path'
before wrapping.  URL-like tokens are left alone so they stay clickable.

Arguments: TEXT."
  ;; Capture the project before entering the temp buffer: both the
  ;; buffer-local `emagent-chat-project-directory' and the #+EMAGENT_PROJECT
  ;; property live in the chat buffer and are invisible from inside it.
  (let ((project (or (and (boundp 'emagent-chat-project-directory)
                          emagent-chat-project-directory)
                     (emagent-session-store-read-project-property))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              "\\(\\(?:^\\|[ \t:]\\)\\)\\([^ \t\n]+/[^ \t\n]*\\)" nil t)
        (let ((path (match-string 2)))
          (unless (emagent-chat--url-like-p path)
            (replace-match
             (concat (match-string 1)
                     "="
                     (emagent-chat--display-path path project)
                     "=")
             t t))))
      (buffer-string))))

(defun emagent-chat--format-tool-line (label)
  "Return a Thinking-block tool line for LABEL, safe in `org-mode'.
The decision annotation (Allow/Deny) is placed before the file path so it
is visible without scrolling on long paths.  When there is no path but the
label has a `tool: detail' separator, the annotation goes between them so
the result reads `tool (Allow: X): detail' rather than appending at the end."
  (let* ((annotation (emagent-chat--tool-label-annotation label))
         (base (if annotation
                   (string-trim
                    (replace-regexp-in-string
                     (concat " *" (regexp-quote annotation) "\\'")
                     "" label))
                 label))
         (reordered
          (if annotation
              (let* ((parts (split-string base " " t))
                     (path-idx (cl-position-if
                                (lambda (s) (string-match-p "/" s))
                                parts)))
                (if path-idx
                    (let* ((pre (string-join (seq-take parts path-idx) " "))
                           (post (string-join (seq-drop parts path-idx) " "))
                           ;; Strip trailing ":" from "Tool:" and reattach after
                           ;; annotation: "Tool (Allow: X): /path" not "Tool: (Allow: X) /path".
                           (pre-clean (if (string-suffix-p ":" pre)
                                          (substring pre 0 -1) pre))
                           (sep (if (string-suffix-p ":" pre) ": " " ")))
                      (concat (if (string-empty-p pre-clean) ""
                                (concat pre-clean " "))
                              annotation sep post))
                  ;; No path: insert annotation between "Tool" and ": detail"
                  ;; → "Tool (Allow: X): detail" instead of "Tool: detail (Allow: X)".
                  (let ((colon-pos (string-match ": " base)))
                    (if colon-pos
                        (concat (substring base 0 colon-pos)
                                " " annotation
                                (substring base colon-pos))
                      (concat base " " annotation)))))
            base)))
    (format "→ %s" (emagent-chat--org-verbatim-paths reordered))))

(defun emagent-chat--combined-arrow-label (label code)
  "Return the arrow-line LABEL for a combined arrow + block display.
Abbreviates to the operation verb when the block already carries the detail.

Arguments: CODE."
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
Strips the path detail (already visible in the block code) to avoid redundancy.

Arguments: LABEL."
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
without leaving a dangling line beneath the block.

Body lines that look like Org src delimiters are comma-escaped so a command
that documents `#+END_SRC' cannot close the generated block early."
  (let* ((lang (or lang "text"))
         (note (when (and annotation (not (string-empty-p annotation)))
                 (concat (emagent-chat--src-comment-prefix lang)
                         (string-trim annotation) "\n"))))
    (format "#+begin_src %s\n%s%s\n#+end_src"
            lang
            (or note "")
            (emagent-chat--escape-src-body (string-trim-right code)))))

(defun emagent-chat--format-permission-line (question)
  "Return a permission question line for QUESTION."
  (format "? %s" (emagent-chat--org-verbatim-paths question)))

(defun emagent-chat--permission-content-block (tool-call)
  "Return org subsection markup for TOOL-CALL, or nil."
  (when (and tool-call (fboundp 'emagent-acp--tool-call-content-block))
    (emagent-acp--tool-call-content-block tool-call)))

(defun emagent-chat--tool-call-rendered-text (id)
  "Return the buffer text already shown for tool-call ID's line, or nil."
  (when-let* ((entry (gethash id emagent-chat--tool-call-lines))
              (start (car entry)) (end (cdr entry)))
    (when (and (markerp start) (marker-position start)
               (markerp end) (marker-position end))
      (buffer-substring-no-properties (marker-position start) (marker-position end)))))

(defun emagent-chat--content-block-code (text)
  "Return the code payload inside the first org src block in TEXT, or nil."
  (let ((case-fold-search t))
    (when (and text (string-match
                      "#\\+begin_src[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)\n#\\+end_src"
                      text))
      (match-string 1 text))))

(defun emagent-chat--permission-redundant-p (tool-call content-block question)
  "Return non-nil when CONTENT-BLOCK or QUESTION repeats TOOL-CALL's line.
Covers a duplicated src-block payload (an eval/execute form already shown as
the pending tool-call line) and a duplicated plain path/detail (a QUESTION
that just restates what the tool-call line already displays)."
  (when-let* ((id (and tool-call (map-elt tool-call 'toolCallId)))
              (rendered (emagent-chat--tool-call-rendered-text id)))
    (or (when-let* ((pending (emagent-chat--content-block-code content-block))
                    (shown (emagent-chat--content-block-code rendered)))
          (string= (string-trim pending) (string-trim shown)))
        (and (not content-block)
             (stringp question)
             (not (string-empty-p (string-trim question)))
             (string-match-p (regexp-quote (string-trim question)) rendered)))))

(defun emagent-chat--insert-permission-newline-if-needed ()
  "Insert a separating newline unless point is already on a fresh line."
  (unless (bolp)
    (insert "\n")))

(defconst emagent-chat--tool-decision-re
  " \\((Allow: [^)\n]+)\\|(Allow)\\|(Denied)\\)"
  "Regexp matching a permission decision or source annotation on a tool-call line.
No end-anchor: the annotation may appear before a path on the same line.")

(defun emagent-chat--repair-tool-line-faces (start end)
  "Re-apply path and decision faces after org font-lock on tool-call lines.

Arguments: START, END."
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
  "Font-lock tool line START..END and repair org emphasis on paths.
Only touch START..END — do not re-fontify the whole response tail."
  (when (and start end (<= start end))
    (ignore-errors
      (font-lock-ensure start end))
    (emagent-chat--repair-tool-line-faces start end)))

(defun emagent-chat--fontify-tool-block (start end)
  "Fontify an Org src-block tool display between START and END natively.
Only touch START..END — do not re-fontify the whole response tail."
  (when (and start end (<= start end))
    (ignore-errors
      (font-lock-ensure start end))))

(defun emagent-chat--ensure-reasoning-for-tool ()
  "Ensure the open response can accept tool annotations in Reasoning."
  (when (emagent-chat--open-response-p)
    (emagent-chat--ensure-reasoning-scaffold)))

(defun emagent-chat--separate-before-tool ()
  "Ensure point is on a fresh line before inserting a tool line.
Consecutive tool lines and src blocks stay adjacent; a blank line is added
only before the first tool line after prose."
  (unless (bolp) (insert "\n"))
  (unless (or (bobp)
              (save-excursion
                (forward-line -1)
                (or (looking-at-p "[ \t]*$")
                    (looking-at-p "→ ")
                    (looking-at-p "#\\+[Ee][Nn][Dd]_[Ss][Rr][Cc]")
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
    (emagent-chat--end-send-pending-if-active)
    (emagent-chat--with-stable-view
     (lambda ()
       (with-current-buffer (current-buffer)
         (let ((inhibit-read-only t))
           (emagent-chat--writable)
           ;; Write any buffered reasoning first so the tool line lands after
           ;; the prose received so far, never splitting a pending sentence.
           ;; Force a final flush so a held inline-code span is emitted before
           ;; the tool line rather than stranded after it.
           (emagent-chat--flush-thought-pending t)
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
                                   ;; Text block = file path: arrow with display path, no block.
                                   (let* ((annotation (emagent-chat--tool-label-annotation label))
                                          (base (if annotation
                                                    (string-trim
                                                     (replace-regexp-in-string
                                                      (concat " *" (regexp-quote annotation) "\\'")
                                                      "" label))
                                                  label))
                                          (verb (car (split-string base "[ :/]" t)))
                                          (full-label (concat (or verb base)
                                                              ": "
                                                              (emagent-chat--display-path code)
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
                        ;; Text block = file path: show the display path on the
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
                                                   ": "
                                                   (emagent-chat--display-path code)
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
              (emagent-chat--sync-thought-marker-after-tool end))))
        t))))

(defun emagent-chat-show-tool-call (id label &optional lang code)
  "Show or update a tool-call display for ACP toolCallId ID with LABEL.
When CODE is non-empty, render it as an Org src block in LANG instead of a
single → line."
  (emagent-chat--append-tool-line label id lang code))

(defun emagent-chat-permission-prompt (question choices callback &optional tool-call)
  "Show permission UI for QUESTION at the end of `** Thinking'.

When TOOL-CALL carries a shell command or edit payload, inserts that content,
then CHOICES as buttons.  Otherwise inserts a ? question line before the
buttons.  Skips that content/question line when it would just repeat
TOOL-CALL's already-rendered pending tool-call line.

CHOICES is a list of (LABEL . VALUE) pairs.  Non-blocking: inserts the dialog
and returns immediately.  CALLBACK is called with the chosen VALUE when a
button is clicked.

Keyboard shortcuts (via keymap text property on the buttons line):
  y / RET  — Allow once    s — Allow for session
  w        — Allow always  a — Allow all (session)
  n        — Deny."
  (when (emagent-chat--open-response-p)
    (let* ((buf (current-buffer))
           (raw-content-block (emagent-chat--permission-content-block tool-call))
           (redundant (emagent-chat--permission-redundant-p
                       tool-call raw-content-block question))
           (content-block (unless redundant raw-content-block))
           (responded nil)
           btn-keymap
           question-beg question-end
           content-beg content-end
           buttons-beg buttons-end
           first-button)
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
                  (unless (or content-block redundant)
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
                         (unless first-button
                           (setq first-button (copy-marker (point) nil)))
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
                  (when first-button
                    (emagent-tools--apply-button-line-keymap
                     (marker-position first-button)
                     (marker-position buttons-end)
                     btn-keymap))
                  (setq emagent-chat--permission-pending t))
              (setq question-beg nil content-beg nil buttons-beg nil))))
        (emagent-chat--notify-inactive-update)
        (if (not buttons-beg)
            (let ((content-block (or content-block raw-content-block))
                  (preamble (concat
                             "\n** Request permissions\n"
                             (when content-block
                               (concat content-block "\n")))))
              (emagent-tools--buttons-prompt
               (if content-block "" question)
               choices buf callback preamble))
          (emagent-tools--focus-inline-buttons buf first-button))))))

(provide 'emagent-chat-tools-ui)
;;; emagent-chat-tools-ui.el ends here