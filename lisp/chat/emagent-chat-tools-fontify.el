;;; emagent-chat-tools-fontify.el --- Tool-line format and font-lock for emagent  -*- lexical-binding: t; -*-

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

;; Tool-line / permission formatting and font-lock helpers for the Thinking block.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'emagent-session-store)
(require 'emagent-chat-header)
(require 'emagent-chat-markup)

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

(provide 'emagent-chat-tools-fontify)

;;; emagent-chat-tools-fontify.el ends here
