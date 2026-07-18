;;; emagent-chat-markup.el --- Convert agent markdown responses to Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6

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

;; Markdown-to-Org conversion pipeline for agent responses.
;; Pure string transforms with no chat buffer state dependencies.

;;; Code:

(require 'cl-lib)
(require 'org)

(defun emagent-chat--lang-from-filename (file)
  "Return an org babel language tag for FILE, or nil when unknown."
  (pcase (downcase (or (file-name-extension file) ""))
    ("el" "elisp")
    ("elc" "elisp")
    ("org" "org")
    ("py" "python")
    ("js" "javascript")
    ("ts" "typescript")
    ("sh" "shell")
    ("bash" "shell")
    ("zsh" "shell")
    ("java" "java")
    ("go" "go")
    ("rs" "rust")
    ("rb" "ruby")
    ("json" "json")
    ("yaml" "yaml")
    ("yml" "yaml")
    ("md" "markdown")
    ("mermaid" "mermaid")
    (_ nil)))

(defun emagent-chat--lang-from-src-tag (tag)
  "Return a normalized org babel language tag for TAG."
  (cond
   ((string-match "\\`[0-9]+:[0-9]+:\\(.+\\)\\'" tag)
    (or (emagent-chat--lang-from-filename (match-string 1 tag)) "text"))
   ((member tag '("elisp" "emacs-lisp")) "elisp")
   (t tag)))

(defun emagent-chat--table-row-p (line)
  "Return non-nil when LINE resembles an org/markdown table row."
  (let ((trimmed (string-trim line)))
    ;; Require at least two chars so a lone "|" is not both prefix and suffix;
    ;; callers do (substring trimmed 1 -1), which signals on a 1-char string.
    (and (>= (length trimmed) 2)
         (string-prefix-p "|" trimmed)
         (string-suffix-p "|" trimmed))))

(defun emagent-chat--table-hline-p (line)
  "Return non-nil when LINE is a table separator row."
  (when (emagent-chat--table-row-p line)
    (let ((inner (substring (string-trim line) 1 -1)))
      (and (not (string-empty-p inner))
           ;; Markdown |---|---| and org |---+---| hlines; reject data rows.
           (not (string-match-p "[^-+:|[:space:]]" inner))))))

(defun emagent-chat--table-ncols (line)
  "Return the number of columns in table row LINE."
  (length (split-string (substring (string-trim line) 1 -1) "|" t)))

(defun emagent-chat--org-table-hline (ncols)
  "Return an org table separator row for NCOLS columns."
  (concat "|" (mapconcat (lambda (_) "---------") (number-sequence 1 ncols) "+") "|"))

(defun emagent-chat--normalize-table-row (line)
  "Normalize spacing in a single table row.

Arguments: LINE."
  (let* ((trimmed (string-trim line))
         (cells (mapcar #'string-trim (split-string (substring trimmed 1 -1) "|" t))))
    (concat "|" (mapconcat (lambda (cell) (format " %s " cell)) cells "|") "|")))

(defun emagent-chat--fix-table-block (rows)
  "Convert markdown table ROWS into a valid org table block."
  (let* ((body (if (and (> (length rows) 1)
                        (emagent-chat--table-hline-p (nth 1 rows)))
                   (append (list (car rows)) (nthcdr 2 rows))
                 rows))
         (normalized (mapcar #'emagent-chat--normalize-table-row body))
         (ncols (emagent-chat--table-ncols (car normalized)))
         (hline (emagent-chat--org-table-hline ncols)))
    (append (list (car normalized) hline) (cdr normalized))))

(defun emagent-chat--align-org-tables-in-region (start end)
  "Align every org table between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "^|" end t)
        (beginning-of-line)
        (when (org-at-table-p)
          (org-table-align)
          (goto-char (or (org-table-end nil) (point-max))))))))

(defun emagent-chat--convert-markdown-tables (text)
  "Convert markdown-style pipe tables into org tables.

Arguments: TEXT."
  (let* ((lines (split-string text "\n"))
         (parts nil)
         (i 0)
         (n (length lines)))
    (while (< i n)
      (let ((line (nth i lines)))
        (if (emagent-chat--table-row-p line)
            (let ((start i))
              (while (and (< i n) (emagent-chat--table-row-p (nth i lines)))
                (setq i (1+ i)))
              (let* ((rows (seq-subseq lines start i))
                     (fixed (emagent-chat--fix-table-block rows))
                     (prev (car parts)))
                (when (and prev (not (string-empty-p prev))
                           (not (emagent-chat--table-row-p prev)))
                  (push "" parts))
                (push (mapconcat #'identity fixed "\n") parts)
                (when (< i n)
                  (push "" parts))))
          (progn
            (push line parts)
            (setq i (1+ i))))))
    (mapconcat #'identity (nreverse parts) "\n")))

(defun emagent-chat--convert-markdown-headings (text)
  "Convert markdown ### / ## headings in TEXT to org headings.

A prose-only transform: it is applied outside src blocks so a fenced
\"## Title\" is left as literal code."
  (replace-regexp-in-string
   "^## \\(.*\\)$" "** \\1"
   (replace-regexp-in-string "^###+ \\(.*\\)$" "* \\1" text)))

(defun emagent-chat--convert-markdown-prose (text)
  "Convert markdown links and glued sentences in prose TEXT.

Prose-only transforms applied outside src blocks, so a fenced `arr[i](fn)' or
`path.Join' is left as literal code."
  (let ((result
         ;; Markdown links [text](url) → [[url][text]] org links.
         (replace-regexp-in-string
          "\\[\\([^][\n]+\\)\\](\\([^)\n]+\\))"
          "[[\\2][\\1]]"
          text)))
    ;; Insert a space when sentence-ending punctuation is immediately followed
    ;; by a capital letter.  Bind case-fold-search=nil so [A-Z] only matches
    ;; true uppercase — without this, domain names like github.corp get spaces.
    ;; Require a lowercase letter right before the punctuation too, so an
    ;; ALL-CAPS token like VDUNGEON.DAT or a CONSTANT.EXT filename is left
    ;; alone — real glued sentences ("Done.Next step") end in lowercase.
    (let ((case-fold-search nil))
      (replace-regexp-in-string
       "\\([a-z]\\)\\([.?!]\\)\\([A-Z]\\)"
       "\\1\\2 \\3"
       result))))

(defun emagent-chat--url-like-p (text)
  "Return non-nil if TEXT is a clickable URL or Org link target."
  (string-match-p "\\`\\(?:https?://\\|file:\\|emagent://\\|mailto:\\)"
                  text))

(defun emagent-chat--convert-inline-code-spans (text)
  "Replace markdown `code` spans in TEXT with org markup.

URL-like spans become org links (`[[url]]') so they stay clickable; other
spans become org verbatim (`=code=').

Use a literal replacement: lambda return values are still scanned for
replacement escapes, so spans containing backslashes would error."
  (replace-regexp-in-string
   "`\\([^`\n]+\\)`"
   (lambda (match)
     (let ((code (substring match 1 -1)))
       (if (emagent-chat--url-like-p code)
           (format "[[%s]]" code)
         (format "=%s=" code))))
   text nil t))

(defun emagent-chat--normalize-response-spacing (text)
  "Normalize spacing around blocks and tables in TEXT.

Heading conversion lives in `emagent-chat--convert-markdown-headings' and
link/sentence conversion in `emagent-chat--convert-markdown-prose' (both applied
outside src blocks); this handles only whole-text spacing that must see the
block and table markers."
  (let ((result (replace-regexp-in-string "\\`[\n\r]+" "" text)))
    (setq result
          (replace-regexp-in-string
           "\\(\n[ \t]*\\)+#\\+END_SRC"
           "\n#+END_SRC"
           result))
    (setq result
          (replace-regexp-in-string
           "#\\+END_SRC[ \t]*\n\\([^[:space:]\n]\\)"
           "#+END_SRC\n\n\\1"
           result))
    (setq result
          (replace-regexp-in-string
           "\\([^[:space:]\n]\\)\n#\\+BEGIN_SRC "
           "\\1\n\n#+BEGIN_SRC "
           result))
    (setq result
          (replace-regexp-in-string
           "\\([^[:space:]\n|]\\)\n\\(|\\)"
           "\\1\n\n\\2"
           result))
    (setq result
          (replace-regexp-in-string
           "\\(|[^\n]*|\n\\)\\([^|\n#]\\)"
           "\\1\n\n\\2"
           result))
    result))

(defun emagent-chat--escape-reasoning-line (line)
  "Escape LINE so Org will not parse it as a headline or keyword.

Reasoning is rendered as the body of the `** Thinking' subsection (not inside
a block), so a leading `*' or `#' must be neutralized with a leading space.
Exception: `#+begin_src'/`#+end_src' markers inserted by our fence conversion
must not be escaped — they need to remain valid org src block delimiters."
  (cond
   ;; Preserve org src block markers produced by emagent-chat--split-fences.
   ((string-match-p "\\`#\\+\\(?:begin_src\\|end_src\\)\\b" (downcase line))
    line)
   ((string-match-p "\\`[ \t]*[*#]" line)
    (concat " " line))
   (t line)))

(defun emagent-chat--escape-reasoning-text (text &optional mid-line)
  "Convert markdown markup in reasoning TEXT to org before inserting it.
Applied once per flush (text is already outside any code fence at this point).
Order matters: heading and bold conversions run before escape-reasoning-line
so the escape pass never sees raw # / ** markers.

With MID-LINE non-nil, TEXT continues an existing buffer line rather than
starting one, so the first line's leading `*'/`#' is left unescaped: org only
mis-parses such a marker at a real line start, and escaping it here would
inject a spurious leading space after the resumed text (e.g. a `**bold**' span
held across a streaming boundary)."
  (if (string-empty-p (or text ""))
      ""
    (let* (;; Markdown links [text](url) → [[url][text]]
           (text (replace-regexp-in-string
                  "\\[\\([^][\n]+\\)\\](\\([^)\n]+\\))"
                  "[[\\2][\\1]]" text))
           ;; Markdown headings → bold text
           (text (replace-regexp-in-string
                  "^#\\{1,6\\} \\(.*\\)$" "*\\1*" text))
           ;; Markdown bold **text** → org bold *text*
           (text (replace-regexp-in-string
                  "\\*\\*\\([^*\n]+\\)\\*\\*" "*\\1*" text))
           ;; Markdown inline code `code` → org verbatim =code=
           (text (emagent-chat--convert-inline-code-spans text))
           (lines (split-string text "\n")))
      ;; Finally escape any remaining # / * at line starts so org
      ;; does not mis-parse them as keywords or headings.
      (mapconcat #'identity
                 (cl-loop for line in lines
                          for first = t then nil
                          collect (if (and mid-line first)
                                      line
                                    (emagent-chat--escape-reasoning-line line)))
                 "\n"))))

(defconst emagent-chat--src-block-re
  "^[ \t]*#\\+BEGIN_SRC\\(?:.*\n\\)*?[ \t]*#\\+END_SRC[ \t]*$"
  "Match a complete org src block from its BEGIN_SRC line to its END_SRC line.")

(defun emagent-chat--escape-src-body (body)
  "Comma-escape lines in BODY that Org would misread as src-block delimiters.

A code block that documents Org can contain a literal `#+END_SRC' (or
`#+BEGIN_SRC') line; left as-is it closes the generated block early for both
Org's own parser and `emagent-chat--src-block-re', mangling everything after it.
Prefixing the delimiter with a comma is Org's escape convention (stripped on
export/edit), so the line stays part of the block body."
  (let ((case-fold-search t))
    (replace-regexp-in-string
     "^\\([ \t]*\\)\\(#\\+\\(?:BEGIN\\|END\\)_SRC\\)"
     "\\1,\\2"
     body)))

(defun emagent-chat--map-outside-src-blocks (fn text)
  "Return TEXT with FN applied to every span outside org src blocks.

Src blocks (`#+BEGIN_SRC'…`#+END_SRC') are emitted verbatim, so markdown
transforms run over prose only and never rewrite code-block interiors (a fenced
\"## Title\" or backtick span must survive untouched)."
  (let ((case-fold-search t) (pos 0) (len (length text)) (out nil))
    (while (and (< pos len)
                (string-match emagent-chat--src-block-re text pos))
      (let ((mb (match-beginning 0)) (me (match-end 0)))
        (push (funcall fn (substring text pos mb)) out)
        (push (substring text mb me) out)
        (setq pos me)))
    (push (funcall fn (substring text pos)) out)
    (apply #'concat (nreverse out))))

(defun emagent-chat--demote-response-headings (text)
  "Demote every Org headline in TEXT to level >= 3.

The assistant answer is rendered under the level-2 `** Response' subsection, so
its own headings must nest beneath it rather than starting new turns.  Headings
inside src blocks are left alone."
  (emagent-chat--map-outside-src-blocks
   (lambda (s)
     (replace-regexp-in-string
      "^\\*+ "
      (lambda (m)
        (if (< (1- (length m)) 3) "*** " m))
      s))
   text))

(defun emagent-chat--convert-code-fences (text)
  "Convert markdown ``` fences in TEXT to org src blocks."
  (let ((pos 0)
        (parts nil))
    (while (string-match "```" text pos)
      (let ((fence-start (match-beginning 0))
            (after-fence (match-end 0)))
        (push (substring text pos fence-start) parts)
        (if (not (string-match "\n" text after-fence))
            (progn (push "```" parts) (setq pos after-fence))
          (let* ((tag-end (match-beginning 0))
                 (body-start (match-end 0))
                 (tag (string-trim (substring text after-fence tag-end))))
            (if (not (string-match "```" text body-start))
                (let ((body (substring text body-start)))
                  (push (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                                (emagent-chat--lang-from-src-tag tag)
                                (emagent-chat--escape-src-body body))
                        parts)
                  (setq pos (length text)))
              (let* ((body-end (match-beginning 0))
                     (close-end (match-end 0))
                     (body (substring text body-start body-end)))
                (push (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                              (emagent-chat--lang-from-src-tag tag)
                              (emagent-chat--escape-src-body body))
                      parts)
                (setq pos close-end)))))))
    (push (substring text pos) parts)
    (apply #'concat (nreverse parts))))

(defun emagent-chat--close-unclosed-org-src (text)
  "Append missing #+END_SRC lines for unclosed org src blocks in TEXT."
  (let ((lines (split-string text "\n"))
        (open nil)
        (result nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^#\\+BEGIN_SRC\\b" line)
        (when open (push "#+END_SRC" result))
        (setq open t)
        (push line result))
       ((string-match-p "^#\\+END_SRC\\b" line)
        (setq open nil)
        (push line result))
       (t (push line result))))
    (when open (push "#+END_SRC" result))
    (mapconcat #'identity (nreverse result) "\n")))

(defun emagent-chat--fix-org-src-citations (text)
  "Rewrite file-citation language tags in org src block headers.

Arguments: TEXT."
  (let ((start 0)
        (parts nil))
    (while (string-match "^#\\+BEGIN_SRC +\\([0-9]+:[0-9]+:\\([^ \t\n]+\\)\\)\n" text start)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (file (match-string 2 text)))
        (push (substring text start match-start) parts)
        (push (format "#+BEGIN_SRC %s\n"
                      (or (emagent-chat--lang-from-filename file) "text"))
              parts)
        (setq start match-end)))
    (push (substring text start) parts)
    (apply #'concat (nreverse parts))))

(defun emagent-chat--normalize-elisp-src-tags (text)
  "Rewrite emacs-lisp org src headers to elisp for font-lock.

Arguments: TEXT."
  (replace-regexp-in-string
   "#\\+BEGIN_SRC emacs-lisp\\b"
   "#+BEGIN_SRC elisp"
   text))

(defun emagent-chat--unwrap-outer-org-src (text)
  "Remove a single outer #+BEGIN_SRC org wrapper around all of TEXT."
  (let ((trimmed (string-trim text)))
    (if (string-match (concat "\\`#\\+BEGIN_SRC +org\\s-*\n"
                              "\\(\\(?:.\\|\n\\)*\\)"
                              "\n#\\+END_SRC\\s-*\\'")
                      trimmed)
        (match-string 1 trimmed)
      text)))

(defun emagent-chat--convert-agent-markup (text)
  "Convert leftover markdown markup in agent responses to org.

Code fences are converted to src blocks first; the remaining prose transforms
\(inline backticks, tables, heading/spacing normalization) then run only outside
those blocks, so a fenced backtick span, `## heading', or table row is never
rewritten inside code.

Arguments: TEXT."
  (let* ((text (replace-regexp-in-string "\r\n?" "\n" text))
         (fenced (emagent-chat--normalize-elisp-src-tags
                  (emagent-chat--convert-code-fences
                   (emagent-chat--fix-org-src-citations
                    (emagent-chat--unwrap-outer-org-src text)))))
         ;; Prose-corrupting transforms (inline code, headings, tables, links,
         ;; sentence spacing) run only outside src blocks so code survives verbatim.
         (prose (emagent-chat--map-outside-src-blocks
                 (lambda (s)
                   (emagent-chat--convert-markdown-prose
                    (emagent-chat--convert-markdown-tables
                     (emagent-chat--convert-markdown-headings
                      (emagent-chat--convert-inline-code-spans s)))))
                 fenced)))
    ;; Whole-text spacing normalization needs to see the block/table markers.
    (emagent-chat--close-unclosed-org-src
     (emagent-chat--normalize-response-spacing prose))))

(defvar-local emagent-chat--font-lock-deferred-p nil
  "When non-nil, defer org font-lock until the emagent buffer is active.")

(declare-function emagent-acp-turn-in-flight-p "emagent-acp-usage")

(defun emagent-chat--buffer-active-p (&optional buffer)
  "Return non-nil when BUFFER is displayed in the selected window."
  (let ((buf (or buffer (current-buffer))))
    (and (window-live-p (selected-window))
         (eq buf (window-buffer (selected-window))))))

(defalias 'emagent-chat--buffer-visible-p 'emagent-chat--buffer-active-p)

(defun emagent-chat--buffer-displayed-p (&optional buffer)
  "Return non-nil when BUFFER is shown in a window on a visible frame.

Unlike `emagent-chat--buffer-active-p', this is true even when the buffer is
not in the selected window (e.g. side-by-side with another buffer, or while
Emacs itself is unfocused).  It is nil only when no visible frame displays the
buffer (every window hidden or the frame iconified)."
  (and (get-buffer-window-list (or buffer (current-buffer)) nil 'visible) t))

(defun emagent-chat--font-lock-region-start ()
  "Return a start position for incremental font-lock in the current buffer.

When a response is open, only re-fontify a trailing window of that response.
Long tool-heavy turns accumulate large `#+begin_src' blocks; re-fontifying
them all on every stream/tool update blocked the event loop (the \"hang\" on
large sessions).  Between turns, fall back to the user zone — never the
whole session log."
  (let* ((response-start
          (and (boundp 'emagent-chat--response-body-start)
               emagent-chat--response-body-start
               (marker-position emagent-chat--response-body-start)))
         (user-start
          (and (fboundp 'emagent-chat--user-zone-start)
               (emagent-chat--user-zone-start)))
         (floor (or response-start user-start (point-min)))
         ;; ~12k chars covers recent thought/tool/response chunks without
         ;; redoing earlier src blocks in the same turn.
         (window 12000))
    (if response-start
        (max floor (- (point-max) window))
      floor)))

(defun emagent-chat--font-lock-response-tail ()
  "Re-fontify the response tail without flushing the entire session buffer."
  (when font-lock-mode
    (let ((start (emagent-chat--font-lock-region-start))
          (end (point-max)))
      (when (< start end)
        (condition-case nil
            (font-lock-fontify-region start end)
          (error nil))))))

(defun emagent-chat--maybe-font-lock-flush ()
  "Run org font-lock on the response tail when safe; defer otherwise.

Defer when the buffer is not selected, and also while an ACP turn is
busy or finishing — fontifying large Thinking/tool regions on every
chunk pegs the command loop for both Cursor and Claude."
  (if (and (emagent-chat--buffer-active-p)
           (not (and (fboundp 'emagent-acp-turn-in-flight-p)
                     (emagent-acp-turn-in-flight-p))))
      (progn
        (setq emagent-chat--font-lock-deferred-p nil)
        (emagent-chat--font-lock-response-tail))
    (setq emagent-chat--font-lock-deferred-p t)))

(provide 'emagent-chat-markup)
;;; emagent-chat-markup.el ends here
