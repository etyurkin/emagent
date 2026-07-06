;;; emagent-chat-markup.el --- Convert agent markdown responses to Org  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Markdown-to-Org conversion pipeline for agent responses.
;; Pure string transforms with no chat buffer state dependencies.

;;; Code:

(require 'cl-lib)
(require 'org)

;;;###autoload
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

;;;###autoload
(defun emagent-chat--lang-from-src-tag (tag)
  "Return a normalized org babel language tag for TAG."
  (cond
   ((string-match "\\`[0-9]+:[0-9]+:\\(.+\\)\\'" tag)
    (or (emagent-chat--lang-from-filename (match-string 1 tag)) "text"))
   ((member tag '("elisp" "emacs-lisp")) "elisp")
   (t tag)))

;;;###autoload
(defun emagent-chat--table-row-p (line)
  "Return non-nil when LINE looks like an org/markdown table row."
  (let ((trimmed (string-trim line)))
    (and (not (string-empty-p trimmed))
         (string-prefix-p "|" trimmed)
         (string-suffix-p "|" trimmed))))

;;;###autoload
(defun emagent-chat--table-hline-p (line)
  "Return non-nil when LINE is a table separator row."
  (when (emagent-chat--table-row-p line)
    (let ((inner (substring (string-trim line) 1 -1)))
      (and (not (string-empty-p inner))
           ;; Markdown |---|---| and org |---+---| hlines; reject data rows.
           (not (string-match-p "[^-+:|[:space:]]" inner))))))

;;;###autoload
(defun emagent-chat--table-ncols (line)
  "Return the number of columns in table row LINE."
  (length (split-string (substring (string-trim line) 1 -1) "|" t)))

;;;###autoload
(defun emagent-chat--org-table-hline (ncols)
  "Return an org table separator row for NCOLS columns."
  (concat "|" (mapconcat (lambda (_) "---------") (number-sequence 1 ncols) "+") "|"))

;;;###autoload
(defun emagent-chat--normalize-table-row (line)
  "Normalize spacing in a single table row."
  (let* ((trimmed (string-trim line))
         (cells (mapcar #'string-trim (split-string (substring trimmed 1 -1) "|" t))))
    (concat "|" (mapconcat (lambda (cell) (format " %s " cell)) cells "|") "|")))

;;;###autoload
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

;;;###autoload
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
  "Convert markdown-style pipe tables into org tables."
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

(defun emagent-chat--normalize-response-spacing (text)
  "Normalize spacing and leftover markdown headings in agent responses."
  (let ((result (replace-regexp-in-string "\\`[\n\r]+" "" text)))
    (setq result
          (replace-regexp-in-string
           "^###+ \\(.*\\)$" "* \\1" result))
    (setq result
          (replace-regexp-in-string
           "^## \\(.*\\)$" "** \\1" result))
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
    ;; Insert a space when sentence-ending punctuation is immediately followed
    ;; by a capital letter.  Bind case-fold-search=nil so [A-Z] only matches
    ;; true uppercase — without this, domain names like github.corp get spaces.
    (setq result
          (let ((case-fold-search nil))
            (replace-regexp-in-string
             "\\([.?!]\\)\\([A-Z]\\)"
             "\\1 \\2"
             result)))
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

;;;###autoload
(defun emagent-chat--escape-reasoning-text (text)
  "Convert markdown markup in reasoning TEXT to org before inserting it.
Applied once per flush (text is already outside any code fence at this point).
Order matters: heading and bold conversions run before escape-reasoning-line
so the escape pass never sees raw # / ** markers."
  (if (string-empty-p (or text ""))
      ""
    (let* (;; Markdown headings → bold text (not org sub-headings, which
           ;; would break the ** Thinking block structure).
           (text (replace-regexp-in-string
                  "^#\\{1,6\\} \\(.*\\)$" "*\\1*" text))
           ;; Markdown bold **text** → org bold *text*
           (text (replace-regexp-in-string
                  "\\*\\*\\([^*\n]+\\)\\*\\*" "*\\1*" text))
           ;; Markdown inline code `code` → org verbatim =code=
           (text (replace-regexp-in-string
                  "`\\([^`\n]+\\)`" "=\\1=" text)))
      ;; Finally escape any remaining # / * at line starts so org
      ;; does not mis-parse them as keywords or headings.
      (mapconcat #'emagent-chat--escape-reasoning-line
                 (split-string text "\n") "\n"))))

;;;###autoload
(defun emagent-chat--demote-response-headings (text)
  "Demote every Org headline in TEXT to level >= 3.

The assistant answer is rendered under the level-2 `** Response' subsection, so
its own headings must nest beneath it rather than starting new turns."
  (replace-regexp-in-string
   "^\\*+ "
   (lambda (m)
     (if (< (1- (length m)) 3) "*** " m))
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
                                body)
                        parts)
                  (setq pos (length text)))
              (let* ((body-end (match-beginning 0))
                     (close-end (match-end 0))
                     (body (substring text body-start body-end)))
                (push (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                              (emagent-chat--lang-from-src-tag tag)
                              body)
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
  "Rewrite file-citation language tags in org src block headers."
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
  "Rewrite emacs-lisp org src headers to elisp for font-lock."
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

;;;###autoload
(defun emagent-chat--convert-agent-markup (text)
  "Convert leftover markdown markup in agent responses to org."
  (emagent-chat--close-unclosed-org-src
   (emagent-chat--normalize-response-spacing
    (emagent-chat--convert-markdown-tables
     (emagent-chat--normalize-elisp-src-tags
      ;; Convert single-backtick inline code `foo` → =foo= after triple-backtick
      ;; fences are already converted to #+BEGIN_SRC blocks, so this only affects
      ;; inline code spans that are not inside any block.
      (replace-regexp-in-string
       "`\\([^`\n]+\\)`" "=\\1="
       (emagent-chat--convert-code-fences
        (emagent-chat--fix-org-src-citations
         (emagent-chat--unwrap-outer-org-src text)))))))))

(defvar-local emagent-chat--font-lock-deferred-p nil
  "When non-nil, defer org font-lock until the emagent buffer is active.")

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

(defun emagent-chat--maybe-font-lock-flush ()
  "Run org font-lock when active; defer otherwise."
  (if (emagent-chat--buffer-active-p)
      (progn
        (setq emagent-chat--font-lock-deferred-p nil)
        (font-lock-flush))
    (setq emagent-chat--font-lock-deferred-p t)))

(provide 'emagent-chat-markup)
;;; emagent-chat-markup.el ends here
