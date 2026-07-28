;;; emagent-chat-ui.el --- Inline permission-button UI for emagent chat  -*- lexical-binding: t; -*-

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
;; Shared chat UI, org markup conversion/fontification, and view helpers.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'org)
(require 'emagent-acp-usage)
(require 'emagent-session)
(require 'project)

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
  (concat "|" (mapconcat (lambda (_) "---------")
                         (number-sequence 1 ncols) "+") "|"))

(defun emagent-chat--normalize-table-row (line)
  "Normalize spacing in a single table row.

Arguments: LINE."
  (let* ((trimmed (string-trim line))
         (cells (mapcar #'string-trim
                        (split-string (substring trimmed 1 -1) "|" t))))
    (concat "|" (mapconcat (lambda (cell) (format " %s " cell))
                           cells "|") "|")))

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
  "Convert markdown links, bold, and glued sentences in prose TEXT.

Prose-only transforms applied outside src blocks, so a fenced `arr[i](fn)' or
`path.Join' is left as literal code.  Bold runs after inline-code conversion
in `emagent-chat--convert-agent-markup' so `**`code`**' becomes
`*=code=*' rather than leaving markdown stars around org verbatim."
  (let ((result
         ;; Markdown links [text](url) → [[url][text]] org links.
         (replace-regexp-in-string
          "\\[\\([^][\n]+\\)\\](\\([^)\n]+\\))"
          "[[\\2][\\1]]"
          text)))
    ;; Markdown bold **text** → org bold *text*.  Same pattern as the
    ;; streaming insert path; without this, finish rewrite reintroduces
    ;; markdown stars around already-converted =verbatim= spans.
    (setq result
          (replace-regexp-in-string
           "\\*\\*\\([^*\n]+\\)\\*\\*" "*\\1*" result))
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
link/sentence conversion in `emagent-chat--convert-markdown-prose'
\(both applied outside src blocks); this handles only whole-text spacing
that must see the block and table markers."
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
  "Match a complete org src block from BEGIN_SRC line to END_SRC line.")

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
    (while (string-match
            "^#\\+BEGIN_SRC +\\([0-9]+:[0-9]+:\\([^ \t\n]+\\)\\)\n"
            text start)
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
\(inline backticks, bold, tables, heading/spacing normalization) then run only
outside those blocks, so a fenced backtick span, `## heading', or table row
is never rewritten inside code.

Arguments: TEXT."
  (let* ((text (replace-regexp-in-string "\r\n?" "\n" text))
         (fenced (emagent-chat--normalize-elisp-src-tags
                  (emagent-chat--convert-code-fences
                   (emagent-chat--fix-org-src-citations
                    (emagent-chat--unwrap-outer-org-src text)))))
         ;; Prose-corrupting transforms (inline code, bold, headings, tables,
         ;; links, sentence spacing) run only outside src blocks so code survives.
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

(defun emagent-chat--align-org-tables-in-region (start end)
  "Align every org table between START and END.

Uses a line-shape check instead of `org-at-table-p' so we never invoke
`org-element-at-point' on every `|' match (that pegged Emacs at 100% CPU
when deferred align ran from redisplay hooks on large chat buffers)."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "^|" nil t)
        (beginning-of-line)
        ;; Cheap shape check — never `org-at-table-p' (org-element).
        (if (looking-at-p "^|.*|")
            (condition-case nil
                (progn
                  (org-table-align)
                  (goto-char (or (org-table-end nil) (point-max))))
              (error (forward-line 1)))
          (forward-line 1))))))

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
    (save-excursion
      (let ((start (emagent-chat--font-lock-region-start))
            (end (point-max)))
        (when (< start end)
          (condition-case nil
              (font-lock-fontify-region start end)
            (error nil)))))))

(defun emagent-chat--maybe-font-lock-flush ()
  "Run org font-lock on the response tail when safe; defer otherwise.

Defer when the buffer is not selected, and also while an ACP turn is
busy or finishing — fontifying large Thinking/tool regions on every
chunk pegs the command loop for both Cursor and Claude."
  (if (and (emagent-chat--buffer-active-p)
           (not (emagent-acp-turn-in-flight-p)))
      (progn
        (setq emagent-chat--font-lock-deferred-p nil)
        (emagent-chat--font-lock-response-tail))
    (setq emagent-chat--font-lock-deferred-p t)))

(defun emagent-chat--flush-deferred-font-lock ()
  "Font-lock the response tail when a deferred flush was requested.

Skipped while an ACP turn is still in flight so settle happens once."
  (when (and emagent-chat--font-lock-deferred-p
             (emagent-chat--buffer-active-p)
             (not (emagent-acp-turn-in-flight-p)))
    (setq emagent-chat--font-lock-deferred-p nil)
    (emagent-chat--font-lock-response-tail)))

(defvar-local emagent-chat--table-align-start nil
  "Marker for the start of a pending idle org-table align region, or nil.")

(defvar-local emagent-chat--table-align-end nil
  "Marker for the end of a pending idle org-table align region, or nil.")

(defvar-local emagent-chat--table-align-timer nil
  "Idle timer that aligns `emagent-chat--table-align-start'..end, or nil.")

(defun emagent-chat--cancel-scheduled-table-align ()
  "Cancel any pending idle org-table alignment for this buffer."
  (when emagent-chat--table-align-timer
    (cancel-timer emagent-chat--table-align-timer)
    (setq emagent-chat--table-align-timer nil))
  (when (markerp emagent-chat--table-align-start)
    (set-marker emagent-chat--table-align-start nil))
  (when (markerp emagent-chat--table-align-end)
    (set-marker emagent-chat--table-align-end nil))
  (setq emagent-chat--table-align-start nil
        emagent-chat--table-align-end nil))

(defun emagent-chat--run-scheduled-table-align ()
  "Align the pending org-table region, if any, without dirtying the buffer."
  (setq emagent-chat--table-align-timer nil)
  (when-let* ((start emagent-chat--table-align-start)
              (end emagent-chat--table-align-end)
              (s (marker-position start))
              (e (marker-position end))
              ((< s e)))
    (setq emagent-chat--table-align-start nil
          emagent-chat--table-align-end nil)
    (set-marker start nil)
    (set-marker end nil)
    (let ((was-modified (buffer-modified-p))
          (inhibit-read-only t))
      (unwind-protect
          (progn
            (when (fboundp 'emagent-chat--writable)
              (emagent-chat--writable))
            (save-excursion
              (emagent-chat--align-org-tables-in-region s e)))
        (set-buffer-modified-p was-modified)))))

(defun emagent-chat--schedule-align-org-tables (start end)
  "Schedule alignment of org tables between START and END.

Uses `run-at-time' (not an idle timer): continuous ACP/MCP process I/O
resets Emacs idle, so `run-with-idle-timer' deferred aligns often never
fired.  Finish prefers a synchronous align; this helper remains for
callers that must not block the current filter.  Never runs from
redisplay/`window-configuration-change-hook' — that path hung Emacs on
large chats via `org-element' parses."
  (when (and (integer-or-marker-p start)
             (integer-or-marker-p end)
             (< (if (markerp start) (marker-position start) start)
                (if (markerp end) (marker-position end) end)))
    (emagent-chat--cancel-scheduled-table-align)
    (setq emagent-chat--table-align-start (copy-marker start t)
          emagent-chat--table-align-end (copy-marker end nil))
    (let ((buf (current-buffer)))
      (setq emagent-chat--table-align-timer
            (run-at-time
             0 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (emagent-chat--run-scheduled-table-align)))))))))

(defconst emagent-chat--model-link-re
  "\\[\\[emagent://\\([^][]+\\)\\]\\(?:\\[\\([^][]*\\)\\]\\)?\\]"
  "Matches a `/model' override link `[[emagent://AGENT/MODEL][short]]'.
Group 1 is the link target `AGENT/MODEL' (shown on hover); group 2 the
short model label shown as the link text.  Being an org link, the
marker is fontified by org, survives saving the session to disk, and
reveals the full agent/model id on hover.  The `emagent://' scheme
tags this as the model marker so unrelated links a user pastes are not
mistaken for it.")

(defun emagent-chat--model-link-path-id (path)
  "Return the model id from a link PATH `AGENT/MODEL' (or bare MODEL).
PATH may carry a leading `//' authority slash from the raw link.  The
agent is the first segment; the model id is the rest, so model ids are
returned intact even if they contain slashes."
  (let ((path (string-remove-prefix "//" path)))
    (if (string-match "/" path)
        (substring path (match-end 0))
      path)))

(defun emagent-chat--region-turn-model (start end)
  "Return the model id of the first `/model' link between START and END."
  (save-excursion
    (goto-char start)
    (when (re-search-forward emagent-chat--model-link-re end t)
      (emagent-chat--model-link-path-id (match-string-no-properties 1)))))

(defun emagent-chat--strip-model-links (text)
  "Remove `/model' override links from outgoing TEXT.
The marker is client UI — the slash command is documented as never sent
to the agent."
  (string-trim
   (replace-regexp-in-string
    (concat "[ \t]*" emagent-chat--model-link-re) "" text)))

(defun emagent-chat--model-link (model-id)
  "Return the `/model' marker link for MODEL-ID.
The visible text is the short model name; the link target is
`agent/full-model-id', revealed on hover.  The `emagent://' scheme
\(never shown) tags this as the model marker so unrelated links a user
pastes are not mistaken for it."
  (let* ((agent (emagent-session-agent))
         (short (or (emagent-model-normalize-id model-id) model-id))
         (path (if agent (format "%s/%s" agent model-id) model-id)))
    (format "[[emagent://%s][%s]]" path short)))

(defun emagent-chat--follow-model-link (path &optional _prefix)
  "Describe the `/model' override link PATH when activated."
  (message "Model for this turn: %s (delete the link to cancel)"
           (string-remove-prefix "//" path)))

(defun emagent-chat--model-link-help-echo (_window object position)
  "Tooltip for a `/model' link: the `agent/model' target.

Arguments: OBJECT, POSITION."
  (with-current-buffer (if (bufferp object) object (current-buffer))
    (save-excursion
      (goto-char position)
      (when (or (looking-at emagent-chat--model-link-re)
                (and (search-backward "[[" (max (point-min) (- position 200)) t)
                     (looking-at emagent-chat--model-link-re)))
        (format "Model for this turn: %s" (match-string-no-properties 1))))))

(defun emagent-chat--display-path (path &optional project-dir)
  "Return PATH formatted for display in the chat UI.
Under the session project root: ./projectname/relative/path.
Under user home but outside the project: ~/….
Otherwise: the absolute PATH.

Relative paths resolve against the project directory, not
`default-directory' — saving the session file moves `default-directory'
to the session file's directory, which is unrelated to the project.

Arguments: PROJECT-DIR."
  (let* ((project (when-let ((raw (or project-dir
                                      (and (boundp 'emagent-chat-project-directory)
                                           emagent-chat-project-directory)
                                      (emagent-session-store-read-project-property))))
                    (file-truename
                     (file-name-as-directory (expand-file-name raw)))))
         (expanded (file-truename (expand-file-name path project)))
         (home (file-truename (expand-file-name "~")))
         (home-prefix (concat home "/")))
    (cond
     ((and project
           (string-prefix-p project expanded)
           (not (string= expanded (directory-file-name project))))
      (concat "./"
              (file-name-nondirectory (directory-file-name project))
              "/"
              (file-relative-name expanded project)))
     ((string-prefix-p home-prefix expanded)
      (abbreviate-file-name expanded))
     ((string= expanded home)
      "~")
     (t expanded))))

(defun emagent-chat--session-directory ()
  "Return the ACP working directory for the current emagent buffer.
Reads #+EMAGENT_PROJECT from the buffer header if set, falling back to
variable `buffer-file-name', `project-current' or `user-emacs-directory'."
  (expand-file-name
   (or (emagent-session-store-read-project-property)
       (and buffer-file-name (file-name-directory buffer-file-name))
       (if (boundp 'emagent-default-directory) emagent-default-directory)
       (and (fboundp 'project-current)
            (when-let ((proj (project-current nil default-directory)))
              (project-root proj)))
       user-emacs-directory)))

(defun emagent-tools--apply-button-line-keymap (beg end keymap)
  "Attach KEYMAP to the button line spanning BEG through END (exclusive).
Shortcuts then work anywhere on that line, including at line beginning."
  (when (and beg end keymap (< beg end))
    (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position))))
      (put-text-property line-beg (1- end) 'keymap keymap))))

(defun emagent-tools--goto-first-button (pos)
  "Move point to the first button at or after POS; return non-nil on success."
  (when pos
    (goto-char pos)
    (or (button-at (point))
        (when-let ((btn (next-button (max (1- pos) (point-min)))))
          (goto-char (button-start btn))
          t))))

(defun emagent-tools--focus-inline-buttons (chat-buffer button-pos)
  "Move point to BUTTON-POS in CHAT-BUFFER so button keymaps accept shortcuts."
  (when (and chat-buffer (buffer-live-p chat-buffer) button-pos)
    (when-let ((pos (if (markerp button-pos)
                        (marker-position button-pos)
                      button-pos)))
      (if-let ((win (get-buffer-window chat-buffer)))
          (progn
            (select-window win)
            (with-current-buffer chat-buffer
              (emagent-tools--goto-first-button pos)
              (recenter -3)))
        (with-current-buffer chat-buffer
          (emagent-tools--goto-first-button pos))))))

(defun emagent-tools--choice-shortcut (value)
  "Return a single-character keyboard shortcut for VALUE, or nil."
  (cond
   ((memq value '(yes :allow-once :accept)) "y")
   ((memq value '(no :deny :reject)) "n")
   ((eq value :allow-session) "s")
   ((eq value :allow-always) "w")
   ((memq value '(all :allow-all)) "a")
   (t nil)))

(defun emagent-tools--buttons-prompt (prompt choices chat-buffer callback &optional preamble)
  "Insert optional PREAMBLE, PROMPT, and CHOICES as buttons in CHAT-BUFFER.
CHOICES is a list of (LABEL . VALUE) pairs.  Non-blocking: inserts the dialog
and returns immediately.  CALLBACK is called with the VALUE when a button is
clicked.  Falls back to `completing-read' (synchronous) when CHAT-BUFFER is
nil or dead, calling CALLBACK with the chosen value.

Accept/reject choices bind both lower- and upper-case Y/N.  Labels show the
shortcut in parentheses.  When a trailing `* user>' stub is present, the
dialog is inserted above it rather than after it."
  (if (not (and chat-buffer (buffer-live-p chat-buffer)))
      (let* ((labels (mapcar #'car choices))
             (label (completing-read (concat prompt " ") labels nil t)))
        (funcall callback (cdr (assoc label choices))))
    (let (start-mark end-mark first-button (responded nil))
      (let ((do-respond
             (lambda (v)
               (unless responded
                 (setq responded t)
                 (when (and start-mark end-mark
                            (marker-buffer start-mark)
                            (marker-buffer end-mark))
                   (with-current-buffer chat-buffer
                     (let ((inhibit-read-only t))
                       (when (fboundp 'emagent-chat--writable)
                         (funcall #'emagent-chat--writable))
                       (delete-region (marker-position start-mark)
                                      (marker-position end-mark)))))
                 (funcall callback v)))))
        (with-current-buffer chat-buffer
          (let ((inhibit-read-only t))
            (when (fboundp 'emagent-chat--writable)
              (funcall #'emagent-chat--writable))
            (goto-char
             ;; Only park above a real trailing * user> stub.  Bare
             ;; user-zone-start can be point-min when no response exists
             ;; yet, which would put the dialog at the buffer head.
             (let ((zone (and (fboundp 'emagent-chat--user-zone-start)
                              (emagent-chat--user-zone-start))))
               (if (and zone
                        (fboundp 'emagent-chat--user-heading-at-point-p)
                        (save-excursion
                          (goto-char zone)
                          (emagent-chat--user-heading-at-point-p)))
                   zone
                 (point-max))))
            (unless (bolp) (insert "\n"))
            (setq start-mark (copy-marker (point) nil))
            (when preamble (insert preamble))
            (insert "\n" prompt "\n")
            ;; Build keymap with all shortcuts BEFORE inserting buttons,
            ;; then pass it to each insert-button so the button's own
            ;; overlay keymap contains our shortcuts (higher priority than
            ;; any external overlay we add afterward).
            (let ((btn-keymap (make-sparse-keymap)))
              (set-keymap-parent btn-keymap button-map)
              ;; First pass: define all shortcuts in btn-keymap
              (dolist (choice choices)
                (when-let ((key (emagent-tools--choice-shortcut (cdr choice))))
                  (let ((handler
                         (let ((vv (cdr choice)))
                           (lambda ()
                             (interactive)
                             (funcall do-respond vv)))))
                    (define-key btn-keymap (kbd key) handler)
                    (define-key btn-keymap (kbd (upcase key)) handler))))
              ;; Second pass: insert buttons with btn-keymap as their keymap
              (dolist (choice choices)
                (let* ((v (cdr choice))
                       (key (emagent-tools--choice-shortcut v))
                       (label (if key
                                  (format "[%s (%s)]" (car choice) key)
                                (concat "[" (car choice) "]"))))
                  (unless first-button
                    (setq first-button (copy-marker (point) nil)))
                  (insert-button
                   label
                   'keymap btn-keymap
                   'action (lambda (_b) (funcall do-respond v))
                   'follow-link t)
                  (insert "  ")))
              (insert "\n")
              (setq end-mark (copy-marker (point) nil))
              (when first-button
                (emagent-tools--apply-button-line-keymap
                 (marker-position first-button)
                 (marker-position end-mark)
                 btn-keymap))
              ;; Stop sticky follow so later tool/stream inserts do not
              ;; yank point off the dialog (Y/N keymap needs point here).
              (when (boundp 'emagent-chat--follow-output)
                (setq emagent-chat--follow-output nil)))))
        (emagent-tools--focus-inline-buttons chat-buffer first-button)))))

(defvar-local emagent-chat--send-pending nil
  "Non-nil from send until `emagent-acp-send-prompt' dispatches the turn.

Covers connecting, per-turn model switches (`/model'), and other pre-dispatch
work.  The mode line shows a spinner during this window so large resumed
sessions do not look idle while the agent re-hydrates context for a new model.")

(defvar-local emagent-chat--send-token nil
  "Token for the in-flight pre-dispatch send; cleared on cancel or dispatch.")

(defun emagent-chat--send-active-p (token)
  "Return non-nil when TOKEN is still the active pre-dispatch send."
  (and emagent-chat--send-pending (eq emagent-chat--send-token token)))

(defun emagent-chat--send-pending-begin ()
  "Mark the buffer as preparing a send and refresh the mode line."
  (setq emagent-chat--send-pending t
        emagent-chat--send-token (cl-gensym "emagent-send"))
  (when (fboundp 'emagent-chat--refresh-mode-line)
    (emagent-chat--refresh-mode-line))
  (when (fboundp 'emagent-chat--spinner-ensure-running)
    (emagent-chat--spinner-ensure-running)))

(defun emagent-chat--send-pending-end ()
  "Clear the pre-dispatch send marker and refresh the mode line."
  (when emagent-chat--send-pending
    (setq emagent-chat--send-pending nil
          emagent-chat--send-token nil)
    (when (fboundp 'emagent-chat--refresh-mode-line)
      (emagent-chat--refresh-mode-line))))

(defvar emagent-chat--live-buffers (make-hash-table :weakness 'key :test 'eq)
  "Weak set of live `emagent-mode' buffers.

Used by focus/spinner refresh paths instead of scanning `buffer-list'.")

(defun emagent-chat--register-live-buffer (&optional buffer)
  "Register BUFFER (default current) as a live emagent chat buffer."
  (puthash (or buffer (current-buffer)) t emagent-chat--live-buffers))

(defun emagent-chat--unregister-live-buffer (&optional buffer)
  "Unregister BUFFER (default current) from the live emagent set."
  (remhash (or buffer (current-buffer)) emagent-chat--live-buffers))

(defun emagent-chat--map-live-buffers (fn)
  "Call FN with each live registered emagent buffer."
  (maphash (lambda (buf _)
             (when (buffer-live-p buf)
               (funcall fn buf)))
           emagent-chat--live-buffers))

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
Emacs itself is unfocused).  It is nil only when no visible frame displays
the buffer (every window hidden or the frame iconified)."
  (and (get-buffer-window-list (or buffer (current-buffer)) nil 'visible) t))

(defun emagent-chat--follow-output-pos ()
  "Return the buffer position streaming output should keep in view."
  (or (when (fboundp 'emagent-chat--open-response-body-bounds)
        (when-let ((bounds (emagent-chat--open-response-body-bounds)))
          (cdr bounds)))
      (point-max)))

(defun emagent-chat--ensure-follow-window (&optional buffer)
  "Arm sticky follow and scroll BUFFER's window to the live output end.

Call after opening a response (send or quiet Build).  Preparing/Thinking
are inserted without `emagent-chat--with-streaming-view', so without this
the first stream chunk can see the follow position off-screen and clear
sticky follow before any recenter runs."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (setq emagent-chat--follow-output t)
      (let ((pos (emagent-chat--follow-output-pos)))
        (goto-char pos)
        (when-let ((win (get-buffer-window buf 'visible)))
          (set-window-point win pos)
          (when (eq win (selected-window))
            (recenter -1)))))))

(defun emagent-chat--live-tail-start ()
  "Return start of the live exchange (prompt + open response), or nil."
  (when (and (fboundp 'emagent-chat--open-response-begin)
             (fboundp 'emagent-chat--user-heading-re))
    (when-let ((begin (emagent-chat--open-response-begin)))
      (save-excursion
        (goto-char begin)
        (if (re-search-backward (emagent-chat--user-heading-re) nil t)
            (line-beginning-position)
          begin)))))

(defun emagent-chat--window-at-bottom-p (window)
  "Return non-nil when WINDOW should follow newly inserted chat output.

Follow when point is on the live prompt/response and either sticky follow
is armed or the window sits on the live end.  Exact `point-max' alone is
not enough: after send, point often remains on the prompt while the
Preparing/Thinking scaffold grows past it.

Sticky follow survives the end briefly leaving the window (Preparing is
inserted outside streaming-view).  It clears when point leaves the live
exchange.  Mid-buffer points without sticky do not re-arm follow."
  (and window (window-live-p window)
       (eq (window-buffer window) (current-buffer))
       (let* ((wp (window-point window))
              (follow-pos (emagent-chat--follow-output-pos))
              (tail (emagent-chat--live-tail-start))
              (end-visible (or noninteractive
                              (pos-visible-in-window-p follow-pos window)))
              (in-live-tail (if tail (>= wp tail) (= wp (point-max)))))
         (cond
          ((not in-live-tail)
           (when (eq window (selected-window))
             (setq emagent-chat--follow-output nil))
           nil)
          ;; Sticky send/Build follow: keep tracking even if the end left
          ;; the window before the first recenter could run.
          (emagent-chat--follow-output t)
          ((not end-visible) nil)
          ((= wp follow-pos)
           (setq emagent-chat--follow-output t)
           t)
          ((= wp (point-max))
           (setq emagent-chat--follow-output t)
           t)
          (t nil)))))

(defun emagent-chat--save-window-views ()
  "Return saved scroll state for windows displaying the current buffer."
  (let (views)
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (push `(:window ,win
              :start ,(window-start win)
              :at-bottom ,(emagent-chat--window-at-bottom-p win))
            views))
    views))

(defun emagent-chat--restore-window-views (views)
  "Restore scroll state from VIEWS returned by `emagent-chat--save-window-views'.

Windows marked for follow keep newly inserted text in view by moving
their `window-point' to `emagent-chat--follow-output-pos'."
  (dolist (view views)
    (let ((win (plist-get view :window)))
      (when (window-live-p win)
        (if (plist-get view :at-bottom)
            (let ((pos (emagent-chat--follow-output-pos)))
              (set-window-point win pos)
              (with-selected-window win
                (goto-char pos)
                (recenter -1)))
          (set-window-start win (plist-get view :start) t))))))

(defun emagent-chat--with-stable-view (fn)
  "Run FN while preserving window scroll unless already at buffer end."
  (let* ((saved-point (point-marker))
         (saved-windows (emagent-chat--save-window-views))
         (selected (selected-window))
         (follow (cl-some (lambda (v)
                            (and (eq (plist-get v :window) selected)
                                 (plist-get v :at-bottom)))
                          saved-windows)))
    (unwind-protect
        (funcall fn)
      (emagent-chat--restore-window-views saved-windows)
      (unless follow
        (when (marker-position saved-point)
          (goto-char saved-point)))
      (set-marker saved-point nil))))

(defun emagent-chat--with-streaming-view (fn)
  "Run FN during live streaming, following windows already at buffer end.

Windows scrolled away from the end keep their `window-start'; windows that
were showing `point-max' are scrolled to the new end after FN returns.
Inserts use `save-excursion', so this explicit follow is required — Emacs
does not auto-scroll when `window-point' is not at the insertion point."
  (let ((views (emagent-chat--save-window-views)))
    (unwind-protect
        (funcall fn)
      (emagent-chat--restore-window-views views))))

(provide 'emagent-chat-ui)
;;; emagent-chat-ui.el ends here
