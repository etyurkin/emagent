;;; emagent-prompts-elisp-guide.el --- Elisp guide prompt for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.8
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
;; Emacs Lisp reference guide served via the elisp_guide tool.

;;; Code:

(defconst emagent-acp-elisp-guide
  "# Emacs Lisp Guide for emagent

Reference document for the agent. Call the `elisp_guide` tool before writing
non-trivial Emacs Lisp. Covers patterns, idioms, common pitfalls, and the
functions most useful in emagent sessions.

---

## Core rules

1. **Always `check_elisp` before `eval`** for forms longer than 3 lines.
2. **Prefer lisp-sitter structural edits** when available (`structural_tree`, `structural_bounds`,
   `structural_replace`, `structural_insert`) — over full-file rewrites.
   When lisp-sitter is not installed, use `write_file` + `check_elisp` for basic editing.
4. **Wrap multiple forms in `progn`** or pass them as separate eval calls.
5. **Use `let*` for sequential bindings** — never nest more than 3 levels deep.
6. **Return a useful string** from eval — the result is your tool output.
7. **Prefer emagent tools** over raw Elisp for file I/O (boundary checks, undo).
8. **Discover before guessing** — `apropos` → `apropos_doc` → `describe_symbol`.

---

## Structural editing (.el, .lisp, .cl, .scm)

When lisp-sitter is installed (check `--json tree` MCP tools are available),
use sexp-boundary tools instead of line-based search/replace or full-file rewrites.

Workflow:

1. `structural_tree` — list top-level forms (defun, define, class, ...)
2. **New file:** `structural_insert` with `after_symbol` `__start__` and the first complete form
3. **Add forms:** `structural_insert` with `__end__` or an existing symbol name
4. **Replace form:** `structural_bounds` → `structural_replace` (complete form text)

`structural_replace` and `structural_insert` validate syntax before save.
For `.el` files, the new form is eval'd so definitions are live for `eval` immediately.

Never pass partial form bodies to `structural_replace` — always a complete s-expression.

### Multi-node refactors

When changing several top-level forms, plan with `structural_tree`, then apply one
structural edit per form. Each call validates and saves independently — a mistake
only affects one form. Do not rewrite the whole file with `write_file`.

---

## Paren rules (read before writing any Elisp)

```
(when COND BODY)          ;; 2 closing parens  — one for cond, one for when
(if COND THEN ELSE)       ;; 2 closing parens
(let* ((x 1) (y 2)) BODY) ;; 2 closing parens  — one for bindings, one for let*
(progn A B C)             ;; 1 closing paren
(mapcar FN LIST)          ;; 1 closing paren
(with-current-buffer B F) ;; 1 closing paren
```

Write each binding on its own line:
```elisp
(let* ((files (directory-files dir t))
       (count (length files))
       (result (format \"Found %d files\" count)))
  result)
```

---

## Strings

```elisp
(string-join '(\"a\" \"b\" \"c\") \", \")          ;; \"a, b, c\"
(split-string \"a,b,c\" \",\" t)               ;; (\"a\" \"b\" \"c\")  — t trims empties
(string-trim \"  hello  \")                  ;; \"hello\"
(concat \"foo\" \"bar\")                       ;; \"foobar\"
(format \"%s has %d items\" name count)
(string-match-p REGEXP string)             ;; non-nil if matches
(replace-regexp-in-string \"old\" \"new\" s)
(substring s 0 5)                          ;; first 5 chars
(string-prefix-p \"foo\" s)
(string-suffix-p \".el\" s)
(upcase s) / (downcase s) / (capitalize s)
(number-to-string 42) / (string-to-number \"42\")
(truncate-string-to-width s 80 nil nil \"…\")
```

---

## Lists and sequences

```elisp
(length list)
(car list) / (cdr list) / (cadr list)       ;; first / rest / second
(nth 2 list)                                ;; 0-indexed
(last list) / (butlast list)
(cons x list)                               ;; prepend
(append list1 list2)                        ;; concatenate
(reverse list)
(seq-take list 5) / (seq-drop list 5)
(seq-find PRED list)
(seq-filter PRED list)
(seq-remove PRED list)
(mapcar FN list)
(seq-map FN seq)
(seq-reduce FN seq init)
(seq-some PRED list)                        ;; first truthy result
(seq-every-p PRED list)
(flatten-tree '(1 (2 (3))))                 ;; (1 2 3)
(delq nil list)                             ;; remove nils
(delete-dups list)
(sort list #'<) / (sort list #'string<)
(member x list) / (memq x list)            ;; string= vs eq
(assoc key alist) / (alist-get key alist)
(plist-get plist :key) / (plist-put plist :key val)
```

---

## Hash tables

```elisp
(let ((h (make-hash-table :test 'equal)))
  (puthash \"key\" \"value\" h)
  (gethash \"key\" h)          ;; \"value\"
  (remhash \"key\" h)
  (hash-table-count h)
  (maphash (lambda (k v) ...) h)
  h)
```

---

## Buffers

```elisp
(buffer-list)                              ;; all buffers
(get-buffer \"name\")                        ;; nil if not found
(get-buffer-create \"name\")                 ;; creates if needed
(find-buffer-visiting \"/path/to/file\")     ;; nil if not visiting
(buffer-name buffer)
(buffer-file-name buffer)
(buffer-modified-p buffer)

;; Work inside a buffer without switching
(with-current-buffer buffer
  (buffer-string)                          ;; entire content as string
  (buffer-substring-no-properties beg end)
  (goto-char (point-min))
  (insert \"text\")
  (delete-region beg end)
  (re-search-forward \"regexp\" nil t)       ;; nil limit, no error on fail
  (count-lines (point-min) (point-max))
  (line-number-at-pos))

;; Read without opening visibly
(with-temp-buffer
  (insert-file-contents \"/path/to/file\")
  (buffer-string))

;; Process each line
(with-current-buffer buffer
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        ;; process line
        )
      (forward-line 1))))
```

---

## Files and directories

```elisp
(file-exists-p path)
(file-directory-p path)
(file-name-directory \"/a/b/c.el\")          ;; \"/a/b/\"
(file-name-nondirectory \"/a/b/c.el\")       ;; \"c.el\"
(file-name-base \"/a/b/c.el\")               ;; \"c\"
(expand-file-name \"~/project\")            ;; absolute path
(file-relative-name \"/a/b/c\" \"/a\")        ;; \"b/c\"
(directory-files dir nil \".*\\\\.el$\")       ;; list .el files
(directory-files-recursively dir \".*\\\\.java$\")
(make-directory path t)                    ;; t = create parents

;; Structural files (.el, .py, .lisp, .cl): structural_* tools
;; Other project files: read_file / write_file (undo-able, boundary-checked)
```

---

## JSON

```elisp
;; Parse JSON string
(let ((data (json-parse-string json-str
                               :object-type 'alist
                               :array-type 'list
                               :null-object nil
                               :false-object :false)))
  (alist-get 'key data))

;; Serialize
(json-serialize '((key . \"value\") (n . 42)))
```

---

## Regular expressions

Emacs regexp differs from PCRE: use \\\\( \\\\) for groups (escaped parens), \\\\| for alternation.

```elisp
(string-match \"\\\\(foo\\\\)\\\\|\\\\(bar\\\\)\" s)   ;; match
(match-string 1 s)                          ;; captured group 1
(replace-regexp-in-string \"\\\\bword\\\\b\" \"replacement\" s)
(re-search-forward \"pattern\" nil t)        ;; nil=no limit, t=no error
```

---

## Org-mode operations

```elisp
(require 'org)
(require 'org-element)
(org-element-map (org-element-parse-buffer) 'headline
  (lambda (hl)
    (org-element-property :raw-value hl)))

;; Find TODO items
(org-element-map (org-element-parse-buffer) 'headline
  (lambda (hl)
    (when (string= (org-element-property :todo-keyword hl) \"TODO\")
      (org-element-property :raw-value hl))))
```

---

## Error handling

```elisp
(condition-case err
    (do-risky-thing)
  (file-missing
   (format \"File not found: %s\" (error-message-string err)))
  (error
   (format \"Error: %s\" (error-message-string err))))

(ignore-errors (risky-call))

(unwind-protect
    (progn (open-something) (use-it))
  (close-something))                        ;; always runs
```

---

## Common pitfalls

| Mistake | Correct |
|---------|---------|
| `(car nil)` → nil | guard with `(when list (car list))` |
| `(string= nil \"foo\")` → error | `(equal nil \"foo\")` → nil |
| `(+ 1 \"2\")` → error | `(+ 1 (string-to-number \"2\"))` |
| `(let (x y) ...)` → both nil | `(let ((x 1) (y 2)) ...)` |
| Forgetting `save-excursion` | wrap buffer navigation in it |
| `(search-forward s)` throws on miss | use `(search-forward s nil t)` |
| `(setq x (cons item x))` | builds in reverse — nreverse at end |

---

## Patterns for common agent tasks

### Replace one function in a .el file
```
structural_tree path: emagent-tools.el
structural_replace path: emagent-tools.el
  symbol: emagent-tool-read-file
  new_body: |
    (defun emagent-tool-read-file (path &optional line limit)
      \"Return contents of PATH as a string.\"
      ...)
check_structural_file path: emagent-tools.el
```

### Add a helper defun after an existing one
```
structural_insert path: foo.el
  after_symbol: foo-setup
  node: |
    (defun foo-teardown ()
      ...)
```

### Count occurrences of a pattern in project files
```elisp
(let ((total 0))
  (dolist (file (directory-files-recursively
                 emagent-tools--project-directory \"\\\\.java$\"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward \"pattern\" nil t)
        (cl-incf total))))
  (format \"Found %d occurrences\" total))
```

### Collect all headings from an org file
```elisp
(with-temp-buffer
  (insert-file-contents \"/path/to/file.org\")
  (org-mode)
  (let ((headings nil))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (hl)
        (push (format \"%s %s\"
                      (make-string (org-element-property :level hl) ?*)
                      (org-element-property :raw-value hl))
              headings)))
    (string-join (nreverse headings) \"\\n\")))
```

### Process lines matching a pattern
```elisp
(with-temp-buffer
  (insert-file-contents file)
  (let (results)
    (goto-char (point-min))
    (while (re-search-forward \"^\\\\(ERROR\\\\|WARN\\\\)\" nil t)
      (push (buffer-substring-no-properties
             (line-beginning-position) (line-end-position))
            results))
    (string-join (nreverse results) \"\\n\")))
```

### Transform a list to a formatted table
```elisp
(let ((items '((\"Alice\" 42) (\"Bob\" 37) (\"Carol\" 55))))
  (concat \"| Name | Age |\\n|------+-----|\\n\"
          (string-join
           (mapcar (lambda (row)
                     (format \"| %s | %d |\" (car row) (cadr row)))
                   items)
           \"\\n\")))
```"
  "Emacs Lisp reference guide served to the agent via the `elisp_guide' MCP tool.")

(provide 'emagent-prompts-elisp-guide)

;;; emagent-prompts-elisp-guide.el ends here
