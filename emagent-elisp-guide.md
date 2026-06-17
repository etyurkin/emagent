# Emacs Lisp Guide for emagent

Reference document for the agent. Call the `elisp_guide` tool before writing
non-trivial Emacs Lisp. Covers patterns, idioms, common pitfalls, and the
functions most useful in emagent sessions.

---

## Core rules

1. **Always `check_elisp` before `eval`** for forms longer than 3 lines.
2. **Wrap multiple forms in `progn`** or pass them as separate eval calls.
3. **Use `let*` for sequential bindings** — never nest more than 3 levels deep.
4. **Return a useful string** from eval — the result is your tool output.
5. **Prefer emagent tools** over raw Elisp for file I/O (boundary checks, undo).
6. **Discover before guessing** — `apropos` → `apropos_doc` → `describe_symbol`.

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
       (result (format "Found %d files" count)))
  result)
```

---

## Strings

```elisp
(string-join '("a" "b" "c") ", ")          ;; "a, b, c"
(split-string "a,b,c" "," t)               ;; ("a" "b" "c")  — t trims empties
(string-trim "  hello  ")                  ;; "hello"
(string-trim-left s) / (string-trim-right s)
(concat "foo" "bar")                       ;; "foobar"
(format "%s has %d items" name count)
(string-match-p REGEXP string)             ;; non-nil if matches
(replace-regexp-in-string "old" "new" s)
(substring s 0 5)                          ;; first 5 chars
(string-prefix-p "foo" s)
(string-suffix-p ".el" s)
(upcase s) / (downcase s) / (capitalize s)
(number-to-string 42) / (string-to-number "42")
(truncate-string-to-width s 80 nil nil "…")
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
  (puthash "key" "value" h)
  (gethash "key" h)          ;; "value"
  (remhash "key" h)
  (hash-table-count h)
  (maphash (lambda (k v) ...) h)
  h)
```

---

## Buffers

```elisp
;; Access
(buffer-list)                              ;; all buffers
(current-buffer)
(get-buffer "name")                        ;; nil if not found
(get-buffer-create "name")                 ;; creates if needed
(find-buffer-visiting "/path/to/file")     ;; nil if not visiting
(buffer-name buffer)
(buffer-file-name buffer)
(buffer-modified-p buffer)

;; Work inside a buffer without switching
(with-current-buffer buffer
  (buffer-string)                          ;; entire content as string
  (buffer-substring-no-properties beg end)
  (goto-char (point-min))
  (insert "text")
  (delete-region beg end)
  (search-forward "pattern")
  (re-search-forward "regexp" nil t)       ;; nil limit, no error on fail
  (count-lines (point-min) (point-max))
  (line-number-at-pos))

;; Read without opening visibly
(with-temp-buffer
  (insert-file-contents "/path/to/file")
  (buffer-string))

;; Process each line
(with-current-buffer buffer
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
        ;; process line
        )
      (forward-line 1))))
```

---

## Files and directories

```elisp
(file-exists-p path)
(file-directory-p path)
(file-readable-p path)
(file-name-directory "/a/b/c.el")          ;; "/a/b/"
(file-name-nondirectory "/a/b/c.el")       ;; "c.el"
(file-name-base "/a/b/c.el")               ;; "c"
(file-name-extension "/a/b/c.el")          ;; "el"
(expand-file-name "~/.emacs.d")            ;; absolute path
(abbreviate-file-name "/Users/foo/bar")    ;; "~/bar"
(file-relative-name "/a/b/c" "/a")        ;; "b/c"
(directory-files dir nil ".*\\.el$")       ;; list .el files
(directory-files-recursively dir ".*\\.java$")
(make-directory path t)                    ;; t = create parents

;; PREFER emagent read_file / write_file for project files
;; Use these only for files outside the project (with user confirmation)
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

;; Parse from buffer/file
(with-temp-buffer
  (insert-file-contents path)
  (json-parse-buffer :object-type 'alist))

;; Serialize
(json-serialize '((key . "value") (n . 42)))  ;; "{\"key\":\"value\",\"n\":42}"
```

---

## Regular expressions

Emacs regexp syntax differs from PCRE:

```
.        any char          \(  \)   group (escaped parens!)
*        0 or more         \|       alternation
+        1 or more         \b  \B   word boundary
?        0 or 1            \w  \W   word char
^  $     start/end line    \s- \s.  whitespace class
[abc]    char class        [:alpha:] [:digit:] etc.
```

```elisp
(string-match "\\(foo\\)\\|\\(bar\\)" s)   ;; match, note escaped parens
(match-string 1 s)                          ;; captured group 1
(replace-regexp-in-string "\\bword\\b" "replacement" s)
(re-search-forward "pattern" limit noerror)
```

---

## Org-mode operations

```elisp
(require 'org)
(require 'org-element)

;; Parse current org buffer
(org-element-parse-buffer)

;; Get element at point
(let ((el (org-element-at-point)))
  (org-element-type el)                     ;; 'headline, 'paragraph, etc.
  (org-element-property :raw-value el)      ;; heading text
  (org-element-property :level el)          ;; heading level (1,2,3...)
  (org-element-property :tags el))          ;; tag list

;; Iterate headlines
(org-element-map (org-element-parse-buffer) 'headline
  (lambda (hl)
    (org-element-property :raw-value hl)))

;; Find TODO items
(org-element-map (org-element-parse-buffer) 'headline
  (lambda (hl)
    (when (string= (org-element-property :todo-keyword hl) "TODO")
      (org-element-property :raw-value hl))))

;; Format timestamp
(format-time-string "[%Y-%m-%d %a]")

;; Insert at correct position
(org-end-of-subtree)
(insert "\n* New heading\n")
```

---

## Error handling

```elisp
;; Catch specific errors
(condition-case err
    (do-risky-thing)
  (file-missing
   (format "File not found: %s" (error-message-string err)))
  (error
   (format "Error: %s" (error-message-string err))))

;; Ignore errors silently
(ignore-errors (risky-call))

;; Signal an error
(user-error "Something went wrong: %s" detail)  ;; shown to user nicely
(error "Internal error: %s" detail)             ;; more severe

;; Cleanup on error
(unwind-protect
    (progn (open-something) (use-it))
  (close-something))                            ;; always runs
```

---

## Useful utilities

```elisp
;; Clipboard
(kill-new "text to copy")                   ;; copy to kill ring
(current-kill 0)                            ;; current kill ring content

;; Minibuffer
(read-string "Prompt: " "default")
(completing-read "Choose: " '("a" "b" "c") nil t)
(yes-or-no-p "Are you sure? ")
(y-or-n-p "Quick? ")

;; Messages
(message "Status: %s" value)               ;; show in echo area

;; Time
(format-time-string "%Y-%m-%d %H:%M:%S")
(float-time)                               ;; seconds since epoch
(time-subtract (current-time) (seconds-to-time 3600))

;; Math
(max 1 2 3) / (min 1 2 3)
(abs -5) / (round 3.7) / (floor 3.7) / (ceiling 3.2)
(mod 10 3) / (% 10 3)

;; Type checks
(stringp x) / (numberp x) / (listp x) / (symbolp x)
(null x) / (not x)
(consp x)                                  ;; non-nil list (has a cdr)
```

---

## Common pitfalls

| Mistake | Correct |
|---------|---------|
| `(car nil)` → nil, not error | guard with `(when list (car list))` |
| `(nth 99 '(1 2))` → nil | check length first |
| Modifying list while iterating | build new list with mapcar/seq-filter |
| `(setq x (cons item x))` | builds in reverse — nreverse at end |
| `(string= nil "foo")` → error | `(equal nil "foo")` → nil |
| `(+ 1 "2")` → error | `(+ 1 (string-to-number "2"))` |
| `(let (x y) ...)` → both nil | `(let ((x 1) (y 2)) ...)` |
| Forgetting `save-excursion` | wrap buffer navigation in save-excursion |
| `(search-forward s)` throws on miss | use `(search-forward s nil t)` — nil limit, t noerror |

---

## Patterns for common agent tasks

### Count occurrences of a pattern in project files
```elisp
(let ((total 0))
  (dolist (file (directory-files-recursively
                 emagent-tools--project-directory "\\.java$"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "pattern" nil t)
        (cl-incf total))))
  (format "Found %d occurrences" total))
```

### Collect all headings from an org file
```elisp
(with-temp-buffer
  (insert-file-contents "/path/to/file.org")
  (org-mode)
  (let ((headings nil))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (hl)
        (push (format "%s %s"
                      (make-string (org-element-property :level hl) ?*)
                      (org-element-property :raw-value hl))
              headings)))
    (string-join (nreverse headings) "\n")))
```

### Process lines matching a pattern
```elisp
(with-temp-buffer
  (insert-file-contents file)
  (let (results)
    (goto-char (point-min))
    (while (re-search-forward "^\\(ERROR\\|WARN\\)" nil t)
      (push (buffer-substring-no-properties
             (line-beginning-position) (line-end-position))
            results))
    (string-join (nreverse results) "\n")))
```

### Transform a list to a formatted table
```elisp
(let ((items '(("Alice" 42) ("Bob" 37) ("Carol" 55))))
  (concat "| Name | Age |\n|------+-----|\n"
          (string-join
           (mapcar (lambda (row)
                     (format "| %s | %d |" (car row) (cadr row)))
                   items)
           "\n")))
```
