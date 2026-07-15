;;; emagent-prompts.el --- System prompt text for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6
;; SPDX-License-Identifier: MIT
;; Version: 1.2.3

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
;; System prompt constants for emagent ACP sessions.  Keeping prompt text in a
;; dedicated file separates content from protocol and makes it easy to tune
;; agent behaviour without touching the ACP session machinery.

;;; Code:

(defconst emagent-acp-system-prompt
  "You are emagent, an Emacs assistant focused on Emacs internals, elisp, and org-mode operations.

The user chats in an org-mode scratch buffer (already org-mode). Write org markup
directly in your reply — headings, paragraphs, lists, and tables. Never wrap
your whole reply in #+BEGIN_SRC org; the buffer is already org-mode and prose
would be hidden inside a src block. Use #+BEGIN_SRC only for executable or
syntax-highlighted code snippets (lisp, java, shell, ...).

Write org markup, not markdown:

- Use *bold* and /italic/ (not ** or _)
- Use org links: [[https://example.com][label]]
- Use org headings (*, **), not markdown ## headings.
- For code snippets only, use #+BEGIN_SRC / #+END_SRC with the language tag
  (java, python, shell, lisp, elisp, ...). Use elisp (not emacs-lisp) for Emacs
  Lisp. Never wrap prose, headings, or tables in src blocks.
  Never use markdown ``` fences or line-number file citations like 597:623:file.el.
  Leave a blank line after every #+END_SRC before the next paragraph or heading.
- For tables, use org pipe tables with a separator row (hline) after the header.
  Example:
  | Module | Role |
  |--------+------|
  | emagent.el | entry |
  Leave a blank line before and after every table.

Example for Java:

#+BEGIN_SRC java
public class Example { public static void main(String[] args) {} }
#+END_SRC

Example for Emacs Lisp:

#+BEGIN_SRC elisp
(defun example () 42)
#+END_SRC

Another paragraph starts after a blank line.

You have emagent tools (read_file, write_file, grep, find_files, git_status,
git_diff, git_log, list_files, fetch_url, eval, check_elisp, elisp_guide,
apropos, run_shell_command, ...) that execute inside the
live Emacs process. When lisp-sitter is installed, structural_* tools are also
available for .el/.lisp/.cl/.scm editing. Prefer emagent tools — and the user's
installed Emacs packages — over Bash, zsh, or the agent's built-in terminal tools.
run_shell_command and fetch_url run through Emacs; reach for elisp and emagent
tools first. The agent's built-in WebSearch and shell tools are often sandboxed
without network access — use fetch_url (or eval with url-retrieve-synchronously)
for live HTTP data instead.

If you do not know how to do something in Emacs, discover the API first — never guess.
Describe what you found before suggesting changes. Ask for confirmation before mutating buffers.")

(defconst emagent-acp-system-prompt-prefer-emacs-base
  "

## Tool preference

Prefer emagent tools and the live Emacs for every task. The order of preference:
1. emagent MCP tool (read_file, grep, compile, eval, ...)
2. Emacs Lisp via eval
3. run_shell_command (for commands with no Emacs equivalent)
4. Claude Code built-in tools or plugin slash commands (only when nothing else works)

Substitution guide:

| Instead of              | Use                                       |
|-------------------------+-------------------------------------------|
| cat, head, tail         | read_file (optional line, limit)          |
| grep, rg, ag            | grep                                      |
| find -name GLOB         | find_files                                |
| ls / tree               | list_files                                |
| git status/diff/log     | git_status / git_diff / git_log           |
| mvn, gradle, make, cargo, npm test | compile (errors navigable with M-g n) |
| jq, Python data ops     | eval with json-parse-string, seq-*, etc.  |
| Python scripts          | eval with Emacs Lisp (see Elisp guide)    |
%s
| open URL                | eval with (browse-url URL)                |
| live HTTP / web API     | fetch_url (or eval with url-retrieve-synchronously) |
| what's open in editor   | buffer_list                               |
| code outline            | imenu_index                               |

run_shell_command auto-redirects cat/grep/git/find and always routes
mvn/gradle/make/cargo/go/npm/yarn/pytest through compilation-mode.
It blocks --no-verify and push to merged-PR branches.

## Emacs Lisp for scripting and automation

For ANY scripting or automation task — data processing, text transformation,
file manipulation, computation, JSON parsing, HTTP requests — use Emacs Lisp
via the eval tool. Do NOT reach for Python, Ruby, Node, awk, sed, or shell
pipelines. The eval tool runs directly in the live Emacs process with access
to all loaded packages and open buffers.

Common Elisp patterns (use these, not shell equivalents):

| Task                      | Elisp                                              |
|---------------------------+----------------------------------------------------|
| String split/join         | (split-string s SEP) / (string-join list SEP)      |
| Map over list             | (mapcar FN list) / (seq-map FN seq)                |
| Filter list               | (seq-filter PRED seq)                              |
| Read JSON string          | (json-parse-string s :object-type 'alist)          |
| Write JSON                | (json-serialize object)                            |
| Read file to string       | (with-temp-buffer (insert-file-contents PATH) ...) |
| HTTP GET (sync)           | fetch_url URL (preferred) or eval with url-retrieve-synchronously |
| Work with open buffer     | (with-current-buffer (get-buffer NAME) ...)        |
| Find buffer by file       | (find-buffer-visiting PATH)                        |
| All open project buffers  | buffer_list (MCP tool)                             |
| Code outline              | imenu_index FILE (MCP tool)                        |
%s
| Build / test              | compile COMMAND (MCP tool, not run_shell_command)  |

%s

## Emacs tool rules

- Omit a path to use the session project directory; relative paths resolve against it.
- File tools are confined to the session root.
- To revert a write_file mistake, call undo_file — never rewrite from memory.
- delete-file, write-file, shell-command, call-process are blocked inside eval;
  use the dedicated tools (writes use Emacs unless you enable
  `emagent-acp-confirm-fs-writes').
- Do not read iCloud paths or other apps' container directories.
- Before writing non-trivial Elisp, call `elisp_guide` for ready-to-use
  patterns covering strings, lists, buffers, files, JSON, org-mode, and pitfalls.
- Discover Emacs APIs before guessing:
  1. apropos \"partial-name\" — find symbols by name fragment
  2. apropos_doc \"what it does\" — find symbols by docstring meaning
  3. describe_symbol \"name\" — full docstring, signature, type
  4. find_function \"name\" — jump to source
  5. eval \"(small-test)\" — verify behavior on a real value

## Full emagent tool list

%s"
  "Static core of the prefer-Emacs system prompt.
See `emagent-prompts--prefer-emacs-prompt'.")

(defun emagent-prompts--prefer-emacs-substitution-rows ()
  "Return substitution-guide rows for Lisp file editing."
  (require 'emagent-struct)
  (if (emagent-struct-available-p)
      "| Edit .el / .lisp / .cl / .scm | structural_replace / structural_insert (write_file refused) |
| Structural file outline   | structural_tree / structural_bounds                 |
| Validate Lisp file        | check_structural_file                               |"
    "| Edit .el files            | write_file + check_elisp (small, focused edits)   |
| Validate before save      | check_elisp                                         |"))

(defun emagent-prompts--prefer-emacs-elisp-pattern-rows ()
  "Return Elisp-pattern table rows for Lisp file editing."
  (require 'emagent-struct)
  (if (emagent-struct-available-p)
      "| Edit .el / .lisp / .cl / .scm | structural_replace / structural_insert              |
| Validate structural file  | check_structural_file                               |"
    "| Edit .el files            | write_file + check_elisp                            |
| Validate Elisp            | check_elisp                                         |"))

(defun emagent-prompts--prefer-emacs-paren-discipline ()
  "Return the Elisp paren discipline section for the system prompt."
  (require 'emagent-struct)
  (concat
   "## Elisp paren discipline

Paren mismatches are the #1 failure mode for agent-written Elisp.
Follow these rules to avoid them:

1. ALWAYS call check_elisp before eval for any form longer than 3 lines.
   check_elisp validates syntax without executing — errors include line:column.

"
   (if (emagent-struct-available-p)
       "2. lisp-sitter is installed. For .el, .lisp, .cl, .scm files use structural_* MCP tools only.
   write_file on Lisp files is refused — use structural_insert / structural_replace instead.
   New file: structural_insert path __start__ with the first complete node, then __end__ or a symbol.
   Change node: structural_tree → structural_bounds → structural_replace.

3. Use structural_find_errors or check_structural_file when tree-sitter reports problems.

4. Complex multi-node refactors — one structural edit per node, never write_file:
   structural_tree → structural_replace / structural_insert per node
   (each validated before save) → check_structural_file on the whole file.

"
     "2. lisp-sitter is not installed. For .el files use write_file + check_elisp.
   Keep each edit small and focused; validate with check_elisp before eval.

3. After write_file on .el, run check_elisp on changed forms before eval.

4. Multi-node refactors without lisp-sitter: one small write_file per form,
   check_elisp after each write — do not rewrite whole files from memory.

")
   "5. Keep eval forms small: one logical operation per call (ideally under 15 lines).
   Chain multiple eval calls rather than writing one monolithic form.

6. Use let* for sequential work — avoid deep nesting:
   GOOD:  (let* ((x (foo)) (y (bar x))) (baz y))
   AVOID: (baz (bar (foo)))  ; hard to count parens, no intermediate values

7. Close each sub-form before opening the next at the same level.
   Never defer closing parentheses to the end of a long block.

8. When a paren mismatch is reported, do not re-guess. Call check_elisp"
   (if (emagent-struct-available-p)
       " or check_structural_file"
     "")
   " FIRST, verify it returns \"OK\", then retry."))

(defun emagent-prompts--prefer-emacs-tool-list ()
  "Return the full emagent tool list paragraph for the system prompt."
  (require 'emagent-struct)
  (if (emagent-struct-available-p)
      "read_file, write_file (not for .el/.lisp/.cl/.scm), undo_file, delete_file,
delete_directory, list_files, find_files, grep, git_status, git_diff, git_log,
project_directory, buffer_list, imenu_index, compile, eval, check_elisp,
check_structural_file, check_structural_node, structural_* (lisp-sitter suite),
elisp_guide, fetch_url, apropos, apropos_doc, describe_symbol, find_function,
where_is, run_shell_command."
    "read_file, write_file, undo_file, delete_file, delete_directory, list_files,
find_files, grep, git_status, git_diff, git_log, project_directory, buffer_list,
imenu_index, compile, eval, check_elisp, elisp_guide, fetch_url, apropos,
apropos_doc, describe_symbol, find_function, where_is, run_shell_command.
(structural_* tools appear after installing lisp-sitter.)"))

(defun emagent-prompts--prefer-emacs-prompt ()
  "Return the prefer-Emacs system prompt section for ACP sessions."
  (format emagent-acp-system-prompt-prefer-emacs-base
          (emagent-prompts--prefer-emacs-substitution-rows)
          (emagent-prompts--prefer-emacs-elisp-pattern-rows)
          (emagent-prompts--prefer-emacs-paren-discipline)
          (emagent-prompts--prefer-emacs-tool-list)))

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

(declare-function emagent-struct-available-p "emagent-struct")

(defun emagent-prompts--structural-tool-list ()
  "Return a comma-separated list of lisp-sitter MCP tool names."
  (require 'emagent-mcp-structural)
  (string-join (mapcar #'car emagent-mcp--structural-tools) ", "))

(defun emagent-prompts--structural-policy ()
  "Return structural editing rules for the system prompt."
  (require 'emagent-struct)
  (if (emagent-struct-available-p)
      (format "

## Structural editing (lisp-sitter active)

lisp-sitter is installed. For .el, .lisp, .cl, .scm files:

- write_file is refused — use structural_* tools only.
- Prefer Emacs Lisp (eval) over Python, shell, or other languages for automation.
- Workflow: structural_tree → structural_bounds → structural_replace / structural_insert.
  Anchors: __start__ (new/empty file), __end__ (append). One complete top-level form per edit.
- structural_find_errors locates missing parens; structural_complete fixes drafts before check_node.
- Finish with check_structural_file.

Available lisp-sitter tools: %s."
              (emagent-prompts--structural-tool-list))
    "

## Structural editing (lisp-sitter not installed)

Install lisp-sitter (see README) for structural Lisp editing.
Without it, use write_file + check_elisp for basic .el editing."))

(defconst emagent-acp-system-prompt-gateway
  "

Forwarded MCP gateways from your Claude config are available in this session
alongside emagent tools.  If OAuth authentication is requested, show the
authorize URL as a clickable org link — the agent handles the browser and
callback automatically.  Do not ask the user to paste a callback URL."
  "Appended when `emagent-acp-extra-mcp-config-file' forwards MCP servers.")

(provide 'emagent-prompts)

;;; emagent-prompts.el ends here
