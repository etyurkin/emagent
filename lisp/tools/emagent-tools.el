;;; emagent-tools.el --- Emacs tool handlers for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.3.1
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
;; Emacs-native tools, shell routing, and structural editing/prompts.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'emagent-chat-ui)
(require 'emagent-log)
(require 'emagent-tools-shell)
(require 'emagent-tools-buffers)

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

You have emagent MCP dispatchers (fs, search, git, shell, eval, elisp,
structural, fetch_url, project_directory, where_is, list_*). Pass op= for
sub-operations on dispatchers (fs op=read, git op=status, structural op=replace).
Prefer emagent tools and installed Emacs packages over Bash or the agent's
built-in terminal. shell and fetch_url run through Emacs.
Use fetch_url (or eval with url-retrieve-synchronously) for live HTTP when the
agent's WebSearch/shell are sandboxed.

If you do not know how to do something in Emacs, discover the API first — never guess.
Describe what you found before suggesting changes. Ask for confirmation before mutating buffers.")

(defconst emagent-acp-system-prompt-prefer-emacs-base
  "

## Tool preference

Prefer emagent MCP tools and the live Emacs for every task. Order:
1. emagent MCP (fs/search/git/shell/eval/elisp/structural/list_*/...)
2. Emacs Lisp via eval for in-editor automation
3. shell op=run when there is no Emacs equivalent
4. Agent built-ins / plugin slash commands only as last resort

Substitution guide:

| Instead of              | Use                                       |
|-------------------------+-------------------------------------------|
| cat, head, tail         | fs op=read (optional line, limit)         |
| Edit / StrReplace       | fs op=edit (old_string, new_string, tick) |
| grep, rg, ag            | search                                    |
| find -name GLOB         | fs op=find                                |
| ls / tree               | fs op=list                                |
| git status/diff/log     | git op=status|diff|log                    |
| mvn, gradle, make, cargo, npm, pytest, go test | shell op=compile |
| jq / data transforms    | eval (or the project's own tools)         |
| Count/filter without dump | eval (return numbers/paths only)        |
%s
| open URL                | eval with (browse-url URL)                |
| live HTTP / web API     | fetch_url                                 |
| what's open in editor   | list_buffers / buffer_info / list_windows  |
| frames / marks / regs   | list_frames / list_marks / list_registers |
| bookmarks               | list_bookmarks                            |
| flymake diagnostics     | list_diagnostics                          |
| subprocesses / network  | list_processes                            |
| project root / keybinds | project_directory / where_is              |
| code outline (any lang) | imenu_index                               |

shell op=run auto-redirects cat/grep/git/find and routes
mvn/gradle/make/cargo/go/npm/yarn/pytest through compilation-mode
(op=compile). It blocks --no-verify and push to merged-PR branches.

## Context discipline

1. Outline before large reads: imenu_index (Java, Python, TS, Go, …).
   For Lisp + lisp-sitter, prefer structural op=tree then op=get.
2. Prefer fs/search with line/limit; do not dump whole files into chat.
3. Builds/tests use shell op=compile (mvn, gradle, pytest, npm, cargo, …).
4. Analyze with eval when useful; return answers, not raw tool dumps.
5. When context is high, the client may suggest /compact — run it.

## Turns and waiting

Tool calls block until they finish or time out. Emagent has no background
\"I'll check progress later\" mode driven by prose.

- Do not end a turn with \"Waiting…\", \"Checking progress…\", or similar.
  That text does nothing; the session goes idle.
- For long builds/tests: shell op=compile (or op=run) with a large enough
  timeout, and stay on that call until the result returns. On timeout,
  retry with a larger timeout — do not narrate waiting.
- The only client-paced loop is ScheduleWakeup (delay + prompt). Call that
  tool if you truly need to resume later; otherwise keep working in-turn.

## Emacs tool rules

- Omit a path to use the session project directory; relative paths resolve against it.
- File tools are confined to the session root.
- To revert an fs write/edit, call fs op=undo with expected_tick — never rewrite from memory.
- Concurrent MCP mutates (write/edit/undo/delete): pass expected_tick from
  the latest fs op=read (or structural op=get) emagent-tick; stale_revision
  means re-read and retry.
- Prefer fs op=edit for targeted non-Lisp edits (unique old_string → new_string).
  Use fs op=write only when replacing the whole file. Do not switch to agent
  Edit/Read mid-turn after an emagent fs op=read.
- delete-file, write-file, shell-command, call-process are blocked inside eval;
  use fs/shell/structural (writes use Emacs unless you enable
  `emagent-acp-confirm-fs-writes').
- Do not read iCloud paths or other apps' container directories.
- Before writing non-trivial Elisp, call elisp op=guide.
- Discover Emacs APIs: elisp op=apropos → apropos_doc → describe → find_function.

## Full emagent tool list

%s"
  "Static core of the prefer-Emacs system prompt.
See `emagent-prompts--prefer-emacs-prompt'.")


(defun emagent-prompts--prefer-emacs-substitution-rows ()
  "Return substitution-guide rows for Lisp file editing."
  (if (emagent-struct-available-p)
      "| Edit .el / .lisp / .cl / .scm | structural op=replace|insert (fs write refused) |
| Edit .clef (Clef)         | fs op=read/write/edit (not lisp-sitter)             |
| Structural file outline   | structural op=tree|outline|bounds                   |
| Validate Lisp file        | structural op=check_file                            |"
    "| Edit .el files            | fs op=write + elisp op=check (small edits)          |
| Edit .clef (Clef)         | fs op=read/write/edit                             |
| Validate before save      | elisp op=check                                      |"))

(defun emagent-prompts--prefer-emacs-elisp-pattern-rows ()
  "Return Elisp-pattern table rows for Lisp file editing."
  (if (emagent-struct-available-p)
      "| Edit .el / .lisp / .cl / .scm | structural op=replace|insert|edit                   |
| Validate structural file  | structural op=check_file                            |"
    "| Edit .el files            | fs op=write + elisp op=check                        |
| Validate Elisp            | elisp op=check                                      |"))

(defun emagent-prompts--prefer-emacs-paren-discipline ()
  "Return the Elisp paren discipline section for the system prompt."
  (concat
   "## Elisp paren discipline

Paren mismatches are the #1 failure mode for agent-written Elisp.
Follow these rules to avoid them:

1. ALWAYS call elisp op=check before eval for any form longer than 3 lines.
   It validates syntax without executing — errors include line:column.

"
   (if (emagent-struct-available-p)
       "2. lisp-sitter is installed. For .el, .lisp, .cl, .scm use structural only.
   fs op=write on Lisp files is refused — use structural op=insert|replace|edit.
   New file: structural op=insert after_symbol=__start__ with the first node.
   Change node: structural op=tree → op=replace (or op=edit).

3. Use structural op=find_errors or op=check_file when tree-sitter reports problems.

4. Complex multi-node refactors — one structural edit per node, never fs write:
   structural op=tree → op=replace|insert|edit per node → op=check_file.

"
     "2. lisp-sitter is not installed. For .el files use fs op=write + elisp op=check.
   Keep each edit small and focused; validate with elisp op=check before eval.

3. After fs op=write on .el, run elisp op=check on changed forms before eval.

4. Multi-node refactors without lisp-sitter: one small fs op=write per form,
   elisp op=check after each write — do not rewrite whole files from memory.

")
   "5. Keep eval forms small: one logical operation per call (ideally under 15 lines).
   Chain multiple eval calls rather than writing one monolithic form.

6. Use let* for sequential work — avoid deep nesting:
   GOOD:  (let* ((x (foo)) (y (bar x))) (baz y))
   AVOID: (baz (bar (foo)))  ; hard to count parens, no intermediate values

7. Close each sub-form before opening the next at the same level.
   Never defer closing parentheses to the end of a long block.

8. When a paren mismatch is reported, do not re-guess. Call elisp op=check"
   (if (emagent-struct-available-p)
       " or structural op=check_file"
     "")
   " FIRST, verify it returns \"OK\", then retry."))

(defun emagent-prompts--prefer-emacs-tool-list ()
  "Return the full emagent tool list paragraph for the system prompt."
  (if (emagent-struct-available-p)
      "fs (read/write/undo/delete/list/find), search, git (status/diff/log),
shell (run/compile), eval, list_buffers, buffer_info, list_windows, list_frames,
list_marks, list_registers, list_bookmarks, list_diagnostics, list_processes,
imenu_index, project_directory, where_is,
elisp (check/guide/apropos/apropos_doc/describe/find_function),
structural (lisp-sitter: tree/get/replace/insert/edit/...), fetch_url."
    "fs (read/write/undo/delete/list/find), search, git (status/diff/log),
shell (run/compile), eval, list_buffers, buffer_info, list_windows, list_frames,
list_marks, list_registers, list_bookmarks, list_diagnostics, list_processes,
imenu_index, project_directory, where_is,
elisp (check/guide/apropos/apropos_doc/describe/find_function), fetch_url.
(structural appears after installing lisp-sitter.)"))

(defun emagent-prompts--prefer-emacs-prompt ()
  "Return the prefer-Emacs system prompt section for ACP sessions."
  (format emagent-acp-system-prompt-prefer-emacs-base
          (emagent-prompts--prefer-emacs-substitution-rows)
          (emagent-prompts--prefer-emacs-tool-list)))

(defconst emagent-acp-elisp-guide
  "# Emacs Lisp Guide for emagent

Reference document for the agent. Call elisp op=guide before writing
non-trivial Emacs Lisp. Covers patterns, idioms, common pitfalls, and the
functions most useful in emagent sessions.

---

## Core rules

1. **Always `elisp op=check` before `eval`** for forms longer than 3 lines.
2. **Prefer lisp-sitter structural edits** when available (`structural op=tree`, `structural op=bounds`,
   `structural op=replace`, `structural op=insert`) — over full-file rewrites.
   When lisp-sitter is not installed, use fs op=write + elisp op=check for basic editing.
4. **Wrap multiple forms in `progn`** or pass them as separate eval calls.
5. **Use `let*` for sequential bindings** — never nest more than 3 levels deep.
6. **Return a useful string** from eval — the result is your tool output.
7. **Prefer emagent tools** over raw Elisp for file I/O (boundary checks, undo).
8. **Discover before guessing** — `elisp op=apropos` → `apropos_doc` → `describe`.
9. **Think in code** — analyze with eval and return only the answer; do not
   dump file bodies or huge tool output into the chat.

---

## Structural editing (.el, .lisp, .cl, .scm)

When lisp-sitter is installed (check `--json tree` MCP tools are available),
use sexp-boundary tools instead of line-based search/replace or full-file rewrites.

Workflow:

1. `structural op=tree` — list top-level forms (defun, define, class, ...)
2. **New file:** `structural op=insert` with `after_symbol` `__start__` and the first complete form
3. **Add forms:** `structural op=insert` with `__end__` or an existing symbol name
4. **Replace form:** `structural op=bounds` → `structural op=replace` (complete form text)

`structural op=replace` and `structural op=insert` validate syntax before save.
For `.el` files, the new form is eval'd so definitions are live for `eval` immediately.

Never pass partial form bodies to `structural op=replace` — always a complete s-expression.

### Multi-node refactors

When changing several top-level forms, plan with `structural op=tree`, then apply one
structural edit per form. Each call validates and saves independently — a mistake
only affects one form. Do not rewrite the whole file with `fs op=write`.

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

;; Structural files (.el, .py, .lisp, .cl): structural op=...
;; Other project files: fs op=read / fs op=write (undo-able, boundary-checked)
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
structural op=tree path: emagent-tools.el
structural op=replace path: emagent-tools.el
  symbol: emagent-tool-read-file
  new_body: |
    (defun emagent-tool-read-file (path &optional line limit)
      \"Return contents of PATH as a string.\"
      ...)
structural op=check_file path: emagent-tools.el
```

### Add a helper defun after an existing one
```
structural op=insert path: foo.el
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

(defun emagent-prompts--structural-tool-list ()
  "Return a short description of the structural MCP dispatcher."
  "structural (ops: tree, get, replace, insert, edit, check_file, ...)")

(defun emagent-prompts--structural-policy ()
  "Return a short Lisp-only structural addon for the system prompt."
  (if (emagent-struct-available-p)
      (format "

## Lisp editing (lisp-sitter)

When editing .el/.lisp/.cl/.scm: use structural op=tree → op=get/replace/insert
(fs op=write refused). One complete top-level form per edit; finish with
op=check_file. Tools: %s."
              (emagent-prompts--structural-tool-list))
    "

## Lisp editing

For .el without lisp-sitter: fs op=write + elisp op=check. Install lisp-sitter
for structural sexp edits."))

(defconst emagent-acp-system-prompt-gateway
  "

## External MCP servers

External MCP servers are configured. Prefer their domain tools over shell/curl.
If a server is a meta-proxy (search/list/describe/dispatch), discover the
backend tool and schema first, then call it. For OAuth, show the authorize
URL as an org link; use `/mcp' login — do not ask for callback paste."
  "Appended when Claude or Cursor has external MCP servers configured.")

(defgroup emagent-struct nil
  "Structural file editing via lisp-sitter CLI."
  :group 'emagent-tools)

(defcustom emagent-struct-lisp-sitter-bin
  (executable-find "lisp-sitter")
  "Path to the lisp-sitter binary.
When nil, structural Lisp tools are unavailable and the agent
falls back to fs op=write + elisp op=check."
  :type '(choice (string :tag "Path to lisp-sitter binary")
                 (const :tag "Not installed" nil))
  :group 'emagent-struct)

(defcustom emagent-struct-eval-after-structural-edit t
  "When non-nil, eval the changed form after a structural write."
  :type 'boolean
  :group 'emagent-struct)

(defcustom emagent-struct-require-for-lisp-files t
  "When non-nil and lisp-sitter is installed, refuse fs op=write on Lisp files.

Agents must use structural MCP ops for .el, .lisp, .cl, and .scm files."
  :type 'boolean
  :group 'emagent-struct)

(defconst emagent-tools--structural-eval-heads
  '(defun cl-defun defmacro cl-defmacro defsubst cl-defsubst
    defvar defvar-local defconst defcustom)
  "Top-level heads safe to eval after a structural write.

Only definition forms are reloaded into the live Emacs; bare progns and
side-effect forms are never eval'd from structural edit results.")

(defun emagent-tools--structural-eval-after-edit (form-str)
  "Eval FORM-STR after a structural write when it is a definition form.

Honors `emagent-struct-eval-after-structural-edit'.  Only heads in
`emagent-tools--structural-eval-heads' run; other top-level forms are
skipped so approving a structural MCP edit cannot execute `delete-file',
`make-process', or similar."
  (when (and emagent-struct-eval-after-structural-edit
             (stringp form-str)
             (not (string-empty-p (string-trim form-str))))
    (condition-case nil
        (let* ((form (read form-str))
               (head (and (consp form) (car form))))
          (when (memq head emagent-tools--structural-eval-heads)
            (eval form)))
      (error nil))))

(defun emagent-struct--clef-file-p (path)
  "Return non-nil when PATH is a Clef source file (.clef)."
  (and (stringp path) (string-match-p "\\.clef\\'" path)))

(defun emagent-struct--reject-unsupported (path)
  "Signal when PATH is not a lisp-sitter language (e.g. .clef)."
  (when (emagent-struct--clef-file-p path)
    (error ".clef is not a lisp-sitter language; use fs op=read/write/edit")))

;; ── Language detection ────────────────────────────────────────────

(defun emagent-struct--lang-for (path)
  "Return language id string for PATH based on extension."
  (emagent-struct--reject-unsupported path)
  (cond
   ((string-match-p "\\.el\\'" path) "elisp")
   ((string-match-p "\\.lisp\\'" path) "commonlisp")
   ((string-match-p "\\.cl\\'" path) "commonlisp")
   ((string-match-p "\\.scm\\'" path) "scheme")
   ((string-match-p "\\.ss\\'" path) "scheme")
   ((string-match-p "\\.sld\\'" path) "scheme")
   (t "elisp")))

(defun emagent-struct--lisp-file-p (path)
  "Return non-nil when PATH is a supported Lisp file."
  (and (stringp path)
       (string-match-p
        "\\.\\(el\\|lisp\\|cl\\|scm\\|ss\\|sld\\)\\'" path)))

;; ── CLI invocation ────────────────────────────────────────────────

(defun emagent-struct--lisp-sitter-error (output)
  "Format non-zero lisp-sitter OUTPUT as an error string."
  (truncate-string-to-width
   (car (split-string output "\n" t)) 80 nil nil "…"))

(defun emagent-struct--call-async (callback content &rest args)
  "Pipe CONTENT to lisp-sitter ARGS; call CALLBACK with (output is-error)."
  (emagent-struct--ensure)
  (apply #'emagent-tools--run-process-input-async
         (lambda (output is-error)
           (if is-error
               (funcall callback
                        (format "lisp-sitter exited: %s"
                                (emagent-struct--lisp-sitter-error output))
                        t)
             (funcall callback (string-trim output) nil)))
         content emagent-struct-lisp-sitter-bin args))

(defun emagent-struct--call (content &rest args)
  "Pipe CONTENT as stdin to lisp-sitter ARGS, return trimmed stdout.
Signal an error when lisp-sitter exits non-zero."
  (emagent-struct--ensure)
  (with-temp-buffer
    (let ((out (current-buffer))
          exit)
      (with-temp-buffer
        (insert content)
        (setq exit (apply #'call-process-region (point-min) (point-max)
                          emagent-struct-lisp-sitter-bin nil out nil args)))
      (if (= exit 0)
          (string-trim (buffer-string))
        (error "Lisp-sitter exited %d: %s" exit
               (emagent-struct--lisp-sitter-error (buffer-string)))))))

(defun emagent-struct--call-path (&rest args)
  "Run lisp-sitter ARGS against a file path; return trimmed stdout."
  (emagent-struct--ensure)
  (dolist (arg args)
    (when (stringp arg) (emagent-struct--reject-unsupported arg)))
  (with-temp-buffer
    (let ((exit (apply #'call-process emagent-struct-lisp-sitter-bin nil
                      (current-buffer) nil args)))
      (if (= exit 0)
          (string-trim (buffer-string))
        (error "Lisp-sitter exited %d: %s" exit
               (emagent-struct--lisp-sitter-error (buffer-string)))))))

(defun emagent-struct--call-path-async (callback &rest args)
  "Run lisp-sitter ARGS against a file path; call CALLBACK with (output is-error)."
  (emagent-struct--ensure)
  (dolist (arg args)
    (when (stringp arg) (emagent-struct--reject-unsupported arg)))
  (apply #'emagent-tools--run-process-async
         (lambda (output is-error)
           (if is-error
               (funcall callback
                        (format "lisp-sitter exited: %s"
                                (emagent-struct--lisp-sitter-error output))
                        t)
             (funcall callback (string-trim output) nil)))
         emagent-struct-lisp-sitter-bin args))

(define-error 'emagent-struct-unavailable
  "lisp-sitter is not installed; install it with `make install` in the lisp-sitter repo"
  'error)

(defun emagent-struct--ensure ()
  "Signal an error when lisp-sitter is unavailable."
  (unless (and emagent-struct-lisp-sitter-bin
               (file-executable-p emagent-struct-lisp-sitter-bin))
    (signal 'emagent-struct-unavailable
            (list "lisp-sitter binary not found on exec-path"))))

;; ── Public API ────────────────────────────────────────────────────

(defun emagent-struct-available-p ()
  "Return non-nil when lisp-sitter is installed and executable."
  (and emagent-struct-lisp-sitter-bin
       (file-executable-p emagent-struct-lisp-sitter-bin)))

(defun emagent-struct-tree (content path &optional depth)
  "Return JSON structural outline of CONTENT for PATH's language.

Arguments: DEPTH."
  (emagent-struct--ensure)
  (let ((args (list "tree" "-" "--json" "--lang" (emagent-struct--lang-for path))))
    (when (and depth (> depth 1))
      (setq args (append args (list "--depth" (number-to-string depth)))))
    (apply #'emagent-struct--call content args)))

(defun emagent-struct-bounds (content path symbol)
  "Return START:END string for SYMBOL in CONTENT for PATH's language."
  (emagent-struct--ensure)
  (emagent-struct--call content "bounds" "-" symbol
                        "--lang" (emagent-struct--lang-for path)))

(defun emagent-struct-replace (content path symbol new-body)
  "Replace SYMBOL's form in CONTENT with NEW-BODY.
Return the updated file content.  CONTENT is PATH's current content."
  (emagent-struct--ensure)
  (emagent-struct--call content "replace" "-" symbol
                        "--body" new-body
                        "--lang" (emagent-struct--lang-for path)))

(defun emagent-struct-insert (content path after-symbol node)
  "Insert NODE after AFTER-SYMBOL in CONTENT for PATH's language.
Return the updated file content."
  (emagent-struct--ensure)
  (emagent-struct--call content "insert" "-" after-symbol
                        "--node" node
                        "--lang" (emagent-struct--lang-for path)))

(defun emagent-struct-check (content path)
  "Validate CONTENT for PATH's language.  Return \"OK\" or error text."
  (emagent-struct--ensure)
  (let ((out (emagent-struct--call content "check" "-"
                                   "--lang" (emagent-struct--lang-for path))))
    (if (string-match "^[^:]+: \\(.*\\)$" out)
        (match-string 1 out)
      out)))

(defun emagent-struct-check-node (content lang)
  "Validate a single complete top-level CONTENT for LANG.
Returns \"OK\" or error text."
  (emagent-struct--ensure)
  (emagent-struct--call content "check-node"
                        "--lang" lang "--body-file" "-"))

(defun emagent-struct-get (content path symbol)
  "Return the full text of SYMBOL's form from CONTENT for PATH's language."
  (emagent-struct--ensure)
  (emagent-struct--call content "get" "-" symbol
                        "--lang" (emagent-struct--lang-for path)))

(defun emagent-struct-complete (lang body)
  "Complete missing closing parens in BODY for LANG."
  (emagent-struct--ensure)
  (emagent-struct--call body "complete" "--lang" lang "--body-file" "-"))

(defun emagent-struct-find-errors (path)
  "Return tree-sitter error report for file at absolute PATH."
  (emagent-struct--call-path "find-errors" path))

(defun emagent-struct-context (path)
  "Return structural context (outline + forms) for file at PATH."
  (emagent-struct--call-path "context" path))

(defun emagent-struct-format-file (path &optional write)
  "Re-indent file at PATH; when WRITE is non-nil, save the result."
  (let ((args (list "fmt" path)))
    (when write (setq args (append args '("--write"))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-rename-file (path old new &optional refs no-refs)
  "Rename form OLD to NEW in file at PATH; return updated file text.

Arguments: REFS, NO-REFS."
  (let ((args (list "rename" path old new)))
    (when refs (setq args (append args '("--refs"))))
    (when no-refs (setq args (append args '("--no-refs"))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-wrap-file (path symbol wrap &optional bindings condition)
  "Wrap SYMBOL's body in WRAP construct in file at PATH.

Arguments: BINDINGS, CONDITION."
  (let ((args (list "wrap" path symbol "--in" wrap)))
    (when bindings (setq args (append args (list "--bindings" bindings))))
    (when condition (setq args (append args (list "--condition" condition))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-remove-file (path symbol &optional keep-calls)
  "Remove top-level SYMBOL from file at PATH.

Arguments: KEEP-CALLS."
  (let ((args (list "remove" path symbol)))
    (when keep-calls (setq args (append args '("--keep-calls"))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-move-file (path symbol after)
  "Move SYMBOL after AFTER in file at PATH."
  (emagent-struct--call-path "move" path symbol after))

(defun emagent-struct-substitute-file (path symbol pattern replacement)
  "Substitute PATTERN with REPLACEMENT inside SYMBOL in file at PATH."
  (emagent-struct--call-path "substitute" path symbol
                             "--pattern" pattern "--replacement" replacement))

(defun emagent-struct-extract-file (path symbol pattern name &optional params)
  "Extract PATTERN into new function NAME inside SYMBOL in file at PATH.

Arguments: PARAMS."
  (let ((args (list "extract" path symbol "--pattern" pattern "--name" name)))
    (when (and params (not (string-empty-p params)))
      (setq args (append args (list "--params" params))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-callers-file (path symbol)
  "Return callers of SYMBOL in file at PATH."
  (emagent-struct--call-path "callers" path symbol))

(defun emagent-struct-instrument-file (path symbol &optional with at wrap)
  "Instrument SYMBOL in file at PATH.

Arguments: WITH, WRAP."
  (let ((args (list "instrument" path symbol)))
    (when with (setq args (append args (list "--with" with))))
    (when at (setq args (append args (list "--at" at))))
    (when wrap (setq args (append args (list "--wrap" wrap))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-flatten-file (path symbol)
  "Inline SYMBOL's body at call sites in file at PATH."
  (emagent-struct--call-path "flatten" path symbol))

(defun emagent-struct-convert-let-file (path symbol to)
  "Convert let/let* for SYMBOL to TO in file at PATH."
  (emagent-struct--call-path "convert-let" path symbol "--to" to))

(defun emagent-struct-splice-file (path symbol pattern)
  "Splice PATTERN inside SYMBOL in file at PATH."
  (emagent-struct--call-path "splice" path symbol "--pattern" pattern))

(defun emagent-struct-raise-file (path symbol pattern)
  "Raise PATTERN inside SYMBOL in file at PATH."
  (emagent-struct--call-path "raise" path symbol "--pattern" pattern))

(defun emagent-struct-write-required-p (path)
  "Return non-nil when PATH must be edited with structural tools."
  (and emagent-struct-require-for-lisp-files
       (emagent-struct-available-p)
       (emagent-struct--lisp-file-p path)))

(defvar emagent-acp-prefer-emacs)

(defvar emagent-tools--chat-buffer)
(defvar emagent-tools--session-allowed-tools)
(defvar emagent-tools-allow-all-function)
(defvar emagent-tools-age--session-key)
(defvar emagent-mcp--current-session-token)
(defvar emagent-tools--project-directory)
(defvar emagent-tools--root-boundary)
(defvar emagent-tools--acp-session-p)
(defvar emagent-tools--expected-file-tick)

(defvar emagent-usage--session-key)

(defgroup emagent-shell nil
  "Shell command routing for emagent."
  :group 'emagent)

(defcustom emagent-shell-block-no-verify t
  "When non-nil, refuse git commands that use --no-verify."
  :type 'boolean
  :group 'emagent-shell)

(defcustom emagent-shell-guard-push t
  "When non-nil, refuse git push when gh reports the branch PR is merged."
  :type 'boolean
  :group 'emagent-shell)

(defcustom emagent-shell-redirect t
  "When non-nil with `emagent-acp-prefer-emacs', redirect simple shell commands."
  :type 'boolean
  :group 'emagent-shell)

(defcustom emagent-shell-suggest t
  "When non-nil with `emagent-acp-prefer-emacs', refuse substitutable shell."
  :type 'boolean
  :group 'emagent-shell)

(defun emagent-shell--prefer-emacs-p ()
  "Return non-nil when Emacs-native routing is active."
  (and (boundp 'emagent-acp-prefer-emacs)
       emagent-acp-prefer-emacs
       emagent-shell-redirect))

(defun emagent-shell--compound-command-p (command)
  "Return non-nil when COMMAND has pipes or other shell operators.

Quoted spans are ignored so `echo \"a|b\"' stays simple.  Compound
commands must run as real shell — prefer-Emacs redirects would mangle
`grep x file | head' into a bogus path."
  (let ((bare (emagent-shell--strip-quoted (or command ""))))
    (or (string-match-p "[|;&<>`]" bare)
        (string-match-p "\\$(" bare))))

(defun emagent-shell--suggest-p ()
  "Return non-nil when shell suggestions are active."
  (and (boundp 'emagent-acp-prefer-emacs)
       emagent-acp-prefer-emacs
       emagent-shell-suggest))

(defun emagent-shell--strip-quoted (command)
  "Remove single- and double-quoted spans from COMMAND.

Delegates to `emagent-policy-match--strip-quoted' so shell routing and
policy share the same quoting rules (apostrophes inside doubles, etc.)."
  (emagent-policy-match--strip-quoted command))

(defun emagent-shell--git-no-verify-p (command)
  "Return non-nil when COMMAND is git with a --no-verify argv.

Unquoted flags are matched after stripping quoted spans so a commit
message like `-m \"--no-verify inside\"' is not a false positive.
A flag that is itself a quoted argv (`'--no-verify' or \"--no-verify\")
is still detected on the raw command."
  (and (stringp command)
       (string-match-p "\\<git\\>" command)
       (or (string-match-p
            "\\(?:\\`\\|[[:space:]]\\)--no-verify\\(?:\\'\\|[[:space:]]\\)"
            (emagent-shell--strip-quoted command))
           (string-match-p
            "\\(?:\\`\\|[[:space:]]\\)['\"]--no-verify['\"]\\(?:\\'\\|[[:space:]]\\)"
            command))))

(defun emagent-shell--git-push-p (command)
  "Return non-nil when COMMAND is a git push."
  (string-match-p "\\`git[[:space:]]+push\\>" (string-trim command)))

(defconst emagent-shell--build-executables
  '("mvn" "./mvnw" "gradle" "./gradlew" "make" "cmake" "ninja"
    "cargo" "go" "pytest" "python" "python3"
    "npm" "yarn" "pnpm" "bun")
  "Executable names that produce compiler-style output.
These are always redirected to `emagent-tool-compile' for navigable errors.")

(defun emagent-shell--build-command-p (words)
  "Return non-nil when WORDS names a build/test/compile executable."
  (member (car words) emagent-shell--build-executables))

(defvar emagent-tools--timeout-override)

(defvar emagent-tools--shell-output-limit)

(defun emagent-shell--call-with-timeout (timeout thunk)
  "Call THUNK with TIMEOUT bound as `emagent-tools--timeout-override'."
  (if timeout
      (let ((emagent-tools--timeout-override timeout))
        (funcall thunk))
    (funcall thunk)))

(defun emagent-shell--captured-timeout ()
  "Return the per-call timeout from the current dynamic binding, or nil."
  (and emagent-tools--timeout-override
       (emagent-tools--clamp-timeout emagent-tools--timeout-override)))

(defun emagent-shell--run-in-directory (directory fn)
  "Run FN with `default-directory' set to DIRECTORY."
  (let ((default-directory (emagent-tools--root-directory directory)))
    (funcall fn)))

(defun emagent-shell--unquote (text)
  "Strip one layer of shell quotes from TEXT."
  (if (and (stringp text) (>= (length text) 2))
      (pcase (aref text 0)
        (?\" (if (= (aref text (1- (length text))) ?\")
                 (substring text 1 -1)
               text))
        (?\' (if (= (aref text (1- (length text))) ?\')
                 (substring text 1 -1)
               text))
        (_ text))
    text))

(defun emagent-shell--words (command)
  "Split COMMAND into words, respecting simple quotes."
  (if (fboundp 'split-string-shell-argument)
      (split-string-shell-argument command)
    (split-string command "[[:space:]]+" t)))

(defun emagent-shell--command-to-string (command)
  "Like `shell-command-to-string' for COMMAND, yielding to the event loop."
  (let ((buf (generate-new-buffer " *emagent-shell*"))
        done)
    (unwind-protect
        (progn
          (let ((proc (start-process-shell-command "emagent-shell" buf command)))
            (set-process-sentinel proc (lambda (_p _e) (setq done t))))
          (while (not done)
            (accept-process-output nil 0.05))
          (with-current-buffer buf
            (buffer-string)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun emagent-shell--read-only-network-p (command)
  "Return non-nil when COMMAND is a read-only HTTP GET via curl or wget."
  (let ((cmd (emagent-shell--strip-quoted (string-trim command))))
    (cond
     ((string-match-p "\\`curl\\>" cmd)
      (and (string-match-p "https?://" cmd)
           (not (string-match-p
                 "\\(?:-X[[:space:]]*\\(?:POST\\|PUT\\|DELETE\\|PATCH\\)\\|-d\\|--data\\|-F\\|--form\\|-o[[:space:]]\\|-O\\|>[[:space:]]\\)"
                 cmd))))
     ((string-match-p "\\`wget\\>" cmd)
      (and (string-match-p "https?://" cmd)
           (not (string-match-p "\\(?:--post\\|-O\\|--output-document\\|>[[:space:]]\\)" cmd))))
     (t nil))))

(defun emagent-shell--current-branch ()
  "Return the current git branch name, or nil."
  (string-trim (apply #'emagent-tools--run-git "branch" "--show-current")))

(defun emagent-shell--branch-pr-merged-p (branch)
  "Return non-nil when gh reports BRANCH has a merged PR."
  (when (and branch (not (string-empty-p branch)) (executable-find "gh"))
    (let ((state (string-trim
                  (emagent-shell--command-to-string
                   (format "gh pr view --head %s --json state -q .state 2>/dev/null"
                           (shell-quote-argument branch))))))
      (string= state "MERGED"))))

(defun emagent-shell--guard-git-push (directory)
  "Signal an error when pushing a branch whose PR is already merged.

Arguments: DIRECTORY."
  (when emagent-shell-guard-push
    (emagent-shell--run-in-directory
     directory
     (lambda ()
       (let ((branch (emagent-shell--current-branch)))
         (when (and branch (not (string-empty-p branch)))
           (if (executable-find "gh")
               (when (emagent-shell--branch-pr-merged-p branch)
                 (user-error
                  "Branch '%s' has an already-merged PR; checkout main, pull, and create a new branch instead"
                  branch))
             (require 'emagent-log)
             (emagent-log "emagent: gh CLI not found; skipping merged-PR check for push"))))))))

(defun emagent-shell--guard-git-push-async (directory callback &optional timeout)
  "Call CALLBACK with nil on success or an error string when push is blocked.

Arguments: DIRECTORY, TIMEOUT."
  (if (not emagent-shell-guard-push)
      (funcall callback nil)
    (emagent-shell--call-with-timeout timeout
     (lambda ()
       (emagent-tools--run-git-async
        (lambda (branch-out is-error)
          (if is-error
              (funcall callback branch-out)
            (let ((branch (string-trim branch-out)))
              (cond
               ((or (null branch) (string-empty-p branch))
                (funcall callback nil))
               ((not (executable-find "gh"))
                (require 'emagent-log)
                (emagent-log "emagent: gh CLI not found; skipping merged-PR check for push")
                (funcall callback nil))
               (t
                (emagent-shell--call-with-timeout timeout
                 (lambda ()
                   (emagent-tools--run-shell-async
                    (lambda (state-out is-error-gh)
                      (if is-error-gh
                          (funcall callback nil)
                        (if (string= (string-trim state-out) "MERGED")
                            (funcall callback
                                     (format "Branch '%s' has an already-merged PR. Checkout main, pull, and create a new branch instead."
                                             branch))
                          (funcall callback nil))))
                    (format "gh pr view --head %s --json state -q .state 2>/dev/null"
                            (shell-quote-argument branch))
                    directory))))))))
        "branch" "--show-current")))))

(defun emagent-shell--redirect-git (words directory)
  "Internal helper for WORDS and DIRECTORY."
  (emagent-shell--run-in-directory
   directory
   (lambda ()
     (pcase words
       (`("git" "status" . ,_)
        (emagent-tool-git-status))
       (`("git" "diff" . ,rest)
        (emagent-tool-git-diff (and rest (string-join rest " "))))
       (`("git" "log" . ,rest)
        (emagent-tool-git-log (and rest (string-join rest " "))))
       (_ nil)))))

(defun emagent-shell--redirect-cat (words directory)
  "Internal helper for WORDS and DIRECTORY."
  (emagent-shell--run-in-directory
   directory
   (lambda ()
     (pcase words
       (`("cat" ,path)
        (emagent-tool-read-file (emagent-shell--unquote path)))
       (_ nil)))))

(defun emagent-shell--redirect-head (words directory)
  "Internal helper for WORDS and DIRECTORY."
  (emagent-shell--run-in-directory
   directory
   (lambda ()
     (pcase words
       (`("head" "-n" ,n ,path)
        (emagent-tool-read-file (emagent-shell--unquote path)
                                1 (string-to-number n)))
       (`("head" ,path)
        (emagent-tool-read-file (emagent-shell--unquote path) 1 10))
       (_ nil)))))

(defun emagent-shell--redirect-grep (words directory)
  "Internal helper for WORDS and DIRECTORY."
  (emagent-shell--run-in-directory
   directory
   (lambda ()
     (let ((pattern nil)
           (path nil)
           (skip-next nil))
       (dolist (word (cdr words))
         (cond
          (skip-next
           (setq skip-next nil))
          ((member word '("-r" "-R" "-n" "-H" "-h" "--color=auto" "--color=never"))
           nil)
          ((string-prefix-p "-" word)
           (setq skip-next t))
          ((null pattern)
           (setq pattern (emagent-shell--unquote word)))
          (t
           (setq path (emagent-shell--unquote word)))))
       (when pattern
         (emagent-tool-grep pattern path))))))

(defun emagent-shell--redirect-rg (words directory)
  "Internal helper for WORDS and DIRECTORY."
  (emagent-shell--run-in-directory
   directory
   (lambda ()
     (let ((pattern nil)
           (path nil))
       (dolist (word (cdr words))
         (cond
          ((and (string-prefix-p "-" word) (not (string-match-p "^-[0-9]+$" word)))
           nil)
          ((null pattern)
           (setq pattern (emagent-shell--unquote word)))
          (t
           (setq path (emagent-shell--unquote word)))))
       (when pattern
         (emagent-tool-grep pattern path))))))

(defun emagent-shell--redirect-find (command directory)
  "Internal helper for COMMAND and DIRECTORY."
  (when (string-match
         "\\`find\\(?:[[:space:]]+\\([^[:space:]]+\\)\\)?[[:space:]]+-name[[:space:]]+\\([^[:space:]]+\\)"
         command)
    (let ((root (match-string 1 command))
          (glob (emagent-shell--unquote (match-string 2 command))))
      (emagent-tool-find-files glob (or root directory)))))

(defun emagent-shell--try-redirect (command directory)
  "Run COMMAND via an emagent tool when it matches a simple pattern.

Arguments: DIRECTORY."
  (when (and (emagent-shell--prefer-emacs-p)
             (not (emagent-shell--compound-command-p command)))
    (let* ((trimmed (string-trim command))
           (words (emagent-shell--words trimmed))
           (tool (pcase (car words)
                   ("git" (emagent-shell--redirect-git words directory))
                   ("cat" (emagent-shell--redirect-cat words directory))
                   ("head" (emagent-shell--redirect-head words directory))
                   ("grep" (emagent-shell--redirect-grep words directory))
                   ((or "rg" "ag") (emagent-shell--redirect-rg words directory))
                   (_ nil))))
      (or tool
          (emagent-shell--redirect-find trimmed directory)))))

(defun emagent-shell--suggest-alternative (command)
  "Return a user-facing hint when COMMAND should use an emagent tool."
  (when (emagent-shell--suggest-p)
    (let ((cmd (string-trim command)))
      (cond
       ((emagent-shell--compound-command-p cmd) nil)
       ((string-match-p "\\`git[[:space:]]+status\\>" cmd) nil)
       ((string-match-p "\\`git[[:space:]]+diff\\>" cmd) nil)
       ((string-match-p "\\`git[[:space:]]+log\\>" cmd) nil)
       ((string-match-p "\\<git\\>" cmd)
        "Use emagent git op=status|diff|log instead of shell git.")
       ((string-match-p "\\`\\(?:grep\\|rg\\|ag\\)\\>" cmd)
        "Use emagent search instead of shell grep/rg/ag.")
       ((string-match-p "\\`find\\>" cmd)
        "Use emagent fs op=find or fs op=list instead of shell find.")
       ((string-match-p "\\`\\(?:cat\\|head\\|tail\\)\\>" cmd)
        "Use emagent fs op=read (optional line and limit) instead of cat/head/tail.")
       ((string-match-p "\\`jq\\>" cmd)
        "Use emagent eval with json-parse-string / json-read instead of jq.")
       ((string-match-p "\\`open[[:space:]]" cmd)
        "Use emagent eval with browse-url instead of open.")
       (t nil)))))

(defun emagent-shell--redirect-git-async (words callback &optional timeout)
  "Internal helper for WORDS and CALLBACK and TIMEOUT."
  (emagent-shell--call-with-timeout timeout
   (lambda ()
     (pcase words
       (`("git" "status" . ,_)
        (emagent-tool-git-status-async callback))
       (`("git" "diff" . ,rest)
        (emagent-tool-git-diff-async callback (and rest (string-join rest " "))))
       (`("git" "log" . ,rest)
        (emagent-tool-git-log-async callback (and rest (string-join rest " "))))
       (_ (funcall callback nil nil))))))

(defun emagent-shell--redirect-cat-async (words callback)
  "Internal helper for WORDS and CALLBACK."
  (pcase words
    (`("cat" ,path)
     (funcall callback (emagent-tool-read-file (emagent-shell--unquote path)) nil))
    (_ (funcall callback nil nil))))

(defun emagent-shell--redirect-head-async (words callback)
  "Internal helper for WORDS and CALLBACK."
  (pcase words
    (`("head" "-n" ,n ,path)
     (funcall callback
              (emagent-tool-read-file (emagent-shell--unquote path)
                                      1 (string-to-number n))
              nil))
    (`("head" ,path)
     (funcall callback
              (emagent-tool-read-file (emagent-shell--unquote path) 1 10)
              nil))
    (_ (funcall callback nil nil))))

(defun emagent-shell--redirect-grep-async (words _directory callback &optional timeout)
  "Internal helper for WORDS and CALLBACK and TIMEOUT."
  (let ((pattern nil)
        (path nil)
        (skip-next nil))
    (dolist (word (cdr words))
      (cond
       (skip-next (setq skip-next nil))
       ((member word '("-r" "-R" "-n" "-H" "-h" "--color=auto" "--color=never"))
        nil)
       ((string-prefix-p "-" word)
        (setq skip-next t))
       ((null pattern)
        (setq pattern (emagent-shell--unquote word)))
       (t
        (setq path (emagent-shell--unquote word)))))
    (if pattern
        (emagent-shell--call-with-timeout timeout
         (lambda ()
           (emagent-tool-grep-async callback pattern path)))
      (funcall callback nil nil))))

(defun emagent-shell--redirect-rg-async (words _directory callback &optional timeout)
  "Internal helper for WORDS and CALLBACK and TIMEOUT."
  (let ((pattern nil)
        (path nil))
    (dolist (word (cdr words))
      (cond
       ((and (string-prefix-p "-" word) (not (string-match-p "^-[0-9]+$" word)))
        nil)
       ((null pattern)
        (setq pattern (emagent-shell--unquote word)))
       (t
        (setq path (emagent-shell--unquote word)))))
    (if pattern
        (emagent-shell--call-with-timeout timeout
         (lambda ()
           (emagent-tool-grep-async callback pattern path)))
      (funcall callback nil nil))))

(defun emagent-shell--try-redirect-async (command directory callback &optional timeout)
  "Run COMMAND via an emagent tool when it matches; call CALLBACK with result.

Arguments: DIRECTORY, TIMEOUT."
  (if (or (not (emagent-shell--prefer-emacs-p))
          (emagent-shell--compound-command-p command))
      (funcall callback nil nil)
    (let* ((trimmed (string-trim command))
           (words (emagent-shell--words trimmed))
           (first (car words)))
      (pcase first
        ("git"
         (emagent-shell--redirect-git-async words callback timeout))
        ("cat"
         (emagent-shell--redirect-cat-async words callback))
        ("head"
         (emagent-shell--redirect-head-async words callback))
        ("grep"
         (emagent-shell--redirect-grep-async words directory callback timeout))
        ((or "rg" "ag")
         (emagent-shell--redirect-rg-async words directory callback timeout))
        (_
         (let ((found (emagent-shell--redirect-find trimmed directory)))
           (if found
               (funcall callback found nil)
             (funcall callback nil nil))))))))

(defun emagent-shell--run-command-body-async (cmd words directory callback
                                                  &optional timeout)
  "Run guarded shell CMD asynchronously; deliver via CALLBACK.

Arguments: WORDS, DIRECTORY, TIMEOUT."
  (if (emagent-shell--build-command-p words)
      (emagent-shell--call-with-timeout timeout
       (lambda ()
         (emagent-tool-compile-async callback cmd directory)))
    (emagent-shell--try-redirect-async cmd directory
     (lambda (redirected is-error)
       (if redirected
           (funcall callback redirected is-error)
         (let ((suggestion (emagent-shell--suggest-alternative cmd)))
           (if suggestion
               (funcall callback suggestion t)
             (emagent-shell--call-with-timeout timeout
              (lambda ()
                (emagent-tools--run-shell-async callback cmd directory)))))))
     timeout)))

(defun emagent-shell-run-command-async (command directory callback)
  "Like `emagent-shell-run-command' for COMMAND via CALLBACK.
CALLBACK is called as \(CALLBACK OUTPUT IS-ERROR).  Synchronous guards
\(policy, --no-verify) run immediately; push guard, redirects, compile,
and shell fallback are non-blocking.

Arguments: DIRECTORY."
  (let* ((cmd (string-trim command))
         (words (emagent-shell--words cmd))
         (timeout (emagent-shell--captured-timeout))
         (guard-error
          (condition-case err
              (progn
                (emagent-policy-enforce (emagent-policy-check-shell cmd) cmd)
                (when (and emagent-shell-block-no-verify
                           (emagent-shell--git-no-verify-p cmd))
                  (user-error
                   "--no-verify bypasses pre-commit hooks; fix the pre-commit issue instead"))
                nil)
            (error (error-message-string err)))))
    (if guard-error
        (funcall callback guard-error t)
      (if (emagent-shell--git-push-p cmd)
          (emagent-shell--guard-git-push-async
           directory
           (lambda (push-err)
             (if push-err
                 (funcall callback push-err t)
               (emagent-shell--run-command-body-async
                cmd words directory callback timeout)))
           timeout)
        (emagent-shell--run-command-body-async
         cmd words directory callback timeout)))))

(defun emagent-shell-run-command (command &optional directory)
  "Run COMMAND with Emacs-native routing, guards, and redirects.

Arguments: DIRECTORY."
  (let* ((cmd (string-trim command))
         (words (emagent-shell--words cmd)))
    (emagent-policy-enforce (emagent-policy-check-shell cmd) cmd)
    (when (and emagent-shell-block-no-verify
               (emagent-shell--git-no-verify-p cmd))
      (user-error
       "--no-verify bypasses pre-commit hooks; fix the pre-commit issue instead"))
    (when (emagent-shell--git-push-p cmd)
      (emagent-shell--guard-git-push directory))
    ;; Build/test commands always go through compilation-mode for navigable errors,
    ;; regardless of the prefer-emacs setting.
    (if (emagent-shell--build-command-p words)
        (emagent-tool-compile cmd directory)
      (or (emagent-shell--try-redirect cmd directory)
          (let ((suggestion (emagent-shell--suggest-alternative cmd)))
            (when suggestion
              (user-error "%s" suggestion))
            (let* ((default-directory (emagent-tools--root-directory directory))
                   (output (emagent-shell--command-to-string cmd)))
              (if (> (length output) emagent-tools--shell-output-limit)
                  (concat (substring output 0 emagent-tools--shell-output-limit)
                          "\n… (output truncated)")
                output)))))))

(defvar auto-insert)

(defvar emagent-tools-show-written-buffer)


(defun emagent-tools--protected-fs-path-p (path)
  "Return non-nil when PATH must not be accessed via Emacs on macOS."
  (emagent-tools--protected-truename-p (file-truename (expand-file-name path))))

(defun emagent-tools--file-buffer (path)
  "Return a buffer visiting PATH, visiting it if the file exists."
  (let ((resolved (emagent-tools--root-directory path)))
    (or (find-buffer-visiting resolved)
        (when (file-exists-p resolved)
          (find-file-noselect resolved)))))

(defun emagent-tools--extract-buffer-text (buffer &optional line limit)
  "Return text from BUFFER starting at LINE for LIMIT lines."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (and line (> line 1))
          (forward-line (1- line)))
        (let ((start (point)))
          (if limit
              (forward-line limit)
            (goto-char (point-max)))
          (buffer-substring-no-properties start (point)))))))

(defun emagent-tools--read-file-content (path &optional line limit)
  "Read PATH through Emacs, including unsaved buffer contents.

Reconciles an unmodified visiting buffer that drifted from disk before
reading, so external edits are visible to tools and tick CAS.

Arguments: LINE, LIMIT."
  (emagent-tools--reconcile-visited-file path)
  (let* ((resolved (emagent-tools--root-directory path))
         (buffer (find-buffer-visiting resolved)))
    (if buffer
        (emagent-tools--extract-buffer-text buffer line limit)
      (with-temp-buffer
        (insert-file-contents resolved)
        (emagent-tools--extract-buffer-text (current-buffer) line limit)))))

(defun emagent-tools--read-elisp-file-content (path)
  "Like `emagent-tools--read-file-content' but return \"\" when PATH is missing."
  (condition-case-unless-debug nil
      (emagent-tools--read-file-content path)
    (file-missing "")))

(defun emagent-tools--file-line-count (path)
  "Return the number of lines in PATH (visiting buffer or file)."
  (let* ((resolved (emagent-tools--root-directory path))
         (buffer (find-buffer-visiting resolved)))
    (if buffer
        (with-current-buffer buffer
          (count-lines (point-min) (point-max)))
      (with-temp-buffer
        (insert-file-contents resolved)
        (count-lines (point-min) (point-max))))))

(defun emagent-tools--outline-for-path (path)
  "Return an outline for PATH (imenu, or structural tree for Lisp)."
  (require 'emagent-tools-age)
  (let ((resolved (emagent-tools--root-directory path)))
    (emagent-tools-age-mark-outlined resolved)
    (cond
     ((and (emagent-struct-available-p)
           (emagent-struct--lisp-file-p resolved))
      (concat "[outline: structural tree]\n"
              (emagent-tool-structural-tree resolved)
              "\n\n[Use structural op=get or fs read with line=/limit= for body.]"))
     (t
      (concat "[outline: imenu]\n"
              (emagent-tool-imenu-index resolved)
              "\n\n[Use fs read with line=/limit= or search for body.]")))))

(defun emagent-tool-read-file (path &optional line limit refresh)
  "Return contents of PATH as a string.

When `emagent-tools--acp-session-p' is set, prefix the text with an
emagent-tick header for optimistic concurrency on later writes.

Large unbounded reads return an outline first (imenu, or structural
tree for Lisp).  Files larger than
`emagent-tools-compact-read-hard-max-lines' require line+limit.
Identical large repeats may be aged (see `emagent-tools-age').
Same-tick re-reads across turns return an unchanged stub.

Arguments: LINE, LIMIT, REFRESH."
  (require 'emagent-tools-age)
  (when (emagent-tools--protected-fs-path-p path)
    (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                (emagent-tools--root-directory path)))
  (let* ((resolved (emagent-tools--root-directory path))
         (limit-provided (and limit t))
         (line-n (cond
                  ((null line) nil)
                  ((numberp line) line)
                  ((stringp line) (string-to-number line))
                  (t nil)))
         (limit-n (cond
                   ((null limit) nil)
                   ((numberp limit) limit)
                   ((stringp limit) (string-to-number limit))
                   (t nil)))
         (lines (and (not limit-provided) (not line-n)
                     (emagent-tools--file-line-count path)))
         (args (format "line=%s limit=%s" line-n limit-n))
         (text
          (cond
           ((and (not limit-provided)
                 (not line-n)
                 lines
                 (> lines emagent-tools-compact-read-hard-max-lines))
            (format
             "Refusing unbounded read of %s (%d lines). Pass line= and limit= (max %d without bounds)."
             resolved lines emagent-tools-compact-read-hard-max-lines))
           ((and (not limit-provided)
                 (not line-n)
                 lines
                 (> lines emagent-tools-compact-read-max-lines)
                 (not (emagent-tools-age-outlined-p resolved)))
            (emagent-tools--outline-for-path path))
           (t
            (emagent-tools-compact-read
             (emagent-tools--read-file-content path line-n limit-n)
             limit-provided))))
         (tick (and emagent-tools--acp-session-p
                    (emagent-tools--file-tick path)))
         (text (if tick
                   (emagent-tools-age-tick-note resolved args tick text refresh)
                 text))
         (text (emagent-tools-age-note "fs-read" resolved args text refresh))
         (text (if emagent-tools--acp-session-p
                   (emagent-tools--format-with-file-tick path text tick)
                 text)))
    text))

(defun emagent-tools--read-structural-file-content (path)
  "Like `emagent-tools--read-file-content' but return \"\" when PATH is missing."
  (condition-case-unless-debug nil
      (emagent-tools--read-file-content path)
    (file-missing "")))

(defvar emagent-tools--expected-file-tick nil
  "Expected `emagent-tools--file-tick' for the current MCP mutating call.
Bound by the MCP dispatcher from the tool's expected_tick argument.")

(defvar emagent-tools--skip-file-tick-guard nil
  "When non-nil, `emagent-tools--write-file-content' skips tick CAS.
Used only for rare internal writes that must not participate in CAS.")

(defun emagent-tools--file-tick (path)
  "Return a revision token string for PATH's current contents.

Uses a SHA-1 of buffer-or-disk file text so a no-op buffer sync keeps the
same tick, while any concurrent edit invalidates it.  Directories use
`emagent-tools--directory-tick' (nested size/mtime plus visiting buffers).
Missing paths use \"0\"."
  (let ((resolved (emagent-tools--root-directory path)))
    (cond
     ((not (file-exists-p resolved)) "0")
     ((file-directory-p resolved)
      (emagent-tools--directory-tick resolved))
     (t
      (condition-case nil
          (secure-hash 'sha1 (emagent-tools--read-file-content resolved))
        (file-missing "0"))))))

(defun emagent-tools--directory-tick (resolved)
  "Return a revision token for directory RESOLVED.

Fingerprints nested paths by relative name, kind, size, and mtime.
Visiting buffers contribute a content hash so unsaved nested edits
change the tick even when disk mtime is unchanged."
  (let ((parts nil))
    (dolist (file (directory-files-recursively resolved "" t))
      (let ((rel (file-relative-name file resolved)))
        (unless (string-match-p "\\`\\.git\\(/\\|\\'\\)" rel)
          (let* ((attrs (file-attributes file))
                 (dirp (eq (car attrs) t))
                 (size (or (file-attribute-size attrs) 0))
                 (mtime (file-attribute-modification-time attrs)))
            (push (format "%s\0%s\0%s\0%s"
                          rel
                          (if dirp "d" "f")
                          size
                          mtime)
                  parts)
            (unless dirp
              (when-let ((buf (find-buffer-visiting file)))
                (when (buffer-live-p buf)
                  (push (format "%s\0buf\0%s"
                                rel
                                (secure-hash
                                 'sha1
                                 (with-current-buffer buf
                                   (save-restriction
                                     (widen)
                                     (buffer-substring-no-properties
                                      (point-min) (point-max))))))
                        parts))))))))
    (format "dir:%s"
            (secure-hash 'sha1
                         (mapconcat #'identity
                                    (sort parts #'string<)
                                    "\n")))))

(defun emagent-tools--normalize-file-tick (tick)
  "Return TICK as a comparable string, or nil when TICK is absent."
  (cond
   ((null tick) nil)
   ((stringp tick) (if (string-empty-p tick) nil tick))
   ((numberp tick) (format "%s" tick))
   (t (format "%s" tick))))

(defun emagent-tools--capture-session-context ()
  "Return a plist of the active emagent tool session bindings.

Captured so async process sentinels can restore root confinement and
ACP tick/age state after the MCP `let' that started the tool has ended."
  (list :project emagent-tools--project-directory
        :root emagent-tools--root-boundary
        :acp emagent-tools--acp-session-p
        :tick emagent-tools--expected-file-tick
        :chat emagent-tools--chat-buffer
        :age-key (or (and (boundp 'emagent-mcp--current-session-token)
                          emagent-mcp--current-session-token)
                     (and emagent-tools--chat-buffer
                          (buffer-live-p emagent-tools--chat-buffer)
                          (format "buf:%s"
                                  (buffer-name emagent-tools--chat-buffer)))
                     'global)
        :prefer-emacs (and (boundp 'emagent-acp-prefer-emacs)
                           emagent-acp-prefer-emacs)
        :allowed (and (boundp 'emagent-tools--session-allowed-tools)
                      emagent-tools--session-allowed-tools)
        :allow-all (and (boundp 'emagent-tools-allow-all-function)
                        emagent-tools-allow-all-function)))

(defun emagent-tools--with-session-context (ctx thunk)
  "Call THUNK with tool session bindings restored from CTX.

CTX is a plist from `emagent-tools--capture-session-context'."
  (let ((emagent-tools--project-directory (plist-get ctx :project))
        (emagent-tools--root-boundary (plist-get ctx :root))
        (emagent-tools--acp-session-p (plist-get ctx :acp))
        (emagent-tools--expected-file-tick (plist-get ctx :tick))
        (emagent-tools--chat-buffer (plist-get ctx :chat))
        (emagent-tools-age--session-key (plist-get ctx :age-key))
        (emagent-usage--session-key (plist-get ctx :age-key))
        (emagent-acp-prefer-emacs (plist-get ctx :prefer-emacs))
        (emagent-tools--session-allowed-tools (plist-get ctx :allowed))
        (emagent-tools-allow-all-function (plist-get ctx :allow-all)))
    (funcall thunk)))

(defun emagent-tools--wrap-session-callback (ctx callback)
  "Return CALLBACK wrapped to restore CTX before each invocation.

CALLBACK is called as (CALLBACK RESULT &optional IS-ERROR)."
  (lambda (result &optional is-error)
    (emagent-tools--with-session-context
     ctx
     (lambda ()
       (funcall callback result is-error)))))

(defun emagent-tools--reconcile-visited-file (path &optional require-clean)
  "Align PATH's visiting buffer with disk before read/tick/mutate.

Unmodified buffers whose file changed on disk are reverted so content
and emagent-tick match disk.  When REQUIRE-CLEAN is non-nil, a modified
buffer with a disk change (or a deleted file) signals `stale_revision'
instead of clobbering.  If the file was deleted on disk: keep a modified
buffer so reads still see unsaved edits; with REQUIRE-CLEAN signal
`stale_revision'; drop only a clean visiting buffer (avoid File no longer
exists from `revert-buffer')."
  (let* ((resolved (emagent-tools--root-directory path))
         (buffer (find-buffer-visiting resolved)))
    (when buffer
      (if (not (file-exists-p resolved))
          (cond
           ((with-current-buffer buffer (buffer-modified-p))
            (when require-clean
              (user-error
               (concat "stale_revision: path=%s deleted on disk while "
                       "buffer has unsaved edits; re-read and retry")
               resolved)))
           (t
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        (with-current-buffer buffer
          (unless (verify-visited-file-modtime)
            (cond
             ((and require-clean (buffer-modified-p))
              (user-error
               (concat "stale_revision: path=%s changed on disk while "
                       "buffer has unsaved edits; re-read and retry")
               resolved))
             ((not (buffer-modified-p))
              (revert-buffer t t t)))))))
    resolved))

(defun emagent-tools--guard-file-tick (path expected-tick)
  "Signal when EXPECTED-TICK does not match PATH's current revision.

When `emagent-tools--acp-session-p' is set, EXPECTED-TICK is required.
Otherwise a nil EXPECTED-TICK skips the check (non-MCP callers).
Also reconciles a visiting buffer that drifted from disk."
  (when (and emagent-tools--acp-session-p
             (not emagent-tools--skip-file-tick-guard))
    (emagent-tools--reconcile-visited-file path t)
    (let* ((expected (emagent-tools--normalize-file-tick expected-tick))
           (current (emagent-tools--file-tick path))
           (resolved (emagent-tools--root-directory path)))
      (unless expected
        (user-error
         (concat "expected_tick required for MCP mutate of %s; "
                 "call fs op=read (or structural op=get) and pass its "
                 "emagent-tick on write/delete/structural mutate "
                 "(current_tick=%s)")
         resolved current))
      (unless (string= expected current)
        (user-error
         (concat "stale_revision: path=%s expected_tick=%s current_tick=%s; "
                 "re-read (fs op=read) and retry")
         resolved expected current)))))

(defun emagent-tools--format-with-file-tick (path text &optional tick)
  "Return TEXT prefixed with PATH's emagent-tick header for MCP clients.

TICK, when non-nil, is used instead of recomputing the revision."
  (format "emagent-tick: %s\n---\n%s"
          (or tick (emagent-tools--file-tick path))
          (or text "")))

(defun emagent-tools--append-file-tick (path text)
  "Return TEXT with PATH's current emagent-tick appended."
  (format "%s\nemagent-tick: %s"
          (or text "")
          (emagent-tools--file-tick path)))

(defun emagent-tools--write-file-content (path content &optional expected-tick)
  "Write CONTENT to PATH through an Emacs buffer.
Each call is recorded as a single undoable change in the target buffer.
EXPECTED-TICK, when non-nil, overrides `emagent-tools--expected-file-tick'."
  (emagent-tools--guard-file-tick
   path (or expected-tick emagent-tools--expected-file-tick))
  (let* ((resolved (emagent-tools--root-directory path))
         (dir (file-name-directory resolved)))
    (when (file-directory-p resolved)
      (user-error "Cannot write file content to directory %s" resolved))
    (when (and emagent-elisp-validate-on-write
               (emagent-elisp-elisp-file-p resolved))
      (when-let ((err (emagent-elisp--validate-content-strict content resolved)))
        (user-error "Validation failed for %s: %s" resolved err)))
    ;; Create parents after validation so a rejected write leaves no orphans.
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t))
    (let ((buffer (or (find-buffer-visiting resolved)
                      (let ((auto-insert nil))
                        (find-file-noselect resolved)))))
      (with-temp-buffer
        (insert content)
        (let ((content-buffer (current-buffer))
              (inhibit-read-only t))
          (with-current-buffer buffer
            (save-restriction
              (widen)
              (undo-boundary)
              (replace-buffer-contents content-buffer 1.0)
              (undo-boundary))
            (basic-save-buffer))))
      ;; Showing the result is best-effort: the file is already saved, so a
      ;; display failure must not surface as a write_file tool error.
      (condition-case-unless-debug err
          (pcase emagent-tools-show-written-buffer
            ('magit-diff
             (with-current-buffer buffer
               ;; `magit-diff-buffer-file' is autoloaded, so `fboundp' alone
               ;; doesn't prove magit is loaded; `magit-toplevel' has no
               ;; autoload cookie and would be void.
               (if (and (fboundp 'magit-diff-buffer-file)
                        (fboundp 'magit-toplevel)
                        (magit-toplevel))
                   (magit-diff-buffer-file)
                 (display-buffer buffer))))
            ((pred identity)
             (display-buffer buffer)))
        (error
         (emagent-log "write_file: showing %s failed: %s"
                      resolved (error-message-string err))))
      resolved)))

(cl-defun emagent-tools--unified-diff-async (callback old new label)
  "Return unified diff between OLD and NEW for LABEL via CALLBACK."
  (if (string= old new)
      (funcall callback "" nil)
    (let ((old-file (make-temp-file "emagent-old"))
          (new-file (make-temp-file "emagent-new")))
      (write-region old nil old-file nil 'silent)
      (write-region new nil new-file nil 'silent)
      (unless (executable-find "diff")
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))
        (funcall callback "" nil)
        (cl-return-from emagent-tools--unified-diff-async))
      (emagent-tools--run-process-async
       (lambda (output is-error)
         (ignore-errors (delete-file old-file))
         (ignore-errors (delete-file new-file))
         (funcall callback output is-error))
       "diff" "-u"
       "--label" (concat "a/" label)
       "--label" (concat "b/" label)
       old-file new-file))))

(defun emagent-tools--unified-diff (old new label)
  "Return a unified diff string between OLD and NEW content for LABEL."
  (if (string= old new)
      ""
    (let ((old-file (make-temp-file "emagent-old"))
          (new-file (make-temp-file "emagent-new")))
      (unwind-protect
          (progn
            (write-region old nil old-file nil 'silent)
            (write-region new nil new-file nil 'silent)
            (with-temp-buffer
              (call-process "diff" nil t nil "-u"
                            "--label" (concat "a/" label)
                            "--label" (concat "b/" label)
                            old-file new-file)
              (buffer-string)))
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))))))

(cl-defun emagent-tool-write-file-async (callback path content)
  "Write CONTENT to PATH; call CALLBACK with (result is-error)."
  (condition-case err
      (when (emagent-struct-write-required-p path)
        (user-error
         "Refusing fs write on %s: lisp-sitter is installed — use structural"
         (emagent-tools--root-directory path)))
    (error (funcall callback (error-message-string err) t)
           (cl-return-from emagent-tool-write-file-async)))
  (let* ((ctx (emagent-tools--capture-session-context))
         (resolved (emagent-tools--root-directory path))
         (label (file-name-nondirectory resolved))
         (acp emagent-tools--acp-session-p))
    (condition-case err
        (progn
          (when (file-directory-p resolved)
            (user-error "Cannot write file content to directory %s" resolved))
          (when (emagent-tools--protected-fs-path-p path)
            (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                        resolved))
          (let ((old (emagent-tools--read-elisp-file-content path)))
            (emagent-tools--write-file-content path content)
            (emagent-tools--unified-diff-async
             (emagent-tools--wrap-session-callback
              ctx
              (lambda (diff is-error)
                (if is-error
                    (funcall callback diff t)
                  (let ((result (if (string-empty-p diff)
                                    (format "Wrote %s (no changes)" resolved)
                                  diff)))
                    (funcall callback
                             (if acp
                                 (emagent-tools--append-file-tick path result)
                               result)
                             nil)))))
             old content label)))
      (error (funcall callback (error-message-string err) t)))))

(defun emagent-tool-write-file (path content)
  "Write CONTENT to PATH through Emacs after user confirmation."
  (when (emagent-struct-write-required-p path)
    (user-error
     "Refusing fs write on %s: lisp-sitter is installed — use structural"
     (emagent-tools--root-directory path)))
  (let* ((resolved (emagent-tools--root-directory path))
         (label (file-name-nondirectory resolved)))
    (when (file-directory-p resolved)
      (user-error "Cannot write file content to directory %s" resolved))
    (when (emagent-tools--protected-fs-path-p path)
      (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                  resolved))
    (let ((old (emagent-tools--read-elisp-file-content path)))
      (emagent-tools--write-file-content path content)
      (let* ((diff (emagent-tools--unified-diff old content label))
             (result (if (string-empty-p diff)
                         (format "Wrote %s (no changes)" resolved)
                       diff)))
        (if emagent-tools--acp-session-p
            (emagent-tools--append-file-tick path result)
          result)))))

(defun emagent-tools--apply-string-edit (content old-string new-string &optional replace-all)
  "Return CONTENT with OLD-STRING replaced by NEW-STRING.

When REPLACE-ALL is non-nil, replace every occurrence.  Otherwise OLD-STRING
must appear exactly once.  Signal `user-error' when the match count is wrong."
  (unless (and (stringp old-string) (not (string-empty-p old-string)))
    (user-error "Old_string is required and must be non-empty"))
  (unless (stringp new-string)
    (user-error "New_string is required"))
  (let ((count 0)
        (start 0)
        (len (length old-string)))
    (while (and (< start (length content))
                (setq start (string-search old-string content start)))
      (setq count (1+ count)
            start (+ start (max 1 len))))
    (cond
     ((= count 0)
      (user-error "Old_string not found in file"))
     ((and (not replace-all) (> count 1))
      (user-error
       "Old_string matched %d times; pass replace_all=true or make it unique"
       count))
     (t
      (if replace-all
          (string-replace old-string new-string content)
        (let ((at (string-search old-string content)))
          (concat (substring content 0 at)
                  new-string
                  (substring content (+ at len)))))))))

(cl-defun emagent-tool-edit-file-async (callback path old-string new-string
                                                 &optional replace-all)
  "Replace OLD-STRING with NEW-STRING in PATH; call CALLBACK with result.

Same guards as `emagent-tool-write-file-async' (Lisp structural refuse,
protected paths, tick CAS via write).  REPLACE-ALL replaces every match."
  (condition-case err
      (let* ((old (emagent-tools--read-elisp-file-content path))
             (new (emagent-tools--apply-string-edit
                   old old-string new-string replace-all)))
        (emagent-tool-write-file-async callback path new))
    (error (funcall callback (error-message-string err) t))))

(defun emagent-tool-edit-file (path old-string new-string &optional replace-all)
  "Replace OLD-STRING with NEW-STRING in PATH (sync).

Arguments: REPLACE-ALL."
  (let* ((old (emagent-tools--read-elisp-file-content path))
         (new (emagent-tools--apply-string-edit
               old old-string new-string replace-all)))
    (emagent-tool-write-file path new)))

(defun emagent-tools--structural-sync-path (file)
  "Sync FILE buffer to disk when the visiting buffer is modified.

Reconcile disk drift first.  Flushing a modified buffer skips tick CAS:
the visiting buffer is the editor source of truth, and MCP writes
already update that buffer in place."
  (emagent-tools--reconcile-visited-file file t)
  (let* ((resolved (emagent-tools--root-directory file))
         (buffer (find-buffer-visiting resolved)))
    (when (and buffer (buffer-modified-p buffer))
      (let ((emagent-tools--skip-file-tick-guard t)
            (content (emagent-tools--read-structural-file-content file)))
        (emagent-tools--write-file-content file content)))
    resolved))

(defun emagent-tools--structural-apply-file-result (file result)
  "Write RESULT to FILE when it is updated content, not a status line."
  (if (or (string-prefix-p "Wrote " result) (string-empty-p result))
      result
    (progn
      (emagent-tools--write-file-content file result)
      (let ((msg (format "Wrote %s" (emagent-tools--root-directory file))))
        (if emagent-tools--acp-session-p
            (emagent-tools--append-file-tick file msg)
          msg)))))

(defun emagent-tool-check-structural-file (file)
  "Validate FILE with lisp-sitter (when available)."
  (if (emagent-struct-available-p)
      (emagent-struct-check (emagent-tools--read-structural-file-content file) file)
    (emagent-elisp-check-file-content
     (emagent-tools--read-structural-file-content file) file)))

(defun emagent-tool-check-structural-node (file node)
  "Validate NODE text with lisp-sitter for FILE's language."
  (if (emagent-struct-available-p)
      (emagent-struct-check-node node (emagent-struct--lang-for file))
    (if (string-match-p "\\.el\\'" file)
        (emagent-elisp-check-form node)
      (format "No checker for %s (install lisp-sitter)" file))))

(defun emagent-tool-structural-tree (file &optional depth)
  "Return a structural outline of FILE using lisp-sitter.

Arguments: DEPTH."
  (require 'emagent-tools-age)
  (if (emagent-struct-available-p)
      (progn
        (emagent-tools-age-mark-outlined
         (emagent-tools--root-directory file))
        (emagent-struct-tree
         (emagent-tools--read-structural-file-content file) file depth))
    (let ((err (emagent-tools--read-structural-file-content file)))
      (if (string-empty-p err)
          ""
        (format "install lisp-sitter to see structural outline of %s" file)))))

(defun emagent-tool-structural-get (file symbol)
  "Return full text of top-level SYMBOL in FILE.
When `emagent-tools--acp-session-p' is set, append emagent-tick for writes."
  (let ((text (emagent-struct-get
               (emagent-tools--read-structural-file-content file)
               file symbol)))
    (if emagent-tools--acp-session-p
        (emagent-tools--append-file-tick file text)
      text)))

(defun emagent-tool-structural-find-errors (file)
  "Return tree-sitter MISSING/ERROR nodes for FILE."
  (emagent-struct-find-errors (emagent-tools--structural-sync-path file)))

(defun emagent-tool-structural-context (file)
  "Return outline and full text of each top-level form in FILE."
  (emagent-struct-context (emagent-tools--structural-sync-path file)))

(defun emagent-tool-structural-complete (lang body)
  "Complete missing closing parens in BODY for LANG."
  (emagent-struct-complete lang body))

(defun emagent-tool-structural-format (file &optional write)
  "Re-indent FILE with lisp-sitter.

When WRITE is non-nil, save via the normal write path so ACP tick CAS
and the visiting buffer stay in sync (never `fmt --write' on disk).

Arguments: WRITE."
  (let ((path (emagent-tools--structural-sync-path file)))
    (if write
        (emagent-tools--structural-apply-file-result
         file (emagent-struct-format-file path nil))
      (emagent-struct-format-file path nil))))

(defun emagent-tool-structural-rename (file old new &optional refs no-refs)
  "Rename top-level form OLD to NEW in FILE.

Arguments: REFS, NO-REFS."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-rename-file (emagent-tools--structural-sync-path file)
                               old new refs no-refs)))

(defun emagent-tool-structural-wrap (file symbol wrap &optional bindings condition)
  "Wrap SYMBOL's body in WRAP in FILE.

Arguments: BINDINGS, CONDITION."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-wrap-file (emagent-tools--structural-sync-path file)
                             symbol wrap bindings condition)))

(defun emagent-tool-structural-remove (file symbol &optional keep-calls)
  "Remove top-level SYMBOL from FILE.

Arguments: KEEP-CALLS."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-remove-file (emagent-tools--structural-sync-path file)
                               symbol keep-calls)))

(defun emagent-tool-structural-move (file symbol after)
  "Move top-level SYMBOL after AFTER in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-move-file (emagent-tools--structural-sync-path file)
                             symbol after)))

(defun emagent-tool-structural-substitute (file symbol pattern replacement)
  "Replace PATTERN with REPLACEMENT inside SYMBOL in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-substitute-file (emagent-tools--structural-sync-path file)
                                   symbol pattern replacement)))

(defun emagent-tool-structural-extract (file symbol pattern name &optional params)
  "Extract PATTERN into new function NAME inside SYMBOL in FILE.

Arguments: PARAMS."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-extract-file (emagent-tools--structural-sync-path file)
                                symbol pattern name params)))

(defun emagent-tool-structural-callers (file symbol)
  "Return callers of SYMBOL in FILE."
  (emagent-struct-callers-file (emagent-tools--structural-sync-path file) symbol))

(defun emagent-tool-structural-instrument (file symbol &optional with at wrap)
  "Instrument SYMBOL in FILE with tracing.

Arguments: AT, WRAP."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-instrument-file (emagent-tools--structural-sync-path file)
                                   symbol with at wrap)))

(defun emagent-tool-structural-flatten (file symbol)
  "Inline SYMBOL at call sites in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-flatten-file (emagent-tools--structural-sync-path file) symbol)))

(defun emagent-tool-structural-convert-let (file symbol to)
  "Convert let/let* for SYMBOL to TO in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-convert-let-file (emagent-tools--structural-sync-path file)
                                    symbol to)))

(defun emagent-tool-structural-splice (file symbol pattern)
  "Splice PATTERN inside SYMBOL in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-splice-file (emagent-tools--structural-sync-path file)
                                symbol pattern)))

(defun emagent-tool-structural-raise (file symbol pattern)
  "Raise PATTERN inside SYMBOL in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-raise-file (emagent-tools--structural-sync-path file)
                              symbol pattern)))

(defun emagent-tool-structural-bounds (file symbol)
  "Return START:END byte positions for SYMBOL in FILE."
  (emagent-struct-bounds (emagent-tools--read-structural-file-content file)
                         file symbol))

(defun emagent-tool-structural-replace (file symbol new-body)
  "Replace top-level node SYMBOL in FILE with complete NEW-BODY text."
  (when-let ((err (emagent-tools--eval-form-guard new-body)))
    (user-error "%s" err))
  (let* ((content (emagent-tools--read-structural-file-content file))
         (updated (emagent-struct-replace content file symbol new-body))
         (result (progn
                   (emagent-tools--write-file-content file updated)
                   (emagent-tools--structural-eval-after-edit new-body)
                   (format "Wrote %s" (expand-file-name file)))))
    (if emagent-tools--acp-session-p
        (emagent-tools--append-file-tick file result)
      result)))

(defun emagent-tool-structural-insert (file after-symbol node)
  "Insert complete top-level NODE after AFTER-SYMBOL in FILE."
  (when-let ((err (emagent-tools--eval-form-guard node)))
    (user-error "%s" err))
  (let* ((content (emagent-tools--read-structural-file-content file))
         (updated (emagent-struct-insert content file after-symbol node))
         (result (progn
                   (emagent-tools--write-file-content file updated)
                   (emagent-tools--structural-eval-after-edit node)
                   (format "Wrote %s" (expand-file-name file)))))
    (if emagent-tools--acp-session-p
        (emagent-tools--append-file-tick file result)
      result)))

(defun emagent-tools--structural-apply-async (callback file args)
  "Run lisp-sitter ARGS on synced FILE; write result and call CALLBACK."
  (let ((ctx (emagent-tools--capture-session-context)))
    (apply #'emagent-struct--call-path-async
           (emagent-tools--wrap-session-callback
            ctx
            (lambda (result is-error)
              (if is-error
                  (funcall callback result t)
                (condition-case err
                    (funcall callback
                             (emagent-tools--structural-apply-file-result
                              file result)
                             nil)
                  (error (funcall callback (error-message-string err) t))))))
           args)))

(defun emagent-tool-check-structural-file-async (callback file)
  "Validate FILE with lisp-sitter asynchronously.

Arguments: CALLBACK."
  (if (emagent-struct-available-p)
      (let ((content (emagent-tools--read-structural-file-content file)))
        (apply #'emagent-struct--call-async
               (lambda (out is-error)
                 (if is-error
                     (funcall callback out t)
                   (funcall callback
                            (if (string-match "^[^:]+: \\(.*\\)$" out)
                                (match-string 1 out)
                              out)
                            nil)))
               content "check" "-" "--lang" (emagent-struct--lang-for file)))
    (funcall callback
             (emagent-elisp-check-file-content
              (emagent-tools--read-structural-file-content file) file)
             nil)))

(defun emagent-tool-check-structural-node-async (callback file node)
  "Validate NODE text with lisp-sitter for FILE's language asynchronously.

Arguments: CALLBACK."
  (if (emagent-struct-available-p)
      (apply #'emagent-struct--call-async callback node "check-node"
             "--lang" (emagent-struct--lang-for file) "--body-file" "-")
    (funcall callback
             (if (string-match-p "\\.el\\'" file)
                 (emagent-elisp-check-form node)
               (format "No checker for %s (install lisp-sitter)" file))
             nil)))

(defun emagent-tool-structural-tree-async (callback file &optional depth)
  "Return a structural outline of FILE asynchronously.

Arguments: CALLBACK, DEPTH."
  (if (emagent-struct-available-p)
      (let* ((content (emagent-tools--read-structural-file-content file))
             (args (list "tree" "-" "--json" "--lang"
                         (emagent-struct--lang-for file))))
        (when (and depth (> depth 1))
          (setq args (append args (list "--depth" (number-to-string depth)))))
        (apply #'emagent-struct--call-async callback content args))
    (let ((content (emagent-tools--read-structural-file-content file)))
      (funcall callback
               (if (string-empty-p content)
                   ""
                 (format "install lisp-sitter to see structural outline of %s" file))
               nil))))

(defun emagent-tool-structural-get-async (callback file symbol)
  "Return full text of top-level SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (let ((content (emagent-tools--read-structural-file-content file))
        (ctx (emagent-tools--capture-session-context))
        (acp emagent-tools--acp-session-p))
    (apply #'emagent-struct--call-async
           (emagent-tools--wrap-session-callback
            ctx
            (lambda (result is-error)
              (if (or is-error (not acp))
                  (funcall callback result is-error)
                (funcall callback
                         (emagent-tools--append-file-tick file result)
                         nil))))
           content "get" "-" symbol
           "--lang" (emagent-struct--lang-for file))))

(defun emagent-tool-structural-find-errors-async (callback file)
  "Return tree-sitter MISSING/ERROR nodes for FILE asynchronously.

Arguments: CALLBACK."
  (emagent-struct--call-path-async
   callback "find-errors" (emagent-tools--structural-sync-path file)))

(defun emagent-tool-structural-context-async (callback file)
  "Return outline and full text of each top-level form in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-struct--call-path-async
   callback "context" (emagent-tools--structural-sync-path file)))

(defun emagent-tool-structural-complete-async (callback lang body)
  "Complete missing closing parens in BODY for LANG asynchronously.

Arguments: CALLBACK."
  (apply #'emagent-struct--call-async callback body "complete"
         "--lang" lang "--body-file" "-"))

(defun emagent-tool-structural-format-async (callback file &optional write)
  "Re-indent FILE with lisp-sitter asynchronously.

When WRITE is non-nil, format to stdout then write through
`emagent-tools--structural-apply-async' so emagent-tick and the
visiting buffer update like other structural mutates.

Arguments: CALLBACK, WRITE."
  (let ((path (emagent-tools--structural-sync-path file)))
    (if write
        (emagent-tools--structural-apply-async
         callback file (list "fmt" path))
      (emagent-struct--call-path-async callback "fmt" path))))

(defun emagent-tool-structural-rename-async (callback file old new &optional refs no-refs)
  "Rename top-level form OLD to NEW in FILE asynchronously.

Arguments: CALLBACK, REFS, NO-REFS."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "rename" path old new)))
    (when refs (setq args (append args '("--refs"))))
    (when no-refs (setq args (append args '("--no-refs"))))
    (emagent-tools--structural-apply-async callback file args)))

(defun emagent-tool-structural-wrap-async (callback file symbol wrap
                                                   &optional bindings condition)
  "Wrap SYMBOL's body in WRAP in FILE asynchronously.

Arguments: CALLBACK, BINDINGS, CONDITION."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "wrap" path symbol "--in" wrap)))
    (when bindings (setq args (append args (list "--bindings" bindings))))
    (when condition (setq args (append args (list "--condition" condition))))
    (emagent-tools--structural-apply-async callback file args)))

(defun emagent-tool-structural-remove-async (callback file symbol &optional keep-calls)
  "Remove top-level SYMBOL from FILE asynchronously.

Arguments: CALLBACK, KEEP-CALLS."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "remove" path symbol)))
    (when keep-calls (setq args (append args '("--keep-calls"))))
    (emagent-tools--structural-apply-async callback file args)))

(defun emagent-tool-structural-move-async (callback file symbol after)
  "Move top-level SYMBOL after AFTER in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "move" (emagent-tools--structural-sync-path file) symbol after)))

(defun emagent-tool-structural-substitute-async (callback file symbol pattern replacement)
  "Replace PATTERN with REPLACEMENT inside SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "substitute" (emagent-tools--structural-sync-path file) symbol
         "--pattern" pattern "--replacement" replacement)))

(defun emagent-tool-structural-extract-async (callback file symbol pattern name
                                                      &optional params)
  "Extract PATTERN into new function NAME inside SYMBOL in FILE asynchronously.

Arguments: CALLBACK, PARAMS."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "extract" path symbol "--pattern" pattern "--name" name)))
    (when (and params (not (string-empty-p params)))
      (setq args (append args (list "--params" params))))
    (emagent-tools--structural-apply-async callback file args)))

(defun emagent-tool-structural-callers-async (callback file symbol)
  "Return callers of SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-struct--call-path-async
   callback "callers" (emagent-tools--structural-sync-path file) symbol))

(defun emagent-tool-structural-instrument-async (callback file symbol
                                                         &optional with at wrap)
  "Instrument SYMBOL in FILE with tracing asynchronously.

Arguments: CALLBACK, AT, WRAP."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "instrument" path symbol)))
    (when with (setq args (append args (list "--with" with))))
    (when at (setq args (append args (list "--at" at))))
    (when wrap (setq args (append args (list "--wrap" wrap))))
    (emagent-tools--structural-apply-async callback file args)))

(defun emagent-tool-structural-flatten-async (callback file symbol)
  "Inline SYMBOL at call sites in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "flatten" (emagent-tools--structural-sync-path file) symbol)))

(defun emagent-tool-structural-convert-let-async (callback file symbol to)
  "Convert let/let* for SYMBOL to TO in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "convert-let" (emagent-tools--structural-sync-path file) symbol "--to" to)))

(defun emagent-tool-structural-splice-async (callback file symbol pattern)
  "Splice PATTERN inside SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "splice" (emagent-tools--structural-sync-path file) symbol "--pattern" pattern)))

(defun emagent-tool-structural-raise-async (callback file symbol pattern)
  "Raise PATTERN inside SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "raise" (emagent-tools--structural-sync-path file) symbol "--pattern" pattern)))

(defun emagent-tool-structural-bounds-async (callback file symbol)
  "Return START:END byte positions for SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (let ((content (emagent-tools--read-structural-file-content file)))
    (apply #'emagent-struct--call-async callback content "bounds" "-" symbol
           "--lang" (emagent-struct--lang-for file))))

(cl-defun emagent-tool-structural-replace-async (callback file symbol new-body)
  "Replace top-level node SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (condition-case err
      (when-let ((guard (emagent-tools--eval-form-guard new-body)))
        (user-error "%s" guard))
    (error (funcall callback (error-message-string err) t)
           (cl-return-from emagent-tool-structural-replace-async)))
  (let* ((content (emagent-tools--read-structural-file-content file))
         (lang (emagent-struct--lang-for file))
         (ctx (emagent-tools--capture-session-context))
         (acp emagent-tools--acp-session-p))
    (apply #'emagent-struct--call-async
           (emagent-tools--wrap-session-callback
            ctx
            (lambda (updated is-error)
              (if is-error
                  (funcall callback updated t)
                (condition-case err
                    (progn
                      (emagent-tools--write-file-content file updated)
                      (emagent-tools--structural-eval-after-edit new-body)
                      (let ((result (format "Wrote %s"
                                            (expand-file-name file))))
                        (funcall callback
                                 (if acp
                                     (emagent-tools--append-file-tick
                                      file result)
                                   result)
                                 nil)))
                  (error (funcall callback
                                  (error-message-string err) t))))))
           content "replace" "-" symbol "--body" new-body
           "--lang" lang)))

(cl-defun emagent-tool-structural-insert-async (callback file after-symbol node)
  "Insert complete top-level NODE after AFTER-SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (condition-case err
      (when-let ((guard (emagent-tools--eval-form-guard node)))
        (user-error "%s" guard))
    (error (funcall callback (error-message-string err) t)
           (cl-return-from emagent-tool-structural-insert-async)))
  (let* ((content (emagent-tools--read-structural-file-content file))
         (lang (emagent-struct--lang-for file))
         (ctx (emagent-tools--capture-session-context))
         (acp emagent-tools--acp-session-p))
    (apply #'emagent-struct--call-async
           (emagent-tools--wrap-session-callback
            ctx
            (lambda (updated is-error)
              (if is-error
                  (funcall callback updated t)
                (condition-case err
                    (progn
                      (emagent-tools--write-file-content file updated)
                      (emagent-tools--structural-eval-after-edit node)
                      (let ((result (format "Wrote %s"
                                            (expand-file-name file))))
                        (funcall callback
                                 (if acp
                                     (emagent-tools--append-file-tick
                                      file result)
                                   result)
                                 nil)))
                  (error (funcall callback
                                  (error-message-string err) t))))))
           content "insert" "-" after-symbol "--node" node
           "--lang" lang)))

(defcustom emagent-allowed-tools '(emagent-tool-fetch-url)
  "Symbols naming tools that may run without confirmation."
  :type '(repeat symbol)
  :group 'emagent-tools)

(defvar emagent-tools--session-allowed-tools nil
  "Tools allowed without confirmation for the current session only.

Bound by the MCP dispatcher from the chat buffer's persisted allow-list so a
per-document choice (see `emagent-tools-allow-all-function') is honoured on
the next call without touching the global `emagent-allowed-tools'.")

(defvar emagent-tools-allow-all-function nil
  "Function of one tool symbol, called when the user chooses \"allow all\".

Bound by the MCP dispatcher to persist the choice per project directory under
`emagent-permissions-directory'.  Nil means the choice only lasts for the
current call.")

(defvar emagent-tools--chat-buffer nil
  "The emagent chat buffer for the active session.
When non-nil, permission prompts are shown as inline buttons there instead
of in the minibuffer.  Bound per MCP dispatch by `emagent-mcp--run-tool'.")

(defvar emagent-tools--acp-session-p nil
  "When non-nil, skip Emacs-side tool confirmation for this call.
ACP chat sessions use `session/request_permission' instead; a second MCP
prompt would not block the agent and is ignored.")

(defun emagent-tools--remember-allowed-tool (tool-name)
  "Record TOOL-NAME as allowed for this session and persist it when possible."
  (unless (memq tool-name emagent-tools--session-allowed-tools)
    (push tool-name emagent-tools--session-allowed-tools))
  (when (functionp emagent-tools-allow-all-function)
    (funcall emagent-tools-allow-all-function tool-name)))

(defun emagent-tools--allowed-p (tool-name)
  "Return non-nil when TOOL-NAME is allowed without confirmation."
  (or (memq tool-name emagent-allowed-tools)
      (memq tool-name emagent-tools--session-allowed-tools)))

(cl-defun emagent-tools--write-diff-string-async (callback resolved new-content)
  "Compare RESOLVED with NEW-CONTENT; call CALLBACK with (diff is-error)."
  (unless (executable-find "diff")
    (funcall callback nil nil)
    (cl-return-from emagent-tools--write-diff-string-async))
  (let ((old-file (make-temp-file "emagent-old-"))
        (new-file (make-temp-file "emagent-new-")))
    (if (file-exists-p resolved)
        (copy-file resolved old-file t)
      (write-region "" nil old-file nil 'quiet))
    (write-region new-content nil new-file nil 'quiet)
    (emagent-tools--run-process-async
     (lambda (output is-error)
       (ignore-errors (delete-file old-file))
       (ignore-errors (delete-file new-file))
       ;; diff exits 1 when the files differ — that is the success case
       ;; here, not an error.  Distinguish it from real trouble (exit 2)
       ;; by the unified-diff header.
       (if (or (string-empty-p output)
               (and is-error (not (string-prefix-p "---" output))))
           (funcall callback nil nil)
         (funcall callback output nil)))
     "diff" "-u"
     "--label" (concat (file-name-nondirectory resolved) " (current)")
     "--label" (concat (file-name-nondirectory resolved) " (proposed)")
     old-file new-file)))

(defun emagent-tools--diff-strings (name old-content new-content)
  "Return a unified diff between OLD-CONTENT and NEW-CONTENT strings, or nil.
NAME labels the sides as `NAME (current)' / `NAME (proposed)'.  Returns nil
when the contents are identical or the diff binary is unavailable."
  (when (executable-find "diff")
    (let ((old-file (make-temp-file "emagent-old-"))
          (new-file (make-temp-file "emagent-new-")))
      (unwind-protect
          (progn
            (write-region old-content nil old-file nil 'quiet)
            (write-region new-content nil new-file nil 'quiet)
            (with-temp-buffer
              (call-process "diff" nil t nil "-u"
                            "--label" (concat name " (current)")
                            "--label" (concat name " (proposed)")
                            old-file new-file)
              (unless (= (point-min) (point-max))
                (buffer-string))))
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))))))

(defun emagent-tools--write-diff-string (resolved new-content)
  "Return a unified diff string comparing RESOLVED with NEW-CONTENT, or nil."
  (emagent-tools--diff-strings
   (file-name-nondirectory resolved)
   (if (file-exists-p resolved)
       (with-temp-buffer
         (insert-file-contents resolved)
         (buffer-string))
     "")
   new-content))

(defun emagent-tools--confirm-write (tool-name resolved new-content &optional chat-buffer)
  "Show diff of NEW-CONTENT vs RESOLVED in CHAT-BUFFER with inline buttons.
Inserts a #+begin_src diff block (when changes exist) followed by Allow /
Allow all / Deny buttons; the whole block is removed after the decision.
Falls back to a minibuffer prompt when CHAT-BUFFER is unavailable.
Returns non-nil when the write is approved.

When `emagent-tools--acp-session-p' is set, return t — ACP handles permission.

Arguments: TOOL-NAME."
  (if (or emagent-tools--acp-session-p (emagent-tools--allowed-p tool-name))
      t
    (let* ((diff (emagent-tools--write-diff-string resolved new-content))
           (preamble (when diff (format "\n#+begin_src diff\n%s#+end_src" diff)))
           (choice nil))
      (emagent-tools--buttons-prompt
       (format "Write %s?" (file-name-nondirectory resolved))
       '(("Allow" . yes) ("Allow all" . all) ("Deny" . no))
       chat-buffer
       (lambda (v) (setq choice v))
       preamble)
      (pcase choice
        ('all (emagent-tools--remember-allowed-tool tool-name) t)
        ('yes t)
        (_ nil)))))

(defun emagent-tools--with-stdout (thunk)
  "Call THUNK after an introspection command and return `help-buffer' text.

THUNK should populate *Help* (e.g. via `describe-function').  Returns
whatever THUNK returns; call sites typically read `help-buffer'."
  (funcall thunk))

(defvar emagent-acp-elisp-guide nil "Forward declaration for the ACP Elisp guide string.")

(defun emagent-tools--symbols-in-form (form symbols)
  "Return symbols from SYMBOLS found anywhere in FORM."
  (emagent-policy-match--symbols-in-form form symbols))

(defun emagent-tools--eval-form-dangerous-allowed-p (form-str dangerous)
  "Return non-nil when evaluating FORM-STR is approved with DANGEROUS symbols.
When `emagent-tools--acp-session-p' is set, return t — ACP handles permission."
  (or emagent-tools--acp-session-p
      (let* ((ops (mapconcat #'symbol-name dangerous ", "))
             (preview (truncate-string-to-width form-str 400 nil nil "…"))
             (preamble (format "\n#+begin_src elisp\n%s\n#+end_src" preview))
             (prompt (format "Eval contains: *%s*" ops)))
        (if (and emagent-tools--chat-buffer
                 (buffer-live-p emagent-tools--chat-buffer))
            (let (result)
              (emagent-tools--buttons-prompt
               prompt '(("Allow" . yes) ("Deny" . no))
               emagent-tools--chat-buffer
               (lambda (v) (setq result v))
               preamble)
              (eq 'yes result))
          (y-or-n-p (format "Eval contains %s — allow? " ops))))))

(defun emagent-tools--eval-form-check (form-str)
  "Return nil when FORM-STR may run; otherwise a permission plist.
`:deny' blocks execution; `:confirm' needs user approval at the ACP gate."
  (emagent-policy-check-elisp form-str))

(defun emagent-tools--eval-form-execute (form-str)
  "Evaluate FORM-STR after guardrails; return nil on success or an error string."
  (condition-case err
      (progn (eval (emagent-tools--eval-form-read form-str)) nil)
    (error (error-message-string err))))

(defun emagent-tool-describe-symbol (symbol)
  "Return documentation for SYMBOL as a string."
  (let ((symbol (if (stringp symbol) (intern symbol) symbol)))
    (cond
     ((fboundp symbol)
      (emagent-tools--with-stdout
       (lambda ()
         (describe-function symbol)
         (with-current-buffer (help-buffer)
           (buffer-string)))))
     ((boundp symbol)
      (emagent-tools--with-stdout
       (lambda ()
         (describe-variable symbol)
         (with-current-buffer (help-buffer)
           (buffer-string)))))
     (t
      (format "No function or variable named %s" symbol)))))

(defun emagent-tool-where-is (command)
  "Return key bindings for COMMAND as a string."
  (let ((command (if (stringp command) (intern-soft command) command)))
    (if (commandp command)
        (emagent-tools--with-stdout
         (lambda ()
           (where-is command)
           (with-current-buffer (help-buffer)
             (buffer-string))))
      (format "Unknown command: %s" command))))

(defconst emagent-tools--apropos-max-results 100 "Max matches returned by apropos tools.")

(defun emagent-tool-apropos (pattern)
  "Return Emacs symbols matching PATTERN, one per line.
Searches symbol names.  Use to discover functions and variables before
calling them."
  (let* ((regexp (if (stringp pattern) pattern (format "%s" pattern)))
         (matches (apropos-internal regexp)))
    (if matches
        (string-join
         (mapcar #'symbol-name
                 (seq-take (sort matches #'string-lessp)
                           emagent-tools--apropos-max-results))
         "\n")
      "No matches")))

(defun emagent-tool-apropos-doc (pattern)
  "Return Emacs symbols whose docstring matches PATTERN, one per line.
Use when you know what a function does but not its name — e.g. apropos_doc
\"split string by delimiter\" to find `split-string'.
Slower than apropos (scans all docstrings) but finds symbols by meaning."
  (let* ((regexp (if (stringp pattern) pattern (format "%s" pattern)))
         (results nil)
         (limit emagent-tools--apropos-max-results))
    (mapatoms
     (lambda (sym)
       (when (< (length results) limit)
         (ignore-errors
           (let* ((fdoc (and (fboundp sym) (documentation sym t)))
                  (vdoc (and (boundp sym)
                             (documentation-property sym 'variable-documentation t)))
                  (doc (or fdoc vdoc)))
             (when (and doc (string-match-p regexp doc))
               (push (format "%s — %s"
                             sym
                             (truncate-string-to-width
                              (car (split-string doc "\n"))
                              80 nil nil "…"))
                     results)))))))
    (if results
        (string-join (nreverse results) "\n")
      "No matches")))

(defun emagent-tool-find-function (symbol)
  "Return the source location of SYMBOL as a string."
  (let ((symbol (if (stringp symbol) (intern-soft symbol) symbol)))
    (if (and symbol (fboundp symbol))
        (emagent-tools--with-stdout
         (lambda ()
           (find-function symbol)
           (with-current-buffer (help-buffer)
             (buffer-string))))
      (format "No function named %s" symbol))))

(defun emagent-tool-elisp-guide ()
  "Return the emagent Emacs Lisp reference guide.
Covers validation, structural editing, core patterns, string/list/buffer/file/
JSON/`org-mode' operations, error handling, common pitfalls, and code templates.
Call this before writing non-trivial Elisp."
  (require 'emagent-tools)
  emagent-acp-elisp-guide)

(defun emagent-tool-check-elisp (form)
  "Check FORM for Emacs Lisp syntax errors without executing it.
Returns \"OK\" when the form parses cleanly, or an error description.
Always call this before eval for any form longer than 3 lines."
  (emagent-elisp-check-form (if (stringp form) form (prin1-to-string form))))

(defun emagent-tool-eval (form)
  "Evaluate Emacs Lisp FORM and return the result as a string.
Use this for small utilities and text processing — not Python or shell.
Blocked symbols must go through dedicated emagent-tool-* helpers.
For forms longer than 3 lines, call elisp op=check first."
  (interactive)
  (emagent-tools--eval-form-safely
   (if (stringp form) form (prin1-to-string form))))

(defun emagent-tool-org-element ()
  "Return structured org element at point as a string."
  (if (derived-mode-p 'org-mode)
      (let* ((element (org-element-at-point))
             (type (org-element-type element))
             (props (cond
                     ((eq type 'headline)
                      `((type . headline)
                        (title . ,(org-element-property :raw-value element))
                        (level . ,(org-element-property :level element))
                        (tags . ,(org-element-property :tags element))))
                     ((eq type 'paragraph)
                      `((type . paragraph)
                        (contents . ,(org-element-contents element))))
                     (t
                      `((type . ,type)
                        (properties . ,element))))))
        (prin1-to-string props))
    "Not in org-mode"))

(defvaralias 'emagent-tools-eval-blocked-symbols 'emagent-policy-elisp-blocked-symbols)

(defvaralias 'emagent-tools-eval-dangerous-symbols 'emagent-policy-elisp-dangerous-symbols)

(defgroup emagent-tools nil
  "Emacs tool handlers for emagent."
  :group 'emagent)

(defvar emagent-tools--project-directory nil
  "Project directory for the active emagent session.")

(defvar emagent-tools--root-boundary nil
  "When non-nil, the absolute directory emagent file tools must stay within.

Bound per session by the emagent MCP dispatcher so a tool call cannot reach
outside the session's project root.  Nil disables the check (the historical
behaviour for non-MCP call sites).")

(defvar-local emagent-tools--buffer-project-directory nil
  "Per-chat project directory; preferred when the dynamic binding is nil.")

(defun emagent-tools-set-project-directory (directory)
  "Set project DIRECTORY used by emagent-tool-* when PATH is omitted.

Updates the process-wide default and, when called from a chat buffer,
the buffer-local project so concurrent chats do not clobber each other."
  (let ((dir (and directory (expand-file-name directory))))
    (setq emagent-tools--project-directory dir)
    (when (local-variable-p 'emagent-mcp--token (current-buffer))
      (setq-local emagent-tools--buffer-project-directory dir))
    dir))

(defun emagent-tool-project-directory ()
  "Return the emagent session project directory as a string."
  (emagent-tools--root-directory nil))

(defcustom emagent-tools-show-written-buffer nil
  "How to reveal a file after emagent writes it.

nil        — do nothing (default; agent writes never touch the window layout)
t          — display the buffer
magit-diff — run `magit-diff-buffer-file' (falls back to `display-buffer'
             when magit is unavailable or the file is outside a git repo)"
  :type '(choice (const :tag "Don't show" nil)
                 (const :tag "Display buffer" t)
                 (const :tag "Magit diff" magit-diff))
  :group 'emagent-tools)

(provide 'emagent-tools)
;;; emagent-tools.el ends here
