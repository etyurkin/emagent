;;; emagent-prompts-prefer-emacs.el --- Prefer-Emacs prompt builders for emagent -*- lexical-binding: t; -*-

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
;; Prefer-Emacs helpers and prompt builders for ACP sessions.

;;; Code:

(require 'emagent-struct)

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
    (if (emagent-struct-available-p)
      "| Edit .el / .lisp / .cl / .scm | structural_replace / structural_insert (write_file refused) |
| Structural file outline   | structural_tree / structural_bounds                 |
| Validate Lisp file        | check_structural_file                               |"
    "| Edit .el files            | write_file + check_elisp (small, focused edits)   |
| Validate before save      | check_elisp                                         |"))

(defun emagent-prompts--prefer-emacs-elisp-pattern-rows ()
  "Return Elisp-pattern table rows for Lisp file editing."
    (if (emagent-struct-available-p)
      "| Edit .el / .lisp / .cl / .scm | structural_replace / structural_insert              |
| Validate structural file  | check_structural_file                               |"
    "| Edit .el files            | write_file + check_elisp                            |
| Validate Elisp            | check_elisp                                         |"))

(defun emagent-prompts--prefer-emacs-paren-discipline ()
  "Return the Elisp paren discipline section for the system prompt."
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

(provide 'emagent-prompts-prefer-emacs)

;;; emagent-prompts-prefer-emacs.el ends here
