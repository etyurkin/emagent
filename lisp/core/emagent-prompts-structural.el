;;; emagent-prompts-structural.el --- Structural editing prompts for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.7
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
;; Structural tool list, policy, and gateway prompt text.

;;; Code:

(require 'emagent-struct)
(defvar emagent-mcp--structural-tools
  "MCP structural tool alist from `emagent-mcp-structural'.")

(defun emagent-prompts--structural-tool-list ()
  "Return a comma-separated list of lisp-sitter MCP tool names."
  (require 'emagent-mcp-structural)
  (string-join (mapcar #'car emagent-mcp--structural-tools) ", "))

(defun emagent-prompts--structural-policy ()
  "Return structural editing rules for the system prompt."
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

## External MCP servers

Configured MCP servers beyond emagent are available in this session.  Prefer
them for their domain over reinventing the same calls with shell/curl.

Some servers are meta-proxies: they expose search/list/describe/dispatch tools
rather than domain tools directly.  When those discovery tools are present,
use them first to find the right backend tool and its schema, then invoke it
— do not ask the user how the proxy works.

If OAuth authentication is requested, show the authorize URL as a clickable
org link — the agent or `/mcp' login handles the browser/callback.  Do not
ask the user to paste a callback URL."
  "Appended when Claude or Cursor has external MCP servers configured.")

(provide 'emagent-prompts-structural)

;;; emagent-prompts-structural.el ends here
