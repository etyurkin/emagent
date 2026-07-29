;;; emagent-chat.el --- Org scratch buffer UI for emagent -*- lexical-binding: t; -*-

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
;; Chat major mode/UI helpers and client MCP management.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'org)
(require 'bookmark)
(require 'project)
(require 'emagent-acp-protocol)
(require 'emagent-chat-ui)
(require 'emagent-cursor-command)
(require 'emagent-log)
(require 'emagent-session)
(require 'emagent-archive)
(require 'emagent-tools)

(defun emagent-mcp--arg (args key &optional default)
  "Return KEY from ARGS hash-table, or DEFAULT when missing or JSON null."
  (let ((value (and (hash-table-p args) (gethash key args))))
    (if (or (null value) (eq value :null))
        default
      value)))

(defun emagent-mcp--bool (args key)
  "Return non-nil when KEY in ARGS is JSON true."
  (eq (emagent-mcp--arg args key) t))

(defun emagent-mcp--path-prop ()
  "JSON schema property for a session-relative file path."
  '(("path" . ((type . "string")
               (description . "Absolute path, or relative to the session root.")))))

(defun emagent-mcp--expected-tick-prop ()
  "JSON schema property for optimistic-concurrency file ticks."
  '(("expected_tick" . ((type . "string")
                        (description . "emagent-tick from fs op=read or structural op=get.")))))

(defun emagent-mcp--op-prop (ops description)
  "Return JSON schema property for op among OPS with DESCRIPTION."
  `(("op" . ((type . "string")
             (description . ,description)
             (enum . ,(apply #'vector ops))))))

(defconst emagent-mcp--fs-mutating-ops '("write")
  "Fs ops that require expected_tick under ACP.")

(defconst emagent-mcp--structural-ops
  '("check_file" "check_node" "find_errors" "tree" "outline" "bounds" "get"
    "context" "replace" "insert" "complete" "format" "rename" "wrap" "remove"
    "move" "substitute" "extract" "callers" "instrument" "flatten"
    "convert_let" "splice" "raise" "edit")
  "Ops accepted by the structural MCP dispatcher.")

(defconst emagent-mcp--structural-mutating-ops
  '("replace" "insert" "format" "rename" "wrap" "remove" "move"
    "substitute" "extract" "instrument" "flatten" "convert_let"
    "splice" "raise" "edit")
  "Structural ops that require expected_tick under ACP.")

(defun emagent-mcp--maybe-guard-file-tick (name args)
  "Require a matching expected_tick for mutating dispatcher NAME.

ARGS is the tool argument hash-table from the MCP call."
  (let ((op (emagent-mcp--arg args "op")))
    (cond
     ((and (string= name "fs")
           (member op emagent-mcp--fs-mutating-ops))
      (emagent-tools--guard-file-tick (emagent-mcp--arg args "path")
                                      (emagent-mcp--arg args "expected_tick")))
     ((and (string= name "structural")
           (member op emagent-mcp--structural-mutating-ops))
      (emagent-tools--guard-file-tick (emagent-mcp--arg args "path")
                                      (emagent-mcp--arg args "expected_tick"))))))

(defun emagent-mcp--require-op (args ops)
  "Return op from ARGS, or signal when missing / not in OPS."
  (let ((op (emagent-mcp--arg args "op")))
    (unless (and (stringp op) (member op ops))
      (user-error "Op required; one of: %s" (string-join ops ", ")))
    op))

(defun emagent-mcp--tool (name description properties required handler &rest plist)
  "Build an MCP tool registry entry.

Arguments: NAME, DESCRIPTION, PROPERTIES, REQUIRED, HANDLER, PLIST."
  (append (list name description properties required handler) plist))

(defgroup emagent-mcp nil
  "In-Emacs MCP server for emagent."
  :group 'emagent)

(defcustom emagent-mcp-port 8771
  "TCP port for the in-Emacs MCP server on 127.0.0.1.

A fixed port keeps the agent configuration (e.g. ~/.cursor/mcp.json) stable
across Emacs restarts.  Set it to 0 to let the OS assign an ephemeral port
instead; emagent then writes whatever port it gets into the agent config."
  :type 'integer
  :group 'emagent-mcp)

(defcustom emagent-mcp-cursor-config-file
  (expand-file-name "~/.cursor/mcp.json")
  "Path to the global cursor-agent MCP config emagent manages for Cursor."
  :type 'file
  :group 'emagent-mcp)

(defconst emagent-mcp-server-name "emagent"
  "Name advertised for the emagent MCP server.")

(defconst emagent-mcp-protocol-version "2025-06-18"
  "MCP protocol version emagent speaks when a client omits one.")

(defvar emagent-mcp--server nil
  "The singleton MCP server network process, or nil.")

(defvar emagent-mcp--port nil
  "Actual port the MCP server is listening on, or nil.")

(defvar emagent-mcp--sessions (make-hash-table :test 'equal)
  "Map session token to plist (:root :cwd :buffer :prefer-emacs).")

(defvar-local emagent-mcp--token nil
  "Per-buffer MCP session token.")

(defun emagent-mcp-make-token ()
  "Return a fresh opaque session token."
  (let ((seed (format "%s-%s-%s-%s"
                      (random most-positive-fixnum)
                      (emacs-pid)
                      (float-time)
                      (recent-keys))))
    (substring (md5 seed) 0 24)))

(defun emagent-mcp-buffer-token ()
  "Return this buffer's MCP session token, creating one if needed."
  (or emagent-mcp--token
      (setq emagent-mcp--token (emagent-mcp-make-token))))

;; Defined in emagent-acp.el; declared here to avoid a circular require.
(defvar emagent-acp-prefer-emacs)

(defvar emagent-tools--timeout-override)

(defun emagent-mcp--timeout (args)
  "Return the integer `timeout' in ARGS, or nil when absent."
  (let ((value (emagent-mcp--arg args "timeout")))
    (and (integerp value) value)))

(defconst emagent-mcp--timeout-prop
  '(("timeout" . ((type . "integer")
                  (description . "Optional seconds to wait before the subprocess is killed. Defaults to emagent-tools-subprocess-timeout; on timeout, retry with a larger value."))))
  "Shared JSON-schema property for a per-call subprocess timeout.")

(defun emagent-mcp--string-result (value)
  "Coerce a tool VALUE into a string for MCP text content."
  (cond
   ((stringp value) value)
   ((null value) "")
   (t (format "%s" value))))

(defun emagent-mcp--call-sync-as-async (handler args callback)
  "Run sync HANDLER on ARGS and deliver to CALLBACK as (result is-error)."
  (condition-case err
      (funcall callback (emagent-mcp--string-result (funcall handler args)) nil)
    (error (funcall callback (error-message-string err) t))))

(defun emagent-mcp--fs (args)
  "Dispatch fs tool ARGS."
  (pcase (emagent-mcp--require-op
          args '("read" "write" "undo" "delete" "delete_directory" "list" "find"))
    ("read" (emagent-tool-read-file (emagent-mcp--arg args "path")
                                    (emagent-mcp--arg args "line")
                                    (emagent-mcp--arg args "limit")
                                    (emagent-mcp--bool args "refresh")))
    ("write" (emagent-tool-write-file (emagent-mcp--arg args "path")
                                      (emagent-mcp--arg args "content" "")))
    ("undo" (emagent-tool-undo-file (emagent-mcp--arg args "path")
                                    (emagent-mcp--arg args "steps")))
    ("delete" (emagent-tool-delete-file (emagent-mcp--arg args "path")))
    ("delete_directory"
     (emagent-tool-delete-directory (emagent-mcp--arg args "path")
                                    (emagent-mcp--bool args "recursive")))
    ("list" (emagent-tool-list-files (emagent-mcp--arg args "path")))
    ("find" (emagent-tool-find-files (emagent-mcp--arg args "glob")
                                     (emagent-mcp--arg args "path")))))

(defun emagent-mcp--fs-async (args callback)
  "Async dispatch for fs tool ARGS via CALLBACK."
  (pcase (emagent-mcp--arg args "op")
    ("write"
     (emagent-tool-write-file-async
      callback
      (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "content" "")))
    (_ (emagent-mcp--call-sync-as-async #'emagent-mcp--fs args callback))))

(defun emagent-mcp--search (args)
  "Dispatch search tool ARGS."
  (emagent-mcp--require-op args '("grep"))
  (emagent-tool-grep (emagent-mcp--arg args "pattern")
                     (emagent-mcp--arg args "path")))

(defun emagent-mcp--search-async (args callback)
  "Async dispatch for search tool ARGS via CALLBACK."
  (emagent-mcp--require-op args '("grep"))
  (let ((emagent-tools--timeout-override (emagent-mcp--timeout args)))
    (emagent-tool-grep-async
     callback
     (emagent-mcp--arg args "pattern")
     (emagent-mcp--arg args "path"))))

(defun emagent-mcp--git (args)
  "Dispatch git tool ARGS."
  (pcase (emagent-mcp--require-op args '("status" "diff" "log"))
    ("status" (emagent-tool-git-status))
    ("diff" (emagent-tool-git-diff (emagent-mcp--arg args "args")))
    ("log" (emagent-tool-git-log (emagent-mcp--arg args "args")))))

(defun emagent-mcp--git-async (args callback)
  "Async dispatch for git tool ARGS via CALLBACK."
  (let ((emagent-tools--timeout-override (emagent-mcp--timeout args)))
    (pcase (emagent-mcp--require-op args '("status" "diff" "log"))
      ("status" (emagent-tool-git-status-async callback))
      ("diff" (emagent-tool-git-diff-async callback (emagent-mcp--arg args "args")))
      ("log" (emagent-tool-git-log-async callback (emagent-mcp--arg args "args"))))))

(defun emagent-mcp--shell (args)
  "Dispatch shell tool ARGS."
  (pcase (emagent-mcp--require-op args '("run" "compile"))
    ("run" (emagent-tool-run-shell-command (emagent-mcp--arg args "command")
                                           (emagent-mcp--arg args "directory")))
    ("compile" (emagent-tool-compile (emagent-mcp--arg args "command")
                                     (emagent-mcp--arg args "directory")))))

(defun emagent-mcp--shell-async (args callback)
  "Async dispatch for shell tool ARGS via CALLBACK."
  (let ((emagent-tools--timeout-override (emagent-mcp--timeout args)))
    (pcase (emagent-mcp--require-op args '("run" "compile"))
      ("run" (emagent-tool-run-shell-command-async
              (emagent-mcp--arg args "command")
              (emagent-mcp--arg args "directory")
              callback))
      ("compile" (emagent-tool-compile-async
                  callback
                  (emagent-mcp--arg args "command")
                  (emagent-mcp--arg args "directory"))))))

(defun emagent-mcp--emacs (args)
  "Dispatch Emacs session/context tool ARGS."
  (pcase (emagent-mcp--require-op
          args '("buffers" "imenu" "project_directory" "where_is"))
    ("buffers" (emagent-tool-buffer-list))
    ("imenu" (emagent-tool-imenu-index (emagent-mcp--arg args "file")))
    ("project_directory" (emagent-tool-project-directory))
    ("where_is" (emagent-tool-where-is (emagent-mcp--arg args "command")))))

(defun emagent-mcp--elisp (args)
  "Dispatch elisp discovery/check tool ARGS."
  (pcase (emagent-mcp--require-op
          args '("check" "guide" "apropos" "apropos_doc" "describe" "find_function"))
    ("check" (emagent-tool-check-elisp (emagent-mcp--arg args "form")))
    ("guide" (emagent-tool-elisp-guide))
    ("apropos" (emagent-tool-apropos (emagent-mcp--arg args "pattern")))
    ("apropos_doc" (emagent-tool-apropos-doc (emagent-mcp--arg args "pattern")))
    ("describe" (emagent-tool-describe-symbol (emagent-mcp--arg args "symbol")))
    ("find_function" (emagent-tool-find-function (emagent-mcp--arg args "symbol")))))

(defun emagent-mcp--structural-edit (args)
  "Composite structural edit for ARGS: replace or substitute, then check."
  (let* ((path (emagent-mcp--arg args "path"))
         (symbol (emagent-mcp--arg args "symbol"))
         (pattern (emagent-mcp--arg args "pattern"))
         (replacement (emagent-mcp--arg args "replacement"))
         (new-body (emagent-mcp--arg args "new_body"))
         (wrote
          (cond
           ((and pattern replacement)
            (emagent-tool-structural-substitute path symbol pattern replacement))
           (new-body
            (emagent-tool-structural-replace path symbol new-body))
           (t (user-error
               "Structural op=edit needs new_body, or pattern+replacement"))))
         (check (emagent-tool-check-structural-file path)))
    (emagent-tools--append-file-tick
     path (format "%s\ncheck: %s" wrote check))))

(defun emagent-mcp--structural (args)
  "Dispatch structural (lisp-sitter) tool ARGS."
  (pcase (emagent-mcp--require-op args emagent-mcp--structural-ops)
    ("check_file" (emagent-tool-check-structural-file
                   (emagent-mcp--arg args "path")))
    ("check_node" (emagent-tool-check-structural-node
                   (emagent-mcp--arg args "path")
                   (emagent-mcp--arg args "node")))
    ("find_errors" (emagent-tool-structural-find-errors
                    (emagent-mcp--arg args "path")))
    ((or "tree" "outline")
     (emagent-tool-structural-tree (emagent-mcp--arg args "path")
                                   (emagent-mcp--arg args "depth")))
    ("bounds" (emagent-tool-structural-bounds (emagent-mcp--arg args "path")
                                              (emagent-mcp--arg args "symbol")))
    ("get" (emagent-tool-structural-get (emagent-mcp--arg args "path")
                                        (emagent-mcp--arg args "symbol")))
    ("context" (emagent-tool-structural-context (emagent-mcp--arg args "path")))
    ("replace" (emagent-tool-structural-replace
                (emagent-mcp--arg args "path")
                (emagent-mcp--arg args "symbol")
                (emagent-mcp--arg args "new_body")))
    ("insert" (emagent-tool-structural-insert
               (emagent-mcp--arg args "path")
               (emagent-mcp--arg args "after_symbol")
               (emagent-mcp--arg args "node")))
    ("complete" (emagent-tool-structural-complete
                 (emagent-mcp--arg args "lang")
                 (emagent-mcp--arg args "body")))
    ("format" (emagent-tool-structural-format
               (emagent-mcp--arg args "path")
               (emagent-mcp--bool args "write")))
    ("rename" (emagent-tool-structural-rename
               (emagent-mcp--arg args "path")
               (emagent-mcp--arg args "old")
               (emagent-mcp--arg args "new")
               (emagent-mcp--bool args "refs")
               (emagent-mcp--bool args "no_refs")))
    ("wrap" (emagent-tool-structural-wrap
             (emagent-mcp--arg args "path")
             (emagent-mcp--arg args "symbol")
             (emagent-mcp--arg args "in")
             (emagent-mcp--arg args "bindings")
             (emagent-mcp--arg args "condition")))
    ("remove" (emagent-tool-structural-remove
               (emagent-mcp--arg args "path")
               (emagent-mcp--arg args "symbol")
               (emagent-mcp--bool args "keep_calls")))
    ("move" (emagent-tool-structural-move
             (emagent-mcp--arg args "path")
             (emagent-mcp--arg args "symbol")
             (emagent-mcp--arg args "after")))
    ("substitute" (emagent-tool-structural-substitute
                   (emagent-mcp--arg args "path")
                   (emagent-mcp--arg args "symbol")
                   (emagent-mcp--arg args "pattern")
                   (emagent-mcp--arg args "replacement")))
    ("extract" (emagent-tool-structural-extract
                (emagent-mcp--arg args "path")
                (emagent-mcp--arg args "symbol")
                (emagent-mcp--arg args "pattern")
                (emagent-mcp--arg args "name")
                (emagent-mcp--arg args "params")))
    ("callers" (emagent-tool-structural-callers
                (emagent-mcp--arg args "path")
                (emagent-mcp--arg args "symbol")))
    ("instrument" (emagent-tool-structural-instrument
                   (emagent-mcp--arg args "path")
                   (emagent-mcp--arg args "symbol")
                   (emagent-mcp--arg args "with")
                   (emagent-mcp--arg args "at")
                   (emagent-mcp--arg args "wrap")))
    ("flatten" (emagent-tool-structural-flatten
                (emagent-mcp--arg args "path")
                (emagent-mcp--arg args "symbol")))
    ("convert_let" (emagent-tool-structural-convert-let
                    (emagent-mcp--arg args "path")
                    (emagent-mcp--arg args "symbol")
                    (emagent-mcp--arg args "to")))
    ("splice" (emagent-tool-structural-splice
               (emagent-mcp--arg args "path")
               (emagent-mcp--arg args "symbol")
               (emagent-mcp--arg args "pattern")))
    ("raise" (emagent-tool-structural-raise
              (emagent-mcp--arg args "path")
              (emagent-mcp--arg args "symbol")
              (emagent-mcp--arg args "pattern")))
    ("edit" (emagent-mcp--structural-edit args))))

(defun emagent-mcp--structural-async (args callback)
  "Async dispatch for structural tool ARGS via CALLBACK."
  (pcase (emagent-mcp--arg args "op")
    ("check_file"
     (emagent-tool-check-structural-file-async
      callback (emagent-mcp--arg args "path")))
    ("check_node"
     (emagent-tool-check-structural-node-async
      callback (emagent-mcp--arg args "path") (emagent-mcp--arg args "node")))
    ("find_errors"
     (emagent-tool-structural-find-errors-async
      callback (emagent-mcp--arg args "path")))
    ((or "tree" "outline")
     (emagent-tool-structural-tree-async
      callback (emagent-mcp--arg args "path") (emagent-mcp--arg args "depth")))
    ("bounds"
     (emagent-tool-structural-bounds-async
      callback (emagent-mcp--arg args "path") (emagent-mcp--arg args "symbol")))
    ("get"
     (emagent-tool-structural-get-async
      callback (emagent-mcp--arg args "path") (emagent-mcp--arg args "symbol")))
    ("context"
     (emagent-tool-structural-context-async
      callback (emagent-mcp--arg args "path")))
    ("replace"
     (emagent-tool-structural-replace-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "symbol") (emagent-mcp--arg args "new_body")))
    ("insert"
     (emagent-tool-structural-insert-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "after_symbol") (emagent-mcp--arg args "node")))
    ("complete"
     (emagent-tool-structural-complete-async
      callback (emagent-mcp--arg args "lang") (emagent-mcp--arg args "body")))
    ("format"
     (emagent-tool-structural-format-async
      callback (emagent-mcp--arg args "path") (emagent-mcp--bool args "write")))
    ("rename"
     (emagent-tool-structural-rename-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "old") (emagent-mcp--arg args "new")
      (emagent-mcp--bool args "refs") (emagent-mcp--bool args "no_refs")))
    ("wrap"
     (emagent-tool-structural-wrap-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "symbol") (emagent-mcp--arg args "in")
      (emagent-mcp--arg args "bindings") (emagent-mcp--arg args "condition")))
    ("remove"
     (emagent-tool-structural-remove-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "symbol") (emagent-mcp--bool args "keep_calls")))
    ("move"
     (emagent-tool-structural-move-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "symbol") (emagent-mcp--arg args "after")))
    ("substitute"
     (emagent-tool-structural-substitute-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "symbol") (emagent-mcp--arg args "pattern")
      (emagent-mcp--arg args "replacement")))
    ("extract"
     (emagent-tool-structural-extract-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "symbol") (emagent-mcp--arg args "pattern")
      (emagent-mcp--arg args "name") (emagent-mcp--arg args "params")))
    ("callers"
     (emagent-tool-structural-callers-async
      callback (emagent-mcp--arg args "path") (emagent-mcp--arg args "symbol")))
    ("instrument"
     (emagent-tool-structural-instrument-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "symbol") (emagent-mcp--arg args "with")
      (emagent-mcp--arg args "at") (emagent-mcp--arg args "wrap")))
    ("flatten"
     (emagent-tool-structural-flatten-async
      callback (emagent-mcp--arg args "path") (emagent-mcp--arg args "symbol")))
    ("convert_let"
     (emagent-tool-structural-convert-let-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "symbol") (emagent-mcp--arg args "to")))
    ("splice"
     (emagent-tool-structural-splice-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "symbol") (emagent-mcp--arg args "pattern")))
    ("raise"
     (emagent-tool-structural-raise-async
      callback (emagent-mcp--arg args "path")
      (emagent-mcp--arg args "symbol") (emagent-mcp--arg args "pattern")))
    (_ (emagent-mcp--call-sync-as-async #'emagent-mcp--structural args callback))))

(defconst emagent-mcp--tools
  (list
   (emagent-mcp--tool
    "fs"
    "Emacs FS. op=read|write|undo|delete|delete_directory|list|find. write needs expected_tick. Lisp→structural."
    (append
     (emagent-mcp--op-prop
      '("read" "write" "undo" "delete" "delete_directory" "list" "find")
      "Filesystem operation.")
     (emagent-mcp--path-prop)
     '(("content" . ((type . "string") (description . "Full file contents (write).")))
       ("line" . ((type . "integer") (description . "1-based start line (read).")))
       ("limit" . ((type . "integer") (description . "Max lines (read).")))
       ("steps" . ((type . "integer") (description . "Undo steps (default 1).")))
       ("recursive" . ((type . "boolean") (description . "Recursive delete_directory.")))
       ("glob" . ((type . "string") (description . "Filename glob (find)."))))
     (emagent-mcp--expected-tick-prop))
    '("op")
    #'emagent-mcp--fs
    :async #'emagent-mcp--fs-async)
   (emagent-mcp--tool
    "search"
    "Search the session tree. op=grep."
    (append
     (emagent-mcp--op-prop '("grep") "Search operation.")
     '(("pattern" . ((type . "string") (description . "Regexp to search for."))))
     (emagent-mcp--path-prop)
     emagent-mcp--timeout-prop)
    '("op" "pattern")
    #'emagent-mcp--search
    :async #'emagent-mcp--search-async)
   (emagent-mcp--tool
    "git"
    "Git for the session project. op=status|diff|log."
    (append
     (emagent-mcp--op-prop '("status" "diff" "log") "Git operation.")
     '(("args" . ((type . "string")
                  (description . "Optional extra git diff/log arguments."))))
     emagent-mcp--timeout-prop)
    '("op")
    #'emagent-mcp--git
    :async #'emagent-mcp--git-async)
   (emagent-mcp--tool
    "shell"
    "Run a command through Emacs. op=run (shell) or op=compile (compilation-mode, navigable errors). Prefer compile for builds/tests."
    (append
     (emagent-mcp--op-prop '("run" "compile") "Shell operation.")
     '(("command" . ((type . "string") (description . "Shell command line.")))
       ("directory" . ((type . "string")
                       (description . "Working directory; defaults to session root."))))
     emagent-mcp--timeout-prop)
    '("op" "command")
    #'emagent-mcp--shell
    :async #'emagent-mcp--shell-async)
   (emagent-mcp--tool
    "eval"
    "Evaluate an Emacs Lisp form in the live Emacs. Filesystem and process ops are blocked; use fs/shell/structural instead."
    '(("form" . ((type . "string")
                 (description . "An Emacs Lisp form as a string."))))
    '("form")
    (lambda (args)
      (emagent-tool-eval (emagent-mcp--arg args "form"))))
   (emagent-mcp--tool
    "emacs"
    "Emacs session context. op=buffers|imenu|project_directory|where_is."
    (append
     (emagent-mcp--op-prop
      '("buffers" "imenu" "project_directory" "where_is")
      "Emacs context operation.")
     '(("file" . ((type . "string")
                  (description . "File for imenu; omit for current buffer.")))
       ("command" . ((type . "string")
                     (description . "Command name for where_is.")))))
    '("op")
    #'emagent-mcp--emacs)
   (emagent-mcp--tool
    "elisp"
    "Elisp discovery and validation. op=check|guide|apropos|apropos_doc|describe|find_function."
    (append
     (emagent-mcp--op-prop
      '("check" "guide" "apropos" "apropos_doc" "describe" "find_function")
      "Elisp operation.")
     '(("form" . ((type . "string") (description . "Form to check (check).")))
       ("pattern" . ((type . "string") (description . "Regexp (apropos*).")))
       ("symbol" . ((type . "string") (description . "Symbol (describe/find_function).")))))
    '("op")
    #'emagent-mcp--elisp)
   (emagent-mcp--tool
    "structural"
    "[lisp-sitter] Structural Lisp sexp edits. Mutate needs expected_tick from op=get/fs read. Discover with op=tree|get; details via elisp op=guide."
    (append
     (emagent-mcp--op-prop emagent-mcp--structural-ops "Structural operation.")
     (emagent-mcp--path-prop)
     '(("symbol" . ((type . "string") (description . "Top-level form name.")))
       ("node" . ((type . "string") (description . "Complete node text.")))
       ("new_body" . ((type . "string") (description . "Replacement form (replace/edit).")))
       ("after_symbol" . ((type . "string")
                          (description . "__start__, __end__, or symbol (insert).")))
       ("after" . ((type . "string") (description . "Anchor for move.")))
       ("depth" . ((type . "integer") (description . "Outline depth (tree).")))
       ("lang" . ((type . "string") (description . "elisp/commonlisp/scheme (complete).")))
       ("body" . ((type . "string") (description . "Incomplete form (complete).")))
       ("write" . ((type . "boolean") (description . "Save formatted file (format).")))
       ("old" . ((type . "string") (description . "Rename from.")))
       ("new" . ((type . "string") (description . "Rename to.")))
       ("refs" . ((type . "boolean") (description . "Rename quoted refs.")))
       ("no_refs" . ((type . "boolean") (description . "Definition only.")))
       ("in" . ((type . "string") (description . "Wrapper construct (wrap).")))
       ("bindings" . ((type . "string") (description . "let bindings (wrap).")))
       ("condition" . ((type . "string") (description . "if/when condition (wrap).")))
       ("keep_calls" . ((type . "boolean") (description . "Keep call sites (remove).")))
       ("pattern" . ((type . "string") (description . "Sub-expression pattern.")))
       ("replacement" . ((type . "string") (description . "Replacement sexp.")))
       ("name" . ((type . "string") (description . "New function name (extract).")))
       ("params" . ((type . "string") (description . "Extract params.")))
       ("with" . ((type . "string") (description . "Instrument with.")))
       ("at" . ((type . "string") (description . "Instrument at.")))
       ("wrap" . ((type . "string") (description . "Instrument wrap.")))
       ("to" . ((type . "string") (description . "let or let* (convert_let)."))))
     (emagent-mcp--expected-tick-prop))
    '("op")
    #'emagent-mcp--structural
    :available #'emagent-struct-available-p
    :async #'emagent-mcp--structural-async)
   (emagent-mcp--tool
    "fetch_url"
    "Fetch an http(s) URL and return the response body."
    (append
     '(("url" . ((type . "string") (description . "http:// or https:// URL.")))
       ("max_bytes" . ((type . "integer")
                       (description . "Optional max response size in bytes."))))
     emagent-mcp--timeout-prop)
    '("url")
    (lambda (args)
      (emagent-tool-fetch-url (emagent-mcp--arg args "url")
                              (emagent-mcp--arg args "max_bytes")))
    :async
    (lambda (args cb)
      (let ((emagent-tools--timeout-override (emagent-mcp--timeout args)))
        (emagent-tool-fetch-url-async
         cb
         (emagent-mcp--arg args "url")
         (emagent-mcp--arg args "max_bytes"))))))
  "Registry of emagent MCP tools.
Each entry: (NAME DESCRIPTION PROPERTIES REQUIRED HANDLER . PLIST).
PLIST may have :available, a predicate returning non-nil to show the tool.")

(defcustom emagent-mcp-compact-schemas t
  "When non-nil, shorten tools/list descriptions and property docs."
  :type 'boolean
  :group 'emagent-mcp)

(defun emagent-mcp--compact-properties (properties)
  "Return PROPERTIES with short descriptions when compact schemas are on."
  (if (not emagent-mcp-compact-schemas)
      properties
    (mapcar
     (lambda (cell)
       (let* ((name (car cell))
              (spec (cdr cell))
              (type (map-elt spec 'type))
              (desc (or (map-elt spec 'description) ""))
              (short (if (<= (length desc) 40)
                         desc
                       (concat (substring desc 0 37) "..."))))
         (cons name
               (append (when type `((type . ,type)))
                       (when (and short (not (string-empty-p short)))
                         `((description . ,short)))
                       (let ((enum (map-elt spec 'enum)))
                         (when enum `((enum . ,enum))))))))
     properties)))



(defun emagent-mcp--tool-available-p (entry)
  "Return non-nil when ENTRY is available (no :available predicate, or it passes)."
  (let ((plist (nthcdr 5 entry)))
    (if (plist-member plist :available)
        (funcall (plist-get plist :available))
      t)))

(defun emagent-mcp--tool-entry (name)
  "Return the registry entry for tool NAME, or nil."
  (seq-find (lambda (entry) (string= (car entry) name)) emagent-mcp--tools))

(defun emagent-mcp--alist->hash (alist)
  "Return a hash-table built from ALIST (string keys preserved verbatim)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (cell alist table)
      (puthash (car cell) (cdr cell) table))))

(defun emagent-mcp--tool-schema (properties required)
  "Build a JSON-schema object alist from PROPERTIES and REQUIRED.

PROPERTIES keys are tool argument names, kept as strings via a hash-table so
`json-serialize' does not require them to be symbols."
  `((type . "object")
    (properties . ,(emagent-mcp--alist->hash properties))
    (required . ,(apply #'vector required))))

(defun emagent-mcp--tools-list-payload ()
  "Return the tools/list result as an alist.
Only includes tools whose :available predicate passes."
  `((tools . ,(apply #'vector
                     (mapcar
                      (lambda (entry)
                        `((name . ,(nth 0 entry))
                          (description . ,(nth 1 entry))
                          (inputSchema . ,(emagent-mcp--tool-schema
                                           (emagent-mcp--compact-properties
                                            (nth 2 entry))
                                           (nth 3 entry)))))
                      (seq-filter #'emagent-mcp--tool-available-p
                                  emagent-mcp--tools))))))

(defun emagent-mcp--session-allowed-tools (buffer)
  "Return BUFFER's persisted tool allow-list, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (emagent-session-allowed-tools))))

(defun emagent-mcp--make-allow-all-fn (buffer)
  "Return a function that persists an \"allow all\" choice to BUFFER, or nil."
  (when (buffer-live-p buffer)
    (lambda (tool)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (emagent-session-add-allowed-tool tool))))))

(defun emagent-mcp--run-tool (name args session)
  "Run tool NAME with ARGS in SESSION's context; return a result string."
  (let ((entry (emagent-mcp--tool-entry name)))
    (unless entry
      (error "Unknown tool: %s" name))
    (unless (emagent-mcp--tool-available-p entry)
      (error "Tool %s is not available (install lisp-sitter)" name))
    (let* ((root (plist-get session :root))
           (buffer (plist-get session :buffer))
           (handler (nth 4 entry))
           (emagent-tools--project-directory (or root emagent-tools--project-directory))
           (emagent-tools--root-boundary root)
           (emagent-tools--session-allowed-tools
            (emagent-mcp--session-allowed-tools buffer))
           (emagent-tools-allow-all-function
            (emagent-mcp--make-allow-all-fn buffer))
           (emagent-tools--chat-buffer buffer)
           (emagent-tools--acp-session-p t)
           (emagent-tools--expected-file-tick
            (emagent-mcp--arg args "expected_tick"))
           (emagent-acp-prefer-emacs (if session
                                         (plist-get session :prefer-emacs)
                                       (and (boundp 'emagent-acp-prefer-emacs)
                                            emagent-acp-prefer-emacs))))
      (emagent-mcp--maybe-guard-file-tick name args)
      (emagent-mcp--string-result (funcall handler args)))))

(defun emagent-mcp--run-tool-async (name args session callback)
  "Run tool NAME with ARGS in SESSION and deliver the result to CALLBACK.
CALLBACK is called as (CALLBACK RESULT IS-ERROR).  Tools with an :async
handler in the registry are non-blocking — CALLBACK is called from a
process sentinel.  All other tools are synchronous and CALLBACK is called
immediately before this function returns."
  (let ((entry (emagent-mcp--tool-entry name)))
    (cond
     ((null entry)
      (funcall callback (format "Unknown tool: %s" name) t))
     ((not (emagent-mcp--tool-available-p entry))
      (funcall callback (format "Tool %s is not available (install lisp-sitter)" name) t))
     (t
      (let* ((root (plist-get session :root))
             (buffer (plist-get session :buffer))
             (emagent-tools--project-directory (or root emagent-tools--project-directory))
             (emagent-tools--root-boundary root)
             (emagent-tools--session-allowed-tools
              (emagent-mcp--session-allowed-tools buffer))
             (emagent-tools-allow-all-function
              (emagent-mcp--make-allow-all-fn buffer))
             (emagent-tools--chat-buffer buffer)
             (emagent-tools--acp-session-p t)
             (emagent-tools--expected-file-tick
              (emagent-mcp--arg args "expected_tick"))
             (emagent-acp-prefer-emacs (if session
                                           (plist-get session :prefer-emacs)
                                         (and (boundp 'emagent-acp-prefer-emacs)
                                              emagent-acp-prefer-emacs)))
             (async-fn (plist-get (nthcdr 5 entry) :async)))
        (emagent-mcp--maybe-guard-file-tick name args)
        (if async-fn
            (condition-case err
                (funcall async-fn args
                         (lambda (result is-error)
                           (funcall callback (emagent-mcp--string-result result) is-error)))
              (error (funcall callback (error-message-string err) t)))
          (condition-case err
              (funcall callback
                       (emagent-mcp--string-result (funcall (nth 4 entry) args))
                       nil)
            (error (funcall callback (error-message-string err) t)))))))))

(defun emagent-mcp--json-encode (object)
  "Serialize OBJECT to a JSON string."
  (json-serialize object :null-object :null :false-object :false))

(defun emagent-mcp--rpc-result (id result)
  "Return a JSON-RPC success response string for ID with RESULT."
  (emagent-mcp--json-encode `((jsonrpc . "2.0") (id . ,id) (result . ,result))))

(defun emagent-mcp--rpc-error (id code message)
  "Return a JSON-RPC error response string for ID with CODE and MESSAGE.
A nil ID is serialized as JSON null (not an empty object)."
  (emagent-mcp--json-encode
   `((jsonrpc . "2.0") (id . ,(or id :null))
     (error . ((code . ,code) (message . ,message))))))

(defun emagent-mcp--tool-content (text is-error)
  "Return a tools/call result alist wrapping TEXT, flagged IS-ERROR."
  `((content . ,(vector `((type . "text") (text . ,(or text "")))))
    (isError . ,(if is-error t :false))))

(defun emagent-mcp--initialize-result (params)
  "Return the initialize result, echoing PARAMS protocolVersion when present."
  (let ((version (and (hash-table-p params)
                      (gethash "protocolVersion" params))))
    `((protocolVersion . ,(or version emagent-mcp-protocol-version))
      (capabilities . ((tools . ((listChanged . :false)))))
      (serverInfo . ((name . ,emagent-mcp-server-name)
                     (version . "1.0.2"))))))

(defun emagent-mcp--path-token (path)
  "Extract the session token from request PATH like /mcp/TOKEN."
  (when (and path (string-match "/mcp/\\([^/?#]+\\)" path))
    (match-string 1 path)))

(defun emagent-mcp--parse-headers (lines)
  "Parse HTTP header LINES into a lowercased-key alist."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
                    (cons (downcase (match-string 1 line))
                          (match-string 2 line))))
                lines)))

(defun emagent-mcp--reason-phrase (status)
  "Return the HTTP reason phrase for STATUS."
  (pcase status
    (200 "OK")
    (202 "Accepted")
    (204 "No Content")
    (400 "Bad Request")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (_ "OK")))

(defun emagent-mcp--respond (proc status headers body)
  "Send an HTTP response on PROC with STATUS, HEADERS alist, and BODY string."
  (when (process-live-p proc)
    (let* ((body-bytes (encode-coding-string (or body "") 'utf-8))
           (head (concat
                  (format "HTTP/1.1 %d %s\r\n" status (emagent-mcp--reason-phrase status))
                  (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h)))
                             headers "")
                  (format "Content-Length: %d\r\n" (length body-bytes))
                  "Connection: keep-alive\r\n"
                  "\r\n")))
      (process-send-string proc (encode-coding-string head 'utf-8))
      (when (> (length body-bytes) 0)
        (process-send-string proc body-bytes)))))

(defun emagent-mcp--respond-json (proc json-string)
  "Send JSON-STRING as an application/json HTTP 200 response on PROC."
  (emagent-mcp--respond proc 200 '(("Content-Type" . "application/json")) json-string))

(defun emagent-mcp--defer-tools-call (proc id params token)
  "Handle a tools/call for PROC out of the process filter, then respond.

A tool call may prompt the user for confirmation, and Emacs cannot reliably
read keyboard input from a process filter (keystrokes are dropped).  Running
the call from an idle timer moves the prompt into the command loop where input
works, and keeps Emacs responsive while the user decides.

The HTTP response is sent via a RESPOND callback that may be called either
from within the idle timer (synchronous tools) or from a process sentinel
after the subprocess exits (async tools such as shell), so Emacs
stays fully responsive during long-running shell commands.

Arguments: ID, PARAMS, TOKEN."
  (run-with-idle-timer
   0 nil
   (lambda ()
     (let* ((name (and (hash-table-p params) (gethash "name" params)))
            (args (or (and (hash-table-p params) (gethash "arguments" params))
                      (make-hash-table :test 'equal)))
            (session (and token (gethash token emagent-mcp--sessions)))
            (respond (lambda (result is-error)
                       (emagent-mcp--respond-json
                        proc
                        (emagent-mcp--rpc-result
                         id (emagent-mcp--tool-content result is-error))))))
       (cond
        ((null token)
         (funcall respond "No emagent session token in request path" t))
        ((null session)
         (funcall respond "Unknown or expired emagent session" t))
        (t
         (emagent-mcp--run-tool-async name args session respond)))))))

(defun emagent-mcp--dispatch (proc token message)
  "Dispatch a parsed JSON-RPC MESSAGE (hash-table) from PROC with TOKEN."
  (let ((id (gethash "id" message))
        (method (gethash "method" message))
        (params (gethash "params" message)))
    ;; Fail closed: a throwing synchronous handler must still produce a
    ;; response, otherwise the client blocks until its own timeout.  (The
    ;; deferred tools/call path has its own error handling in the idle timer.)
    (condition-case err
        (pcase method
          ("initialize"
           (emagent-mcp--respond-json
            proc (emagent-mcp--rpc-result id (emagent-mcp--initialize-result params))))
          ("notifications/initialized"
           (emagent-mcp--respond proc 202 nil ""))
          ("ping"
           (emagent-mcp--respond-json
            proc (emagent-mcp--rpc-result id (make-hash-table :test 'equal))))
          ("tools/list"
           (emagent-mcp--respond-json
            proc (emagent-mcp--rpc-result id (emagent-mcp--tools-list-payload))))
          ("tools/call"
           (emagent-mcp--defer-tools-call proc id params token))
          ((guard (null id))
           ;; Any other notification: acknowledge without a body.
           (emagent-mcp--respond proc 202 nil ""))
          (_
           (emagent-mcp--respond-json
            proc (emagent-mcp--rpc-error id -32601 (format "Method not found: %s" method)))))
      (error
       (emagent-log "mcp dispatch error (method %s): %s"
                    method (error-message-string err))
       (if id
           (emagent-mcp--respond-json
            proc (emagent-mcp--rpc-error
                  id -32603 (format "Internal error: %s" (error-message-string err))))
         (emagent-mcp--respond proc 202 nil ""))))))

(defun emagent-mcp--handle-request (proc request-line _headers body)
  "Handle one parsed HTTP request on PROC.

Arguments: REQUEST-LINE, BODY."
  (let* ((parts (split-string request-line " "))
         (http-method (nth 0 parts))
         (path (nth 1 parts))
         (token (emagent-mcp--path-token path)))
    (pcase http-method
      ("OPTIONS" (emagent-mcp--respond proc 204 nil ""))
      ("POST"
       (let ((message (condition-case nil
                          (json-parse-string (decode-coding-string body 'utf-8)
                                             :object-type 'hash-table
                                             :array-type 'list
                                             :null-object :null
                                             :false-object :false)
                        (error nil))))
         (cond
          ((null message)
           (emagent-mcp--respond-json proc (emagent-mcp--rpc-error :null -32700 "Parse error")))
          ((hash-table-p message)
           (emagent-mcp--dispatch proc token message))
          (t
           ;; JSON-RPC batch (list of messages); handle each in order.
           (dolist (item message)
             (when (hash-table-p item)
               (emagent-mcp--dispatch proc token item)))))))
      (_ (emagent-mcp--respond proc 405 nil "")))))

(defcustom emagent-mcp-drain-yield 0.01
  "Seconds to wait before draining the next buffered MCP HTTP request.

The process filter only accumulates bytes; parsing and dispatching a
complete HTTP request runs from a `run-with-timer' tick so a burst of
pipelined tool calls cannot starve Emacs redisplay and other timers.
Handling one request per tick mirrors `emagent-acp-message-drain-yield'
for the ACP wire.  The first buffered request is still drained
immediately (delay 0)."
  :type 'number
  :group 'emagent)

(defun emagent-mcp--drain-one (proc)
  "Parse and handle one complete HTTP request buffered on PROC.
Return non-nil when a request was handled."
  (let* ((buffer (or (process-get proc 'emagent-mcp-data) ""))
         (sep (string-search "\r\n\r\n" buffer)))
    (when sep
      (let* ((head (substring buffer 0 sep))
             (rest (substring buffer (+ sep 4)))
             (lines (split-string head "\r\n"))
             (request-line (car lines))
             (headers (emagent-mcp--parse-headers (cdr lines)))
             (content-length (string-to-number
                              (or (cdr (assoc "content-length" headers)) "0"))))
        (when (>= (length rest) content-length)
          (let ((req-body (substring rest 0 content-length)))
            (process-put proc 'emagent-mcp-data (substring rest content-length))
            (emagent-mcp--handle-request proc request-line headers req-body)
            t))))))

(defun emagent-mcp--schedule-drain (proc delay)
  "Schedule a drain tick for PROC after DELAY seconds.
A no-op when PROC already has a drain tick pending."
  (unless (process-get proc 'emagent-mcp-drain-timer)
    (process-put proc 'emagent-mcp-drain-timer
                 (run-with-timer (max 0 delay) nil #'emagent-mcp--drain proc))))

(defun emagent-mcp--cancel-drain (proc)
  "Cancel PROC's pending drain timer, if any."
  (when-let ((timer (process-get proc 'emagent-mcp-drain-timer)))
    (cancel-timer timer)
    (process-put proc 'emagent-mcp-drain-timer nil)))

(defun emagent-mcp--drain (proc)
  "Handle one buffered HTTP request on PROC, then yield before the next.

Runs from a timer instead of the process filter so a burst of pipelined
requests cannot monopolize the command loop; mirrors the ACP wire's
timer-yield drain with a batch size of one HTTP request per tick."
  (process-put proc 'emagent-mcp-drain-timer nil)
  (when (and (process-live-p proc)
             (emagent-mcp--drain-one proc)
             (process-live-p proc)
             (string-search "\r\n\r\n" (or (process-get proc 'emagent-mcp-data) "")))
    (emagent-mcp--schedule-drain proc emagent-mcp-drain-yield)))

(defun emagent-mcp--filter (proc data)
  "Process filter: accumulate DATA on PROC and schedule a drain tick.

Parsing and dispatch happen later from `emagent-mcp--drain', not here, so
the filter itself never blocks on JSON-RPC handling."
  (process-put proc 'emagent-mcp-data
               (concat (or (process-get proc 'emagent-mcp-data) "") data))
  (emagent-mcp--schedule-drain proc 0))

(defun emagent-mcp--sentinel (proc _event)
  "Clean up PROC connection state when it closes."
  (unless (process-live-p proc)
    (emagent-mcp--cancel-drain proc)
    (process-put proc 'emagent-mcp-data nil)))

(defun emagent-mcp-ensure-server ()
  "Start the MCP server if needed and return its port."
  (unless (process-live-p emagent-mcp--server)
    (let ((proc (make-network-process
                 :name "emagent-mcp"
                 :server t
                 :host "127.0.0.1"
                 :service (if (and emagent-mcp-port (> emagent-mcp-port 0))
                              emagent-mcp-port
                            t)
                 :family 'ipv4
                 :coding 'binary
                 :filter #'emagent-mcp--filter
                 :sentinel #'emagent-mcp--sentinel)))
      (setq emagent-mcp--server proc
            emagent-mcp--port (process-contact proc :service))))
  emagent-mcp--port)

(defun emagent-mcp-maybe-shutdown ()
  "Stop the MCP server when no emagent sessions remain registered."
  (when (and emagent-mcp--server
             (zerop (hash-table-count emagent-mcp--sessions)))
    (ignore-errors (delete-process emagent-mcp--server))
    (setq emagent-mcp--server nil
          emagent-mcp--port nil)))

(cl-defun emagent-mcp-register-session (&key token cwd buffer prefer-emacs acp)
  "Register session TOKEN with project CWD, owning BUFFER, and EMACS-ONLY flag.

When ACP is non-nil, the session is driven by an emagent ACP chat; MCP tool
confirmation is handled via ACP `session/request_permission' instead.

Starts the server if needed and returns the port.

Arguments: PREFER-EMACS."
  (emagent-mcp-ensure-server)
  (puthash token
           (list :root (and cwd (expand-file-name cwd))
                 :cwd cwd
                 :buffer buffer
                 :prefer-emacs prefer-emacs
                 :acp acp)
           emagent-mcp--sessions)
  emagent-mcp--port)

(defun emagent-mcp--acp-session-p (session)
  "Return non-nil when SESSION is owned by an emagent ACP chat buffer."
  (and session (plist-get session :acp)))

(defun emagent-mcp-deregister-session (token)
  "Deregister session TOKEN and stop the server if it was the last one."
  (when token
    (remhash token emagent-mcp--sessions))
  (emagent-mcp-maybe-shutdown))

(defun emagent-mcp-session-url (token)
  "Return the MCP endpoint URL for session TOKEN (and begin the server)."
  (format "http://127.0.0.1:%d/mcp/%s" (emagent-mcp-ensure-server) token))

(defun emagent-mcp--lists-to-vectors (object)
  "Recursively convert JSON arrays (lists) to vectors for `json-serialize'.

`json-parse-buffer' with `:array-type \\='list\\=' yields lists, but
`json-serialize' treats lists as alists and requires symbol keys.

Arguments: OBJECT."
  (cond
   ((hash-table-p object)
    (maphash (lambda (key value)
               (puthash key (emagent-mcp--lists-to-vectors value) object))
             object)
    object)
   ((and (listp object) (not (stringp object)))
    (apply #'vector (mapcar #'emagent-mcp--lists-to-vectors object)))
   (t object)))

(defun emagent-mcp--read-json-file (file)
  "Return the parsed JSON object (hash-table) in FILE, or an empty one."
  (if (file-exists-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (json-parse-buffer :object-type 'hash-table
                               :array-type 'list
                               :null-object :null
                               :false-object :false))
        (error (make-hash-table :test 'equal)))
    (make-hash-table :test 'equal)))

(defun emagent-mcp-ensure-cursor-config ()
  "Merge an `emagent' http MCP entry into the global cursor-agent config.

The url uses ${env:EMAGENT_SESSION_TOKEN} so a single static file routes each
cursor-agent invocation to its own session.  Existing servers are preserved.
Only writes the file when the entry is absent or points to a different port."
  (let* ((port (emagent-mcp-ensure-server))
         (file emagent-mcp-cursor-config-file)
         (expected-url (format "http://127.0.0.1:%d/mcp/${env:EMAGENT_SESSION_TOKEN}" port))
         (data (emagent-mcp--read-json-file file))
         (servers (let ((value (gethash "mcpServers" data)))
                    (if (hash-table-p value) value (make-hash-table :test 'equal))))
         (current-entry (gethash emagent-mcp-server-name servers)))
    (unless (and (hash-table-p current-entry)
                 (equal (gethash "url" current-entry) expected-url))
      (let ((entry (make-hash-table :test 'equal)))
        (puthash "url" expected-url entry)
        (puthash emagent-mcp-server-name entry servers)
        (puthash "mcpServers" servers data)
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (insert (emagent-mcp--json-encode (emagent-mcp--lists-to-vectors data))))))
    file))

(defun emagent-cursor-project-slug (cwd)
  "Return Cursor's ~/.cursor/projects/ slug for absolute CWD."
  (let* ((abs (directory-file-name (expand-file-name cwd)))
         (raw (replace-regexp-in-string "\\`/" "" abs))
         (slug (replace-regexp-in-string "[^A-Za-z0-9]+" "-" raw)))
    (replace-regexp-in-string "-+" "-" slug)))

(defun emagent-cursor-mcp-approvals-file (cwd)
  "Return path to Cursor mcp-approvals.json for CWD."
  (expand-file-name
   "mcp-approvals.json"
   (expand-file-name (emagent-cursor-project-slug cwd)
                     (expand-file-name "projects"
                                       (expand-file-name ".cursor" "~")))))

(defun emagent-cursor--mcp-approval-key (name cwd url)
  "Return Cursor approval id for server NAME at CWD with http URL."
  (let* ((payload (format "{\"path\":%s,\"server\":{\"url\":%s}}"
                          (json-serialize cwd)
                          (json-serialize url)))
         (digest (substring (secure-hash 'sha256 payload) 0 16)))
    (format "%s-%s" name digest)))

(defun emagent-cursor--mcp-server-url (cfg)
  "Return http/sse URL from MCP CFG alist/hash, or nil."
  (or (map-elt cfg 'url)
      (and (hash-table-p cfg) (gethash "url" cfg))))

(defun emagent-mcp--cursor-extra-servers-p ()
  "Return non-nil when ~/.cursor/mcp.json has a non-emagent server."
  (when-let* ((file (bound-and-true-p emagent-mcp-cursor-config-file))
              ((file-readable-p file))
              (data (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'alist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :false))))
              (servers (map-elt data 'mcpServers)))
    (cl-some (lambda (pair)
               (let ((name (if (symbolp (car pair))
                               (symbol-name (car pair))
                             (format "%s" (car pair)))))
                 (not (equal name emagent-mcp-server-name))))
             servers)))

(defun emagent-cursor-write-mcp-approvals (&optional cwd)
  "Approve non-emagent http servers from ~/.cursor/mcp.json for CWD.

Writes ~/.cursor/projects/<slug>/mcp-approvals.json using Cursor's
`name-sha256prefix' key format.  `cursor-agent mcp enable' alone is not
enough: ACP only loads servers listed in that file for the session cwd.
Returns the approvals file path, or nil when there is nothing to write."
  (let* ((cwd (directory-file-name
               (expand-file-name
                (or cwd default-directory))))
         (file (bound-and-true-p emagent-mcp-cursor-config-file))
         (data (and file (file-readable-p file)
                    (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'alist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :false)))))
         (servers (map-elt data 'mcpServers))
         keys)
    (dolist (pair servers)
      (let* ((name (if (symbolp (car pair))
                       (symbol-name (car pair))
                     (format "%s" (car pair))))
             (url (emagent-cursor--mcp-server-url (cdr pair))))
        (unless (or (equal name emagent-mcp-server-name)
                    (not (stringp url))
                    (string-empty-p url))
          (push (emagent-cursor--mcp-approval-key name cwd url) keys))))
    (setq keys (nreverse (delete-dups keys)))
    (when keys
      (let ((approvals (emagent-cursor-mcp-approvals-file cwd)))
        (make-directory (file-name-directory approvals) t)
        (with-temp-file approvals
          (insert (emagent-mcp--json-encode (vconcat keys))))
        (emagent-log "wrote Cursor mcp approvals (%s): %s"
                     (length keys) approvals)
        approvals))))

(defcustom emagent-acp-extra-mcp-config-file "~/.claude.json"
  "JSON file whose top-level `mcpServers' block is forwarded to ACP agents.

Emagent reads the `mcpServers' object from this file and advertises those
servers, alongside the in-Emacs emagent server, to agents that support http MCP
over ACP (e.g. Claude).  This reuses existing Claude MCP server entries without
re-declaring them for emagent.

Only agents wired through ACP `mcpServers' are affected; Cursor discovers MCP
servers from its own ~/.cursor/mcp.json and ignores this option.
Set to nil to forward only the emagent server."
  :type '(choice (const :tag "None" nil) (file :tag "JSON config"))
  :group 'emagent)

(defun emagent-mcp--kv-array (object)
  "Convert OBJECT (alist of KEY . VALUE) to an ACP [{name,value}] vector."
  (vconcat
   (mapcar (lambda (pair)
             `((name . ,(let ((k (car pair)))
                          (if (symbolp k) (symbol-name k) k)))
               (value . ,(cdr pair))))
           object)))

(defun emagent-mcp--convert-gateway-entry (name cfg)
  "Convert config-file MCP entry NAME/CFG to an ACP mcpServer alist, or nil."
  (let ((type (or (map-elt cfg 'type)
                  (and (map-elt cfg 'url) "http"))))
    (pcase type
      ((or "http" "sse")
       (when (map-elt cfg 'url)
         `((type . ,type)
           (name . ,name)
           (url . ,(map-elt cfg 'url))
           (headers . ,(emagent-mcp--kv-array (map-elt cfg 'headers))))))
      (_
       (when (map-elt cfg 'command)
         `((type . "stdio")
           (name . ,name)
           (command . ,(map-elt cfg 'command))
           (args . ,(vconcat (map-elt cfg 'args)))
           (env . ,(emagent-mcp--kv-array (map-elt cfg 'env)))))))))

(defun emagent-mcp-config-file-servers ()
  "Return ACP mcpServer specs from `emagent-acp-extra-mcp-config-file', or nil."
  (when-let* ((file emagent-acp-extra-mcp-config-file)
              (path (expand-file-name file))
              ((file-readable-p path)))
    (condition-case err
        (let* ((data (with-temp-buffer
                       (insert-file-contents path)
                       (json-parse-buffer :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object :false)))
               (servers (map-elt data 'mcpServers)))
          (delq nil
                (mapcar (lambda (pair)
                          (let ((name (symbol-name (car pair))))
                            (unless (equal name emagent-mcp-server-name)
                              (emagent-mcp--convert-gateway-entry name (cdr pair)))))
                        servers)))
      (error
       (require 'emagent-log)
       (emagent-log "could not read MCP servers from %s: %s"
                    path (error-message-string err))
       nil))))

(defun emagent-mcp-session-servers (mcp-http chat-buffer)
  "Return the mcpServers vector to advertise, or nil.

MCP-HTTP is non-nil when the agent advertised http MCP capability.
CHAT-BUFFER is the emagent chat buffer (for the per-buffer token)."
  (when mcp-http
    (with-current-buffer chat-buffer
      (let* ((url (emagent-mcp-session-url (emagent-mcp-buffer-token)))
             (emagent-server `((type . "http")
                               (name . ,emagent-mcp-server-name)
                               (url . ,url)
                               (headers . [])))
             (extra (emagent-mcp-config-file-servers)))
        (vconcat (list emagent-server) extra)))))

(defun emagent-mcp--external-server-names ()
  "Return names of configured external MCP servers (excluding emagent)."
  (let (names)
    (dolist (spec (emagent-mcp-config-file-servers))
      (when-let ((n (map-elt spec 'name)))
        (push n names)))
    (when-let* ((file (bound-and-true-p emagent-mcp-cursor-config-file))
                ((file-readable-p file))
                (data (ignore-errors
                        (with-temp-buffer
                          (insert-file-contents file)
                          (json-parse-buffer :object-type 'alist
                                             :array-type 'list
                                             :null-object nil
                                             :false-object :false))))
                (servers (map-elt data 'mcpServers)))
      (dolist (pair servers)
        (let ((name (if (symbolp (car pair))
                        (symbol-name (car pair))
                      (format "%s" (car pair)))))
          (unless (equal name emagent-mcp-server-name)
            (push name names)))))
    (delete-dups (nreverse names))))

(defun emagent-mcp-gateway-system-prompt ()
  "Return short MCP guidance when external servers are configured, or nil."
  (when-let ((names (emagent-mcp--external-server-names)))
    (concat (bound-and-true-p emagent-acp-system-prompt-gateway)
            (format "\nConfigured servers: %s."
                    (string-join names ", ")))))

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

(defcustom emagent-chat-inactive-bell t
  "When non-nil, ring bell when agent output arrives in an inactive buffer."
  :type 'boolean
  :group 'emagent-chat)

(defcustom emagent-chat-inactive-bell-cooldown 1.0
  "Minimum seconds between inactive-buffer bell notifications."
  :type 'number
  :group 'emagent-chat)

(defcustom emagent-chat-inactive-osx-notification t
  "When non-nil on macOS, show a notification for background attention."
  :type 'boolean
  :group 'emagent-chat)

(defcustom emagent-chat-inactive-notification-title "emagent needs attention"
  "Title for macOS background attention notifications."
  :type 'string
  :group 'emagent-chat)

(defcustom emagent-chat-macos-activate-bundle-id "org.gnu.Emacs"
  "Bundle id used by terminal-notifier to foreground Emacs on click.

Only used when terminal-notifier is installed."
  :type 'string
  :group 'emagent-chat)

(defvar-local emagent-chat--last-inactive-bell-time 0.0
  "Last `float-time' when inactive attention notifications were emitted.")

(defvar emagent-chat--emacs-focused-p t
  "Non-nil when Emacs currently has OS-level input focus.")

(defun emagent-chat--sync-focus-state ()
  "Update `emagent-chat--emacs-focused-p' from `selected-frame' focus."
  (setq emagent-chat--emacs-focused-p
        (if (fboundp 'frame-focus-state)
            (frame-focus-state)
          t)))

(condition-case nil
    (progn
      (unless (advice-member-p #'emagent-chat--sync-focus-state after-focus-change-function)
        (add-function :after after-focus-change-function
                      #'emagent-chat--sync-focus-state))
      (emagent-chat--sync-focus-state))
  (error nil))

(defun emagent-chat--inactive-attention-needed-p ()
  "Return non-nil when background attention notifications should fire."
  (and (not emagent-chat--emacs-focused-p)
       (null (get-buffer-window (current-buffer) 0))))

(defun emagent-chat--notify-macos-inactive-update ()
  "Show a macOS notification for background emagent attention.

Uses terminal-notifier when available (click can activate Emacs),
otherwise falls back to osascript notifications.  Any launcher error is
ignored so chat rendering never stalls on OS notifications."
  (when (and emagent-chat-inactive-osx-notification
             (eq system-type 'darwin)
             (not noninteractive))
    (let* ((title emagent-chat-inactive-notification-title)
           (message (or (buffer-name) "emagent"))
           (notifier (executable-find "terminal-notifier"))
           (osascript (executable-find "osascript")))
      (condition-case nil
          (if notifier
              (start-process
               "emagent-inactive-notify" nil notifier
               "-title" title
               "-message" message
               "-group" "emagent-attention"
               "-activate" emagent-chat-macos-activate-bundle-id)
            (when osascript
              (start-process
               "emagent-inactive-notify" nil osascript "-e"
               (format "display notification %s with title %s"
                       (prin1-to-string message)
                       (prin1-to-string title)))))
        (error nil)))))

(defun emagent-chat--notify-inactive-update ()
  "Emit throttled attention notifications for background permission dialogue."
  (when (emagent-chat--inactive-attention-needed-p)
    (let ((now (float-time)))
      (when (>= (- now emagent-chat--last-inactive-bell-time)
                emagent-chat-inactive-bell-cooldown)
        (setq emagent-chat--last-inactive-bell-time now)
        (condition-case nil
            (progn
              (when emagent-chat-inactive-bell
                (ding t))
              (emagent-chat--notify-macos-inactive-update))
          (error nil))))))

(defvar-local emagent-chat--permission-pending nil
  "Non-nil while a permission dialog is active in the current buffer.
New tool-call lines are suppressed while a dialog awaits user input so the
thinking block stays stable until the user responds.")

(defun emagent-chat--insert-permission-newline-if-needed ()
  "Insert a separating newline unless point is already on a fresh line."
  (unless (bolp)
    (insert "\n")))

(defun emagent-chat--ensure-reasoning-for-tool ()
  "Ensure the open response can accept tool annotations in Reasoning."
  (when (emagent-chat--open-response-p)
    (emagent-chat--ensure-reasoning-scaffold)))

(defun emagent-chat--separate-before-tool ()
  "Ensure point is on a fresh line before inserting a tool line.
Consecutive tool lines and src blocks stay adjacent; a blank line is added
only before the first tool line after prose."
  (unless (bolp) (insert "\n"))
  (unless (or (bobp)
              (save-excursion
                (forward-line -1)
                (or (looking-at-p "[ \t]*$")
                    (looking-at-p "→ ")
                    (looking-at-p "#\\+[Ee][Nn][Dd]_[Ss][Rr][Cc]")
                    (looking-at emagent-chat--thinking-headline-re))))
    (insert "\n")))

(defun emagent-chat--append-tool-line (label &optional id lang code)
  "Append tool LABEL to the open Reasoning block.
When ID is non-nil, remember the span for later in-place updates.  When CODE
is non-empty, render it as an Org src block in LANG instead of a single →
line, with LABEL's trailing decision/(Emacs) annotation beneath."
  (when (and label (not (string-empty-p label))
               (emagent-chat--open-response-p)
               (not emagent-chat--permission-pending))
    (emagent-chat--end-send-pending-if-active)
    (emagent-chat--with-stable-view
     (lambda ()
       (with-current-buffer (current-buffer)
         (let ((inhibit-read-only t))
           (emagent-chat--writable)
           ;; Write any buffered reasoning first so the tool line lands after
           ;; the prose received so far, never splitting a pending sentence.
           ;; Force a final flush so a held inline-code span is emitted before
           ;; the tool line rather than stranded after it.
           (emagent-chat--flush-thought-pending t)
           (emagent-chat--ensure-response-markers)
           (emagent-chat--ensure-reasoning-for-tool)
           (unless (and id (emagent-chat--update-tool-call-line id label lang code))
             (when (and emagent-chat--thought-open-p
                        emagent-chat--thought-marker
                        (marker-position emagent-chat--thought-marker))
               (save-excursion
                 (goto-char emagent-chat--thought-marker)
                 (emagent-chat--separate-before-tool)
                 (let ((line-start (line-beginning-position))
                       (blockp (and code (not (string-empty-p code)))))
                   (insert (if blockp
                               (if (and (equal lang "text")
                                        (not (string-match-p "\n" (or code ""))))
                                   ;; Text block = file path: arrow with display path, no block.
                                   (let* ((annotation (emagent-chat--tool-label-annotation label))
                                          (base (if annotation
                                                    (string-trim
                                                     (replace-regexp-in-string
                                                      (concat " *" (regexp-quote annotation) "\\'")
                                                      "" label))
                                                  label))
                                          (verb (car (split-string base "[ :/]" t)))
                                          (full-label (concat (or verb base)
                                                              ": "
                                                              (emagent-chat--display-path code)
                                                              (if annotation (concat " " annotation) ""))))
                                     (emagent-chat--format-tool-line full-label))
                                 ;; Non-text blocks: arrow + block.
                                 (concat (emagent-chat--format-tool-line
                                          (emagent-chat--combined-arrow-label label code))
                                         "\n"
                                         (emagent-chat--format-tool-block code lang nil)))
                             (emagent-chat--format-tool-line label)))
                   (let ((line-end (line-end-position)))
                     (when id
                       (puthash id (cons (copy-marker line-start nil)
                                         (copy-marker line-end nil))
                                emagent-chat--tool-call-lines))
                     (if blockp
                         (emagent-chat--fontify-tool-block line-start line-end)
                       (emagent-chat--fontify-tool-line line-start line-end)))
                   (emagent-chat--finish-tool-line-in-reasoning)))))))))))

(defun emagent-chat--update-tool-call-line (id label &optional lang code)
  "Replace the displayed tool-call span for ID with LABEL.
When CODE is non-empty, render an Org src block in LANG instead of a line.
Return non-nil when a span was updated."
  (let ((entry (gethash id emagent-chat--tool-call-lines)))
    (when (and entry
               (markerp (car entry)) (marker-position (car entry))
               (markerp (cdr entry)) (marker-position (cdr entry)))
      (let* ((start (car entry))
             (end (cdr entry))
             (blockp (and code (not (string-empty-p code))))
             (annotation (emagent-chat--tool-label-annotation label))
             ;; When transitioning from an arrow line to a block, keep the
             ;; arrow line (without annotation) and append the block below.
             ;; The annotation moves into the block comment so it appears once.
             (current (buffer-substring-no-properties start end))
             ;; Arrow-only: single → line with no block appended yet.
             ;; Arrow-with-block: already combined → line + #+begin_src block.
             (was-arrow-only (string-match-p "\\`→ [^\n]*\\'" current))
             (was-arrow-with-block (and (string-match-p "\\`→ " current)
                                        (not was-arrow-only)))
             (display (cond
                       ((and blockp (or was-arrow-only was-arrow-with-block)
                             (equal lang "text")
                             (not (string-match-p "\n" (or code ""))))
                        ;; Text block = file path: show the display path on the
                        ;; arrow (no block) by reconstructing the label from
                        ;; the untruncated code.
                        (let* ((base (if annotation
                                         (string-trim
                                          (replace-regexp-in-string
                                           (concat " *" (regexp-quote annotation) "\\'")
                                           "" label))
                                       label))
                               (verb (car (split-string base "[ :/]" t)))
                               (full-label (concat (or verb base)
                                                   ": "
                                                   (emagent-chat--display-path code)
                                                   (if annotation (concat " " annotation) ""))))
                          (emagent-chat--format-tool-line full-label)))
                       ((and blockp (or was-arrow-only was-arrow-with-block))
                        ;; Arrow carries annotation; abbreviate if label==code.
                        (concat (emagent-chat--format-tool-line
                                 (emagent-chat--combined-arrow-label label code))
                                "\n"
                                (emagent-chat--format-tool-block code lang nil)))
                       (blockp
                        (emagent-chat--format-tool-block
                         code lang
                         (if (equal lang "text")
                             (emagent-chat--tool-label-title-annotation label)
                           annotation)))
                       (t (emagent-chat--format-tool-line label)))))
        (unless (string= (buffer-substring-no-properties start end) display)
          (save-excursion
            (delete-region start end)
            (goto-char start)
            (insert display)
            (set-marker end (point))
            (if blockp
                (emagent-chat--fontify-tool-block (marker-position start)
                                                  (marker-position end))
              (emagent-chat--fontify-tool-line (marker-position start)
                                               (marker-position end)))
            (when emagent-chat--thought-open-p
              (emagent-chat--sync-thought-marker-after-tool end))))
        t))))

(defun emagent-chat-show-tool-call (id label &optional lang code)
  "Show or update a tool-call display for ACP toolCallId ID with LABEL.
When CODE is non-empty, render it as an Org src block in LANG instead of a
single → line."
  (emagent-chat--append-tool-line label id lang code))

(defun emagent-chat-permission-prompt (question choices callback &optional tool-call)
  "Show permission UI for QUESTION at the end of `** Thinking'.

When TOOL-CALL carries a shell command or edit payload, inserts that content,
then CHOICES as buttons.  Otherwise inserts a ? question line before the
buttons.  Skips that content/question line when it would just repeat
TOOL-CALL's already-rendered pending tool-call line.

CHOICES is a list of (LABEL . VALUE) pairs.  Non-blocking: inserts the dialog
and returns immediately.  CALLBACK is called with the chosen VALUE when a
button is clicked.

Keyboard shortcuts (via keymap text property on the buttons line):
  y / RET  — Allow once    s — Allow for session
  w        — Allow always  a — Allow all (session)
  n        — Deny."
  (when (emagent-chat--open-response-p)
    (let* ((buf (current-buffer))
           (raw-content-block (emagent-chat--permission-content-block tool-call))
           (redundant (emagent-chat--permission-redundant-p
                       tool-call raw-content-block question))
           (content-block (unless redundant raw-content-block))
           (responded nil)
           btn-keymap
           question-beg question-end
           content-beg content-end
           buttons-beg buttons-end
           first-button)
      (let ((cleanup
             (lambda ()
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (emagent-chat--writable)
                   (when (and question-beg question-end
                              (marker-buffer question-beg) (marker-buffer question-end))
                     (delete-region (marker-position question-beg) (marker-position question-end)))
                   (when (and buttons-beg buttons-end
                              (marker-buffer buttons-beg) (marker-buffer buttons-end))
                     (delete-region (marker-position buttons-beg) (marker-position buttons-end)))
                   (when (and content-beg content-end
                              (marker-buffer content-beg) (marker-buffer content-end))
                     (delete-region (marker-position content-beg) (marker-position content-end)))
                   (when-let ((stream (emagent-chat--reasoning-stream-marker)))
                     (setq emagent-chat--thought-marker stream))
                   (setq emagent-chat--permission-pending nil))))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat--ensure-response-markers)
            (emagent-chat--ensure-reasoning-scaffold)
            (if-let ((insert-at (emagent-chat--reasoning-block-tail)))
                (progn
                  (goto-char insert-at)
                  ;; Normalize: keep at most 1 blank line before the dialog.
                  ;; reasoning-block-tail may point past trailing \n\n from the
                  ;; response body; strip the excess so the dialog stays tight.
                  (let ((content-end-pos (save-excursion
                                           (skip-chars-backward
                                            "\n"
                                            (or (emagent-chat--open-response-begin)
                                                (point-min)))
                                           (point))))
                    (when (> (- insert-at content-end-pos) 2)
                      (delete-region (+ content-end-pos 2) insert-at)
                      (goto-char (+ content-end-pos 2))))
                  (when content-block
                    (setq content-beg (copy-marker (point) nil))
                    (emagent-chat--insert-permission-newline-if-needed)
                    (insert content-block "\n")
                    (setq content-end (copy-marker (point) nil)))
                  (goto-char (or (and content-end (marker-position content-end))
                                 insert-at))
                  (unless (or content-block redundant)
                    (setq question-beg (copy-marker (point) nil))
                    (emagent-chat--insert-permission-newline-if-needed)
                    (insert (emagent-chat--format-permission-line question))
                    (put-text-property (marker-position question-beg) (point)
                                       'face 'emagent-permission-prompt)
                    (emagent-chat--repair-tool-line-faces (marker-position question-beg) (point))
                    (insert "\n")
                    (setq question-end (copy-marker (point) nil)))
                  (goto-char (or (and question-end (marker-position question-end))
                                 (and content-end (marker-position content-end))
                                 insert-at))
                  (setq buttons-beg (copy-marker (point) nil))
                  (emagent-chat--insert-permission-newline-if-needed)
                  (setq btn-keymap (make-sparse-keymap))
                  (set-keymap-parent btn-keymap button-map)
                  ;; Build key-hints alist and populate btn-keymap first
                  (let* ((allow-once-shown nil) (allow-always-shown nil) (deny-shown nil)
                         (hints
                          (mapcar
                           (lambda (choice)
                             (let* ((val (cdr choice))
                                    (id (and (stringp val) (downcase val)))
                                    (kh (cond
                                         ((eq val :allow-once) (setq allow-once-shown t) "y")
                                         ((eq val :allow-session) "s")
                                         ((eq val :allow-always) (setq allow-always-shown t) "w")
                                         ((eq val :allow-all) "a")
                                         ((eq val :deny) (setq deny-shown t) "n")
                                         ((and (not allow-always-shown) id
                                               (string-match-p "allow_always\\|always" id))
                                          (setq allow-always-shown t) "w")
                                         ((and (not allow-once-shown) id
                                               (string-match-p "allow\\|yes\\|run" id))
                                          (setq allow-once-shown t) "y")
                                         ((and (not deny-shown) id
                                               (string-match-p "deny\\|no\\|reject" id))
                                          (setq deny-shown t) "n")
                                         (t nil))))
                               (when kh
                                 (define-key btn-keymap (kbd kh)
                                             (let ((v val))
                                               (lambda ()
                                                 (interactive)
                                                 (unless responded
                                                   (setq responded t)
                                                   (funcall cleanup)
                                                   (funcall callback v))))))
                               kh))
                           choices)))
                    ;; Now insert buttons with btn-keymap as their keymap
                    (cl-mapc
                     (lambda (choice kh)
                       (let ((val (cdr choice)))
                         (unless first-button
                           (setq first-button (copy-marker (point) nil)))
                         (insert-button
                          (concat "[" (car choice) "]")
                          'keymap btn-keymap
                          'action
                          (let ((v val))
                            (lambda (_b)
                              (unless responded
                                (setq responded t)
                                (funcall cleanup)
                                (funcall callback v))))
                          'follow-link t)
                         (when kh
                           (insert (propertize (format " [%s]" kh) 'face 'shadow)))
                         (insert "  ")))
                     choices hints))
                  (insert "\n")
                  (setq buttons-end (copy-marker (point) nil))
                  (when first-button
                    (emagent-tools--apply-button-line-keymap
                     (marker-position first-button)
                     (marker-position buttons-end)
                     btn-keymap))
                  (setq emagent-chat--permission-pending t))
              (setq question-beg nil content-beg nil buttons-beg nil))))
        (emagent-chat--notify-inactive-update)
        (if (not buttons-beg)
            (let ((content-block (or content-block raw-content-block))
                  (preamble (concat
                             "\n** Request permissions\n"
                             (when content-block
                               (concat content-block "\n")))))
              (emagent-tools--buttons-prompt
               (if content-block "" question)
               choices buf callback preamble))
          (emagent-tools--focus-inline-buttons buf first-button))))))

(defvar emagent-chat-provider)

(defcustom emagent-claude-mcp-command "claude"
  "Claude Code CLI used for `mcp list' / `mcp login'.

Distinct from `emagent-claude-acp-command' (the ACP bridge binary)."
  :type 'string
  :group 'emagent-chat)

(defun emagent-chat--mcp-command-p (text)
  "Return non-nil when TEXT is a bare `/mcp' (optional server id)."
  (let ((trimmed (string-trim text)))
    (when (string-prefix-p "/" trimmed)
      (let* ((body (substring trimmed 1))
             (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
             (cmd (if space (substring body 0 space) body)))
        (string= cmd "mcp")))))

(defun emagent-chat--mcp-arg (text)
  "Return the optional server id after `/mcp' in TEXT, or nil."
  (let ((trimmed (string-trim text)))
    (when (string-prefix-p "/mcp" trimmed)
      (let ((rest (string-trim (substring trimmed (length "/mcp")))))
        (and (not (string-empty-p rest)) rest)))))

(defun emagent-chat--usage-command-p (text)
  "Return non-nil when TEXT is a bare `/usage' command."
  (let ((trimmed (string-trim text)))
    (when (string-prefix-p "/" trimmed)
      (let* ((body (substring trimmed 1))
             (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
             (cmd (if space (substring body 0 space) body)))
        (string= cmd "usage")))))

(defun emagent-chat--slash-usage-apply (&optional text)
  "Handle `/usage', `/usage baseline', and `/usage clear' from TEXT."
  (when-let ((bounds (emagent-chat--slash-token-bounds)))
    (delete-region (car bounds) (cdr bounds)))
  (require 'emagent-usage)
  (let* ((trimmed (string-trim (or text "")))
         (rest (string-trim (substring trimmed
                                       (min (length trimmed)
                                            (length "/usage"))))))
    (cond
     ((string-match-p "\\`baseline\\>" rest)
      (emagent-usage-baseline-set)
      (message "emagent: usage baseline set"))
     ((string-match-p "\\`clear\\>" rest)
      (emagent-usage-baseline-clear)
      (message "emagent: usage baseline cleared"))
     (t
      (emagent-usage)))))

(defun emagent-chat--notes-command-p (text)
  "Return non-nil when TEXT is a `/notes' client command."
  (let ((trimmed (string-trim text)))
    (when (string-prefix-p "/" trimmed)
      (let* ((body (substring trimmed 1))
             (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
             (cmd (if space (substring body 0 space) body)))
        (string= cmd "notes")))))

(defun emagent-chat--slash-notes-apply (&optional text)
  "Handle `/notes' and `/notes clear' from TEXT without sending to the agent."
  (when-let ((bounds (emagent-chat--slash-token-bounds)))
    (delete-region (car bounds) (cdr bounds)))
  (let* ((trimmed (string-trim (or text "")))
         (rest (string-trim (substring trimmed (min (length trimmed)
                                                   (length "/notes"))))))
    (cond
     ((string-match-p "\\`clear\\>" rest)
      (emagent-session-notes-write "")
      (message "emagent: session notes cleared"))
     ((not (string-empty-p rest))
      (emagent-session-notes-write rest)
      (message "emagent: session notes updated"))
     (t
      (let ((notes (emagent-session-notes-read)))
        (if (string-empty-p notes)
            (message "emagent: no session notes")
          (message "%s" notes)))))))

(defun emagent-chat--mcp-parse-list-line (line)
  "Parse one `mcp list' LINE into \(NAME . STATUS\) or nil.

Supports Cursor \(`name: status'\) and Claude
\(`name: detail - Connected'\) formats."
  (let ((trimmed (string-trim line)))
    (cond
     ((string-empty-p trimmed) nil)
     ((string-prefix-p "⚠" trimmed) nil)
     ((string-prefix-p "Checking MCP" trimmed) nil)
     ((not (string-match "\\`\\([^:]+\\):[[:space:]]*\\(.*\\)\\'" trimmed)) nil)
     (t
      (let* ((name (string-trim (match-string 1 trimmed)))
             (rest (string-trim (match-string 2 trimmed)))
             (status
              (cond
               ((string-match-p "requires_authentication" rest)
                "requires_authentication")
               ((string-match-p "needs[_-]?auth" rest)
                "needs_authentication")
               ((or (string-match-p "✔[[:space:]]*Connected" rest)
                    (string-match-p "\\bConnected\\b" rest))
                "ready")
               ((string-match-p "Pending approval" rest)
                "pending_approval")
               ((string-match-p "\\bready\\b" rest)
                "ready")
               ((string-match-p "\\bdisabled\\b" rest)
                "disabled")
               ((string-match-p "\\berror\\b\\|failed\\|✗\\|✘" rest)
                "error")
               (t rest))))
        (and (not (string-empty-p name))
             (cons name status)))))))

(defun emagent-chat--mcp-parse-list (output)
  "Parse full `mcp list' OUTPUT into an alist of \(NAME . STATUS\)."
  (let (servers)
    (dolist (line (split-string output "\n" t))
      (when-let ((entry (emagent-chat--mcp-parse-list-line line)))
        (push entry servers)))
    (nreverse servers)))

(defun emagent-chat--mcp-needs-auth-p (status)
  "Return non-nil when STATUS indicates OAuth/login is required."
  (and (stringp status)
       (string-match-p
        "requires_authentication\\|needs[_-]?auth\\|authentication\\|unauthorized\\|login"
        (downcase status))))

(defun emagent-chat--mcp-cli ()
  "Return (PROGRAM . DEFAULT-DIRECTORY) for the current provider's MCP CLI."
  (pcase emagent-chat-provider
    ('cursor
     (emagent-cursor-check-command)
     (cons (emagent-cursor-command) (emagent-session-project-directory)))
    ('claude
     (unless (executable-find emagent-claude-mcp-command)
       (user-error "Claude CLI not found on PATH (%s)"
                   emagent-claude-mcp-command))
     (cons emagent-claude-mcp-command (emagent-session-project-directory)))
    (_
     (user-error "/mcp is only supported for Claude and Cursor"))))

(defun emagent-chat--mcp-call (program directory &rest args)
  "Run PROGRAM with ARGS in DIRECTORY; return (EXIT-CODE . OUTPUT).

Prefer `emagent-chat--mcp-start' for interactive UI: a synchronous list can
deadlock when the CLI health-checks the in-Emacs emagent MCP server."
  (with-temp-buffer
    (let* ((default-directory (or directory default-directory))
           (status (apply #'call-process program nil t nil args))
           (out (buffer-string)))
      (cons status out))))

(defun emagent-chat--mcp-list-servers ()
  "Return alist of (NAME . STATUS) from the provider MCP CLI (synchronous).

Only for tests/scripts.  Interactive `/mcp' uses the async path."
  (pcase-let* ((`(,program . ,directory) (emagent-chat--mcp-cli))
               (`(,status . ,out) (emagent-chat--mcp-call program directory
                                                          "mcp" "list")))
    (when (and (numberp status) (/= status 0))
      (emagent-log "mcp list exit %s: %s" status (emagent-log-truncate-line out 200)))
    (let ((servers (emagent-chat--mcp-parse-list out)))
      (unless servers
        (user-error "No MCP servers reported by %s mcp list"
                    program))
      servers)))

(defun emagent-chat--mcp-start (program directory args on-done &optional pty)
  "Start PROGRAM with ARGS in DIRECTORY; call ON-DONE with exit status.

When PTY is non-nil, use a pty (needed for some OAuth CLIs).  A process
filter echoes notable login progress lines to the echo area so feedback
is not lost after `completing-read' returns."
  (let* ((default-directory (or directory default-directory))
         (buf (generate-new-buffer " *emagent-mcp*"))
         (label (mapconcat #'identity args " "))
         (proc
          (make-process
           :name "emagent-mcp"
           :buffer buf
           :command (cons program args)
           :connection-type (if pty 'pty 'pipe)
           :filter
           (lambda (_p chunk)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (goto-char (point-max))
                 (insert chunk)))
             (when (string-match-p
                    "Opening your browser\\|Listening on http\\|Preparing\\|requires authentication\\|login successful\\|authorization"
                    chunk)
               (let ((line (car (last (split-string (string-trim chunk) "\n" t)))))
                 (when (and line (not (string-empty-p line)))
                   (message "emagent: %s" (emagent-log-truncate-line line 120))))))
           :sentinel
           (lambda (p _msg)
             (when (memq (process-status p) '(exit signal))
               (let ((code (process-exit-status p))
                     (out (with-current-buffer (process-buffer p)
                            (buffer-string))))
                 (emagent-log "mcp %s exit %s: %s"
                              label
                              code
                              (emagent-log-truncate-line out 240))
                 (when (buffer-live-p buf)
                   (kill-buffer buf))
                 (when on-done
                   (funcall on-done code out))))))))
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun emagent-chat--mcp-reload-session (buffer &optional name)
  "Reconnect BUFFER's ACP session so MCP tools load after auth.

NAME is the MCP server id used in status messages."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'emagent-mode)
        (unless (fboundp 'emagent-acp-shutdown-buffer)
          (require 'emagent-acp))
        (message "emagent: reconnecting to load MCP tools%s…"
                 (if name (format " (%s)" name) ""))
        (emagent-chat-seed-cursor-slash-commands)
        (when (bound-and-true-p emagent-acp--session)
          (emagent-acp-shutdown-buffer))
        (emagent-acp-ensure-connected
         :on-ready
         (lambda ()
           (message "emagent: MCP server%s ready — reconnected"
                    (if name (format " %s" name) ""))))))))

(defun emagent-chat--mcp-login (name &optional provider)
  "Authenticate MCP server NAME via the provider CLI (async).

PROVIDER defaults to `emagent-chat-provider'.  Captured explicitly so the
exit callback does not depend on buffer-local state after the minibuffer.
On success, reconnects the ACP session so gateway tools are rediscovered."
  (let* ((prov (or provider emagent-chat-provider))
         (buf (current-buffer))
         (emagent-chat-provider prov))
    (pcase-let ((`(,program . ,directory) (emagent-chat--mcp-cli)))
      ;; Defer past minibuffer teardown — a bare `message' right after
      ;; `completing-read' is often cleared and looks like a no-op.
      (run-at-time
       0.05 nil
       (lambda ()
         (message "emagent: authenticating MCP server %s (browser may open)…"
                  name)))
      (emagent-chat--mcp-start
       program directory (list "mcp" "login" name)
       (lambda (code _out)
         (if (zerop code)
             (if (eq prov 'cursor)
                 (let ((emagent-chat-provider prov))
                   (ignore-errors
                     (emagent-cursor-write-mcp-approvals directory))
                   (emagent-chat--mcp-enable
                    name
                    (lambda (_enable-code _enable-out)
                      (emagent-chat--mcp-reload-session buf name))))
               (emagent-chat--mcp-reload-session buf name))
           (message "emagent: MCP login for %s failed (exit %s); see *Emagent Log*"
                    name code)))
       t))))

(defun emagent-chat--mcp-enable (name &optional on-done)
  "Approve Cursor MCP server NAME for the project cwd (async).

ON-DONE, when non-nil, is called as (ON-DONE CODE OUT) after enable
finishes (or immediately when the provider is not Cursor)."
  (if (not (eq emagent-chat-provider 'cursor))
      (when on-done (funcall on-done nil nil))
    (pcase-let ((`(,program . ,directory) (emagent-chat--mcp-cli)))
      (emagent-chat--mcp-start
       program directory (list "mcp" "enable" name)
       (lambda (code out)
         (if (zerop code)
             (emagent-log "mcp enable %s: ok" name)
           (emagent-log "mcp enable %s failed (exit %s)" name code))
         (when on-done
           (funcall on-done code out)))))))

(defun emagent-chat--mcp-pick-server (servers &optional preferred)
  "Prompt for a server from SERVERS alist and login/enable as needed.

PREFERRED, when non-nil, selects that server id without prompting."
  (let* ((provider emagent-chat-provider)
         (names (mapcar #'car servers))
         (name
          (or (and preferred (assoc-string preferred servers) preferred)
              (let ((completion-extra-properties
                     (list :annotation-function
                           (lambda (cand)
                             (concat "  "
                                     (or (cdr (assoc-string cand servers))
                                         ""))))))
                (completing-read "MCP server: " names nil t nil nil
                                 preferred)))))
    (when (and name (not (string-empty-p name)))
      (let ((status (cdr (assoc-string name servers))))
        (cond
         ((emagent-chat--mcp-needs-auth-p status)
          (emagent-chat--mcp-login name provider))
         ((eq provider 'cursor)
          (emagent-chat--mcp-enable name)
          (run-at-time
           0.05 nil
           (lambda ()
             (message "emagent: MCP server %s (%s)"
                      name (or status "ready")))))
         (t
          (run-at-time
           0.05 nil
           (lambda ()
             (message "emagent: MCP server %s (%s)"
                      name (or status "ready"))))))))))

(defun emagent-chat--mcp-select-and-act (&optional preferred)
  "List MCP servers asynchronously, then select one (or PREFERRED).

Must be async: a synchronous `mcp list' deadlocks Emacs when Cursor
health-checks the in-process emagent MCP server."
  (let ((buf (current-buffer))
        (provider emagent-chat-provider))
    (pcase-let ((`(,program . ,directory) (emagent-chat--mcp-cli)))
      (message "emagent: listing MCP servers…")
      (emagent-chat--mcp-start
       program directory '("mcp" "list")
       (lambda (code out)
         (let ((servers (emagent-chat--mcp-parse-list out)))
           (cond
            ((not (buffer-live-p buf))
             nil)
            ((and (numberp code) (/= code 0) (null servers))
             (message "emagent: mcp list failed (exit %s); see *Emagent Log*"
                      code))
            ((null servers)
             (message "emagent: no MCP servers reported"))
            (t
             ;; Idle timer: more reliable than run-at-time 0 for
             ;; minibuffer prompts after an async process sentinel.
             (run-with-idle-timer
              0 nil
              (lambda ()
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (condition-case err
                        (let ((emagent-chat-provider provider))
                          (emagent-chat--mcp-pick-server
                           servers preferred))
                      (quit
                       (message "emagent: MCP selection cancelled"))
                      (error
                       (emagent-log "mcp UI error: %s"
                                    (error-message-string err))
                       (message "emagent: %s"
                                (error-message-string err))))))))))))))))

(defun emagent-chat--slash-mcp-apply (&optional text)
  "Run client `/mcp' UI.  TEXT may be `/mcp' or `/mcp NAME'."
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Turn on emagent-mode in this buffer first"))
  (unless (memq emagent-chat-provider '(cursor claude))
    (user-error "/mcp requires a Claude or Cursor session"))
  (when-let ((bounds (emagent-chat--slash-token-bounds)))
    (delete-region (car bounds) (cdr bounds)))
  (emagent-chat--mcp-select-and-act (and text (emagent-chat--mcp-arg text))))

(defun emagent-cursor-approve-configured-mcp-servers ()
  "Approve non-emagent servers from ~/.cursor/mcp.json for the session cwd.

Writes Cursor's per-project mcp-approvals.json (required for ACP to load
http MCP servers) and also runs `cursor-agent mcp enable' as a belt-and-
braces.  Best-effort; failures are logged."
  (require 'emagent-chat)
  (when (and (eq emagent-chat-provider 'cursor)
             (executable-find (emagent-cursor-command)))
    (let* ((directory (emagent-session-project-directory))
           (program (emagent-cursor-command))
           (file (bound-and-true-p emagent-mcp-cursor-config-file))
           (data (and file (file-readable-p file)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'alist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :false))))
           (servers (map-elt data 'mcpServers)))
      (ignore-errors (emagent-cursor-write-mcp-approvals directory))
      (dolist (pair servers)
        (let ((name (if (symbolp (car pair))
                        (symbol-name (car pair))
                      (format "%s" (car pair)))))
          (unless (equal name "emagent")
            (emagent-chat--mcp-start
             program directory (list "mcp" "enable" name)
             (lambda (code _out)
               (emagent-log "mcp enable %s: %s"
                            name
                            (if (zerop code) "ok" "failed"))))))))))

;; Owned by `emagent-chat-mode-line'; read here without requiring it back.
(defvar emagent-chat--status)

(defgroup emagent-chat nil
  "Emagent chat UI."
  :group 'emagent)

(defvar emagent-chat--spinner-timer nil
  "Repeating timer that advances the spinner while any session is busy.")

(defun emagent-chat--maybe-force-mode-line-update ()
  "Refresh this buffer's mode line in every window that displays it.

Updates whenever the buffer is shown in a visible window, not only the selected
one, so the thinking spinner keeps animating in a side-by-side emagent window
after focus moves elsewhere."
  (when (emagent-chat--buffer-displayed-p)
    (force-mode-line-update)))

(defun emagent-chat--spinner-after-custom-set (sym val)
  "Set SYM to VAL and refresh emagent mode lines."
  (set-default sym val)
  (set sym val)
  (when (and (eq sym 'emagent-chat-spinner-interval)
             emagent-chat--spinner-timer)
    (cancel-timer emagent-chat--spinner-timer)
    (setq emagent-chat--spinner-timer
          (run-with-timer 0 val #'emagent-chat--spinner-tick)))
  (emagent-chat--map-live-buffers
   (lambda (buf)
     (with-current-buffer buf
       (when (fboundp 'emagent-chat--mode-line-recompute)
         (emagent-chat--mode-line-recompute))
       (emagent-chat--maybe-force-mode-line-update))))
  nil)

(defcustom emagent-chat-spinner-interval 0.4
  "Seconds between spinner animation frames."
  :type 'number
  :group 'emagent-chat
  :set #'emagent-chat--spinner-after-custom-set)

(defcustom emagent-chat-spinner-height 1.15
  "Scale factor for spinner dots or the braille glyph (`height' face property).
When nil, the spinner inherits the mode-line height."
  :type '(choice (const :tag "inherit" nil) number)
  :group 'emagent-chat
  :set #'emagent-chat--spinner-after-custom-set)

(defcustom emagent-chat-spinner-style 'dots
  "How to render the busy spinner in the mode line.
`braille' is one Unicode braille character; `dots' is three horizontal dots."
  :type '(choice (const :tag "Braille glyph" braille)
                 (const :tag "Dot grid" dots))
  :group 'emagent-chat
  :set #'emagent-chat--spinner-after-custom-set)

(defcustom emagent-chat-spinner-dot-on "●"
  "Character for a lit spinner dot when `emagent-chat-spinner-style' is `dots'."
  :type 'string
  :group 'emagent-chat
  :set #'emagent-chat--spinner-after-custom-set)

(defcustom emagent-chat-spinner-dot-off "○"
  "Character for an unlit spinner dot when `emagent-chat-spinner-style' is `dots'."
  :type 'string
  :group 'emagent-chat
  :set #'emagent-chat--spinner-after-custom-set)

(defface emagent-chat-spinner
  '((t (:inherit (bold mode-line-emphasis))))
  "Face for the mode-line busy spinner glyph."
  :group 'emagent-chat)

(defconst emagent-chat--spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Braille spinner frames shown while the agent is busy.")

(defvar emagent-chat--spinner-frame 0
  "Current spinner frame index into `emagent-chat--spinner-frames'.")

(defvar emagent-chat--spinner-start-time nil
  "Epoch time when the busy spinner animation started, or nil when idle.")

(defconst emagent-chat--spinner-dot-frames '((t nil nil) (nil t nil) (nil nil t) (nil t nil))
  "Four-frame chase: @00, 0@0, 00@, 0@0, and so on.")

(defun emagent-chat--spinner-frame-count ()
  "Return the number of spinner frames for the active style."
  (pcase emagent-chat-spinner-style
    ('dots (length emagent-chat--spinner-dot-frames))
    (_ (length emagent-chat--spinner-frames))))

(defun emagent-chat--spinner-dot-face (lit)
  "Return the face for spinner dot LIT state."
  (let ((height emagent-chat-spinner-height))
    (if lit
        (if height
            `(:inherit emagent-chat-spinner :height ,height)
          'emagent-chat-spinner)
      (if height
          `(:inherit shadow :height ,height)
        'shadow))))

(defun emagent-chat--spinner-dot-char (lit)
  "Return a propertized on/off dot character.

Arguments: LIT."
  (propertize (if lit emagent-chat-spinner-dot-on emagent-chat-spinner-dot-off)
              'face (emagent-chat--spinner-dot-face lit)))

(defun emagent-chat--spinner-dot-grid ()
  "Return three horizontal dots for the current spinner frame."
  (let ((pattern (nth emagent-chat--spinner-frame emagent-chat--spinner-dot-frames)))
    (concat (emagent-chat--spinner-dot-char (nth 0 pattern))
            (emagent-chat--spinner-dot-char (nth 1 pattern))
            (emagent-chat--spinner-dot-char (nth 2 pattern)))))

(defun emagent-chat--spinner-braille ()
  "Return the current frame as one braille character."
  (let ((face (if emagent-chat-spinner-height
                  `(:inherit emagent-chat-spinner
                            :height ,emagent-chat-spinner-height)
                'emagent-chat-spinner)))
    (propertize (aref emagent-chat--spinner-frames emagent-chat--spinner-frame)
                'face face)))

(defun emagent-chat--spinner-sync-frame ()
  "Update `emagent-chat--spinner-frame' from elapsed time since spinner start."
  (when emagent-chat--spinner-start-time
    (let* ((count (emagent-chat--spinner-frame-count))
           (interval (max 0.05 emagent-chat-spinner-interval)))
      (setq emagent-chat--spinner-frame
            (% (floor (/ (- (float-time) emagent-chat--spinner-start-time)
                         interval))
               count))
      t)))

(defun emagent-chat--spinner-string ()
  "Return the current spinner rendering for the mode line."
  (emagent-chat--spinner-sync-frame)
  (pcase emagent-chat-spinner-style
    ('dots (emagent-chat--spinner-dot-grid))
    (_ (emagent-chat--spinner-braille))))

(defun emagent-chat--mode-line-spinner-suffix ()
  "Return the propertized busy spinner suffix for the mode line."
  (concat " " (emagent-chat--spinner-string)))

(defun emagent-chat--spinner-active-p ()
  "Return non-nil when the mode-line thinking spinner should animate."
  (and (or (plist-get emagent-chat--status :busy) emagent-chat--send-pending)
       (not (plist-get emagent-chat--status :waiting-permission))))

(defun emagent-chat--spinner-animate-p (&optional buffer)
  "Return non-nil when BUFFER is displayed and should animate the spinner.

The spinner keeps animating whenever the buffer is shown in any visible
window, including an unselected window (two emagent buffers side by side) or
while Emacs is unfocused.  It stops only when no visible frame displays the
buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and (emagent-chat--spinner-active-p)
         (emagent-chat--buffer-displayed-p (current-buffer)))))

(defun emagent-chat--any-spinner-active-p ()
  "Return non-nil when any active emagent buffer needs spinner animation."
  (catch 'found
    (emagent-chat--map-live-buffers
     (lambda (buf)
       (when (with-current-buffer buf
               (emagent-chat--spinner-animate-p buf))
         (throw 'found t))))
    nil))

(defun emagent-chat--spinner-stop ()
  "Cancel the spinner timer when no session needs animation."
  (when emagent-chat--spinner-timer
    (cancel-timer emagent-chat--spinner-timer))
  (setq emagent-chat--spinner-timer nil
        emagent-chat--spinner-start-time nil))

(defun emagent-chat--spinner-ensure-running ()
  "Start or stop the spinner timer based on active busy emagent buffers."
  (if (emagent-chat--any-spinner-active-p)
      (unless emagent-chat--spinner-timer
        (setq emagent-chat--spinner-start-time (float-time)
              emagent-chat--spinner-frame 0)
        (emagent-chat--spinner-restart-timer))
    (emagent-chat--spinner-stop)))

(defun emagent-chat--spinner-refresh-buffer (buffer)
  "Refresh BUFFER's mode line when it is the active busy emagent buffer."
  (with-current-buffer buffer
    (when (emagent-chat--spinner-animate-p buffer)
      (when (fboundp 'emagent-chat--mode-line-recompute)
        (emagent-chat--mode-line-recompute))
      (emagent-chat--maybe-force-mode-line-update)
      t)))

(defun emagent-chat--spinner-refresh-idle ()
  "Apply the current spinner frame to the active busy emagent buffer.

Uses `force-mode-line-update' only — never `redisplay'.  A forced redisplay
from this timer re-entered `window-configuration-change-hook' and could run
deferred org table alignment on the chat buffer until Emacs pegged CPU."
  (let (active-refreshed)
    (emagent-chat--map-live-buffers
     (lambda (buf)
       (with-current-buffer buf
         (when (emagent-chat--spinner-refresh-buffer buf)
           (setq active-refreshed t)))))
    (when active-refreshed
      (force-mode-line-update t))
    (emagent-chat--spinner-ensure-running)))

(defun emagent-chat--spinner-tick ()
  "Refresh visible busy mode lines for the current spinner frame."
  (emagent-chat--spinner-sync-frame)
  (emagent-chat--spinner-refresh-idle))

(defun emagent-chat--spinner-restart-timer ()
  "Restart the spinner timer using `emagent-chat-spinner-interval'."
  (when emagent-chat--spinner-timer
    (cancel-timer emagent-chat--spinner-timer))
  (setq emagent-chat--spinner-timer
        (run-with-timer 0 emagent-chat-spinner-interval
                        #'emagent-chat--spinner-tick)))

(defun emagent-chat--spinner-start ()
  "Start the spinner timer if not already running."
  (emagent-chat--spinner-ensure-running)
  (when (derived-mode-p 'emagent-mode)
    (when (fboundp 'emagent-chat--mode-line-recompute)
      (emagent-chat--mode-line-recompute))
    (emagent-chat--maybe-force-mode-line-update)))

;; Owned by the facade `emagent-chat' (which requires this file); forward
;; declared here so this file never requires it back.
(defvar emagent-chat--turn-model)

(defvar-local emagent-chat--mode-line-head nil
  "Cached mode-line status prefix for the current emagent buffer.")

(defvar-local emagent-chat--mode-line-tail nil
  "Cached mode-line metadata suffix for the current emagent buffer.")

(defvar-local emagent-chat--mode-line-cache nil
  "Cached full mode-line string for `emagent-mode-line'.")

(defvar-local emagent-chat--mode-line-stale-p nil
  "When non-nil, recompute the mode line when this buffer becomes active.")

(defvar-local emagent-chat--status nil
  "Plist snapshot of ACP session status, pushed by the ACP layer via :cb-status.

Keys: :busy :waiting-permission :ready :prompt-finishing :tool :tool-kind :rss
:emacs-rss :model-id :ctx-usage (a (USED . SIZE) cons or nil) :ctx-unavailable.
The mode line renders from this snapshot so the UI never calls up into the ACP
runtime.")

(defun emagent-chat--stat (key)
  "Return status field KEY from the pushed ACP snapshot."
  (plist-get emagent-chat--status key))

(defun emagent-chat-model-display ()
  "Return a short model label for the mode line.
Prefer the pending `/model' target while preparing a send, then the live ACP
session model pushed via `emagent-chat-set-status' (including transient
per-turn switches), otherwise the buffer's saved #+EMAGENT_MODEL."
  (let ((id (cond
              ((and emagent-chat--send-pending emagent-chat--turn-model)
               emagent-chat--turn-model)
              ((emagent-chat--stat :ready)
               (emagent-chat--stat :model-id))
              (t (emagent-session-model)))))
    (when id (emagent-session-model-display id))))

(defun emagent-chat-set-model (model)
  "Store ACP MODEL id in the current buffer and refresh the mode line."
  (emagent-session-set-model model)
  (emagent-chat--refresh-mode-line))

(defun emagent-chat-set-status (status)
  "Store the ACP STATUS snapshot for this buffer and refresh the mode line.
This is the ACP layer's downward entry point (wired as :cb-status); it replaces
the mode line pulling session state back out of the ACP layer."
  (setq emagent-chat--status status)
  (when (emagent-chat--stat :busy)
    (emagent-chat--spinner-ensure-running))
  (if (emagent-chat--stat :busy)
      (emagent-chat--refresh-mode-line-soon)
    (emagent-chat--refresh-mode-line)))

(defun emagent-chat--mode-line-recompute ()
  "Rebuild cached mode-line strings for the current emagent buffer."
  (let ((parts (emagent-chat--mode-line-strings)))
    (setq emagent-chat--mode-line-head (car parts)
          emagent-chat--mode-line-tail (cdr parts)
          emagent-chat--mode-line-cache (concat (car parts) (cdr parts))
          emagent-chat--mode-line-stale-p nil)))

(defvar-local emagent-chat--mode-line-refresh-timer nil
  "One-shot idle timer that coalesces mode-line recomputes for this buffer.")

(defun emagent-chat--refresh-mode-line ()
  "Recompute and invalidate the mode line in the current buffer immediately."
  (when emagent-chat--mode-line-refresh-timer
    (cancel-timer emagent-chat--mode-line-refresh-timer)
    (setq emagent-chat--mode-line-refresh-timer nil))
  (emagent-chat--mode-line-recompute)
  (emagent-chat--maybe-force-mode-line-update))

(defun emagent-chat--refresh-mode-line-soon ()
  "Queue a single mode-line recompute for the current buffer."
  (let ((buf (current-buffer)))
    (if (emagent-chat--buffer-active-p)
        (progn
          (when emagent-chat--mode-line-refresh-timer
            (cancel-timer emagent-chat--mode-line-refresh-timer))
          (let ((refresh
                 (lambda ()
                   (setq emagent-chat--mode-line-refresh-timer nil)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (emagent-chat--mode-line-recompute)
                       (emagent-chat--maybe-force-mode-line-update))))))
            (setq emagent-chat--mode-line-refresh-timer
                  (run-with-idle-timer 0 nil refresh))))
      (progn
        (setq emagent-chat--mode-line-stale-p t)
        (when emagent-chat--mode-line-refresh-timer
          (cancel-timer emagent-chat--mode-line-refresh-timer)
          (setq emagent-chat--mode-line-refresh-timer nil))))))

(defun emagent-chat--refresh-mode-line-on-focus ()
  "Recompute a stale or busy mode line after this buffer becomes active."
  (when (emagent-chat--buffer-active-p)
    (when (or emagent-chat--mode-line-stale-p
              (emagent-chat--stat :busy))
      (emagent-chat--mode-line-recompute)
      (force-mode-line-update))))

(defun emagent-chat--mode-line-strings ()
  "Return (HEAD . TAIL) strings for the emagent mode line."
  (let* ((busy  (emagent-chat--stat :busy))
         (waiting-permission (emagent-chat--stat :waiting-permission))
         (ready (emagent-chat--stat :ready))
         (tool  (emagent-chat--stat :tool))
         (kind  (emagent-chat--stat :tool-kind))
         (rss   (emagent-chat--stat :rss))
         (emacs-rss (emagent-chat--stat :emacs-rss))
         (connected (or busy ready))
         (spinner (when (emagent-chat--spinner-animate-p)
                    (emagent-chat--mode-line-spinner-suffix)))
         (busy-face '(bold mode-line-emphasis))
         (head (cond
                (waiting-permission
                 (propertize "emagent:Allow?" 'face 'warning))
                ((and (not busy) ready
                      (emagent-chat--open-response-p)
                      (not (emagent-chat--stat :prompt-finishing)))
                 (propertize "emagent:stalled" 'face 'warning))
                ((and busy tool (member kind '("write" "execute")))
                 (concat (propertize "Executing" 'face busy-face)
                         spinner))
                (emagent-chat--send-pending
                 (concat (propertize
                          (if emagent-chat--turn-model "Switching" "Preparing")
                          'face busy-face)
                         spinner))
                (busy
                 (concat (propertize "Thinking" 'face busy-face)
                         spinner))
                (ready (propertize "emagent:Idle" 'face 'success))
                (connected (propertize "emagent:connecting" 'face 'warning))
                (t "emagent")))
         (model (emagent-chat-model-display))
         (sep (propertize " | " 'face 'shadow))
         (model-str (when (and model (not (string-empty-p model)))
                      (propertize model 'face 'shadow)))
         (mode-id (emagent-chat--stat :mode-id))
         (mode-str (when (and (stringp mode-id)
                              (not (string-empty-p mode-id))
                              (not (member mode-id '("agent" "default"))))
                     (propertize mode-id 'face 'shadow)))
         (context (emagent-chat--mode-line-context-usage))
         (tokens (emagent-chat--mode-line-token-usage))
         (mcp-bytes (emagent-chat--mode-line-mcp-bytes))
         (rss-str (when rss
                    (propertize (format "agent:%dMB" rss)
                                'face (cond ((>= rss 1000) 'error)
                                            ((>= rss 500)  'warning)
                                            (t             'success)))))
         (emacs-rss-str
          (when emacs-rss
            (propertize (format "emacs:%dMB" emacs-rss)
                        'face (cond ((>= emacs-rss 1000) 'error)
                                    ((>= emacs-rss 500)  'warning)
                                    (t             'success)))))
         (tail (concat (when model-str (concat sep model-str))
                       (when mode-str  (concat sep mode-str))
                       (when context   (concat sep (string-trim-left context)))
                       (when tokens    (concat sep (string-trim-left tokens)))
                       (when mcp-bytes (concat sep (string-trim-left mcp-bytes)))
                       (when rss-str   (concat sep rss-str))
                       (when emacs-rss-str (concat sep emacs-rss-str)))))
    (cons head tail)))

(defun emagent-chat--mode-line-mcp-bytes ()
  "Return a propertized MCP payload-bytes fragment, or nil."
  (when-let* (((fboundp 'emagent-tools-age-bytes))
              (n (emagent-tools-age-bytes))
              ((and (numberp n) (> n 0))))
    (let* ((threshold (and (boundp 'emagent-tools-age-bytes-threshold)
                           emagent-tools-age-bytes-threshold))
           (str (cond
                 ((>= n 1000000) (format " mcp:%.1fM" (/ n 1000000.0)))
                 ((>= n 1000) (format " mcp:%.0fk" (/ n 1000.0)))
                 (t (format " mcp:%d" n)))))
      (propertize str
                  'face (cond
                         ((and (integerp threshold)
                               (> threshold 0)
                               (>= n threshold))
                          'error)
                         ((and (integerp threshold)
                               (> threshold 0)
                               (>= n (/ threshold 2)))
                          'warning)
                         (t 'shadow))))))

(defun emagent-chat--mode-line-token-usage ()
  "Return a propertized token/cost fragment, or nil."
  (when (bound-and-true-p emagent-usage-show-mode-line)
    (let* ((tok (emagent-chat--stat :total-tokens))
           (cost (emagent-chat--stat :cost-usd))
           (tok-str
            (when (and (numberp tok) (> tok 0))
              (cond
               ((>= tok 1000000) (format "tok:%.1fM" (/ tok 1000000.0)))
               ((>= tok 1000) (format "tok:%.1fk" (/ tok 1000.0)))
               (t (format "tok:%d" tok)))))
           (cost-str
            (when (and (numberp cost) (> cost 0))
              (format "$%.2f" cost))))
      (when (or tok-str cost-str)
        (propertize (concat " "
                            (or tok-str "")
                            (when (and tok-str cost-str) " ")
                            (or cost-str ""))
                    'face 'shadow)))))

(defun emagent-chat--mode-line-escape (text)
  "Return TEXT with `%' doubled for mode-line, preserving faces.

doom-modeline's `doom-modeline-display-text' also doubles `%', but
`string-replace' drops faces on the inserted characters — so we escape
ourselves and keep the face on both bytes of each `%%'."
  (when text
    (let ((chunks nil)
          (i 0)
          (n (length text)))
      (while (< i n)
        (let* ((ch (aref text i))
               (face (get-text-property i 'face text))
               (piece (if (eq ch ?%) "%%" (string ch))))
          (push (if face (propertize piece 'face face) piece) chunks)
          (setq i (1+ i))))
      (apply #'concat (nreverse chunks)))))

(defun emagent-chat--mode-line-display (text)
  "Escape TEXT for the mode line; dim when doom-modeline is inactive."
  (let ((escaped (emagent-chat--mode-line-escape text)))
    (cond
     ((null escaped) nil)
     ((and (bound-and-true-p doom-modeline-mode)
           (fboundp 'doom-modeline--active)
           (not (doom-modeline--active)))
      (propertize escaped
                  'face
                  `(:inherit (mode-line-inactive
                              ,(get-text-property 0 'face text)))))
     (t escaped))))

(defun emagent-chat--mode-line-context-usage ()
  "Return a propertized context fill string, or nil.
Shows a percentage when known, `ctx:~N%' for Cursor proxy estimates,
`ctx:n/a' when connected but unestimable, and nil otherwise.

Callers must run the result through `emagent-chat--mode-line-escape'
before installing it into `mode-line-format' (doom-modeline also escapes,
but drops faces on `%%')."
  (if-let* ((pair (emagent-chat--stat :ctx-usage))
            (used (car pair))
            (size (cdr pair))
            ((and (numberp used) (numberp size) (> size 0))))
      (let ((pct (min 100.0 (* 100.0 (/ (float used) size)))))
        (propertize (format " ctx:%.0f%%" pct)
                    'face (cond
                           ((>= pct 80) 'error)
                           ((>= pct 50) 'warning)
                           (t           'success))))
    (if-let ((pct (and (fboundp 'emagent-chat--context-fill-percent)
                       (emagent-chat--context-fill-percent))))
        (propertize (format " ctx:~%.0f%%" pct)
                    'face (cond
                           ((>= pct 80) 'error)
                           ((>= pct 50) 'warning)
                           (t           'shadow)))
      (when (emagent-chat--stat :ctx-unavailable)
        (propertize " ctx:n/a" 'face 'shadow)))))

(defun emagent-mode-line ()
  "Return cached emagent status text for the mode line."
  (emagent-chat--mode-line-escape
   (or emagent-chat--mode-line-cache
       (progn (emagent-chat--mode-line-recompute)
              emagent-chat--mode-line-cache))))

(defvar emagent-chat--doom-modeline-registered-p nil)

(defun emagent-chat--register-doom-modeline ()
  "Register emagent segment and modeline layout with doom-modeline."
  ;; doom-modeline-def-* are macros; eval quoted forms at runtime so
  ;; byte-compilation of emagent-chat.el does not expand them early.
  (eval
   '(progn
      (doom-modeline-def-segment emagent-ml
        "Emagent session status."
        (when (derived-mode-p 'emagent-mode)
          (when emagent-chat--mode-line-head
            (concat (doom-modeline-spc)
                    emagent-chat--mode-line-head
                    ;; Avoid doom-modeline-display-text: it doubles % via
                    ;; string-replace and drops faces on the inserted chars.
                    (emagent-chat--mode-line-display
                     emagent-chat--mode-line-tail)))))
      (unless emagent-chat--doom-modeline-registered-p
        (setq emagent-chat--doom-modeline-registered-p t)
        (unless (assoc 'emagent-mode doom-modeline-mode-alist)
          (add-to-list 'doom-modeline-mode-alist
                       (cons 'emagent-mode 'emagent-chat)))
        (unless (fboundp 'doom-modeline-format--emagent-chat)
          (doom-modeline-def-modeline 'emagent-chat
            '(matches buffer-info remote-host parrot)
            '(buffer-position selection-info minor-modes process emagent-ml vcs
                              input-method buffer-encoding battery misc-info
                              major-mode)))))
   t))

(defvar doom-modeline-mode)

(defun emagent-chat--setup-doom-modeline ()
  "Use the emagent doom-modeline layout when doom-modeline is active."
  (when (featurep 'doom-modeline)
    (emagent-chat--register-doom-modeline)
    (when (and doom-modeline-mode (fboundp 'doom-modeline-set-modeline))
      (doom-modeline-set-modeline 'emagent-chat))))

;; Apply the emagent doom-modeline layout whenever an emagent buffer is
;; activated.  `emagent-chat--setup-doom-modeline' registers the format and
;; no-ops unless doom-modeline is loaded, so no `with-eval-after-load' hook
;; on the optional dependency is needed.
(add-hook 'emagent-mode-hook #'emagent-chat--setup-doom-modeline)

(defun emagent-chat--acp-send (text &optional compress)
  "Send TEXT via `emagent-acp-send', loading connect on first use.
Optional COMPRESS is forwarded for `/compress' session reset."
  (unless (fboundp 'emagent-acp-send)
    (require 'emagent-acp))
  (emagent-acp-send text compress))

(defun emagent-chat--acp-quit ()
  "Shut down this buffer's ACP session, loading send on first use."
  (unless (fboundp 'emagent-acp-shutdown-buffer)
    (require 'emagent-acp))
  (emagent-acp-shutdown-buffer))

(defun emagent-chat--operation-active-p ()
  "Return non-nil when the buffer has work Esc-Esc should stop."
  (or emagent-chat--send-pending
      (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))))

(defun emagent-chat--abort-open-response ()
  "Delete the in-flight response scaffold opened before dispatch."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (when (emagent-chat--open-response-p)
      (when-let* ((beg (emagent-chat--open-response-begin))
                  (end (emagent-chat--response-region-end beg)))
        (save-excursion
          (goto-char beg)
          (while (and (> (point) (point-min))
                      (progn (forward-line -1)
                             (string-empty-p
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))))
            (setq beg (line-beginning-position)))
          (delete-region beg end)))))
  (emagent-chat--reset-response-state)
  (emagent-chat--sync-user-zone-marker))

(defun emagent-chat--stop-operation ()
  "Stop any in-flight emagent work in the current buffer.
Return non-nil when something was stopped."
  (when (emagent-chat--operation-active-p)
    (when (and emagent-chat--send-pending
               (not (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))))
      (setq emagent-chat--send-token nil)
      (when (fboundp 'emagent-acp--clear-when-connected-queue)
        (emagent-acp--clear-when-connected-queue))
      (emagent-chat--abort-open-response))
    (when (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
      (progn
        (unless (fboundp 'emagent-acp--finalize-in-flight-prompt)
          (require 'emagent-acp))
        (emagent-acp--finalize-in-flight-prompt
         "/Stopped — awaiting new instructions./")))
    (emagent-chat--send-pending-end)
    (when (fboundp 'emagent-chat--refresh-mode-line)
      (emagent-chat--refresh-mode-line))
    (when (fboundp 'emagent-chat--spinner-ensure-running)
      (emagent-chat--spinner-ensure-running))
    t))

(defun emagent-chat--insert-user-heading-with-text (text)
  "Insert TEXT as a complete `* username> TEXT' heading and return point after it."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (goto-char (emagent-chat--user-zone-start))
    (if (emagent-chat--user-heading-at-point-p)
        (delete-region (line-beginning-position) (line-end-position))
      (unless (bolp) (insert "\n")))
    (insert (emagent-chat--user-heading-prefix) text)
    (unless (= (char-before) ?\n) (insert "\n"))
    (point)))

(defun emagent-btw (text)
  "Send TEXT to the agent immediately as a btw side note (C-c C-e b).

When the agent is still thinking, cancels the in-flight turn, keeps any
partial response, and sends `btw, TEXT' as a new prompt."
  (interactive "sBTW: ")
  (when (string-empty-p (string-trim text))
    (user-error "BTW message is empty"))
  (let ((text (format "btw, %s" (string-trim text))))
    (when (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
      (progn
        (unless (fboundp 'emagent-acp--finalize-in-flight-prompt)
          (require 'emagent-acp))
        (emagent-acp--finalize-in-flight-prompt)))
    (emagent-log "btw send: %s" (emagent-log-truncate-line text 80))
    (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
      (emagent-chat--begin-response response-pos))
    (emagent-chat--ensure-follow-window)
    (emagent-chat--send-pending-begin)
    (emagent-chat--acp-send text)))

(defun emagent-chat--dispatch-compress ()
  "Handle a /compress-family slash command from `emagent-chat-send'.

Summarizes the prior conversation and forwards the summary to
`emagent-acp-send' with the compress flag, so ACP resets the session with
it once the turn finishes (see `emagent-acp-send-prompt').  With no prior
conversation, fails the just-opened response instead of dispatching."
  (when (fboundp 'emagent-chat--reset-compact-hint-cooldown)
    (emagent-chat--reset-compact-hint-cooldown))
  (let ((history (emagent-chat--conversation-history-text)))
    (if (string-empty-p history)
        (emagent-chat-fail-assistant "No conversation to compress")
      (emagent-chat--acp-send (emagent-chat--compress-prompt-text history) t))))

(defvar-local emagent-chat--last-auto-compact nil
  "Time of the last automatic /compress in this buffer, or nil.")

(defun emagent-chat--run-auto-compress ()
  "Insert an auto-/compress turn and dispatch compression.

Caller must ensure the session is idle and history is non-empty."
  (emagent-log "auto-compact: starting /compress")
  (setq emagent-chat--last-auto-compact (current-time))
  (let ((response-pos
         (emagent-chat--insert-user-heading-with-text "/compress (auto)")))
    (emagent-chat--send-pending-begin)
    (emagent-chat--begin-response response-pos)
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (emagent-chat--insert-preparing-scaffold))
    (emagent-chat--ensure-follow-window)
    (emagent-chat--dispatch-compress)))

(defun emagent-chat-send ()
  "Send the `* user>' prompt at point to the agent (C-c C-c).

Point must be on the prompt's heading line or in its body lines.  The
prompt is sent as-is (heading prefix stripped); nothing in the buffer
is rewritten, so text properties like the `/model' stamp survive.
Sending a previous prompt replaces its old response."
  (interactive)
  (let ((bounds (emagent-chat--send-bounds)))
    (unless bounds
      (user-error "Not on a user prompt"))
    (let* ((raw (string-trim (buffer-substring-no-properties
                              (car bounds) (cdr bounds))))
           ;; The `/model' link overrides the buffer model for this turn; it
           ;; is client UI, stripped from what the agent receives.  With no
           ;; link, keep whatever override is sticky (a prior failure may
           ;; have chosen to keep one).
           (input (emagent-chat--strip-model-links
                   (string-trim (emagent-chat--strip-user-heading raw))))
           (override (emagent-chat--region-turn-model (car bounds) (cdr bounds))))
      (when (string-empty-p input)
        (user-error "Prompt is empty"))
      ;; Client slash commands never go to the agent.
      (cond
       ((emagent-chat--mcp-command-p input)
        (emagent-chat--slash-mcp-apply input))
       ((emagent-chat--usage-command-p input)
        (emagent-chat--slash-usage-apply input))
       ((emagent-chat--notes-command-p input)
        (emagent-chat--slash-notes-apply input))
       ((and (fboundp 'emagent-archive-command-p)
             (emagent-archive-command-p input))
        (emagent-archive-apply input))
       (t
        (when override
          (setq emagent-chat--turn-model override))
        (let ((response-pos
               (save-excursion
                 (goto-char (cdr bounds))
                 (end-of-line)
                 (if (eobp)
                     (let ((inhibit-read-only t)) (insert "\n"))
                   (forward-line 1))
                 (point))))
          (emagent-chat--delete-following-response response-pos)
          (emagent-log "send: %s" (emagent-log-truncate-line input 80))
          (emagent-chat--send-pending-begin)
          (emagent-chat--begin-response response-pos)
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (if emagent-chat--turn-model
                (emagent-chat--insert-switching-scaffold)
              (emagent-chat--insert-preparing-scaffold)))
          ;; Preparing is inserted outside streaming-view; pin the window
          ;; to the live end so the first thought chunk does not clear
          ;; sticky follow when that end is briefly off-screen.
          (emagent-chat--ensure-follow-window)
          (if (and (emagent-chat--bare-slash-command-p input)
                   (emagent-chat--compress-command-p input))
              (emagent-chat--dispatch-compress)
            (emagent-chat--acp-send input))))))))

(defun emagent-chat-interrupt ()
  "Stop any in-flight emagent work (ESC ESC).

Closes a streaming response, cancels a pre-dispatch send, or clears a
connect/model-switch wait.  When idle, falls through to `keyboard-quit'."
  (interactive)
  (if (emagent-chat--stop-operation)
      (message "emagent: stopped")
    (keyboard-quit)))

(defun emagent-chat-new-prompt ()
  "Insert a '* username>' heading at point for a new prompt (C-c C-n).

Useful when the heading stub was accidentally deleted.  If point is
above the user zone, jumps to the end of the buffer first."
  (interactive)
  (let ((inhibit-read-only t)
        (zone-start (emagent-chat--user-zone-start)))
    (when (< (point) zone-start)
      (goto-char (point-max)))
    (emagent-chat--writable)
    (unless (bolp) (insert "\n"))
    (insert (emagent-chat--user-heading-prefix))))

(defun emagent-chat-quit ()
  "Disconnect this buffer's ACP agent and bury the window."
  (interactive)
  (emagent-chat--acp-quit)
  (bury-buffer))

(eval-when-compile (require 'flymake))

(defun emagent-chat--acp-attach (text)
  "Attach TEXT via `emagent-acp-attach-context', loading send on first use."
  (unless (fboundp 'emagent-acp-attach-context)
    (require 'emagent-acp))
  (emagent-acp-attach-context text))

(defun emagent-chat-attach-buffer ()
  "Attach a buffer summary to the next prompt."
  (interactive)
  (let ((text (emagent-context-buffer-summary)))
    (emagent-log "attached buffer summary to next prompt")
    (emagent-chat--acp-attach text)))

(defun emagent-chat-yank (&optional arg)
  "Yank text or paste a clipboard image.

If the clipboard contains an image, saves it to a temp file under
`emagent-chat--image-dir' and inserts a [[file:...]] org link at point.
Otherwise behaves exactly like `yank' (ARG is forwarded)."
  (interactive "*P")
  (let ((clip (emagent-chat--clipboard-image)))
    (if clip
        (let ((file (emagent-chat--save-clipboard-image (car clip) (cdr clip))))
          (insert (format "[[file:%s]]" file))
          (message "emagent: clipboard image → %s" (file-name-nondirectory file)))
      (yank arg))))

(defvar emagent-chat--image-dir
  (expand-file-name "emagent/images" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory where clipboard images pasted into emagent buffers are saved.")

(defun emagent-chat--ensure-image-dir ()
  "Ensure `emagent-chat--image-dir' exists and return its path."
  (unless (file-directory-p emagent-chat--image-dir)
    (make-directory emagent-chat--image-dir t))
  emagent-chat--image-dir)

(defun emagent-chat--clipboard-image ()
  "Return (MIME-TYPE-STRING . RAW-BYTES) for a clipboard image, or nil.

Tries PNG, JPEG, GIF, WebP in order and returns the first available type."
  (when (fboundp 'gui-get-selection)
    (let ((targets (ignore-errors (gui-get-selection 'CLIPBOARD 'TARGETS))))
      (when targets
        (let ((target-list (cond ((vectorp targets) (append targets nil))
                                 ((listp targets)   targets)
                                 (t                 nil))))
          (cl-some
           (lambda (mime)
             (when (memq (intern mime) target-list)
               (let ((data (ignore-errors (gui-get-selection 'CLIPBOARD (intern mime)))))
                 (when (and data (not (equal data "")))
                   (cons mime data)))))
           '("image/png" "image/jpeg" "image/gif" "image/webp")))))))

(defun emagent-chat--save-clipboard-image (mime data)
  "Write clipboard image DATA (raw bytes) of MIME type to a temp file.
Returns the file path."
  (let* ((ext (pcase mime
                ("image/jpeg" "jpg")
                ("image/gif"  "gif")
                ("image/webp" "webp")
                (_            "png")))
         (file (expand-file-name
                (format "img-%s.%s" (format-time-string "%Y%m%d-%H%M%S") ext)
                (emagent-chat--ensure-image-dir))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (write-region (point-min) (point-max) file nil 'silent))
    file))

(defun emagent-chat-attach-image ()
  "Insert an image link at point for the next prompt (C-c C-e i).

If the clipboard contains an image, saves it to a temp file under
`emagent-chat--image-dir' and inserts a [[file:...]] org link at point.
Otherwise opens a file picker.

On send, emagent finds all [[file:...]] image links in the heading,
base64-encodes them, and sends them as multimodal content blocks alongside
the prompt text."
  (interactive)
  (let ((clip (emagent-chat--clipboard-image)))
    (if clip
        (let ((file (emagent-chat--save-clipboard-image (car clip) (cdr clip))))
          (insert (format "[[file:%s]]" file))
          (message "emagent: clipboard image → %s (C-c C-c to send)"
                   (file-name-nondirectory file)))
      (let ((file (expand-file-name
                   (read-file-name "Attach image: " nil nil t))))
        (insert (format "[[file:%s]]" file))
        (message "emagent: %s attached (C-c C-c to send)"
                 (file-name-nondirectory file))))))

(defun emagent-chat--compilation-error-lines ()
  "Return error lines from *compilation* buffer using text properties, or nil."
  (when-let ((buf (get-buffer "*compilation*")))
    (with-current-buffer buf
      (let (lines)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (get-text-property (point) 'compilation-message)
              (let ((text (string-trim
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))
                (unless (string-empty-p text)
                  (push text lines))))
            (forward-line 1)))
        (nreverse lines)))))

(defun emagent-chat--flymake-error-lines ()
  "Return flymake diagnostic lines from all open file-visiting buffers."
  (when (and (require 'flymake nil t) (fboundp 'flymake-diagnostics))
    (let (lines)
      (dolist (buf (buffer-list))
        (when (and (buffer-file-name buf)
                   (buffer-local-value 'flymake-mode buf))
          (let ((diags (with-current-buffer buf (flymake-diagnostics))))
            (dolist (d diags)
              (push (format "%s:%s [%s] %s"
                            (abbreviate-file-name (buffer-file-name buf))
                            (with-current-buffer buf
                              (line-number-at-pos
                               (flymake-diagnostic-beg d)))
                            (flymake-diagnostic-type d)
                            (flymake-diagnostic-text d))
                    lines)))))
      (nreverse lines))))

(defun emagent-chat-attach-error-context ()
  "Attach compilation errors and flymake diagnostics to the next prompt.

Scans `*compilation*' for error lines and all open file buffers for
active flymake diagnostics.  Attaches a combined error context block."
  (interactive)
  (let* ((compile-lines (emagent-chat--compilation-error-lines))
         (flymake-lines (emagent-chat--flymake-error-lines))
         (all (append compile-lines flymake-lines)))
    (if all
        (let ((text (concat "[Error context]\n" (string-join all "\n"))))
          (emagent-log "attached %d error(s) to next prompt" (length all))
          (emagent-chat--acp-attach text))
      (message "emagent: no errors found in compilation buffer or flymake"))))

(defun emagent-chat-attach-files ()
  "Pick project files and attach summaries to the next prompt.

Presents `completing-read-multiple' over files under
`emagent-session-project-directory'.  For each chosen file includes its
relative path, size in lines, and a short content preview."
  (interactive)
  (let* ((root (or (emagent-session-project-directory)
                   default-directory))
         (all-files (directory-files-recursively root "[^.].*" nil t))
         (rel-files (seq-filter
                     (lambda (f)
                       (not (string-match-p "/\\.git/" f)))
                     (mapcar (lambda (f) (file-relative-name f root))
                             all-files)))
         (chosen (completing-read-multiple
                  "Attach files (comma-separated): " rel-files nil t))
         (blocks
          (mapcar (lambda (rel)
                    (let* ((abs (expand-file-name rel root))
                           (size (and (file-exists-p abs)
                                      (with-temp-buffer
                                        (insert-file-contents abs nil 0 4096)
                                        (count-lines (point-min) (point-max))))))
                      (format "[File: %s (%s lines)]\n%s"
                              rel (or size "?")
                              (condition-case nil
                                  (with-temp-buffer
                                    (insert-file-contents abs nil 0 2048)
                                    (buffer-string))
                                (error "(unreadable)")))))
                  chosen)))
    (if blocks
        (let ((text (string-join blocks "\n\n")))
          (emagent-log "attached %d file(s) to next prompt" (length blocks))
          (emagent-chat--acp-attach text))
      (message "emagent: no files selected"))))

(defgroup emagent-chat nil
  "Emagent chat UI."
  :group 'emagent)

(defun emagent-chat-cycle-or-org-cycle ()
  "Cycle visibility with `org-cycle' (responses fold as native Org subtrees)."
  (interactive)
  (org-cycle))

(defconst emagent-chat--client-slash-commands
  '(((name . "model")
     (description . "switch model for this turn (marker stripped before send)"))
    ((name . "mcp")
     (description . "list/authenticate MCP servers (Claude or Cursor CLI)"))
    ((name . "usage")
     (description . "token usage + session tax; `/usage baseline'|`clear' for deltas"))
    ((name . "notes")
     (description . "show/set session notes; `/notes clear' empties them"))
    ((name . "archive")
     (description . "move older turns into NAME-archive/NNN.org; `/archive force'")))
  "Slash commands emagent handles itself; never sent to the agent.")

(defun emagent-chat--client-slash-command (name)
  "Return the client slash-command plist named NAME, or nil."
  (seq-find (lambda (c) (equal (map-elt c 'name) name))
            emagent-chat--client-slash-commands))

(defun emagent-chat--slash-model-apply-1 (bounds)
  "Prompt for a model and replace BOUNDS with the `/model' marker link."
  (let* ((state emagent-acp--session)
         (choices (and state (emagent-acp--model-choices state nil))))
    (cond
     ((not choices)
      (message "emagent: no models available yet — M-x emagent-connect"))
     (t
      (let* ((selection (completing-read "Model for this turn: "
                                         (mapcar #'car choices) nil t))
             (model-id (cdr (assoc-string selection choices))))
        (when model-id
          (delete-region (car bounds) (cdr bounds))
          (goto-char (car bounds))
          ;; An org link: agent/short-model as text, full id as target
          ;; (shown on hover).  Org fontifies it, it survives saving the
          ;; session file, deleting it cancels the override, and send strips
          ;; it from the outgoing prompt.
          (insert (emagent-chat--model-link model-id))))))))

(defun emagent-chat--slash-model-apply ()
  "Prompt for a model and replace the `/model' token with its marker link.
The `[[emagent://AGENT/MODEL][short]]' link makes the send path switch to
MODEL for this turn (restoring the buffer model afterward); the link is
stripped from the text sent to the agent."
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Turn on emagent-mode in this buffer first"))
  (let ((bounds (emagent-chat--slash-token-bounds))
        (buf (current-buffer)))
    (unless bounds
      (user-error "No `/model' token at point"))
    (if (emagent-acp--connected-p)
        (emagent-chat--slash-model-apply-1 bounds)
      (progn
        (unless (fboundp 'emagent-acp-ensure-connected)
          (require 'emagent-acp))
        (emagent-acp-ensure-connected
         :on-ready
         (lambda ()
           (with-current-buffer buf
             (emagent-chat--slash-model-apply-1
              (or (emagent-chat--slash-token-bounds) bounds)))))))))

(defun emagent-chat--run-client-slash-command (name)
  "Run the client slash command NAME (dispatch after completion)."
  (pcase name
    ("model" (emagent-chat--slash-model-apply))
    ("mcp" (emagent-chat--slash-mcp-apply))
    ("usage" (emagent-chat--slash-usage-apply))
    ("notes" (emagent-chat--slash-notes-apply))
    ("archive" (emagent-archive-apply))))

(defvar emagent-chat-provider)

(defvar-local emagent-chat-slash-commands nil
  "Slash commands for this buffer as plists (:name :description :hint).

Populated from the ACP agent via =available_commands_update= after the session
connects.  Cursor sessions also seed documented built-ins and custom commands
from ~/.cursor/commands and .cursor/commands/.")

(defcustom emagent-chat-notify-slash-commands t
  "When non-nil, show a message after the agent publishes slash commands."
  :type 'boolean
  :group 'emagent-chat)

(defun emagent-chat--slash-command-name (name)
  "Return slash command NAME without a leading \"/\"."
  (if (and (stringp name) (not (string-empty-p name)) (string-prefix-p "/" name))
      (substring name 1)
    name))

(defun emagent-chat--slash-command-plist (name description &optional hint)
  "Return a slash-command plist for NAME.

Arguments: DESCRIPTION, HINT."
  `((name . ,(emagent-chat--slash-command-name name))
    (description . ,(or description ""))
    (hint . ,(or hint ""))))

(defun emagent-chat--normalize-slash-commands (commands)
  "Normalize COMMANDS from ACP JSON into slash-command plists."
  (let ((items (cond
                ((vectorp commands) (append commands nil))
                ((listp commands) commands)
                (t nil))))
    (mapcar (lambda (cmd)
              (emagent-chat--slash-command-plist
               (map-elt cmd 'name)
               (map-elt cmd 'description)
               (map-nested-elt cmd '(input hint))))
            items)))

(defun emagent-chat--merge-slash-commands (base extra)
  "Merge EXTRA into BASE by command name; EXTRA overrides BASE."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (cmd base)
      (puthash (map-elt cmd 'name) cmd table))
    (dolist (cmd extra)
      (puthash (map-elt cmd 'name) cmd table))
    (let (result)
      (maphash (lambda (_ cmd) (push cmd result)) table)
      (sort result (lambda (a b) (string< (map-elt a 'name) (map-elt b 'name)))))))

(defun emagent-chat-clear-slash-commands ()
  "Clear slash commands until the agent publishes a fresh list."
  (setq emagent-chat-slash-commands nil))

(defun emagent-chat-seed-cursor-slash-commands ()
  "Merge Cursor built-in and custom slash commands into the buffer list."
  (when (and (eq emagent-chat-provider 'cursor)
             (fboundp 'emagent-cursor-slash-commands))
    (setq emagent-chat-slash-commands
          (emagent-chat--merge-slash-commands
           (emagent-cursor-slash-commands (emagent-session-project-directory))
           emagent-chat-slash-commands))))

(defun emagent-chat-set-slash-commands (commands)
  "Merge normalized COMMANDS from the agent into `emagent-chat-slash-commands'."
  (let ((incoming (emagent-chat--normalize-slash-commands commands)))
    (setq emagent-chat-slash-commands
          (if (and (eq emagent-chat-provider 'cursor)
                   (fboundp 'emagent-cursor-slash-commands))
              (emagent-chat--merge-slash-commands
               (emagent-cursor-slash-commands (emagent-session-project-directory))
               incoming)
            incoming))
    (when (and emagent-chat-notify-slash-commands
               emagent-chat-slash-commands)
      (emagent-log "%d slash commands from agent"
                   (length emagent-chat-slash-commands)))))

(defun emagent-chat--command-needle-base (needle)
  "Return NEEDLE with a trailing colon removed, for skill-name matching."
  (if (and (not (string-empty-p needle)) (string-suffix-p ":" needle))
      (substring needle 0 -1)
    needle))

(defun emagent-chat--command-skill-part (name)
  "Return the skill segment of slash command NAME (after the first colon)."
  (if (string-match ":" name)
      (substring name (match-end 0))
    name))

(defun emagent-chat--command-matches-needle-p (name needle)
  "Return non-nil when NEEDLE appears as a substring of NAME or its skill part."
  (or (string-empty-p needle)
      (string-match-p (regexp-quote needle) name)
      (string-match-p (regexp-quote needle)
                      (emagent-chat--command-skill-part name))))

(defun emagent-chat--slash-token-bounds ()
  "Return (START . END) for the slash command token at point, or nil.

Detects a `/name' token that point is within, anywhere on the line — at the
start (after the user heading) or mid-prompt (e.g. `commit, use /model') — so
long as the `/' is preceded by the heading, the line start, or whitespace."
  (let ((zone (emagent-chat--user-zone-start))
        (user-point (point)))
    (when (>= user-point zone)
      (save-excursion
        (let ((floor (line-beginning-position)))
          ;; Do not scan back into the user heading prefix.
          (goto-char floor)
          (when (looking-at (emagent-chat--user-heading-re))
            (setq floor (match-end 0)))
          ;; Walk back over the non-whitespace token containing point.
          (goto-char user-point)
          (skip-chars-backward "^ \t\n" floor)
          (when (looking-at "/")
            (let ((start (point))
                  (end (progn (goto-char user-point)
                              (skip-chars-forward "^ \t\n" (line-end-position))
                              (point))))
              (when (<= start user-point end)
                (cons start end)))))))))

(defun emagent-chat-slash-command-completion-at-point ()
  "Complete agent slash commands at point."
  (when-let* ((bounds (emagent-chat--slash-token-bounds))
              (slash-start (car bounds))
              (end (cdr bounds))
              (prefix (buffer-substring-no-properties slash-start end))
              ((string-prefix-p "/" prefix))
              ;; Client commands (e.g. /model) are always offered; agent
              ;; commands are merged in once the session publishes them.
              ;; Cursor built-ins are seeded on mode enable / `emagent-connect'
              ;; without waiting for a dummy prompt.
              (commands (append emagent-chat--client-slash-commands
                                emagent-chat-slash-commands)))
    ;; Start the completion region AFTER the "/" so the framework sees the
    ;; bare name (e.g. "relax", "session:relax") as its input.  This lets
    ;; any completion style (basic, orderless, flex) filter naturally without
    ;; the leading "/" confusing prefix or substring matching.
    (list (1+ slash-start) end
          (mapcar (lambda (cmd) (map-elt cmd 'name)) commands)
          :annotation-function
          (lambda (candidate)
            (concat "  " (or (map-elt (cl-find candidate commands
                                               :key (lambda (c) (map-elt c 'name))
                                               :test #'string=)
                                      'description)
                             "")))
          :exit-function
          (lambda (str status)
            ;; A client command runs its handler instead of staying as text.
            (when (and (memq status '(finished sole exact))
                       (emagent-chat--client-slash-command str))
              (emagent-chat--run-client-slash-command str)))
          :exclusive t)))

(defun emagent-chat-tab ()
  "On a slash-command token, complete or run it; otherwise run `org-cycle'."
  (interactive)
  (let* ((bounds (emagent-chat--slash-token-bounds))
         (name (and bounds
                    (buffer-substring-no-properties (1+ (car bounds)) (cdr bounds)))))
    (cond
     ;; A complete client command (e.g. fully-typed /model) runs directly, since
     ;; `completion-at-point' would treat an exact sole match as nothing to do
     ;; and never fire the exit-function.
     ((and name (emagent-chat--client-slash-command name))
      (emagent-chat--run-client-slash-command name))
     ;; Otherwise complete: client commands (/model) are always offered, agent
     ;; commands merge in once the session publishes them.
     (bounds
      (call-interactively #'completion-at-point))
     (t
      (call-interactively #'emagent-chat-cycle-or-org-cycle)))))

(defvar emagent-chat-provider)

(defvar emagent-chat--response-headline-re)

(defun emagent-chat--imenu-create-index ()
  "Return an imenu index of user messages for an emagent buffer."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(.*\\)$" nil t)
        (let ((heading (match-string 1)))
          (unless (string-match-p
                   "\\`emagent>\\|\\`\\(?:[/#]\\)\\|\\`\\(?:Thinking\\|Response\\|Request permissions\\)\\'"
                   heading)
            (push (cons heading (match-beginning 0)) index))))
      (nreverse index))))

(defun emagent-chat--bookmark-make-record ()
  "Return a bookmark record for the current emagent buffer."
  (let* ((session-id (emagent-session-id))
         (project-dir (emagent-session-project-directory))
         (model (emagent-session-model))
         (provider (when emagent-chat-provider (symbol-name emagent-chat-provider))))
    `(,(buffer-name)
      (handler . emagent-chat--bookmark-jump)
      (session-id . ,session-id)
      (project-dir . ,project-dir)
      (model . ,model)
      (provider . ,provider)
      (position . ,(point)))))

(defun emagent-chat--bookmark-jump (bookmark)
  "Jump to an emagent BOOKMARK, reopening or reconnecting the session."
  (let* ((session-id (bookmark-prop-get bookmark 'session-id))
         (project-dir (bookmark-prop-get bookmark 'project-dir))
         (model (bookmark-prop-get bookmark 'model))
         (provider (when-let ((p (bookmark-prop-get bookmark 'provider)))
                     (intern p)))
         (pos (bookmark-prop-get bookmark 'position))
         (buffer (when project-dir
                   (emagent-chat-open :project-dir project-dir))))
    (when buffer
      (with-current-buffer buffer
        (when model (emagent-chat-set-model model))
        (when provider (emagent-session-set-agent provider))
        (when session-id (emagent-session-set-id session-id)))
      (pop-to-buffer buffer)
      (when pos (goto-char pos)))))

(defun emagent-chat--last-response-bounds ()
  "Return (BEG . END) for the last completed `** Response' body, or nil."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward emagent-chat--response-headline-re nil t)
      (forward-line 1)
      (skip-chars-forward "\n")
      (let ((beg (point))
            (end (if (re-search-forward "^\\* " nil t)
                     (line-beginning-position)
                   (point-max))))
        (cons beg end)))))

(defun emagent-chat--collect-src-blocks (beg end)
  "Return list of (LANG . CODE) for each src block between BEG and END."
  (let (blocks)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^#\\+BEGIN_SRC \\(.*\\)\n" end t)
        (let* ((lang (string-trim (match-string 1)))
               (start (point))
               (block-end (and (re-search-forward "^#\\+END_SRC\\s-*$" end t)
                               (match-beginning 0))))
          (when block-end
            (push (cons lang (buffer-substring-no-properties start block-end))
                  blocks)))))
    (nreverse blocks)))

(defun emagent-chat-insert-last-response ()
  "Insert the last completed agent response into another buffer.

Prompts for a target buffer with `completing-read'."
  (interactive)
  (if-let* ((bounds (emagent-chat--last-response-bounds))
            (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (let* ((others (seq-filter (lambda (b) (not (eq b (current-buffer))))
                                 (buffer-list)))
             (choice (completing-read "Insert response into buffer: "
                                      (mapcar #'buffer-name others) nil t))
             (target (get-buffer choice)))
        (with-current-buffer target
          (insert text))
        (message "emagent: inserted response into %s" choice))
    (message "emagent: no completed response found")))

(defun emagent-chat-insert-src-block ()
  "Pick a src block from the last response and insert it into another buffer."
  (interactive)
  (if-let* ((bounds (emagent-chat--last-response-bounds))
            (blocks (emagent-chat--collect-src-blocks (car bounds) (cdr bounds))))
      (let* ((choices
              (cl-loop for (lang . code) in blocks
                       for i from 1
                       collect
                       (cons (format "%d [%s] %s" i lang
                                     (truncate-string-to-width
                                      (car (split-string code "\n")) 60 nil nil "…"))
                             code)))
             (pick (completing-read "Insert src block: "
                                    (mapcar #'car choices) nil t))
             (code (cdr (assoc pick choices)))
             (others (seq-filter (lambda (b) (not (eq b (current-buffer))))
                                 (buffer-list)))
             (target (get-buffer
                      (completing-read "Into buffer: "
                                       (mapcar #'buffer-name others) nil t))))
        (with-current-buffer target
          (insert code))
        (message "emagent: inserted src block into %s" (buffer-name target)))
    (message "emagent: no src blocks found in last response")))

(defvar emagent-default-provider)

(defun emagent-chat--set-provider ()
  "Set the buffer's provider from the saved session or the default.

The ACP send/attach/quit callbacks are wired separately by
`emagent-acp-connect' (which requires `emagent-chat'; this file cannot
require it back) via its own `emagent-mode-hook' function."
  (setq emagent-chat-provider (or (emagent-session-agent) emagent-default-provider)))

(defun emagent-chat--on-mode-enable ()
  "Wire callbacks when enabling `emagent-mode'.

Do not auto-connect on mode activation; delayed ACP reconnect callbacks can
rewrite session metadata and mark restored buffers modified.  Connection still
happens on first send via `emagent-acp-send', or explicitly via
`emagent-connect'.

Cursor built-in slash commands are seeded locally so TAB works before the
first prompt without spawning the agent.  Claude agent slash commands require
`emagent-connect' (or any send) so the agent can publish them."
  (emagent-chat--set-provider)
  (emagent-chat--setup-faces)
  (emagent-chat-seed-cursor-slash-commands))

(add-hook 'emagent-mode-hook #'emagent-chat--on-mode-enable)

(defun emagent-chat--ensure-org-startup ()
  "Ensure the buffer requests Org block folding on startup."
  (unless (save-excursion
            (goto-char (point-min))
            ;; Accept both modern #+STARTUP: and legacy STARTUP: (without #+).
            (re-search-forward "^\\(?:#\\+\\)?STARTUP:.*\\bhideblocks\\b" nil t))
    (emagent-session-store-write-top-property "STARTUP" "hideblocks")))

(defun emagent-chat--disable-incompatible-org-minor-modes ()
  "Turn off org minor modes that break on emagent chat buffer content."
  (setq-local org-element-use-cache nil)
  ;; Toggling org-appear off runs org-element parsing on the element at point.
  ;; During desktop restore point can sit mid-buffer with the org cache in an
  ;; inconsistent state, which signals \"Invalid search bound (wrong side of
  ;; point)\".  Disabling org-appear must never abort emagent-mode setup.
  (when (and (fboundp 'org-appear-mode) (bound-and-true-p org-appear-mode))
    (condition-case nil
        (org-appear-mode -1)
      (error nil))))

(defun emagent-chat--setup-buffer-display ()
  "Configure prose wrapping and table scrolling for emagent buffers.

Prose uses `visual-line-mode'.  Wide org tables scroll horizontally via
`org-phscroll-mode', which applies only inside table regions — not buffer-wide.
`truncate-lines' must stay nil; phscroll does not work when it is t.

Runs late on `org-mode-hook' so it overrides user hooks (e.g. org-modern
`kwarks/org--table-buffer-setup') that disable wrapping globally."
  (when (derived-mode-p 'emagent-mode)
    (setq-local org-startup-truncated nil
                truncate-lines nil)
    (when (boundp 'word-wrap)
      (setq-local word-wrap t))
    (visual-line-mode 1)
    (when (fboundp 'org-phscroll-mode)
      (org-phscroll-mode 1))))

(defvar-local emagent-chat--safe-src-fontify-p nil
  "Non-nil when this buffer uses buffer-local safe src fontification.")

(defvar-local emagent-chat--safe-fontify-installed nil
  "Non-nil when `emagent-chat--safe-fontify-region' is installed locally.")

(defconst emagent-chat--org-src-fontify-fn
  (symbol-function 'org-src-font-lock-fontify-block)
  "Original `org-src-font-lock-fontify-block' function cell.")

(defun emagent-chat--setup-faces ()
  "Configure org highlighting, line wrap, and block folding for emagent buffers."
  (emagent-chat--disable-incompatible-org-minor-modes)
  (emagent-chat--enable-safe-src-fontify)
  (setq-local org-src-fontify-natively t
              org-ellipsis "…"
              org-fontify-quote-and-verse-blocks t
              org-cycle-hide-block-startup t
              ;; Render `[[link][text]]' as just TEXT regardless of the
              ;; user's global setting — the `/model' marker and any links
              ;; in agent output should read as links, not raw markup.
              org-link-descriptive t)
  (when font-lock-mode
    (font-lock-flush))
  (emagent-chat--setup-buffer-display))

(defun emagent-chat--setup-faces-deferred ()
  "Re-apply `emagent-chat--setup-faces' after org startup hooks finish."
  (when (derived-mode-p 'emagent-mode)
    (emagent-chat--setup-faces)))

(defun emagent-chat--fragile-shell-src-p (lang start end)
  "Return non-nil when LANG src between START and END would break `sh-mode'.

`sh-mode' signals `end-of-buffer' while font-locking a command substitution
that wraps a heredoc (`$(cat <<'EOF'...)').  Detect that cheaply and skip
native fontification instead of paying for the failing pass.

Arguments: LANG, START, END."
  (and (member (downcase (or lang "")) '("sh" "bash" "shell" "zsh"))
       (< start end)
       (save-excursion
         (save-restriction
           (narrow-to-region start end)
           (goto-char start)
           (and (search-forward "$(" end t)
                (search-forward "<<" end t))))))

(defun emagent-chat--plain-src-block-face (start end)
  "Mark START..END as a plain `org-block' without native lang fontify."
  (add-text-properties
   start end
   '(face org-block src-block t
     font-lock-fontified t fontified t font-lock-multiline t)))

(defun emagent-chat--safe-src-fontify-block (lang start end)
  "Safe `org-src-font-lock-fontify-block' for emagent session buffers.

Skip native fontification for fragile shell heredoc patterns and catch
other lang font-lock errors so large session buffers stay quiet.

Arguments: LANG, START, END."
  (if (emagent-chat--fragile-shell-src-p lang start end)
      (emagent-chat--plain-src-block-face start end)
    (condition-case nil
        (funcall emagent-chat--org-src-fontify-fn lang start end)
      (error
       (emagent-chat--plain-src-block-face start end)
       nil))))

(defun emagent-chat--safe-fontify-region (orig beg end &optional verbose)
  "Around buffer-local `font-lock-fontify-region-function' for sessions.

Rebinds `org-src-font-lock-fontify-block' only while fontifying this
buffer — no global `advice-add' on Org.

Arguments: ORIG, BEG, END, VERBOSE."
  (if (not emagent-chat--safe-src-fontify-p)
      (funcall orig beg end verbose)
    (cl-letf (((symbol-function 'org-src-font-lock-fontify-block)
               #'emagent-chat--safe-src-fontify-block))
      (funcall orig beg end verbose))))

(defun emagent-chat--enable-safe-src-fontify ()
  "Install buffer-local safe src fontification for the current buffer."
  (setq-local emagent-chat--safe-src-fontify-p t)
  (unless emagent-chat--safe-fontify-installed
    (add-function :around (local 'font-lock-fontify-region-function)
                  #'emagent-chat--safe-fontify-region)
    (setq-local emagent-chat--safe-fontify-installed t)))

(add-hook 'org-mode-hook #'emagent-chat--setup-buffer-display 110 t)

(add-hook 'emagent-mode-hook #'emagent-chat--setup-faces 100 t)

(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'emagent-mode)
      (emagent-chat--setup-faces))))

(defun emagent--run-derived-mode ()
  "Run derived `emagent-mode' with Org startup inline images disabled.

Only bind `org-startup-with-inline-images' here.  Do not let-bind
`org-element-use-cache': the mode body and
`emagent-chat--disable-incompatible-org-minor-modes' set it buffer-local,
and let-binding it makes `setq-local' fail on Emacs 29.

Calls `emagent--derived-mode' (captured in `emagent-chat-mode').  When that
alias is not bound yet (load-order edge), retry activation on idle."
  (let ((org-startup-with-inline-images nil))
    (if (fboundp 'emagent--derived-mode)
        (emagent--derived-mode)
      (run-with-idle-timer 0 nil #'emagent--activate-session-now))))

(defun emagent--session-buffer-p ()
  "Return non-nil when current `org-mode' buffer is an emagent session."
  (and (derived-mode-p 'org-mode)
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (let ((limit (min (+ (point-min) 4096) (point-max))))
             (or (looking-at-p "#[ \t]*-\\*-.*\\bmode:[ \t]*emagent\\b.*-\\*-")
                 (re-search-forward "^#\\+EMAGENT_SESSION:[ \t]*\\S-" limit t)))))))

(defun emagent-chat--ensure-mode-cookie ()
  "Insert `# -*- mode: emagent -*-' at point-min when missing.

Session files always carry the cookie so `set-auto-mode' routes through
`emagent-mode' (and its safe Org init bindings) instead of bare
`org-mode'.  Accept any `mode: emagent' file-local cookie line (extra
props allowed).  Housekeeping inserts must not mark a visited file
modified."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unless (looking-at-p
               "#[ \t]*-\\*-.*\\bmode:[ \t]*emagent\\b.*-\\*-")
        (let ((inhibit-read-only t)
              (was-modified (buffer-modified-p)))
          (insert "# -*- mode: emagent -*-\n")
          (set-buffer-modified-p was-modified))))))

(defcustom emagent-activate-on-display t
  "When non-nil, defer emagent session activation until the buffer is shown.

Restored session org files (opened via `find-file', desktop, or the
`# -*- mode: emagent -*-' file cookie) stay in plain `org-mode' until the user
first switches to them.  Activation — and therefore any agent connection — only
happens on first display, so restoring many saved sessions at startup spawns
nothing until each one is actually visited.

When nil, session files activate `emagent-mode' immediately on open."
  :type 'boolean
  :group 'emagent)

(defvar emagent--pending-buffers nil
  "Session buffers awaiting first-display activation of `emagent-mode'.")

(defvar-local emagent--session-pending nil
  "Non-nil when this buffer is a session deferred until first display.")

(defun emagent--mark-session-pending ()
  "Leave the current buffer in `org-mode' and queue activation on first display."
  (unless (derived-mode-p 'org-mode)
    (let ((org-startup-with-inline-images nil))
      (org-mode)))
  ;; Deferred sessions stay in plain `org-mode' until first display.  Apply the
  ;; same org-appear / org-element safeguards as `emagent-mode' so desktop
  ;; restore and find-file do not trip \"Invalid search bound\" on large logs.
  (emagent-chat--disable-incompatible-org-minor-modes)
  (emagent-chat--ensure-mode-cookie)
  (emagent-chat--enable-safe-src-fontify)
  (setq-local emagent--session-pending t)
  (cl-pushnew (current-buffer) emagent--pending-buffers)
  (emagent-log "session deferred until displayed: %s" (buffer-name)))

(defun emagent--activate-session-now ()
  "Activate `emagent-mode' in the current buffer immediately."
  (setq emagent--pending-buffers (delq (current-buffer) emagent--pending-buffers))
  (kill-local-variable 'emagent--session-pending)
  (unless (derived-mode-p 'emagent-mode)
    (condition-case err
        (emagent-mode-force)
      (error
       (emagent-log "could not enable emagent-mode in %s: %s"
                    (or (buffer-name) "<dead-buffer>")
                    (error-message-string err))))))

(defun emagent-mode--defer-p (&optional force)
  "Return non-nil when `emagent-mode' should defer activation until display.

Defers for undisplayed file-visiting buffers when
`emagent-activate-on-display' is on and FORCE is nil.  Does not defer
when already in `emagent-mode' so toggle-off works."
  (and emagent-activate-on-display
       (not force)
       (not (eq major-mode 'emagent-mode))
       buffer-file-name
       (not (emagent-chat--buffer-displayed-p))))

(defun emagent-mode-entry (&optional arg)
  "Public entry for `emagent-mode' with display deferral.

ARG is accepted for major-mode compatibility.  Pass `force' (or non-nil
interactively via `emagent-mode-force') to bypass display deferral.
See `emagent-mode' for the user-facing docstring."
  (interactive "P")
  (let ((force (memq arg '(force t))))
    (if (emagent-mode--defer-p force)
        (emagent--mark-session-pending)
      (emagent--run-derived-mode))))

(defun emagent-mode-force ()
  "Activate `emagent-mode', bypassing display deferral.

Use for explicit opens (`emagent-chat-open') and first-display
activation of deferred session buffers."
  (interactive)
  (emagent-mode-entry 'force))

(defun emagent--maybe-register-session ()
  "On opening a session file, mark it pending or activate it now.

Used for session files without the `mode: emagent' cookie (only the
`#+EMAGENT_SESSION' property), which `set-auto-mode' opens in `org-mode'."
  (when (and (not (derived-mode-p 'emagent-mode))
             (not emagent--session-pending)
             (emagent--session-buffer-p))
    (if (and emagent-activate-on-display
             (not (emagent-chat--buffer-displayed-p)))
        (emagent--mark-session-pending)
      (emagent--activate-session-now))))

(defun emagent--activate-displayed-pending (&rest _)
  "Activate any pending session buffers that have become displayed."
  (dolist (buf (copy-sequence emagent--pending-buffers))
    (if (buffer-live-p buf)
        (when (emagent-chat--buffer-displayed-p buf)
          (with-current-buffer buf
            (emagent--activate-session-now)))
      (setq emagent--pending-buffers (delq buf emagent--pending-buffers)))))

(add-hook 'find-file-hook #'emagent--maybe-register-session)

(add-hook 'window-buffer-change-functions #'emagent--activate-displayed-pending)

(defvar emagent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'emagent-chat-send-or-babel)
    (define-key map (kbd "ESC ESC") #'emagent-chat-interrupt)
    (define-key map (kbd "C-c ?") #'emagent-dispatch)
    (define-key map (kbd "TAB") #'emagent-chat-tab)
    (define-key map (kbd "C-a") #'emagent-chat-beginning-of-line)
    (define-key map (kbd "C-y") #'emagent-chat-yank)
    (define-key map (kbd "C-p") #'emagent-chat-history-previous-or-previous-line)
    (define-key map (kbd "C-n") #'emagent-chat-history-next-or-next-line)
    map)
  "Keymap for `emagent-mode'.")

(with-suppressed-warnings ((callargs emagent-mode) (redefine emagent-mode))
  (define-derived-mode emagent-mode org-mode "Emagent"
    "Major mode for emagent chat scratch buffers.

Derived from `org-mode'.  Type after the `* user>' stub, then
\\[emagent-chat-send] to send the prompt at point (its heading line
plus any body lines).
On a slash-command line (plugin skills such as /workflow:dev), \\[emagent-chat-tab]
completes available commands.  Agent responses are inserted between
`** Thinking' / `** Response' subsections (TAB folds them as Org headlines).

Run \\[emagent-mode] to reconnect a saved session."
    (require 'emagent)
    (setq-local buffer-read-only nil)
    (setq-local emagent-chat--tool-call-lines (make-hash-table :test 'equal))
    ;; Chat is a live transcript log; undo would retain a second copy of every
    ;; streamed insert and tool-line rewrite for the whole session.
    (buffer-disable-undo)
    (emagent-chat--writable)
    (when-let* ((raw (or emagent-chat-project-directory
                         (emagent-session-store-read-project-property)))
                (dir (expand-file-name raw)))
      (setq emagent-chat-project-directory dir)
      ;; Normalize the header form without dirtying a just-opened file.
      ;; Trailing-slash / abbreviate differences used to mark every visit
      ;; modified even when the project path was unchanged.
      (let* ((display (emagent-session-store-display-project-directory dir))
             (current (emagent-session-store-read-project-property))
             (current-norm
              (and current
                   (emagent-session-store-display-project-directory current)))
             (was-modified (buffer-modified-p)))
        (unless (equal current-norm display)
          (emagent-session-store-write-top-property "EMAGENT_PROJECT" display)
          (set-buffer-modified-p was-modified))))
    (setq emagent-chat-session-id (or emagent-chat-session-id
                                      (emagent-session-store-read-session-property))
          emagent-chat-model (or emagent-chat-model (emagent-session-store-read-model-property))
          emagent-chat-provider (emagent-session-agent)
          emagent-chat-allowed-tools (or emagent-chat-allowed-tools
                                         (emagent-session-store-read-allowed-tools-property))
          emagent-chat-allowed-permissions (or emagent-chat-allowed-permissions
                                              (emagent-session-store-read-allowed-permissions-property))
          emagent-chat--font-lock-deferred-p nil)
    (emagent-chat--cancel-scheduled-table-align)
    (when-let ((model (or emagent-chat-model (emagent-session-store-read-model-property))))
      (setq emagent-chat-model (emagent-model-canonical-id model)))
    (setq-local default-directory (emagent-chat--session-directory))
    (if (bound-and-true-p doom-modeline-mode)
        (emagent-chat--setup-doom-modeline)
      (setq-local mode-line-format
                    (append (default-value 'mode-line-format)
                            '(" " (:eval (emagent-mode-line))))))
    (org-indent-mode -1)
    (emagent-chat--disable-incompatible-org-minor-modes)
    (when-let ((dir (emagent-session-project-directory)))
      (rename-buffer (emagent-chat--buffer-name-for-label
                      (emagent-chat--short-cwd-label dir))
                     t))
    (emagent-chat--insert-initial-comment)
    (emagent-chat--sync-user-zone-marker)
    (add-hook 'completion-at-point-functions
              #'emagent-chat-slash-command-completion-at-point -90 t)
    (setq-local imenu-create-index-function #'emagent-chat--imenu-create-index)
    (font-lock-add-keywords nil emagent-chat--tool-line-font-lock-keywords 'append)
    (setq-local bookmark-make-record-function #'emagent-chat--bookmark-make-record)
    (emagent-chat--setup-faces)
    (emagent-chat--mode-line-recompute)
    (emagent-chat--register-live-buffer)
    (add-hook 'kill-buffer-hook #'emagent-chat--unregister-live-buffer nil t)
    (run-with-idle-timer 0 nil #'emagent-chat--setup-faces-deferred)))

(defalias 'emagent--derived-mode (symbol-function 'emagent-mode)
  "Bare `define-derived-mode' implementation of `emagent-mode'.

Captured immediately after `define-derived-mode' so the public
`emagent-mode' defun below can wrap display deferral without `advice-add'.
Called by `emagent--run-derived-mode' in this file.")

(with-suppressed-warnings ((redefine emagent-mode))
  (defun emagent-mode (&optional arg)
    "Major mode for emagent chat scratch buffers.

ARG is accepted for major-mode compatibility; pass `force' (or use
`emagent-mode-force') to bypass display deferral.

Derived from `org-mode'.  Type after the `* user>' stub, then
\\[emagent-chat-send] to send the prompt at point (its heading line
plus any body lines).
On a slash-command line (plugin skills such as /workflow:dev),
\\[emagent-chat-tab] completes available commands.  Agent responses are
inserted between `** Thinking' / `** Response' subsections (TAB folds
them as Org headlines).

When `emagent-activate-on-display' is non-nil, opening an undisplayed
session file defers full activation until first display.

Run \\[emagent-mode] to reconnect a saved session."
    (interactive "P")
    (emagent-mode-entry arg)))

(cl-defun emagent-chat-open (&key project-dir)
  "Open or create an emagent buffer for PROJECT-DIR.

Buffer names look like *emagent myproj* from a short cwd label.
PROJECT-DIR is stored as #+EMAGENT_PROJECT and passed to the ACP agent as cwd."
  (unless project-dir
    (user-error "PROJECT-DIR is required"))
  (let* ((dir (expand-file-name project-dir))
         (label (emagent-chat--short-cwd-label dir))
         (slug (emagent-chat--sanitize-slug label))
         (buffer-name (emagent-chat--buffer-name-for-label label))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'emagent-mode)
        (emagent-mode-force))
      (rename-buffer buffer-name t)
      (setq emagent-chat-slug slug
            emagent-chat-session-id (or emagent-chat-session-id
                                        (emagent-session-store-read-session-property)))
      (emagent-session-set-project-directory dir))
    buffer))

(defun emagent-chat-send-or-babel ()
  "Execute the src block at point, send the prompt, or defer to org.

Precedence: a `#+BEGIN_SRC ... #+END_SRC' block executes via
`org-babel-execute-src-block' (even inside a prompt); on or inside a
`* user>' prompt (old prompts are re-evaluable) `emagent-chat-send'
sends it; anywhere else falls through to `org-ctrl-c-ctrl-c', so
tables realign, checkboxes toggle, and the rest of org ctrl-c ctrl-c keeps
working inside session buffers."
  (interactive)
  (cond
   ((org-in-src-block-p)
    (call-interactively #'org-babel-execute-src-block))
   ((emagent-chat--send-bounds)
    (call-interactively #'emagent-chat-send))
   (t
    (call-interactively #'org-ctrl-c-ctrl-c))))

(defun emagent-dispatch ()
  "Show the emagent command palette."
  (interactive)
  (if (fboundp 'transient-define-prefix)
      (progn
        ;; Redefine each time so palette changes apply after package reload
        ;; without requiring a full Emacs restart.
        (eval
         '(transient-define-prefix emagent--transient-menu ()
            "Emagent commands."
            ["Send & navigate"
             ("SPC" "Send / execute src block" emagent-chat-send-or-babel)
             ("u" "New prompt heading" emagent-chat-new-prompt)
             ("g" "Interrupt agent (ESC ESC)" emagent-chat-interrupt)]
            ["Attach"
             ("a" "Attach buffer context" emagent-chat-attach-buffer)
             ("b" "Send btw side note to agent" emagent-btw)
             ("d" "Attach project files" emagent-chat-attach-files)
             ("e" "Attach error context" emagent-chat-attach-error-context)
             ("i" "Attach image" emagent-chat-attach-image)]
            ["Extract response"
             ("r" "Insert last response into buffer" emagent-chat-insert-last-response)
             ("s" "Insert src block into buffer" emagent-chat-insert-src-block)]
            ;; org's own `C-c ?' command, shadowed by this palette; shown
            ;; only when point is in a table, where it is meaningful.
            ["Table" :if org-at-table-p
             ("f" "Field info (org's C-c ?)" org-table-field-info)]
            ["Session"
             ("c" "Connect / reconnect agent" emagent-connect)
             ("m" "Set session model (/model = one turn)" emagent-set-model)
             ("p" "Change project directory" emagent-set-project-directory)
             ("P" "Reset permissions" emagent-reset-permissions)
             ("t" "Trust workspace on disk" emagent-trust-workspace)
             ("R" "Claude: new session (trust)" emagent-trust-claude-reconnect)
             ("l" "View log" emagent-log-view)])
         t)
        (call-interactively 'emagent--transient-menu))
    (message "emagent: SPC=send, c=connect, g=interrupt, a=attach, i=image, m=model, t=trust, R=reconnect, l=log")))

;; Register session org buffers already open when this file finishes loading
;; (after `emagent--derived-mode' is captured).
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (and (not (derived-mode-p 'emagent-mode))
               (emagent--session-buffer-p))
      (if emagent--session-pending
          (cl-pushnew buf emagent--pending-buffers)
        (if (and emagent-activate-on-display
                 (not (emagent-chat--buffer-displayed-p)))
            (emagent--mark-session-pending)
          (emagent--activate-session-now))))))

(eval-when-compile
  (require 'cl-lib))

(defvar-local emagent-chat--assistant-marker nil
  "Insert position for the in-flight emagent response.")

(defvar-local emagent-chat--response-content-marker nil
  "Marker at the start of the open `** Response' body content.
Owned once the Response headline exists, so the body bounds are read from it
instead of re-searching for the headline on every streamed chunk.")

(defvar-local emagent-chat--thought-open-p nil
  "Non-nil while a Reasoning quote block is open in the in-flight response.")

(defvar-local emagent-chat--thinking-headline-marker nil
  "Marker at the open `** Thinking' headline, owned once the scaffold is inserted.
Read instead of re-searching for the headline on every reasoning chunk.")

(defvar-local emagent-chat--thought-marker nil
  "Insert position for streaming agent reasoning text.")

(defvar-local emagent-chat--reasoning-streamed-p nil
  "Non-nil once reasoning text has been streamed into the open Reasoning block.")

(defvar-local emagent-chat--thought-pending ""
  "Reasoning chunks not yet inserted into the open Thinking block.")

(defvar-local emagent-chat--thought-flush-timer nil
  "Timer that batches reasoning stream inserts into the chat buffer.")

(defcustom emagent-chat-thought-stream-delay 0.05
  "Seconds to batch reasoning stream inserts before updating the buffer.

Lower values feel more live; higher values reduce org font-lock work while the
agent is thinking."
  :type 'number
  :group 'emagent-chat)

(defvar-local emagent-chat--response-pending ""
  "Assistant chunks not yet inserted into the open Response block.")

(defvar-local emagent-chat--response-flush-timer nil
  "Timer that batches assistant stream inserts into the chat buffer.")

(defcustom emagent-chat-response-stream-delay 0.05
  "Seconds to batch assistant stream inserts before updating the buffer.

Lower values feel more live; higher values reduce markdown conversion and
org font-lock work while the agent is answering.  Use 0 to flush every
chunk synchronously (also the effective value in `noninteractive' tests)."
  :type 'number
  :group 'emagent-chat)

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

(defvar-local emagent-chat--tool-call-lines nil
  "Map ACP toolCallId to (START . END) markers for displayed tool-call lines.
Created per buffer in `emagent-mode'; must not use a shared mutable default,
or concurrent chat buffers would alias one table.")

(defvar-local emagent-chat--user-zone-start-marker nil
  "Position where the next user prompt may begin.")

(defvar-local emagent-chat--send-pending nil
  "Non-nil from send until `emagent-acp-send-prompt' dispatches the turn.

Covers connecting, per-turn model switches (`/model'), and other pre-dispatch
work.  The mode line shows a spinner during this window so large resumed
sessions do not look idle while the agent re-hydrates context for a new model.")

(defvar-local emagent-chat--send-token nil
  "Token for the in-flight pre-dispatch send; cleared on cancel or dispatch.")

(defvar-local emagent-chat--turn-model nil
  "Model id overriding the buffer model for the in-flight turn, or nil.

Set at send from the `emagent://AGENT/MODEL' link that `/model' inserts
in the prompt.  It drives the transient ACP model switch and the
`** Thinking (MODEL)' indicator.  Cleared when a turn completes
successfully or when a post-failure dialog declines to keep it; kept
across a failure so `retry' reuses the model.")

(defvar-local emagent-chat--turn-model-base nil
  "Session model to restore to when a per-turn override ends, or nil.
Captured (once) from the live session model just before the first override
switch, so restoring returns to whatever the session was really on.")

(defvar-local emagent-chat-slug nil
  "Filesystem slug for the current emagent buffer.")

;; Per-buffer session identity (project root, model, session id, provider,
;; allowed tools/permissions) now lives in `emagent-session' so lower layers
;; can read it without depending on this UI module.  The buffer-local vars and
;; canonical accessors are defined there; `emagent-chat-*' names below remain as
;; thin compatibility wrappers.

(defface emagent-tool-detail
  '((t (:inherit fixed-pitch)))
  "Face for paths and commands on tool-call lines."
  :group 'emagent-chat)

(defface emagent-tool-permission-decision
  '((t (:inherit shadow)))
  "Face for the permission decision suffix on tool-call lines.
Used for the trailing (Allow: Session) / (Denied) annotation."
  :group 'emagent-chat)

(defface emagent-permission-prompt
  '((t (:inherit font-lock-warning-face)))
  "Face for the permission question line in the Thinking block."
  :group 'emagent-chat)

;; The `/model' marker is an org link — `[[emagent://AGENT/MODEL][short]]' —
;; so org owns its fontification entirely (default `org-link' face, no
;; custom font-lock matcher, no sticky text properties) and the marker
;; survives saving the session file.
(org-link-set-parameters
 "emagent"
 :follow #'emagent-chat--follow-model-link
 :help-echo #'emagent-chat--model-link-help-echo)

(defface emagent-model-choice-agent
  '((t (:inherit font-lock-keyword-face)))
  "Face for the agent name in model selector candidates."
  :group 'emagent-chat)

(defface emagent-model-choice-model
  '((t (:inherit success)))
  "Face for the model base name in model selector candidates."
  :group 'emagent-chat)

(defface emagent-model-choice-detail
  '((t (:inherit font-lock-warning-face)))
  "Face for bracket annotations and aliases in model selector candidates."
  :group 'emagent-chat)

(defconst emagent-chat-default-slug "emagent")

(defvar-local emagent-chat--switching-model-p nil
  "Non-nil while the open response shows a `** Switching model' headline.")

(defvar-local emagent-chat--preparing-p nil
  "Non-nil while the open response shows a `** Preparing…' headline.")

(defconst emagent-chat-switching-headline "** Switching model"
  "Org subsection headline shown while a per-turn `/model' switch is in flight.")

(defconst emagent-chat-preparing-headline "** Preparing…"
  "Org subsection headline shown while connect/pre-dispatch work runs.")

(defconst emagent-chat-thinking-headline "** Thinking"
  "Org subsection headline holding streamed reasoning and tool lines.")

(defconst emagent-chat-response-headline "** Response"
  "Org subsection headline holding the finalized assistant answer.")

(defconst emagent-chat--progress-line "/emagent is thinking…/\n"
  "Placeholder body line shown until a prompt finishes rendering.")

(defconst emagent-chat--thinking-headline-re
  "^\\*\\* Thinking\\(?: (\\[\\[emagent://[^][]+\\]\\[[^][]*\\]\\])\\| \\[\\[emagent://[^][]+\\]\\[[^][]*\\]\\]\\)?[ \t]*$"
  "Regexp matching the Thinking subsection headline.
An optional model link marks a per-turn override
\(see `emagent-chat--turn-model'); both `** Thinking ([[…]])' and
`** Thinking [[…]]' forms are recognized.")

(defconst emagent-chat--switching-headline-re
  "^\\*\\* Switching model to .+…[ \t]*$"
  "Regexp matching the transient model-switch subsection headline.")

(defconst emagent-chat--preparing-headline-re
  "^\\*\\* Preparing…[ \t]*$"
  "Regexp matching the transient preparing subsection headline.")

(defconst emagent-chat--response-headline-re
  "^\\*\\* Response[ \t]*$"
  "Regexp matching the Response subsection headline.")

(defconst emagent-chat--subsection-headline-re
  "^\\*\\* \\(?:Thinking\\|Switching model\\|Preparing…\\|Response\\|Request permissions\\)\\(?:[ \t]\\|$\\)"
  "Regexp matching any emagent response subsection headline.")

(defconst emagent-chat--reasoning-begin-re emagent-chat--thinking-headline-re
  "Regexp matching the Thinking subsection opener.")

(defcustom emagent-chat-fold-reasoning-on-done t
  "When non-nil, fold the Thinking subsection once the agent finishes.

Hides the body of the `** Thinking' Org subsection (`org-fold-hide-subtree'),
leaving its headline visible as a collapsed summary."
  :type 'boolean
  :group 'emagent-chat)

(defconst emagent-chat-initial-comment
  "# -*- mode: emagent -*-
# This buffer is a scratch pad for chatting with emagent.
# Type after '* username> ' and press C-c C-c to send.
#
# C-c ?     command palette: connect, model, attach, new prompt, log, project, trust, …
# ESC ESC   interrupt agent response
# C-x k     kill buffer and disconnect agent
# M-x emagent-mode to reconnect a saved session

")

(defgroup emagent-chat nil
  "Org scratch buffers for emagent."
  :group 'tools)

(defun emagent-chat-cycle-response (&optional _force)
  "Fold or unfold the Org subtree at point (responses are native headlines)."
  (interactive)
  (org-cycle))

(defun emagent-chat--sanitize-slug (name)
  "Return a filesystem-safe slug for NAME."
  (let ((slug (downcase (string-trim name))))
    (if (string-empty-p slug)
        emagent-chat-default-slug
      (replace-regexp-in-string "[^a-zA-Z0-9._-]+" "-" slug))))

(defun emagent-chat--buffers ()
  "Return all buffers in `emagent-mode'."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (eq major-mode 'emagent-mode)))
              (buffer-list)))

(defun emagent-chat--short-cwd-label (directory)
  "Return a short display label for DIRECTORY."
  (let* ((dir (file-truename (expand-file-name directory)))
         (home (file-truename "~"))
         (raw (cond
               ((string= dir home) "~")
               ((string-prefix-p (concat home "/") dir)
                (substring dir (1+ (length home))))
               (t (file-name-nondirectory dir)))))
    (emagent-chat--sanitize-slug (or raw emagent-chat-default-slug))))

(defun emagent-chat--buffer-name-for-label (label)
  "Return a unique *emagent LABEL* buffer name."
  (let ((base (format "*Emagent %s*" label))
        (names (mapcar #'buffer-name (emagent-chat--buffers))))
    (if (not (member base names))
        base
      (let ((n 2))
        (while (member (format "*emagent %s-%d*" label n) names)
          (setq n (1+ n)))
        (format "*emagent %s-%d*" label n)))))

(defun emagent-chat--window-configuration-change (&optional _frames)
  "Flush deferred font-lock and refresh mode lines on focus change."
  (emagent-chat--map-live-buffers
   (lambda (buf)
     (with-current-buffer buf
       (emagent-chat--flush-deferred-font-lock)
       (emagent-chat--refresh-mode-line-on-focus))))
  (emagent-chat--spinner-ensure-running))

(add-hook 'window-configuration-change-hook
          #'emagent-chat--window-configuration-change)

(provide 'emagent-chat)
;;; emagent-chat.el ends here
