;;; emagent-mcp-registry.el --- MCP tool registry and dispatch  -*- lexical-binding: t; -*-

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

;; Tool registry and dispatch for the in-Emacs MCP server.  Require-DAG leaf
;; below `emagent-mcp-server' so the HTTP filter can call
;; `emagent-mcp--run-tool-async' without a circular require.

;;; Code:

(require 'seq)
(require 'emagent-tools)
(require 'emagent-mcp-util)
(require 'emagent-mcp-structural)
;; The per-buffer tool allow-list lives in the session model (below the UI), so
;; the MCP server reads it without depending on the chat module.
(require 'emagent-session)
(require 'emagent-mcp-core)

;; Defined in emagent-acp.el; declared here to avoid a circular require.
(defvar emagent-acp-prefer-emacs)

;;;; Tool registry

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

(defconst emagent-mcp--tools
  (append
   (list
   (list "read_file"
         "Read a file through Emacs, including unsaved buffer contents. Returns its text."
         '(("path" . ((type . "string")
                      (description . "Absolute path, or relative to the session root.")))
           ("line" . ((type . "integer")
                      (description . "1-based line to start from (optional).")))
           ("limit" . ((type . "integer")
                       (description . "Maximum number of lines to read (optional)."))))
         '("path")
         (lambda (args)
           (emagent-tool-read-file (emagent-mcp--arg args "path")
                                   (emagent-mcp--arg args "line")
                                   (emagent-mcp--arg args "limit"))))
   (list "write_file"
         "Write CONTENT to a file through an Emacs buffer (one undoable change). Refused for .el/.lisp/.cl/.scm when lisp-sitter is installed — use structural_* tools instead."
         '(("path" . ((type . "string")
                      (description . "Absolute path, or relative to the session root.")))
           ("content" . ((type . "string")
                         (description . "Full new contents of the file."))))
         '("path" "content")
         (lambda (args)
           (emagent-tool-write-file (emagent-mcp--arg args "path")
                                    (emagent-mcp--arg args "content" "")))
         :async
         (lambda (args cb)
           (emagent-tool-write-file-async
            cb
            (emagent-mcp--arg args "path")
            (emagent-mcp--arg args "content" ""))))
   (list "undo_file"
         "Undo edits in a file's buffer and save. Use to revert a write_file change."
         '(("path" . ((type . "string")
                      (description . "Absolute path, or relative to the session root.")))
           ("steps" . ((type . "integer")
                       (description . "Number of undo steps (default 1)."))))
         '("path")
         (lambda (args)
           (emagent-tool-undo-file (emagent-mcp--arg args "path")
                                   (emagent-mcp--arg args "steps"))))
   (list "delete_file"
         "Delete a file within the session root."
         '(("path" . ((type . "string")
                      (description . "Absolute path, or relative to the session root."))))
         '("path")
         (lambda (args)
           (emagent-tool-delete-file (emagent-mcp--arg args "path"))))
   (list "delete_directory"
         "Delete a directory within the session root."
         '(("path" . ((type . "string")
                      (description . "Absolute path, or relative to the session root.")))
           ("recursive" . ((type . "boolean")
                           (description . "Delete contents recursively."))))
         '("path")
         (lambda (args)
           (emagent-tool-delete-directory (emagent-mcp--arg args "path")
                                          (emagent-mcp--bool args "recursive"))))
   (list "list_files"
         "List files under a directory (relative paths, one per line)."
         '(("path" . ((type . "string")
                      (description . "Directory to list; defaults to the session root."))))
         '()
         (lambda (args)
           (emagent-tool-list-files (emagent-mcp--arg args "path"))))
   (list "grep"
         "Search for a regexp under a directory; returns matching lines."
         (append
          '(("pattern" . ((type . "string")
                          (description . "Regexp to search for.")))
            ("path" . ((type . "string")
                       (description . "Directory to search; defaults to the session root."))))
          emagent-mcp--timeout-prop)
         '("pattern")
         (lambda (args)
           (emagent-tool-grep (emagent-mcp--arg args "pattern")
                              (emagent-mcp--arg args "path")))
         :async
         (lambda (args cb)
           (let ((emagent-tools--timeout-override (emagent-mcp--timeout args)))
             (emagent-tool-grep-async
              cb
              (emagent-mcp--arg args "pattern")
              (emagent-mcp--arg args "path")))))
   (list "find_files"
         "List files matching a shell glob under a directory."
         '(("glob" . ((type . "string")
                      (description . "Filename glob, e.g. *.java or **/*.el.")))
           ("path" . ((type . "string")
                      (description . "Directory to search; defaults to the session root."))))
         '("glob")
         (lambda (args)
           (emagent-tool-find-files (emagent-mcp--arg args "glob")
                                    (emagent-mcp--arg args "path"))))
   (list "git_status"
         "Return git status for the session project directory."
         emagent-mcp--timeout-prop
         '()
         (lambda (_args)
           (emagent-tool-git-status))
         :async
         (lambda (args cb)
           (let ((emagent-tools--timeout-override (emagent-mcp--timeout args)))
             (emagent-tool-git-status-async cb))))
   (list "git_diff"
         "Return git diff output for the session project directory."
         (append
          '(("args" . ((type . "string")
                       (description . "Optional extra git diff arguments."))))
          emagent-mcp--timeout-prop)
         '()
         (lambda (args)
           (emagent-tool-git-diff (emagent-mcp--arg args "args")))
         :async
         (lambda (args cb)
           (let ((emagent-tools--timeout-override (emagent-mcp--timeout args)))
             (emagent-tool-git-diff-async cb (emagent-mcp--arg args "args")))))
   (list "git_log"
         "Return git log output for the session project directory."
         (append
          '(("args" . ((type . "string")
                       (description . "Optional extra git log arguments."))))
          emagent-mcp--timeout-prop)
         '()
         (lambda (args)
           (emagent-tool-git-log (emagent-mcp--arg args "args")))
         :async
         (lambda (args cb)
           (let ((emagent-tools--timeout-override (emagent-mcp--timeout args)))
             (emagent-tool-git-log-async cb (emagent-mcp--arg args "args")))))
   (list "eval"
         "Evaluate an Emacs Lisp form in the live Emacs and return the result. For small utilities and text processing, not shell. Filesystem and process ops are blocked here; use the dedicated tools."
         '(("form" . ((type . "string")
                      (description . "An Emacs Lisp form as a string."))))
         '("form")
         (lambda (args)
           (emagent-tool-eval (emagent-mcp--arg args "form"))))
   (list "fetch_url"
         "Fetch an http(s) URL and return the response body. Use for live web data when the agent's WebSearch or shell tools are sandboxed; runs through Emacs network access."
         (append
          '(("url" . ((type . "string")
                      (description . "http:// or https:// URL to fetch.")))
            ("max_bytes" . ((type . "integer")
                            (description . "Optional maximum response size in bytes."))))
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
              (emagent-mcp--arg args "max_bytes")))))
   (list "apropos"
         "List Emacs symbols whose NAME matches a regexp. Use to discover functions and variables when you know part of the name."
         '(("pattern" . ((type . "string")
                         (description . "Regexp matched against symbol names."))))
         '("pattern")
         (lambda (args)
           (emagent-tool-apropos (emagent-mcp--arg args "pattern"))))
   (list "apropos_doc"
         "List Emacs symbols whose DOCSTRING matches a regexp. Use when you know what a function does but not its name, e.g. \"split string\" or \"insert at point\"."
         '(("pattern" . ((type . "string")
                         (description . "Regexp matched against symbol docstrings."))))
         '("pattern")
         (lambda (args)
           (emagent-tool-apropos-doc (emagent-mcp--arg args "pattern"))))
   (list "describe_symbol"
         "Return documentation for an Emacs function or variable."
         '(("symbol" . ((type . "string")
                        (description . "Symbol name."))))
         '("symbol")
         (lambda (args)
           (emagent-tool-describe-symbol (emagent-mcp--arg args "symbol"))))
   (list "find_function"
         "Return the source location of an Emacs function."
         '(("symbol" . ((type . "string")
                        (description . "Function name."))))
         '("symbol")
         (lambda (args)
           (emagent-tool-find-function (emagent-mcp--arg args "symbol"))))
   (list "where_is"
         "Return the key bindings for an Emacs command."
         '(("command" . ((type . "string")
                         (description . "Command name."))))
         '("command")
         (lambda (args)
           (emagent-tool-where-is (emagent-mcp--arg args "command"))))
   (list "run_shell_command"
         "Run a shell command through Emacs and return its output. The result is returned only when the process exits, so the MCP client may time out on commands that take longer than ~30 s (e.g. curl hitting a service paused in a debugger). For such commands use background execution: append '> /tmp/out.txt 2>&1 & echo \"PID: $!\"' so the shell exits immediately and you can read the result from the file later with read_file."
         (append
          '(("command" . ((type . "string")
                          (description . "The shell command line.")))
            ("directory" . ((type . "string")
                            (description . "Working directory; defaults to the session root."))))
          emagent-mcp--timeout-prop)
         '("command")
         (lambda (args)
           (emagent-tool-run-shell-command (emagent-mcp--arg args "command")
                                           (emagent-mcp--arg args "directory")))
         :async
         (lambda (args cb)
           (let ((emagent-tools--timeout-override (emagent-mcp--timeout args)))
             (emagent-tool-run-shell-command-async
              (emagent-mcp--arg args "command")
              (emagent-mcp--arg args "directory")
              cb))))
   (list "project_directory"
         "Return the session's project directory."
         '()
         '()
         (lambda (_args)
           (emagent-tool-project-directory)))
   (list "compile"
         "Run a build or test command via Emacs compilation-mode. Errors appear in *emagent-compile* and are navigable with next-error / M-g n. Returns the full build output."
         (append
          '(("command" . ((type . "string")
                          (description . "The shell command to compile or test, e.g. 'mvn test', 'cargo build'.")))
            ("directory" . ((type . "string")
                            (description . "Working directory; defaults to the session root."))))
          emagent-mcp--timeout-prop)
         '("command")
         (lambda (args)
           (emagent-tool-compile (emagent-mcp--arg args "command")
                                 (emagent-mcp--arg args "directory")))
         :async
         (lambda (args cb)
           (let ((emagent-tools--timeout-override (emagent-mcp--timeout args)))
             (emagent-tool-compile-async
              cb
              (emagent-mcp--arg args "command")
              (emagent-mcp--arg args "directory")))))
   (list "buffer_list"
         "List open Emacs buffers that visit files inside the session project root. Use to see what the user is currently editing."
         '()
         '()
         (lambda (_args)
           (emagent-tool-buffer-list)))
   (list "imenu_index"
         "Return the structural outline of a file (functions, classes, sections) using Emacs imenu. Works for any language with imenu support."
         '(("file" . ((type . "string")
                      (description . "File path relative to session root; omit for current buffer."))))
         '()
         (lambda (args)
           (emagent-tool-imenu-index (emagent-mcp--arg args "file"))))
   (list "check_elisp"
         "Check an Emacs Lisp form for syntax errors (paren balance, read errors) WITHOUT executing it. Returns \"OK\" or an error description with line:column. Always call this before eval for forms longer than 3 lines."
         '(("form" . ((type . "string")
                      (description . "An Emacs Lisp form as a string to validate."))))
         '("form")
         (lambda (args)
           (emagent-tool-check-elisp (emagent-mcp--arg args "form"))))
   (list "elisp_guide"
         "Return the emagent Emacs Lisp reference guide: patterns, idioms, structural editing (structural_tree, structural_replace, structural_insert), validation (check_elisp, check_structural_file), string/list/buffer/file/JSON/org operations, error handling, common pitfalls, and code templates. Call before writing non-trivial Elisp."
         '()
         '()
         (lambda (_args)
           (emagent-tool-elisp-guide))))
   emagent-mcp--structural-tools)
  "Registry of emagent MCP tools.
Each entry: (NAME DESCRIPTION PROPERTIES REQUIRED HANDLER . PLIST).
PLIST may have :available, a predicate returning non-nil to show the tool.")

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
                          (inputSchema . ,(emagent-mcp--tool-schema (nth 2 entry) (nth 3 entry)))))
                      (seq-filter #'emagent-mcp--tool-available-p emagent-mcp--tools))))))

;;;; Tool dispatch (runs in the live Emacs, bound to the session context)

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
           (emagent-acp-prefer-emacs (if session
                                         (plist-get session :prefer-emacs)
                                       (and (boundp 'emagent-acp-prefer-emacs)
                                            emagent-acp-prefer-emacs))))
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
             (emagent-acp-prefer-emacs (if session
                                           (plist-get session :prefer-emacs)
                                         (and (boundp 'emagent-acp-prefer-emacs)
                                              emagent-acp-prefer-emacs)))
             (async-fn (plist-get (nthcdr 5 entry) :async)))
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

(provide 'emagent-mcp-registry)
;;; emagent-mcp-registry.el ends here
