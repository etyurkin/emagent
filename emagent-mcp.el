;;; emagent-mcp.el --- In-Emacs MCP server for emagent tools -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Exposes the `emagent-tool-*' functions to ACP agents as Model Context
;; Protocol (MCP) tools, served over a localhost HTTP listener hosted inside
;; the running Emacs.  Tool calls therefore execute in the live Emacs process
;; itself -- no `emacsclient', no subprocess, no second Emacs.
;;
;; The server is a refcounted singleton: started lazily when the first emagent
;; session connects and torn down only when the last session is gone.  Each
;; session registers a per-session token mapped to its project root; every
;; tool call carries that token in the request path
;; (http://127.0.0.1:PORT/mcp/TOKEN), so a shared, provider-agnostic server
;; can route each call to the right session and enforce its filesystem root.
;;
;; Two ways in, one server:
;;   - Claude: the token url is passed via `session/new' mcpServers (http).
;;   - Cursor: the cursor-agent CLI reads ~/.cursor/mcp.json, whose url uses
;;     ${env:EMAGENT_SESSION_TOKEN}; emagent sets that env var per session.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'emagent-tools)

;; Defined in emagent-acp.el; declared here to avoid a circular require.
(defvar emagent-acp-prefer-emacs)

(declare-function emagent-log "emagent-log")

;; Defined in emagent-chat.el; declared to avoid a circular require.  The chat
;; buffer owns the per-document tool allow-list (#+EMAGENT_ALLOWED_TOOLS).
(declare-function emagent-chat-allowed-tools "emagent-chat")
(declare-function emagent-chat-add-allowed-tool "emagent-chat")

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

;;;; Tokens and per-buffer identity

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

;;;; Tool registry

(defun emagent-mcp--arg (args key &optional default)
  "Return KEY from ARGS hash-table, or DEFAULT when missing or JSON null."
  (let ((value (and (hash-table-p args) (gethash key args))))
    (if (or (null value) (eq value :null))
        default
      value)))

(defun emagent-mcp--bool (args key)
  "Return non-nil when KEY in ARGS is JSON true."
  (eq (emagent-mcp--arg args key) t))

(defun emagent-mcp--string-result (value)
  "Coerce a tool VALUE into a string for MCP text content."
  (cond
   ((stringp value) value)
   ((null value) "")
   (t (format "%s" value))))

(defconst emagent-mcp--tools
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
         "Write CONTENT to a file through an Emacs buffer (one undoable change). Prompts the user."
         '(("path" . ((type . "string")
                      (description . "Absolute path, or relative to the session root.")))
           ("content" . ((type . "string")
                         (description . "Full new contents of the file."))))
         '("path" "content")
         (lambda (args)
           (emagent-tool-write-file (emagent-mcp--arg args "path")
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
         "Delete a file. Prompts the user."
         '(("path" . ((type . "string")
                      (description . "Absolute path, or relative to the session root."))))
         '("path")
         (lambda (args)
           (emagent-tool-delete-file (emagent-mcp--arg args "path"))))
   (list "delete_directory"
         "Delete a directory. Prompts the user."
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
         '(("pattern" . ((type . "string")
                         (description . "Regexp to search for.")))
           ("path" . ((type . "string")
                      (description . "Directory to search; defaults to the session root."))))
         '("pattern")
         (lambda (args)
           (emagent-tool-grep (emagent-mcp--arg args "pattern")
                              (emagent-mcp--arg args "path"))))
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
         '()
         '()
         (lambda (_args)
           (emagent-tool-git-status)))
   (list "git_diff"
         "Return git diff output for the session project directory."
         '(("args" . ((type . "string")
                      (description . "Optional extra git diff arguments."))))
         '()
         (lambda (args)
           (emagent-tool-git-diff (emagent-mcp--arg args "args"))))
   (list "git_log"
         "Return git log output for the session project directory."
         '(("args" . ((type . "string")
                      (description . "Optional extra git log arguments."))))
         '()
         (lambda (args)
           (emagent-tool-git-log (emagent-mcp--arg args "args"))))
   (list "eval"
         "Evaluate an Emacs Lisp form in the live Emacs and return the result. For small utilities and text processing, not shell. Filesystem and process ops are blocked here; use the dedicated tools."
         '(("form" . ((type . "string")
                      (description . "An Emacs Lisp form as a string."))))
         '("form")
         (lambda (args)
           (emagent-tool-eval (emagent-mcp--arg args "form"))))
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
         "Run a shell command through Emacs (shell-command-to-string) and return its output."
         '(("command" . ((type . "string")
                         (description . "The shell command line.")))
           ("directory" . ((type . "string")
                           (description . "Working directory; defaults to the session root."))))
         '("command")
         (lambda (args)
           (emagent-tool-run-shell-command (emagent-mcp--arg args "command")
                                           (emagent-mcp--arg args "directory"))))
   (list "project_directory"
         "Return the session's project directory."
         '()
         '()
         (lambda (_args)
           (emagent-tool-project-directory)))
   (list "compile"
         "Run a build or test command via Emacs compilation-mode. Errors appear in *emagent-compile* and are navigable with next-error / M-g n. Returns the full build output."
         '(("command" . ((type . "string")
                         (description . "The shell command to compile or test, e.g. 'mvn test', 'cargo build'.")))
           ("directory" . ((type . "string")
                           (description . "Working directory; defaults to the session root."))))
         '("command")
         (lambda (args)
           (emagent-tool-compile (emagent-mcp--arg args "command")
                                 (emagent-mcp--arg args "directory"))))
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
         "Check an Emacs Lisp form for syntax errors (paren balance, read errors) WITHOUT executing it. Returns \"OK\" or an error description. Always call this before eval for forms longer than 3 lines."
         '(("form" . ((type . "string")
                      (description . "An Emacs Lisp form as a string to validate."))))
         '("form")
         (lambda (args)
           (emagent-tool-check-elisp (emagent-mcp--arg args "form")))))
  "Registry of emagent MCP tools: (NAME DESCRIPTION PROPERTIES REQUIRED HANDLER).")

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
  "Return the tools/list result as an alist."
  `((tools . ,(apply #'vector
                     (mapcar
                      (lambda (entry)
                        (cl-destructuring-bind (name description properties required _handler) entry
                          `((name . ,name)
                            (description . ,description)
                            (inputSchema . ,(emagent-mcp--tool-schema properties required)))))
                      emagent-mcp--tools)))))

;;;; Tool dispatch (runs in the live Emacs, bound to the session context)

(defun emagent-mcp--session-allowed-tools (buffer)
  "Return BUFFER's persisted tool allow-list, or nil."
  (when (and (buffer-live-p buffer) (fboundp 'emagent-chat-allowed-tools))
    (with-current-buffer buffer
      (emagent-chat-allowed-tools))))

(defun emagent-mcp--make-allow-all-fn (buffer)
  "Return a function that persists an \"allow all\" choice to BUFFER, or nil."
  (when (and (buffer-live-p buffer) (fboundp 'emagent-chat-add-allowed-tool))
    (lambda (tool)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (emagent-chat-add-allowed-tool tool))))))

(defconst emagent-mcp--confirming-tools
  '("run_shell_command" "compile" "write_file" "delete_file" "delete_directory")
  "MCP tool names that prompt the user before running.")

(defun emagent-mcp--tool-confirm-symbol (name)
  "Return the `emagent-tool-*' symbol used for allow-listing MCP tool NAME."
  (intern (concat "emagent-tool-" (replace-regexp-in-string "_" "-" name))))

(defun emagent-mcp--truncate-detail (text limit)
  "Return TEXT truncated to LIMIT characters with an ellipsis."
  (if (and (stringp text) (> (length text) limit))
      (concat (substring text 0 limit) "…")
    (or text "")))

(defun emagent-mcp--tool-prompt-detail (name args &optional root)
  "Return a multi-line summary of ARGS for tool NAME."
  (pcase name
    ("run_shell_command"
     (let* ((command (or (emagent-mcp--arg args "command") ""))
            (directory (emagent-mcp--arg args "directory"))
            (cwd (cond
                  ((and directory (not (string-empty-p directory)))
                   (expand-file-name directory (or root default-directory)))
                  (root root)
                  (t "(session root)"))))
       (format "  shell: %s\n  in: %s"
               (emagent-mcp--truncate-detail command 400)
               cwd)))
    ("eval"
     (format "  elisp: %s"
             (emagent-mcp--truncate-detail (or (emagent-mcp--arg args "form") "") 300)))
    ("write_file"
     (let* ((path (or (emagent-mcp--arg args "path") ""))
            (content (or (emagent-mcp--arg args "content" "") "")))
       (format "  write: %s\n  size: %d chars"
               (emagent-mcp--truncate-detail path 200)
               (length content))))
    ("delete_file"
     (format "  delete file: %s"
             (emagent-mcp--truncate-detail (or (emagent-mcp--arg args "path") "") 200)))
    ("delete_directory"
     (format "  delete directory: %s%s"
             (emagent-mcp--truncate-detail (or (emagent-mcp--arg args "path") "") 200)
             (if (emagent-mcp--bool args "recursive") " (recursive)" "")))
    (_ (format "  %s" (emagent-mcp--truncate-detail (format "%s" args) 200)))))

(defun emagent-mcp--tool-confirm-prompt (name args &optional root)
  "Return the permission prompt for MCP tool NAME and ARGS."
  (format "Allow %s?\n%s" name (emagent-mcp--tool-prompt-detail name args root)))

(defun emagent-mcp--confirm-tool (name args root &optional buffer)
  "Return non-nil when the user allows MCP tool NAME with ARGS.
BUFFER is the chat buffer; when NAME is write_file it is passed to
`emagent-tools--confirm-write' so the diff appears inline there."
  (let ((tool-symbol (emagent-mcp--tool-confirm-symbol name)))
    (if (string= name "write_file")
        (emagent-tools--confirm-write
         tool-symbol
         (emagent-tools--root-directory (emagent-mcp--arg args "path"))
         (emagent-mcp--arg args "content" "")
         buffer)
      (emagent-tools--confirm
       tool-symbol
       (emagent-mcp--tool-confirm-prompt name args root)))))

(defun emagent-mcp--run-tool (name args session)
  "Run tool NAME with ARGS in SESSION's context; return a result string."
  (let ((entry (emagent-mcp--tool-entry name)))
    (unless entry
      (error "Unknown tool: %s" name))
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
           (emagent-acp-prefer-emacs (if session
                                         (plist-get session :prefer-emacs)
                                       (and (boundp 'emagent-acp-prefer-emacs)
                                            emagent-acp-prefer-emacs))))
      (when (and (member name emagent-mcp--confirming-tools)
                 (not (emagent-mcp--confirm-tool name args root buffer)))
        (user-error "Tool call cancelled"))
      (emagent-mcp--string-result (funcall handler args)))))

;;;; JSON-RPC

(defun emagent-mcp--json-encode (object)
  "Serialize OBJECT to a JSON string."
  (json-serialize object :null-object :null :false-object :false))

(defun emagent-mcp--rpc-result (id result)
  "Return a JSON-RPC success response string for ID with RESULT."
  (emagent-mcp--json-encode `((jsonrpc . "2.0") (id . ,id) (result . ,result))))

(defun emagent-mcp--rpc-error (id code message)
  "Return a JSON-RPC error response string for ID with CODE and MESSAGE."
  (emagent-mcp--json-encode
   `((jsonrpc . "2.0") (id . ,id)
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
                     (version . "0.1.0"))))))

(defun emagent-mcp--handle-tools-call (id params token)
  "Handle a tools/call request; return a JSON-RPC response string."
  (let* ((name (and (hash-table-p params) (gethash "name" params)))
         (args (or (and (hash-table-p params) (gethash "arguments" params))
                   (make-hash-table :test 'equal)))
         (session (and token (gethash token emagent-mcp--sessions))))
    (cond
     ((null token)
      (emagent-mcp--rpc-result
       id (emagent-mcp--tool-content "No emagent session token in request path" t)))
     ((null session)
      (emagent-mcp--rpc-result
       id (emagent-mcp--tool-content "Unknown or expired emagent session" t)))
     (t
      (condition-case err
          (emagent-mcp--rpc-result
           id (emagent-mcp--tool-content (emagent-mcp--run-tool name args session) nil))
        (error
         (emagent-mcp--rpc-result
          id (emagent-mcp--tool-content (error-message-string err) t))))))))

;;;; HTTP layer

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
works, and keeps Emacs responsive while the user decides.  The HTTP response
is sent on PROC when the call completes."
  (run-with-idle-timer
   0 nil
   (lambda ()
     (let ((response (condition-case err
                         (emagent-mcp--handle-tools-call id params token)
                       (error (emagent-mcp--rpc-result
                               id (emagent-mcp--tool-content
                                   (error-message-string err) t))))))
       (emagent-mcp--respond-json proc response)))))

(defun emagent-mcp--dispatch (proc token message)
  "Dispatch a parsed JSON-RPC MESSAGE (hash-table) from PROC with TOKEN."
  (let ((id (gethash "id" message))
        (method (gethash "method" message))
        (params (gethash "params" message)))
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
        proc (emagent-mcp--rpc-error id -32601 (format "Method not found: %s" method)))))))

(defun emagent-mcp--handle-request (proc request-line _headers body)
  "Handle one parsed HTTP request on PROC."
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

(defun emagent-mcp--drain (proc)
  "Parse and handle as many complete HTTP requests as PROC has buffered."
  (let ((more t))
    (while more
      (setq more nil)
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
                (setq more t)))))))))

(defun emagent-mcp--filter (proc data)
  "Process filter: accumulate DATA on PROC and drain complete requests."
  (process-put proc 'emagent-mcp-data
               (concat (or (process-get proc 'emagent-mcp-data) "") data))
  (emagent-mcp--drain proc))

(defun emagent-mcp--sentinel (proc _event)
  "Clean up PROC connection state when it closes."
  (unless (process-live-p proc)
    (process-put proc 'emagent-mcp-data nil)))

;;;; Lifecycle

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

(cl-defun emagent-mcp-register-session (&key token cwd buffer prefer-emacs)
  "Register session TOKEN with project CWD, owning BUFFER, and EMACS-ONLY flag.

Starts the server if needed and returns the port."
  (emagent-mcp-ensure-server)
  (puthash token
           (list :root (and cwd (expand-file-name cwd))
                 :cwd cwd
                 :buffer buffer
                 :prefer-emacs prefer-emacs)
           emagent-mcp--sessions)
  emagent-mcp--port)

(defun emagent-mcp-deregister-session (token)
  "Deregister session TOKEN and stop the server if it was the last one."
  (when token
    (remhash token emagent-mcp--sessions))
  (emagent-mcp-maybe-shutdown))

(defun emagent-mcp-session-url (token)
  "Return the MCP endpoint URL for session TOKEN (starts the server)."
  (format "http://127.0.0.1:%d/mcp/%s" (emagent-mcp-ensure-server) token))

;;;; Cursor configuration

(defun emagent-mcp--lists-to-vectors (object)
  "Recursively convert JSON arrays (lists) to vectors for `json-serialize'.

`json-parse-buffer' with `:array-type \\='list\\=' yields lists, but
`json-serialize' treats lists as alists and requires symbol keys."
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

;;;; External MCP gateway forwarding

(defcustom emagent-acp-extra-mcp-config-file "~/.claude.json"
  "JSON file whose top-level `mcpServers' block is forwarded to ACP agents.

Emagent reads the `mcpServers' object from this file and advertises those
servers, alongside the in-Emacs emagent server, to agents that support http MCP
over ACP (e.g. Claude).  This reuses an existing Claude gateway without
re-declaring it for emagent.

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

(defun emagent-mcp-gateway-system-prompt ()
  "Return gateway guidance when extra MCP servers are configured, or nil."
  (when (and emagent-acp-extra-mcp-config-file
             (emagent-mcp-config-file-servers))
    (bound-and-true-p emagent-acp-system-prompt-gateway)))

(provide 'emagent-mcp)

;;; emagent-mcp.el ends here
