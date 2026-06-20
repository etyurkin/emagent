;;; emagent-cursor.el --- Cursor provider config for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)

(require 'emagent-acp-protocol)
(require 'emagent-mcp)
(require 'emagent-chat)

(defgroup emagent-cursor nil
  "Cursor provider configuration for emagent."
  :group 'emagent)

(defcustom emagent-cursor-acp-command
  '("cursor-agent" "acp")
  "Command and parameters for the Cursor ACP agent.

Uses Cursor's own ACP server (the registry `cursor' entry, `cursor-agent
acp'), which speaks ACP natively.  Cursor discovers the in-Emacs MCP server
from ~/.cursor/mcp.json (see `emagent-mcp-ensure-cursor-config')."
  :type '(repeat string)
  :group 'emagent-cursor)

(defcustom emagent-cursor-acp-extra-args
  '("--approve-mcps" "--force" "--sandbox" "disabled")
  "Extra arguments appended after \"acp\" when spawning cursor-agent.

Cursor documents these flags on the top-level agent command; passing them
here may still take effect depending on the CLI version.  They reduce
sandbox blocks on WebSearch, shell, and MCP tool calls in ACP sessions.
Set to nil to pass no extra flags."
  :type '(repeat string)
  :group 'emagent-cursor)

(defcustom emagent-cursor-environment nil
  "Environment variables for the Cursor ACP agent."
  :type '(repeat string)
  :group 'emagent-cursor)

(defconst emagent-cursor-install-hint
  "Install Cursor's CLI (provides `cursor-agent acp'): https://cursor.com/cli")

(defcustom emagent-cursor-dir
  (expand-file-name ".cursor" "~")
  "Directory where Cursor stores ACP session data."
  :type 'directory
  :group 'emagent-cursor)

(defconst emagent-cursor--tool-enrich-max-attempts 8
  "Maximum store.db polls when Cursor ACP omits tool-call rawInput.")

(defconst emagent-cursor--tool-enrich-base-delay 0.05
  "Initial idle delay between Cursor store.db enrichment retries.")

(defun emagent-cursor-command ()
  "Return the Cursor ACP command name."
  (car emagent-cursor-acp-command))

(defun emagent-cursor-command-params ()
  "Return the Cursor ACP command parameters."
  (append (cdr emagent-cursor-acp-command) emagent-cursor-acp-extra-args))

(defun emagent-cursor-command-params-for-context (context-buffer)
  "Return Cursor ACP args for CONTEXT-BUFFER.

Uses `emagent-chat-cursor-acp-extra-args' when buffer-local and non-nil."
  (append (cdr emagent-cursor-acp-command)
          (with-current-buffer context-buffer
            (if (and (boundp 'emagent-chat-cursor-acp-extra-args)
                     emagent-chat-cursor-acp-extra-args)
                emagent-chat-cursor-acp-extra-args
              emagent-cursor-acp-extra-args))))

(defun emagent-cursor-check-command ()
  "Signal a clear error when the Cursor agent is missing."
  (unless (executable-find (emagent-cursor-command))
    (error "Cursor ACP agent %s not found on PATH.\n%s"
           (emagent-cursor-command) emagent-cursor-install-hint)))

(defun emagent-cursor--environment (context-buffer)
  "Return env vars for the Cursor agent, including the per-session MCP token.

Cursor discovers MCP servers from ~/.cursor/mcp.json, whose emagent url
interpolates ${env:EMAGENT_SESSION_TOKEN}.  Setting it per buffer routes each
invocation to its own in-Emacs MCP session."
  (let ((token (with-current-buffer context-buffer (emagent-mcp-buffer-token))))
    (append emagent-cursor-environment
            (list (format "EMAGENT_SESSION_TOKEN=%s" token)))))

(defun emagent-cursor--json-field (data key)
  "Return KEY from JSON DATA alist or hash-table."
  (cond
   ((hash-table-p data)
    (or (gethash key data)
        (gethash (symbol-name key) data)
        (gethash (downcase (symbol-name key)) data)))
   ((listp data)
    (or (alist-get key data)
        (alist-get (downcase (symbol-name key)) data)))
   (t nil)))

(defun emagent-cursor--tool-call-raw-empty-p (raw)
  "Return non-nil when Cursor ACP rawInput/arguments carry no parameters."
  (or (null raw)
      (and (stringp raw)
           (let ((trimmed (string-trim raw)))
             (or (string-empty-p trimmed)
                 (member trimmed '("{}" "[]" "null")))))
      (and (listp raw) (null raw))
      (and (hash-table-p raw) (zerop (hash-table-count raw)))))

(defun emagent-cursor--store-db-path (session-id)
  "Return store.db for Cursor ACP SESSION-ID, or nil when missing."
  (let ((flat (expand-file-name
               (format "acp-sessions/%s/store.db" session-id)
               emagent-cursor-dir)))
    (cond
     ((file-readable-p flat) flat)
     (t
      (let ((chats (expand-file-name "chats" emagent-cursor-dir))
            found)
        (when (file-directory-p chats)
          (dolist (hash (directory-files chats t) (unless found nil))
            (unless found
              (let ((candidate (expand-file-name
                                 (format "%s/store.db" session-id) hash)))
                (when (file-readable-p candidate)
                  (setq found candidate))))))
        found)))))

(defun emagent-cursor--sql-escape (string)
  "Escape STRING for use inside a single-quoted SQL literal."
  (replace-regexp-in-string "'" "''" string))

(defun emagent-cursor--tool-call-id-variants (tool-call-id)
  "Return likely store.db toolCallId spellings for TOOL-CALL-ID."
  (delete-dups
   (list tool-call-id
         (replace-regexp-in-string "^call_" "tool_" tool-call-id)
         (replace-regexp-in-string "^tool_" "call_" tool-call-id))))

(defun emagent-cursor--tool-call-from-blob-json (json tool-call-id)
  "Parse assistant/tool blob JSON and return (NAME . ARGS) for TOOL-CALL-ID."
  (when-let* ((data (condition-case nil
                        (json-parse-string json
                                           :object-type 'alist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object nil)
                      (error nil)))
              (content (emagent-cursor--json-field data 'content)))
    (catch 'found
      (dolist (item (if (vectorp content) (append content nil) (append content nil)))
        (when (and (equal (emagent-cursor--json-field item 'type) "tool-call")
                   (equal (emagent-cursor--json-field item 'toolCallId) tool-call-id))
          (let ((name (emagent-cursor--json-field item 'toolName))
                (args (emagent-cursor--json-field item 'args)))
            (when (and name args (not (emagent-cursor--tool-call-raw-empty-p args)))
              (throw 'found (cons name args)))))))))

(defun emagent-cursor-tool-call-from-store (session-id tool-call-id)
  "Return (TOOL-NAME . ARGS-ALIST) for TOOL-CALL-ID from Cursor store.db."
  (when-let* ((db (emagent-cursor--store-db-path session-id))
              (sqlite (executable-find "sqlite3")))
    (catch 'found
      (dolist (variant (emagent-cursor--tool-call-id-variants tool-call-id))
        (let* ((needle (emagent-cursor--sql-escape variant))
               (sql (format
                     "SELECT cast(data as text) FROM blobs WHERE cast(data as text) LIKE '%%toolCallId\":\"%s\"%%' LIMIT 20;"
                     needle))
               (out (shell-command-to-string
                     (format "%s %s %s"
                             (shell-quote-argument sqlite)
                             (shell-quote-argument db)
                             (shell-quote-argument sql)))))
          (dolist (line (split-string out "\n" t))
            (when-let ((entry (emagent-cursor--tool-call-from-blob-json line variant)))
              (throw 'found entry))))))))

(defun emagent-cursor--update-put (update key value)
  "Return UPDATE alist with KEY bound to VALUE, replacing any prior binding."
  (cons (cons key value) (assoc-delete-all key update)))

(defun emagent-cursor-enrich-tool-call-update (session-id update)
  "Fill empty rawInput in UPDATE from Cursor store.db when available."
  (let ((raw (or (map-elt update 'rawInput) (map-elt update 'arguments))))
    (if (emagent-cursor--tool-call-raw-empty-p raw)
        (or (when-let* ((id (map-elt update 'toolCallId))
                        (entry (emagent-cursor-tool-call-from-store session-id id)))
              (emagent-cursor--update-put
               (emagent-cursor--update-put
                (assoc-delete-all 'rawInput (assoc-delete-all 'arguments update))
                'rawInput (cdr entry))
               'title (or (map-elt update 'title) (car entry))))
            update)
      update)))

(defconst emagent-cursor--builtin-slash-commands
  '(("plan" . "Switch to Plan mode")
    ("ask" . "Toggle Ask mode (read-only)")
    ("debug" . "Toggle Debug mode")
    ("compress" . "Summarize conversation to free context")
    ("summarize" . "Summarize conversation (/compress alias)")
    ("compact" . "Alias for /compress (Claude-style name)")
    ("model" . "Select a model")
    ("mcp" . "Browse, enable, and configure MCP servers")
    ("rules" . "Create or edit agent rules")
    ("commands" . "Create or edit custom slash commands")
    ("run-everything" . "Toggle Run Everything")
    ("auto-run" . "Alias for /run-everything")
    ("max-mode" . "Toggle max mode on supported models")
    ("new-chat" . "Start a new chat session")
    ("resume" . "Resume a previous chat")
    ("usage" . "View usage statistics")
    ("help" . "Show help for slash commands")
    ("about" . "Environment and CLI setup details")
    ("vim" . "Toggle Vim keybindings for input")
    ("setup-terminal" . "Configure terminal keybindings")
    ("copy-request-id" . "Copy last request ID")
    ("copy-conversation-id" . "Copy conversation ID"))
  "Cursor CLI built-in slash commands.")

(defconst emagent-cursor--slash-command-send-aliases
  '(("compact" . "compress")
    ("auto-run" . "run-everything"))
  "Map emagent/Cursor completion names to CLI names before send.")

(defun emagent-cursor-normalize-slash-prompt (text)
  "Rewrite known Cursor slash-command aliases in TEXT before send."
  (let ((trimmed (string-trim text)))
    (if (and (not (string-empty-p trimmed)) (string-prefix-p "/" trimmed))
        (let* ((body (substring trimmed 1))
               (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
               (cmd (if space (substring body 0 space) body))
               (rest (when space (string-trim (substring body (1+ space)))))
               (canonical (or (cdr (assoc cmd emagent-cursor--slash-command-send-aliases))
                              cmd)))
          (if (string= cmd canonical)
              text
            (progn
              (emagent-log "slash command /%s → /%s" cmd canonical)
              (if rest
                  (format "/%s %s" canonical rest)
                (format "/%s" canonical)))))
      text)))

(defun emagent-cursor--builtin-slash-command-plists ()
  "Return built-in Cursor slash commands as plists."
  (mapcar (lambda (entry)
            (emagent-chat--slash-command-plist (car entry) (cdr entry)))
          emagent-cursor--builtin-slash-commands))

(defun emagent-cursor--custom-slash-command-description (file)
  "Return a short description for custom slash command FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 1024)
    (goto-char (point-min))
    (if (re-search-forward "^#\\s-+\\(.+\\)$" nil t)
        (string-trim (match-string 1))
      "Custom command")))

(defun emagent-cursor--custom-slash-command-plists (directory)
  "Return slash-command plists discovered under DIRECTORY."
  (when (and directory (file-directory-p directory))
    (sort
     (mapcar
      (lambda (file)
        (emagent-chat--slash-command-plist
         (file-name-base file)
         (emagent-cursor--custom-slash-command-description file)))
      (directory-files directory t "\\.md\\'"))
     (lambda (a b) (string< (map-elt a 'name) (map-elt b 'name))))))

(defun emagent-cursor-slash-commands (&optional project-dir)
  "Return Cursor slash commands for completion: built-ins, custom, then project."
  (let ((global (expand-file-name "commands" emagent-cursor-dir))
        (project (and project-dir
                      (expand-file-name ".cursor/commands" project-dir))))
    (emagent-chat--merge-slash-commands
     (emagent-chat--merge-slash-commands
      (emagent-cursor--builtin-slash-command-plists)
      (or (emagent-cursor--custom-slash-command-plists global) '()))
     (or (emagent-cursor--custom-slash-command-plists project) '()))))

(cl-defun emagent-cursor-make-client (&key context-buffer process-directory)
  "Create an ACP client for Cursor using CONTEXT-BUFFER.
PROCESS-DIRECTORY is passed to `make-process' as the working directory
(see `emagent-chat--session-directory' / #+EMAGENT_PROJECT)."
  (emagent-cursor-check-command)
  (emagent-mcp-ensure-cursor-config)
  (emagent-acp-make-client :context-buffer context-buffer
                   :process-directory process-directory
                   :command (emagent-cursor-command)
                   :command-params (emagent-cursor-command-params-for-context context-buffer)
                   :environment-variables (emagent-cursor--environment context-buffer)))

(provide 'emagent-cursor)

;;; emagent-cursor.el ends here
