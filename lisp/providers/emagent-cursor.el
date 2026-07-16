;;; emagent-cursor.el --- Cursor provider config for emagent -*- lexical-binding: t; -*-

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

;; Cursor ACP provider configuration.

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
  '("--sandbox" "disabled")
  "Extra arguments appended after \"acp\" when spawning cursor-agent.

Cursor documents these flags on the top-level agent command; passing them
here may still take effect depending on the CLI version.  The default
disables the agent sandbox so MCP and shell tools reach emagent; Emacs
tool permission prompts use ACP `session/request_permission' (see
`emagent-acp-auto-approve-permissions').  Set to nil to pass no extra flags."
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
invocation to its own in-Emacs MCP session.

Arguments: CONTEXT-BUFFER."
  (let ((token (with-current-buffer context-buffer (emagent-mcp-buffer-token))))
    (append emagent-cursor-environment
            (list (format "EMAGENT_SESSION_TOKEN=%s" token)))))

(defun emagent-cursor--json-field (data key)
  "Return KEY from JSON DATA alist or hash-table."
  (let ((str (symbol-name key)))
    (cond
     ((hash-table-p data)
      (or (gethash key data)
          (gethash str data)
          (gethash (downcase str) data)))
     ((listp data)
      (or (alist-get key data)
          ;; String keys need `equal'; the default `eq' never matches them.
          (alist-get str data nil nil #'equal)
          (alist-get (downcase str) data nil nil #'equal)))
     (t nil))))

(defun emagent-cursor--tool-call-raw-empty-p (raw)
  "Return non-nil when Cursor ACP rawInput/arguments carry no parameters.

Arguments: RAW."
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

(defconst emagent-cursor--store-recent-blobs-sql
  "SELECT cast(data as text) FROM blobs ORDER BY rowid DESC LIMIT 120;"
  "SQL to fetch recent Cursor store.db blobs for tool-call arg lookup.

Avoids `cast(data as text) LIKE …` full-table scans, which block Emacs for
tens of milliseconds on large sessions when run via `shell-command-to-string'.")

(defun emagent-cursor--tool-call-from-sqlite-stdout (out tool-call-id)
  "Parse sqlite3 OUT for TOOL-CALL-ID; return (NAME . ARGS) or nil."
  (when (and (stringp out) (not (string-empty-p out)))
    (catch 'found
      (let ((variants (emagent-cursor--tool-call-id-variants tool-call-id)))
        (dolist (line (split-string out "\n" t))
          (dolist (variant variants)
            (when-let ((entry (emagent-cursor--tool-call-from-blob-json line variant)))
              (throw 'found entry))))))))

(defun emagent-cursor-tool-call-from-store-async (session-id tool-call-id callback)
  "Look up TOOL-CALL-ID in Cursor store.db asynchronously.

CALLBACK is called with (TOOL-NAME . ARGS-ALIST) or nil.  Never blocks the
Emacs command loop with `shell-command-to-string'.  When the lookup cannot
start (missing db/sqlite), CALLBACK still runs on a timer so callers never
re-enter synchronously from this helper.

Arguments: SESSION-ID, TOOL-CALL-ID, CALLBACK."
  (cl-labels
      ((done (entry)
         ;; Always defer: a sync callback here re-enters the Cursor tool-resolve
         ;; finish/drain path on the same stack (and can nest via timer_check).
         (run-at-time 0 nil (lambda () (funcall callback entry)))))
    (let ((db (emagent-cursor--store-db-path session-id))
          (sqlite (executable-find "sqlite3")))
      (if (not (and db sqlite (file-readable-p db)))
          (done nil)
        (let ((buf (generate-new-buffer " *emagent-cursor-sqlite*")))
          (condition-case _err
              (let ((proc (start-process "emagent-cursor-sqlite" buf
                                        sqlite db
                                        emagent-cursor--store-recent-blobs-sql)))
                (set-process-query-on-exit-flag proc nil)
                (set-process-sentinel
                 proc
                 (lambda (p _event)
                   (when (memq (process-status p) '(exit signal))
                     (let* ((ok (zerop (process-exit-status p)))
                            (out (when (and ok (buffer-live-p buf))
                                   (with-current-buffer buf (buffer-string))))
                            (entry (and out (emagent-cursor--tool-call-from-sqlite-stdout
                                             out tool-call-id))))
                       (when (buffer-live-p buf) (kill-buffer buf))
                       (done entry))))))
            (error
             (when (buffer-live-p buf) (kill-buffer buf))
             (done nil))))))))

(defun emagent-cursor--parse-blob-json (text)
  "Parse JSON embedded in a store.db blob TEXT, skipping binary prefixes."
  (when (and (stringp text) (not (string-empty-p text)))
    (let ((start (or (string-match-p "{\"role\":" text)
                     (string-match-p "{\"id\":" text))))
      (when start
        (condition-case nil
            (json-parse-string (substring text start)
                               :object-type 'alist
                               :array-type 'list
                               :null-object nil
                               :false-object nil)
          (error nil))))))

(defun emagent-cursor--tool-call-from-blob-json (json tool-call-id)
  "Parse assistant/tool blob JSON and return (NAME . ARGS) for TOOL-CALL-ID."
  (when-let* ((data (or (condition-case nil
                           (json-parse-string json
                                              :object-type 'alist
                                              :array-type 'list
                                              :null-object nil
                                              :false-object nil)
                         (error nil))
                        (emagent-cursor--parse-blob-json json)))
              (content (emagent-cursor--json-field data 'content)))
    (catch 'found
      (dolist (item (if (vectorp content) (append content nil) (append content nil)))
        (when (and (equal (emagent-cursor--json-field item 'type) "tool-call")
                   (equal (emagent-cursor--json-field item 'toolCallId) tool-call-id))
          (let ((name (emagent-cursor--json-field item 'toolName))
                (args (emagent-cursor--json-field item 'args)))
            (when name
              (throw 'found (cons name
                                  (unless (emagent-cursor--tool-call-raw-empty-p args)
                                    args))))))))))

(defun emagent-cursor-tool-call-from-store (session-id tool-call-id)
  "Return (TOOL-NAME . ARGS-ALIST) for TOOL-CALL-ID from Cursor store.db.

Scans only recent blobs (tool calls resolve within seconds of creation).
Prefer `emagent-cursor-tool-call-from-store-async' on the interactive path —
this synchronous helper is for tests and noninteractive fallbacks.

Arguments: SESSION-ID, TOOL-CALL-ID."
  (when-let* ((db (emagent-cursor--store-db-path session-id))
              (sqlite (executable-find "sqlite3"))
              (out (shell-command-to-string
                    (format "%s %s %s"
                            (shell-quote-argument sqlite)
                            (shell-quote-argument db)
                            (shell-quote-argument
                             emagent-cursor--store-recent-blobs-sql)))))
    (emagent-cursor--tool-call-from-sqlite-stdout out tool-call-id)))

(defconst emagent-cursor--generic-acp-titles
  '("MCP" "Read File" "Read" "Edit" "Write" "grep" "Grep" "Shell" "tool")
  "ACP tool titles replaced by store.db toolName when available.")

(defun emagent-cursor--generic-acp-title-p (title)
  "Return non-nil when Cursor ACP TITLE is too generic to keep after store lookup."
  (and (stringp title)
       (let ((trimmed (string-trim title)))
         (or (member trimmed emagent-cursor--generic-acp-titles)
             (string-match-p "\\`MCP" trimmed)))))

(defun emagent-cursor--tool-display-name (name)
  "Return a concise label for Cursor store.db toolName NAME."
  (cond
   ((not (stringp name)) "tool")
   ((string-match "\\`mcp_emagent_\\(.+\\)\\'" name)
    (match-string 1 name))
   ((string-match "\\`mcp_\\(.+\\)\\'" name)
    (match-string 1 name))
   (t name)))

(defun emagent-cursor--enriched-tool-title (acp-title store-name)
  "Prefer store NAME over generic ACP TITLE.

Arguments: ACP-TITLE, STORE-NAME."
  (let ((display (emagent-cursor--tool-display-name store-name)))
    (if (emagent-cursor--generic-acp-title-p acp-title)
        display
      (or acp-title display))))

(defun emagent-cursor--update-put (update key value)
  "Return UPDATE alist with KEY bound to VALUE, replacing any prior binding."
  (cons (cons key value) (assoc-delete-all key update)))

(defun emagent-cursor--emagent-store-tool-p (store-name)
  "Return non-nil when Cursor store toolName STORE-NAME is an emagent MCP tool."
  (and (stringp store-name)
       (string-match-p "\\`mcp_emagent_" store-name)))

(defun emagent-cursor--apply-store-entry (update entry)
  "Return UPDATE enriched with store.db ENTRY (NAME . ARGS), or UPDATE."
  (if (not entry)
      update
    (let ((enriched
           (emagent-cursor--update-put
            (emagent-cursor--update-put
             (emagent-cursor--update-put
              (assoc-delete-all 'rawInput (assoc-delete-all 'arguments update))
              'rawInput (or (cdr entry) '()))
             'title (emagent-cursor--enriched-tool-title
                     (map-elt update 'title) (car entry)))
            'subtitle nil)))
      (if (emagent-cursor--emagent-store-tool-p (car entry))
          (emagent-cursor--update-put enriched 'emagent-tool t)
        enriched))))

(defun emagent-cursor-enrich-tool-call-update (session-id update)
  "Fill empty rawInput in UPDATE from Cursor store.db when available.

Interactive Cursor tool-call display must not call this on the ACP process
filter — use `emagent-cursor-tool-call-from-store-async' via the resolve
queue instead.  This synchronous helper remains for tests and offline use.

Arguments: SESSION-ID, UPDATE."
  (let ((raw (or (map-elt update 'rawInput) (map-elt update 'arguments))))
    (if (emagent-cursor--tool-call-raw-empty-p raw)
        (or (when-let* ((id (map-elt update 'toolCallId))
                        (entry (emagent-cursor-tool-call-from-store session-id id)))
              (emagent-cursor--apply-store-entry update entry))
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
  "Return Cursor slash commands for completion: built-ins, custom, then project.

Arguments: PROJECT-DIR."
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
\(see `emagent-chat--session-directory' / #+EMAGENT_PROJECT)."
  (emagent-cursor-check-command)
  (emagent-mcp-ensure-cursor-config)
  (emagent-acp-make-client :context-buffer context-buffer
                   :process-directory process-directory
                   :command (emagent-cursor-command)
                   :command-params (emagent-cursor-command-params-for-context context-buffer)
                   :environment-variables (emagent-cursor--environment context-buffer)))

(defun emagent-cursor-relocate-session (session-id _old-dir new-dir)
  "Update the cwd in Cursor's meta.json for SESSION-ID to NEW-DIR.
Cursor sessions live at ~/.cursor/acp-sessions/<session-id>/ (flat, not
project-hashed), so only meta.json needs updating when the project changes."
  (let* ((session-dir (expand-file-name session-id
                                        (expand-file-name "acp-sessions"
                                                          emagent-cursor-dir)))
         (meta-file (expand-file-name "meta.json" session-dir)))
    (when (file-readable-p meta-file)
      (condition-case err
          (let* ((json (with-temp-buffer
                         (insert-file-contents meta-file)
                         (json-parse-buffer :object-type 'alist)))
                 (updated (cons (cons 'cwd new-dir)
                                (assoc-delete-all 'cwd (append json nil)))))
            (with-temp-file meta-file
              (insert (json-encode updated))
              (insert "\n"))
            (message "emagent: updated Cursor session cwd → %s" new-dir))
        (error
         (message "emagent: could not update Cursor meta.json: %s"
                  (error-message-string err)))))))

(provide 'emagent-cursor)

;;; emagent-cursor.el ends here
