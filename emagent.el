;;; emagent.el --- Emacs-native ACP chat assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; SPDX-License-Identifier: MIT

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; URL: https://github.com/etyurkin/emagent
;; Version: 1.3.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: comm tools

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
;; Emagent is a thin ACP chat client for Emacs-native tasks.  It talks to
;; external agents such as cursor-agent-acp or claude-agent-acp and executes
;; Emacs operations locally via org-mode scratch buffers.
;;
;; Public entry points (autoloaded):
;; - `emagent' — start a session (probes agents / models, then connects)
;; - `emagent-mode' — reconnect a saved chat buffer
;; - `emagent-connect' — connect/reconnect without starting a new buffer
;; - `emagent-trust-workspace' / `emagent-trust-claude-reconnect'
;;   — workspace trust helpers
;; - `emagent-set-project-directory' — move a session to a new project cwd
;;
;; Every other command is scoped to `emagent-mode' buffers (`C-c ?' / M-x once
;; the package is loaded).  Internals use the `emagent--*' prefix and are not
;; a supported API.
;;
;; Layers (one-way require DAG):
;; - L0 protocol/state/log primitives
;; - L1 tools, session permissions, MCP server
;; - L2 chat buffer/mode/markup UI (does not require ACP runtime)
;; - L3 ACP connect/prompt/permission runtime (wires chat callbacks)
;; - L4 this file — package facade and public commands

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defmacro emagent--this-file ()
  "`.el' path for the file being loaded or byte-compiled.
Resolved at macroexpansion time via `macroexp-file-name', which works during
both byte-compilation and loading (unlike function `byte-compile-dest-file',
which is a function, not a bound variable, on Emacs 29+)."
  (let ((file (or (macroexp-file-name) load-file-name)))
    (and file
         (if (string-suffix-p ".elc" file)
             (substring file 0 -1)
           file))))

(defmacro emagent--bootstrap-load-path ()
  "Load `emagent-load-path' and register grouped `lisp/' directories."
  `(let ((this-file (emagent--this-file)))
     (when this-file
       (let* ((root (file-name-directory (file-truename this-file)))
              (bootstrap (expand-file-name "lisp/core/emagent-load-path.el" root)))
         (unless (featurep 'emagent-load-path)
           (if (file-exists-p bootstrap)
               (load bootstrap nil t)
             (require 'emagent-load-path nil t)))
         (emagent--register-load-path root)
         (emagent--register-elpaca-recipe)))))

(defun emagent--mode-p (_symbol buffer)
  "Completion predicate: include the command only in `emagent-mode' buffers.
Called by `command-completion-default-include-p' with the command SYMBOL and the
target BUFFER (hence the two-argument signature)."
  (provided-mode-derived-p
   (buffer-local-value 'major-mode buffer) 'emagent-mode))

(eval-and-compile (emagent--bootstrap-load-path))
(emagent--bootstrap-load-path)

(require 'emagent-acp-protocol)
(require 'emagent-log)
(require 'emagent-tools)
(require 'emagent-chat)
(require 'emagent-session)
(require 'emagent-tools-compact)
(require 'emagent-tools-age)
(require 'emagent-usage)
(require 'emagent-archive)
(require 'emagent-acp)
(require 'emagent-cursor)
(require 'emagent-claude)
(require 'project)

;; Optional integrations — loaded only when the dependency is present.
(require 'emagent-consult nil t)
(require 'emagent-embark nil t)

(defgroup emagent nil
  "Emacs-native ACP chat assistant."
  :group 'tools
  :prefix "emagent-")

(defcustom emagent-default-provider 'cursor
  "Default emagent provider (`cursor' or `claude')."
  :type '(choice (const cursor) (const claude))
  :group 'emagent)

(defcustom emagent-extra-exec-paths
  (mapcar #'expand-file-name
          '("~/.local/bin" "/usr/local/bin" "/opt/homebrew/bin"
            "/opt/homebrew/sbin" "/opt/local/bin"))
  "Extra directories searched for agent binaries when not on variable `exec-path'.
When a binary is found in one of these directories, that directory is added
to variable `exec-path' so the agent process can also be started normally."
  :type '(repeat directory)
  :group 'emagent)

(defun emagent--find-executable (command)
  "Find COMMAND on variable `exec-path' or in `emagent-extra-exec-paths'.
When found via `emagent-extra-exec-paths', adds that directory to
variable `exec-path' so subsequent lookups and process starts succeed."
  (or (executable-find command)
      (seq-some (lambda (dir)
                  (let ((path (expand-file-name command dir)))
                    (when (file-executable-p path)
                      (cl-pushnew dir exec-path :test #'equal)
                      path)))
                emagent-extra-exec-paths)))

(defcustom emagent-probe-models-at-start t
  "When non-nil, query installed agents for models when starting emagent.

Each agent is contacted briefly (initialize + session/new).  Available
combinations are logged to `emagent-log-buffer-name' and offered in the
startup picker when there is more than one choice."
  :type 'boolean
  :group 'emagent)

(defun emagent--provider-available-p (provider)
  "Return non-nil when PROVIDER's ACP agent executable can be found.
Searches variable `exec-path' and `emagent-extra-exec-paths'; when found
via the latter, adds that directory to variable `exec-path' so the agent
starts normally."
  (when-let ((command (pcase provider
                        ('cursor (emagent-cursor-command))
                        ('claude (emagent-claude-command)))))
    (emagent--find-executable command)))

(defun emagent--available-providers ()
  "Return the providers whose ACP agent is installed, in preference order."
  (seq-filter #'emagent--provider-available-p '(cursor claude)))

(defun emagent--read-provider ()
  "Return the provider to use for a new session.

Probe the installed ACP agents: when only one is installed use it without
prompting; when both are installed prompt (defaulting to
`emagent-default-provider'); when none are detected fall back to
`emagent-default-provider' so connecting reports a clear install error."
  (let ((available (emagent--available-providers)))
    (pcase available
      ('nil emagent-default-provider)
      (`(,only) only)
      (_ (let ((default (if (memq emagent-default-provider available)
                            emagent-default-provider
                          (car available))))
           (intern (completing-read
                    (format "Emagent agent (default %s): " default)
                    (mapcar #'symbol-name available)
                    nil t nil nil (symbol-name default))))))))

(defun emagent--agent-model-label (provider entry &optional omit-provider-prefix)
  "Return a display label for PROVIDER and model ENTRY.
When OMIT-PROVIDER-PREFIX is non-nil, return the model id only."
  (let* ((id (or (map-elt entry :model-id) (emagent-acp--model-entry-id entry)))
         (name (or (map-elt entry :name) (emagent-acp--model-entry-name entry)))
         (label (emagent-model-choice-label-display id name)))
    (if omit-provider-prefix
        label
      (concat (propertize (symbol-name provider) 'face 'emagent-model-choice-agent)
              " - "
              label))))

(defun emagent--probe-provider-models (provider cwd)
  "Return model entries advertised by PROVIDER at CWD, or nil on failure."
  (emagent-log "probing %s models…" (symbol-name provider))
  (let ((buffer (get-buffer-create " *emagent-probe*"))
        result)
    (with-current-buffer buffer
      (setq default-directory (expand-file-name cwd))
      (setq result
            (condition-case err
                (let ((client (emagent-acp--make-client provider buffer)))
                  (unwind-protect
                      (progn
                        (emagent-acp-send-request
                         :client client
                         :request (emagent-acp-make-initialize-request
                                   :protocol-version 1
                                   :client-info `((name . "emagent")
                                                  (title . "Emagent")
                                                  (version . "1.0.2")))
                         :sync t)
                        (emagent-acp--model-entries-from-response
                         (emagent-acp-send-request
                          :client client
                          :request (emagent-acp-make-session-new-request
                                    :cwd default-directory
                                    :mcp-servers [])
                          :sync t)))
                    (emagent-acp-shutdown :client client)))
              (error
               (let ((msg (error-message-string err)))
                 (emagent-log "could not probe %s: %s" (symbol-name provider) msg)
                 (when (string-match-p "uthenti\\|login\\|auth" msg)
                   (message "emagent: %s probe failed — authentication required (run '%s login')"
                            provider
                            (or (emagent-cursor-command) (symbol-name provider)))))
               nil))))
    (ignore-errors (kill-buffer buffer))
    result))

(defun emagent--agent-model-choices (cwd &optional providers)
  "Return ((LABEL . (PROVIDER . MODEL-ID)) ...) for PROVIDERS at CWD."
  (let ((providers (or providers (emagent--available-providers)))
        (omit-prefix (= (length providers) 1))
        choices)
    (dolist (provider providers)
      (dolist (entry (or (emagent--probe-provider-models provider cwd) '()))
        (push (cons (emagent--agent-model-label provider entry omit-prefix)
                    (cons provider
                          (or (map-elt entry :model-id)
                              (emagent-acp--model-entry-id entry))))
              choices)))
    (sort choices (lambda (a b)
                    (string-lessp (substring-no-properties (car a))
                                  (substring-no-properties (car b)))))))

(defun emagent--read-agent-and-model (cwd &optional fixed-provider)
  "Return (PROVIDER . MODEL-ID) for a new session at CWD.

When FIXED-PROVIDER is non-nil, only that agent is probed.  Falls back to
`emagent--read-provider' when probing is disabled or returns no models."
  (if (not emagent-probe-models-at-start)
      (cons (or fixed-provider (emagent--read-provider)) nil)
    (let* ((providers (if fixed-provider (list fixed-provider)
                        (emagent--available-providers)))
           (choices (emagent--agent-model-choices cwd providers)))
      (if (null choices)
          (cons (or fixed-provider (emagent--read-provider)) nil)
        (emagent-log "available agents/models:")
        (dolist (choice choices)
          (emagent-log "  %s" (car choice)))
        (if (= (length choices) 1)
            (cdr (car choices))
          (let* ((labels (mapcar #'car choices))
                 (selection (emagent-acp--read-labeled-choice
                             "Emagent agent - model: "
                             labels)))
            (or (cdr (assoc-string selection choices))
                (user-error "Unknown agent/model: %s" selection))))))))

(defun emagent--project-directory (prompt)
  "Return a project directory for a new session.

When PROMPT is non-nil, read it interactively; otherwise infer it from the
current buffer."
  (if prompt
      (emagent--read-project-directory)
    (emagent--project-directory-initial)))

(defun emagent--start-with-provider (provider project-dir connect &optional model-id _handshake)
  "Start emagent using PROVIDER in PROJECT-DIR.
CONNECT non-nil connects the ACP session immediately.
When MODEL-ID is non-nil, persist it before connecting."
  (let ((buffer (emagent-chat-open :project-dir project-dir)))
    (with-current-buffer buffer
      (when (eq provider 'cursor)
        (kill-local-variable 'emagent-chat-cursor-acp-extra-args))
      (emagent-session-set-agent provider)
      (when (and model-id (not (string-empty-p model-id)))
        (emagent-chat-set-model model-id))
      (if connect
          (let ((reveal (lambda () (pop-to-buffer buffer))))
            (emagent-acp-ensure-connected :on-reveal reveal))
        (pop-to-buffer buffer)))
    buffer))

(defun emagent--start-session (project-dir &optional fixed-provider)
  "Start emagent in PROJECT-DIR, optionally limiting to FIXED-PROVIDER."
  (let* ((pair (emagent--read-agent-and-model project-dir fixed-provider))
         (provider (car pair))
         (handshake (emagent-trust--configure provider project-dir)))
    (emagent--start-with-provider provider project-dir t (cdr pair) handshake)))

(defun emagent--project-directory-initial ()
  "Default project directory for a new emagent session.
Uses the current buffer's cwd when it is a shell or file-backed buffer,
otherwise the project.el root, otherwise ~/."
  (cond
   ((derived-mode-p 'shell-mode 'eshell-mode 'term-mode 'vterm-mode)
    default-directory)
   (buffer-file-name
    (or (and (fboundp 'project-current)
             (when-let ((proj (project-current nil (file-name-directory buffer-file-name))))
               (project-root proj)))
        (file-name-directory buffer-file-name)))
   ((and (fboundp 'project-current)
         (when-let ((proj (project-current nil default-directory)))
           (project-root proj))))
   (t
    (expand-file-name "~/"))))

(defun emagent--read-project-directory ()
  "Read and return an absolute project directory for a new emagent session."
  (expand-file-name
   (read-directory-name "Emagent project directory: "
                        (emagent--project-directory-initial)
                        nil t)))

;;;###autoload
(defun emagent-trust-workspace (&optional both-agents)
  "Write on-disk workspace trust for Claude and/or Cursor.

Must be called from an `emagent-mode' buffer.  Uses the buffer's
project directory (`emagent-session-project-directory') and agent
\(`emagent-session-agent').

BOTH-AGENTS non-nil (the prefix argument) updates *both* Claude
\(=~/.claude.json=) and Cursor
\(=~/.cursor/projects/...=).  Otherwise only this buffer's agent is updated.

This command does not re-run startup trust setup.  For Claude, the
running agent does not reload ~/.claude.json on session/load; after recording
trust use `emagent-trust-claude-reconnect' in this buffer (or clear
#+EMAGENT_SESSION and toggle `emagent-mode') so a new session picks it up."
  (interactive "P")
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Emagent-trust-workspace must be called from an emagent buffer"))
  (let* ((dir0 (or (emagent-session-project-directory)
                   (user-error "No project directory set in this buffer")))
         (dir (emagent-trust--normalize-dir (expand-file-name dir0)))
         (providers (if both-agents
                        '(claude cursor)
                      (list (or (emagent-session-agent)
                                (user-error "No agent set in this buffer")))))
         (reconnect-hint
          (when (emagent-acp--connected-p)
            (if (memq 'claude providers)
                (concat "  Claude does not re-read trust on session/load — run"
                        " `M-x emagent-trust-claude-reconnect' (or clear"
                        " #+EMAGENT_SESSION and toggle emagent-mode).")
              "  Restart emagent for trust to take effect."))))
    (when (file-remote-p dir)
      (user-error "Remote directories are not supported: %s" dir))
    (dolist (p providers)
      (pcase p
        ('claude (emagent-trust-claude-record-trust dir))
        ('cursor (emagent-trust-cursor-record-trust dir))))
    (message "Recorded trust for %s (%s).%s"
             (mapconcat #'symbol-name providers ", ")
             dir
             (or reconnect-hint ""))))

;;;###autoload
(defun emagent-trust-claude-reconnect ()
  "Clear this buffer's saved ACP session and reconnect Claude.

Claude Code reads ~/.claude.json trust when a new session starts.
Resuming #+EMAGENT_SESSION (ACP session/load) keeps the old agent semantics,
so trust recorded via \\`emagent-trust-workspace' can have no effect until you
drop the saved session id and connect again.

Requires `emagent-mode' in a buffer whose agent is Claude."
  (interactive)
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Turn on emagent-mode in this buffer first"))
  (unless (eq (emagent-session-agent) 'claude)
    (user-error "This buffer's agent is not Claude (see #+EMAGENT_AGENT)"))
  (emagent-session-clear-id)
  (when emagent-acp--session
    (emagent-acp-shutdown-buffer))
  (emagent-acp-ensure-connected)
  (message "emagent: Claude reconnected with a new session (trust from disk)"))

;;;###autoload
(defun emagent-connect ()
  "Connect or reconnect this buffer's ACP agent.

Use after opening a saved session when you want slash commands, models, or
MCP state before the first prompt.  Safe to call when already connected —
queued ready callbacks run immediately.  Cursor built-in slash commands are
also re-seeded so TAB completion stays populated during reconnect."
  (interactive)
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Turn on emagent-mode in this buffer first"))
  (emagent-chat-seed-cursor-slash-commands)
  (emagent-acp-ensure-connected
   :on-ready
   (lambda ()
     (message "emagent: connected"))))

;;;###autoload
(defun emagent (&optional prompt-directory)
  "Start a new emagent session and connect.

Infers the project directory from the current buffer, probes installed
agents for models when `emagent-probe-models-at-start' is non-nil, and
prompts when more than one agent/model combination is available.  After you
pick an agent, workspace trust is checked (`emagent-trust--configure'); see
`emagent-trust-enabled'.  With a prefix argument PROMPT-DIRECTORY, read the
project directory instead."
  (interactive "P")
  (emagent--start-session (emagent--project-directory prompt-directory)))

;;;###autoload
(defun emagent-set-project-directory (new-dir)
  "Change this buffer's project directory, preserving the Claude session.

1. Prompts for NEW-DIR interactively.
2. Moves the Claude session files from the old project hash folder to the
   new one so `session/load' can find them after the cwd changes.
3. Updates #+EMAGENT_PROJECT in the buffer header.
4. Reconnects the agent (the existing session ID is preserved).

Use this instead of editing #+EMAGENT_PROJECT manually — a manual edit
leaves the session files in the old location and causes session/load to fail."
  (interactive
   (list (expand-file-name
          (read-directory-name "New project directory: "
                               (emagent-session-project-directory)
                               nil t))))
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Must be called from an emagent buffer"))
  (let* ((old-dir (emagent-session-project-directory))
         (new-dir (directory-file-name (expand-file-name new-dir)))
         (old-dir-norm (and old-dir (directory-file-name (expand-file-name old-dir)))))
    (when (equal old-dir-norm new-dir)
      (user-error "Directory is already %s" new-dir))
    (let* ((session-id (emagent-session-id)))
      ;; Update provider-specific session storage for the new directory.
      (when (and session-id (not (string-empty-p session-id)) old-dir-norm)
        (pcase emagent-chat-provider
          ('claude (emagent-claude-relocate-session session-id old-dir-norm new-dir))
          ('cursor (emagent-cursor-relocate-session session-id old-dir-norm new-dir))))
      ;; Update the buffer header and reconnect.
      (emagent-session-set-project-directory new-dir)
      (when (bound-and-true-p emagent-acp--session)
        (emagent-acp-shutdown-buffer))
      (emagent-acp-ensure-connected)
      (message "emagent: project → %s, reconnecting…"
               (emagent-session-store-display-project-directory new-dir)))))

(dolist (sym '(emagent-trust-workspace
               emagent-trust-claude-reconnect
               emagent-connect
               emagent-usage
               emagent-set-project-directory
               emagent-chat-send
               emagent-chat-send-or-babel
               emagent-chat-beginning-of-line
               emagent-chat-cycle-response
               emagent-chat-cycle-or-org-cycle
               emagent-chat-insert-last-response
               emagent-chat-insert-src-block
               emagent-chat-attach-buffer
               emagent-chat-yank
               emagent-chat-attach-image
               emagent-chat-attach-error-context
               emagent-chat-attach-files
               emagent-chat-interrupt
               emagent-acp-interrupt
               emagent-chat-new-prompt
               emagent-chat-quit
               emagent-chat-tab
               emagent-chat-history-next
               emagent-chat-history-next-or-next-line
               emagent-chat-history-previous
               emagent-chat-history-previous-or-previous-line
               emagent-chat-permission-prompt
               emagent-dispatch
               emagent-btw
               emagent-reset-permissions
               emagent-log-view
               emagent-log-refresh
               emagent-set-model
               emagent-tool-eval
               emagent-tools--buttons-prompt
               emagent-tool-org-move-subtree-to-parent
               emagent-embark-copy-src-block
               emagent-embark-insert-src-block))
  (put sym 'completion-predicate #'emagent--mode-p))

(provide 'emagent)

;;; emagent.el ends here
