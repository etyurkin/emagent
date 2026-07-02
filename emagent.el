;;; emagent.el --- Emacs-native ACP chat assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; URL: https://github.com/etyurkin/emagent
;; Version: 1.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: comm tools

;;; Commentary:
;;
;; Emagent is a thin ACP chat client for Emacs-native tasks.  It talks to
;; external agents such as cursor-agent-acp or claude-agent-acp and executes
;; Emacs operations locally via org-mode scratch buffers.
;;
;; Main entry points: `emagent', `emagent-cursor-start', `emagent-claude-start',
;; `emagent-mode', and `emagent-set-model'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defmacro emagent--this-file ()
  "`.el' path for the file being loaded or byte-compiled."
  `(let ((file (or load-file-name
                   (when (boundp 'byte-compile-dest-file)
                     (let ((dest byte-compile-dest-file))
                       (when (string-match "\\.elc\\'" dest)
                         (substring dest 0 -1)))))))
     (when file
       (if (string-match "\\.elc\\'" file)
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

(defun emagent--mode-p ()
  "Return non-nil when `emagent-mode' is active in the current buffer."
  (and (boundp 'emagent-mode) emagent-mode))

(eval-and-compile (emagent--bootstrap-load-path))
(emagent--bootstrap-load-path)

(require 'emagent-acp-protocol)
(require 'emagent-log)
(require 'emagent-tools)
(require 'emagent-mcp)
(require 'emagent-context)
(require 'emagent-chat)
(require 'emagent-acp)
(require 'emagent-acp-model)
(require 'emagent-cursor)
(require 'emagent-claude)
(require 'emagent-trust)

(declare-function project-current "project")
(declare-function project-root "project")
(declare-function emagent-chat-append-thought "emagent-chat")
(declare-function emagent-chat-append-assistant "emagent-chat")
(declare-function emagent-chat-finish-assistant "emagent-chat")
(declare-function emagent-chat-fail-assistant "emagent-chat")
(declare-function emagent-chat-set-slash-commands "emagent-chat")
(declare-function emagent-chat--buffer-displayed-p "emagent-chat")
(declare-function emagent-acp--model-entries-from-response "emagent-acp-model")

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
  "Extra directories searched for agent binaries when not on `exec-path'.
When a binary is found in one of these directories, that directory is added
to `exec-path' so the agent process can also be started normally."
  :type '(repeat directory)
  :group 'emagent)

(defun emagent--find-executable (command)
  "Find COMMAND on `exec-path' or in `emagent-extra-exec-paths'.
When found via `emagent-extra-exec-paths', adds that directory to
`exec-path' so subsequent lookups and process starts succeed."
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

(defun emagent--make-client (provider buffer)
  "Create an ACP client for PROVIDER using BUFFER as context."
  (let ((process-directory (and (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (emagent-chat--session-directory)))))
    (pcase provider
      ('cursor (emagent-cursor-make-client :context-buffer buffer
                                           :process-directory process-directory))
      ('claude (emagent-claude-make-client :context-buffer buffer
                                           :process-directory process-directory))
      (_ (user-error "Unknown emagent provider: %s" provider)))))

(cl-defun emagent-acp-ensure-connected (&key on-ready on-reveal)
  "Connect the current emagent buffer to its ACP provider if needed.

When the agent process died but buffer-local state remains, tear it down and
reconnect (resuming the saved session id when present).  Optional ON-READY runs
once the session is ready; ON-REVEAL runs when the chat buffer should be shown."
  (if (emagent-acp--connected-p)
      (when on-ready (funcall on-ready))
    (emagent-acp--teardown-stale-session)
    (let* ((provider (or emagent-chat-provider emagent-default-provider))
           (client (emagent--make-client provider (current-buffer))))
      (emagent-acp-start :client client
                        :chat-buffer (current-buffer)
                        :on-ready on-ready
                        :on-reveal on-reveal
                        :callbacks
                        `((:cb-chunk          . ,#'emagent-chat-append-assistant)
                          (:cb-thought        . ,#'emagent-chat-append-thought)
                          (:cb-finish         . ,#'emagent-chat-finish-assistant)
                          (:cb-fail           . ,#'emagent-chat-fail-assistant)
                          (:cb-slash-commands . ,#'emagent-chat-set-slash-commands))))))

(defun emagent--send-prompt (user-text)
  "Ensure connection and send USER-TEXT from the current buffer."
  (emagent-acp-ensure-connected
   :on-ready (lambda ()
               (emagent-acp-send-prompt user-text))))

(defun emagent-chat--wire-buffer ()
  "Attach per-buffer emagent callbacks."
  (setq emagent-chat--on-send #'emagent--send-prompt
        emagent-chat--on-attach #'emagent-acp-attach-context
        emagent-chat--on-quit #'emagent-acp-shutdown-buffer
        emagent-chat-provider (or (emagent-chat-agent) emagent-default-provider)))

(defun emagent--on-mode-enable ()
  "Wire callbacks when enabling `emagent-mode'.

Do not auto-connect on mode activation; delayed ACP reconnect callbacks can
rewrite session metadata and mark restored buffers modified.  Connection still
happens on first send via `emagent--send-prompt'."
  (emagent-chat--wire-buffer)
  (emagent-chat--setup-faces)
  (add-hook 'kill-buffer-hook #'emagent-acp-shutdown-buffer nil t))

(defun emagent--session-buffer-p ()
  "Return non-nil when current `org-mode' buffer is an emagent session."
  (and (derived-mode-p 'org-mode)
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (let ((limit (min (+ (point-min) 4096) (point-max))))
             (or (looking-at-p "# -*- mode: emagent -*-")
                 (re-search-forward "^#\\+EMAGENT_SESSION:[ \t]*\\S-" limit t)))))))

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

(defvar emagent--force-activation nil
  "Non-nil to fully activate `emagent-mode' even for an undisplayed buffer.
Bound around explicit, user-initiated activation so the deferral advice does
not intercept it.")

(defvar emagent--pending-buffers nil
  "Session buffers awaiting first-display activation of `emagent-mode'.")

(defvar-local emagent--session-pending nil
  "Non-nil when this buffer is a session deferred until first display.")

(defun emagent--mark-session-pending ()
  "Leave the current buffer in `org-mode' and queue activation on first display."
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (setq-local emagent--session-pending t)
  (cl-pushnew (current-buffer) emagent--pending-buffers)
  (emagent-log "session deferred until displayed: %s" (buffer-name)))

(defun emagent--activate-session-now ()
  "Activate `emagent-mode' in the current buffer immediately."
  (setq emagent--pending-buffers (delq (current-buffer) emagent--pending-buffers))
  (kill-local-variable 'emagent--session-pending)
  (unless (derived-mode-p 'emagent-mode)
    (let ((emagent--force-activation t))
      (condition-case err
          (emagent-mode)
        (error
         (emagent-log "could not enable emagent-mode in %s: %s"
                      (or (buffer-name) "<dead-buffer>")
                      (error-message-string err)))))))

(defun emagent-mode--maybe-defer (orig-fn &rest args)
  "Defer ORIG-FN (`emagent-mode') for undisplayed session files on load.

When `emagent-activate-on-display' is on and `emagent-mode' is triggered for a
file-visiting buffer that is not yet displayed (e.g. the `mode: emagent' cookie
during `find-file' or desktop restore), keep the buffer in `org-mode' and mark
it pending instead.  Explicit activation (`emagent--force-activation') and
already-displayed buffers run normally."
  (if (and emagent-activate-on-display
           (not emagent--force-activation)
           buffer-file-name
           (not (emagent-chat--buffer-displayed-p)))
      (emagent--mark-session-pending)
    (apply orig-fn args)))

(advice-add 'emagent-mode :around #'emagent-mode--maybe-defer)

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

;; When emagent loads after session org files were already opened, register
;; them: activate the ones currently displayed, defer the rest.
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (and (not (derived-mode-p 'emagent-mode))
               (not emagent--session-pending)
               (emagent--session-buffer-p))
      (if (and emagent-activate-on-display
               (not (emagent-chat--buffer-displayed-p)))
          (emagent--mark-session-pending)
        (emagent--activate-session-now)))))

(add-hook 'emagent-mode-hook #'emagent--on-mode-enable)

(defun emagent--provider-available-p (provider)
  "Return non-nil when PROVIDER's ACP agent executable can be found.
Searches `exec-path' and `emagent-extra-exec-paths'; when found via the
latter, adds that directory to `exec-path' so the agent starts normally."
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
         (label (emagent-chat--model-choice-label-display id name)))
    (if omit-provider-prefix
        label
      (concat (propertize (symbol-name provider) 'face 'emagent-model-choice-agent)
              " - "
              label))))

(defun emagent--probe-provider-models (provider cwd)
  "Return model entries advertised by PROVIDER, or nil when probing fails."
  (emagent-log "probing %s models…" (symbol-name provider))
  (let ((buffer (get-buffer-create " *emagent-probe*"))
        result)
    (with-current-buffer buffer
      (setq default-directory (expand-file-name cwd))
      (setq result
            (condition-case err
                (let ((client (emagent--make-client provider buffer)))
                  (unwind-protect
                      (progn
                        (emagent-acp-send-request
                         :client client
                         :request (emagent-acp-make-initialize-request
                                   :protocol-version 1
                                   :client-info `((name . "emagent")
                                                  (title . "Emagent")
                                                  (version . "1.0.1")))
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
When MODEL-ID is non-nil, persist it before connecting."
  (let ((buffer (emagent-chat-open :project-dir project-dir)))
    (with-current-buffer buffer
      (when (eq provider 'cursor)
        (kill-local-variable 'emagent-chat-cursor-acp-extra-args))
      (emagent-chat-set-agent provider)
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

(declare-function emagent-trust--ensure-provider-features "emagent-trust")
(declare-function emagent-trust-claude-record-trust "emagent-trust-claude" (directory))
(declare-function emagent-trust-cursor-record-trust "emagent-trust-cursor" (directory))
(declare-function emagent-acp--connected-p "emagent-acp")
(declare-function emagent-claude-relocate-session "emagent-claude" (session-id old-dir new-dir))
(declare-function emagent-cursor-relocate-session "emagent-cursor" (session-id old-dir new-dir))

;;;###autoload
(defun emagent-trust-workspace (&optional both-agents)
  "Write on-disk workspace trust for Claude and/or Cursor.

Must be called from an `emagent-mode' buffer.  Uses the buffer's project
directory (`emagent-chat-project-directory') and agent (`emagent-chat-agent').

By default updates trust only for this buffer's agent.  With a prefix
argument, updates *both* Claude (=~/.claude.json=) and Cursor
(=~/.cursor/projects/...=).

This command does not re-run startup trust setup.  For Claude, the
running agent does not reload ~/.claude.json on session/load; after recording
trust use `emagent-trust-claude-reconnect' in this buffer (or clear
#+EMAGENT_SESSION and toggle `emagent-mode') so a new session picks it up."
  (interactive "P")
  (unless (derived-mode-p 'emagent-mode)
    (user-error "emagent-trust-workspace must be called from an emagent buffer"))
  (emagent-trust--ensure-provider-features)
  (let* ((dir0 (or (emagent-chat-project-directory)
                   (user-error "No project directory set in this buffer")))
         (dir (emagent-trust--normalize-dir (expand-file-name dir0)))
         (providers (if both-agents
                        '(claude cursor)
                      (list (or (emagent-chat-agent)
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
  (unless (eq (emagent-chat-agent) 'claude)
    (user-error "This buffer's agent is not Claude (see #+EMAGENT_AGENT)"))
  (emagent-chat-clear-session-id)
  (when emagent-acp--session
    (emagent-acp-shutdown-buffer))
  (emagent-acp-ensure-connected)
  (message "emagent: Claude reconnected with a new session (trust from disk)"))

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
(defun emagent-cursor-start (&optional prompt-directory)
  "Start a new emagent session with the Cursor provider.

The project directory is inferred from the current buffer; with a prefix
argument PROMPT-DIRECTORY, read it interactively instead."
  (interactive "P")
  (emagent--start-session (emagent--project-directory prompt-directory) 'cursor))

;;;###autoload
(defun emagent-claude-start (&optional prompt-directory)
  "Start a new emagent session with the Claude ACP provider.

The project directory is inferred from the current buffer; with a prefix
argument PROMPT-DIRECTORY, read it interactively instead."
  (interactive "P")
  (emagent--start-session (emagent--project-directory prompt-directory) 'claude))

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
                               (emagent-chat-project-directory)
                               nil t))))
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Must be called from an emagent buffer"))
  (let* ((old-dir (emagent-chat-project-directory))
         (new-dir (directory-file-name (expand-file-name new-dir)))
         (old-dir-norm (and old-dir (directory-file-name (expand-file-name old-dir)))))
    (when (equal old-dir-norm new-dir)
      (user-error "Directory is already %s" new-dir))
    (let* ((session-id (emagent-chat-session-id)))
      ;; Update provider-specific session storage for the new directory.
      (when (and session-id (not (string-empty-p session-id)) old-dir-norm)
        (pcase emagent-chat-provider
          ('claude (emagent-claude-relocate-session session-id old-dir-norm new-dir))
          ('cursor (emagent-cursor-relocate-session session-id old-dir-norm new-dir))))
      ;; Update the buffer header and reconnect.
      (emagent-chat-set-project-directory new-dir)
      (when (bound-and-true-p emagent-acp--session)
        (emagent-acp-shutdown-buffer))
      (emagent-acp-ensure-connected)
      (message "emagent: project → %s, reconnecting…" new-dir))))

(dolist (sym '(emagent-trust-workspace
               emagent-trust-claude-reconnect
               emagent-set-project-directory
               emagent-cursor-start
               emagent-claude-start
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
               emagent-chat-new-prompt
               emagent-chat-quit
               emagent-chat-tab
               emagent-dispatch
               emagent-btw
               emagent-log-view
               emagent-log-refresh
               emagent-set-model
               emagent-tool-eval
               emagent-tool-org-move-subtree-to-parent
               emagent-embark-copy-src-block
               emagent-embark-insert-src-block))
  (put sym 'completion-predicate #'emagent--mode-p))

(provide 'emagent)

;;; emagent.el ends here
