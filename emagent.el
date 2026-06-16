;;; emagent.el --- Emacs-native ACP chat assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; URL: https://github.com/etyurkin/emagent
;; Version: 0.1.0
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

(require 'emagent-acp-protocol)
(require 'emagent-log)
(require 'emagent-tools)
(require 'emagent-mcp)
(require 'emagent-context)
(require 'emagent-chat)
(require 'emagent-acp)
(require 'emagent-cursor)
(require 'emagent-claude)

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
  (pcase provider
    ('cursor (emagent-cursor-make-client :context-buffer buffer))
    ('claude (emagent-claude-make-client :context-buffer buffer))
    (_ (user-error "Unknown emagent provider: %s" provider))))

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
  "Wire callbacks and reconnect ACP when enabling `emagent-mode'."
  (emagent-chat--wire-buffer)
  (add-hook 'kill-buffer-hook #'emagent-acp-shutdown-buffer nil t)
  (when (or (emagent-chat-project-directory)
            (and (emagent-chat-session-id)
                 (not (string-empty-p (emagent-chat-session-id)))))
    (emagent-acp-ensure-connected)))

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

(defun emagent--agent-model-label (provider entry)
  "Return a display label for PROVIDER and model ENTRY."
  (format "%s - %s"
          (symbol-name provider)
          (or (map-elt entry :name) (map-elt entry :model-id))))

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
                                                  (version . "0.1.0")))
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
               (emagent-log "could not probe %s: %s"
                           (symbol-name provider)
                           (error-message-string err))
               nil))))
    result))

(defun emagent--agent-model-choices (cwd &optional providers)
  "Return ((LABEL . (PROVIDER . MODEL-ID)) ...) for PROVIDERS at CWD."
  (let ((providers (or providers (emagent--available-providers)))
        choices)
    (dolist (provider providers)
      (dolist (entry (or (emagent--probe-provider-models provider cwd) '()))
        (push (cons (emagent--agent-model-label provider entry)
                    (cons provider (map-elt entry :model-id)))
              choices)))
    (sort choices (lambda (a b) (string-lessp (car a) (car b))))))

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
          (let* ((default-label
                   (car (or (seq-find (lambda (choice)
                                        (eq (car (cdr choice)) emagent-default-provider))
                                      choices)
                            choices)))
                  (selection (completing-read
                              "Emagent agent - model: "
                              (mapcar #'car choices)
                              nil t nil nil default-label)))
             (cdr (assoc-string selection choices))))))))

(defun emagent--project-directory (prompt)
  "Return a project directory for a new session.

When PROMPT is non-nil, read it interactively; otherwise infer it from the
current buffer."
  (if prompt
      (emagent--read-project-directory)
    (emagent--project-directory-initial)))

(defun emagent--start-with-provider (provider project-dir connect &optional model-id)
  "Start emagent using PROVIDER in PROJECT-DIR.
When MODEL-ID is non-nil, persist it before connecting."
  (let ((buffer (emagent-chat-open :project-dir project-dir)))
    (with-current-buffer buffer
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
  (let ((pair (emagent--read-agent-and-model project-dir fixed-provider)))
    (emagent--start-with-provider (car pair) project-dir t (cdr pair))))

(defun emagent--project-directory-initial ()
  "Default project directory for a new emagent session.
Uses the current buffer's cwd when it is a shell or file-backed buffer,
otherwise ~/."
  (cond
   ((derived-mode-p 'shell-mode 'eshell-mode 'term-mode 'vterm-mode)
    default-directory)
   (buffer-file-name
    (file-name-directory buffer-file-name))
   (t
    (expand-file-name "~/"))))

(defun emagent--read-project-directory ()
  "Read and return an absolute project directory for a new emagent session."
  (expand-file-name
   (read-directory-name "Emagent project directory: "
                        (emagent--project-directory-initial)
                        nil t)))

;;;###autoload
(defun emagent (&optional prompt-directory)
  "Start a new emagent session and connect.

Infers the project directory from the current buffer, probes installed
agents for models when `emagent-probe-models-at-start' is non-nil, and
prompts when more than one agent/model combination is available.  With a
prefix argument PROMPT-DIRECTORY, read the project directory instead."
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

(provide 'emagent)

;;; emagent.el ends here
