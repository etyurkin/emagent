;;; emagent-acp.el --- ACP wire-up for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)

;; Register grouped lisp/ subdirectories on load-path so that
;; cross-directory requires (emagent-log from lisp/core/ etc.)
;; work during byte-compilation by Elpaca or other build tools.
;; Uses `byte-compile-current-file' when set (Elpaca compile).
(eval-and-compile
  (when-let ((file (or load-file-name
                       (and (boundp 'byte-compile-current-file)
                            byte-compile-current-file)))
             (lisp (expand-file-name ".." (file-name-directory file))))
    (when (file-directory-p lisp)
      (dolist (dir (directory-files lisp nil "^[^.]"))
        (let ((path (expand-file-name dir lisp)))
          (when (file-directory-p path)
            (add-to-list 'load-path path)))))))

(require 'emagent-acp-protocol)
(require 'emagent-log)
(require 'emagent-chat)
(require 'emagent-context)
(require 'emagent-mcp)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-gate)
(require 'emagent-acp-usage)
(require 'emagent-acp-model)
(require 'emagent-acp-file)
(require 'emagent-acp-permit)
(require 'emagent-acp-tool-call)
(require 'emagent-acp-request)
(require 'emagent-acp-prompt)
(require 'emagent-prompts)

(declare-function emagent-prompts--structural-policy "emagent-prompts")

(declare-function emagent-chat-clear-slash-commands "emagent-chat-slash")
(declare-function emagent-chat-seed-cursor-slash-commands "emagent-chat-slash")
(declare-function emagent-chat--bare-slash-command-p "emagent-chat-compress")
(declare-function emagent-chat--compress-command-p "emagent-chat-compress")
(declare-function emagent-chat--conversation-history-text "emagent-chat-compress")
(declare-function emagent-chat--compress-prompt-text "emagent-chat-compress")
(declare-function emagent-chat-apply-compression "emagent-chat-compress")
(declare-function emagent-chat-show-tool-call "emagent-chat")
(declare-function emagent-chat-permission-prompt "emagent-chat")
(declare-function emagent-chat--open-response-p "emagent-chat")
(declare-function emagent-chat--refresh-mode-line-soon "emagent-chat-mode-line")
(declare-function emagent-chat--spinner-start "emagent-chat-mode-line")
(declare-function emagent-cursor-enrich-tool-call-update "emagent-cursor")
(declare-function emagent-cursor-normalize-slash-prompt "emagent-cursor")

(defun emagent-acp--system-prompt ()
  "Return the system prompt for new ACP sessions."
  (concat emagent-acp-system-prompt
          (emagent-mcp-gateway-system-prompt)
          (when emagent-acp-prefer-emacs
            emagent-acp-system-prompt-prefer-emacs)
          (when emagent-acp-prefer-emacs
            (emagent-prompts--structural-policy))))

(defun emagent-acp-prefer-emacs-p ()
  "Return non-nil when emagent instructs the agent to prefer Emacs tools."
  emagent-acp-prefer-emacs)

;;;; Public session state accessors (for use by emagent-chat.el)






(defun emagent-set-model ()
  "Set the ACP model for the current emagent session."
  (interactive)
  (let* ((state (emagent-acp--session))
         (session-id (map-elt state :session-id))
         (choices (emagent-acp--model-choices state nil))
         (current (emagent-acp--current-model-id state nil))
         (default-name (and current
                            (emagent-acp--model-display-name state nil current)))
         (selection (completing-read
                     "Set emagent model: "
                     (mapcar #'car choices)
                     nil t nil nil
                     (and default-name
                          (car (seq-find (lambda (choice)
                                           (string-prefix-p default-name (car choice)))
                                         choices)))))
         (model-id (cdr (assoc-string selection choices))))
    (unless session-id
      (user-error "No active session"))
    (unless choices
      (user-error "No models available"))
    (unless model-id
      (user-error "Unknown model: %s" selection))
    (when (and current (string= model-id current))
      (user-error "Model already %s"
                  (emagent-acp--model-display-name state nil model-id)))
    (emagent-acp--config-option-set-model-id
     :state state
     :session-id session-id
     :model-id model-id)))

(defun emagent-acp--trace-update (update-type emagent-acp-notification)
  "Log UPDATE-TYPE and a short payload summary when tracing."
  (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) ""))
        (title (map-nested-elt emagent-acp-notification '(params update title))))
    (pcase update-type
      ((or "agent_message_chunk" "agent_thought_chunk")
       (emagent-acp--trace "recv %s +%d" update-type (length text)))
      ("tool_call"
       (emagent-acp--trace "recv tool_call %s"
                           (or title
                               (map-nested-elt emagent-acp-notification '(params update toolCallId))
                               "running")))
      ("tool_call_update"
       (emagent-acp--trace "recv tool_call_update %s"
                           (or title
                               (map-nested-elt emagent-acp-notification '(params update toolCallId))
                               "running")))
      (_
       (emagent-acp--trace "recv %s" (or update-type "session/update"))))))

(cl-defun emagent-acp--on-notification (&key state emagent-acp-notification)
  (when (equal (map-elt emagent-acp-notification 'method) "session/update")
    (let ((update-type (map-nested-elt emagent-acp-notification '(params update sessionUpdate))))
      (emagent-acp--trace-update update-type emagent-acp-notification)
      (pcase update-type
        ("agent_message_chunk"
         (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) "")))
           (unless (map-elt state :replaying-history)
             (emagent-acp--detect-external-refusal-in-text state text)
             (map-put! state :assistant-text (concat (map-elt state :assistant-text) text))
             (when (map-elt state :prompt-finishing)
               (emagent-acp--schedule-prompt-render state))
             (when-let ((buf (and (emagent-acp--stream-to-buffer-p state)
                                 (emagent-acp--chat-buffer state))))
               (with-current-buffer buf
                 (when-let ((cb (map-elt state :cb-chunk)))
                   (funcall cb text)))))))
        ("agent_thought_chunk"
         (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) "")))
           (emagent-acp--thought-chunk state text)))
        ("tool_call"
         (emagent-acp--on-tool-call state (map-nested-elt emagent-acp-notification '(params update))))
        ("tool_call_update"
         (emagent-acp--on-tool-call state (map-nested-elt emagent-acp-notification '(params update))))
        ("config_option_update"
         (emagent-acp--save-config-options
          state
          (map-nested-elt emagent-acp-notification '(params update configOptions)))
         (when-let ((model-id (emagent-acp--current-model-id state nil)))
           (emagent-acp--persist-model-id state model-id)))
        ("usage_update"
         (emagent-acp--update-usage-from-notification
          state
          (map-nested-elt emagent-acp-notification '(params update))))
        ("available_commands_update"
         (let ((commands (map-nested-elt emagent-acp-notification
                                         '(params update availableCommands))))
           (when-let* ((buffer (emagent-acp--chat-buffer state))
                       (cb (map-elt state :cb-slash-commands)))
             (with-current-buffer buffer
               (funcall cb commands)))))
        (_ nil)))))

(cl-defun emagent-acp--subscribe (&key state)
  (let ((buffer (emagent-acp--chat-buffer state)))
    (emagent-acp-subscribe-to-errors
     :client (map-elt state :client)
     :buffer buffer
     :on-error
     (lambda (emagent-acp-error)
       (let ((message (or (map-elt emagent-acp-error 'message)
                          (format "%s" emagent-acp-error))))
         (emagent-acp--log-agent-stderr message)
         (when (and (map-elt state :busy)
                    (emagent-acp--fatal-agent-error-p message))
           (emagent-acp--abort-prompt state message))
         (when (emagent-acp--stderr-notify-p emagent-acp-error)
           (emagent-acp--notify-user state (format "emagent error: %s" message))))))
    (emagent-acp-subscribe-to-notifications
     :client (map-elt state :client)
     :buffer buffer
     :on-notification
     (lambda (notification)
       (emagent-acp--on-notification :state state
                                     :emagent-acp-notification notification)))
    (emagent-acp-subscribe-to-requests
     :client (map-elt state :client)
     :buffer buffer
     :on-request
     (lambda (request)
       (emagent-acp--on-request :state state :emagent-acp-request request)))))

(cl-defun emagent-acp--authenticate (&key state method-id on-ready)
  "Send an authenticate request with METHOD-ID, then connect the session.

Called when `initialize' returns authMethods (e.g. cursor_login).
The authenticate call completes the credential handshake so the agent
grants full plan access (including Auto model) to this ACP session."
  (emagent-acp--progress state (format "authenticating (%s)…" method-id))
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-authenticate-request :method-id method-id)
   :on-success (lambda (_response)
                 (emagent-acp--connect-session :state state :on-ready on-ready))
   :on-failure (lambda (error _raw)
                 (emagent-log "authenticate %s failed: %s — proceeding anyway"
                              method-id
                              (or (map-elt error 'message) (format "%s" error)))
                 (emagent-acp--connect-session :state state :on-ready on-ready))))

(cl-defun emagent-acp--initialize (&key state on-ready)
  (emagent-acp--progress state "initializing ACP…")
  (emagent-acp--send-request
   :state state
   :request (if emagent-acp-file-access
                (emagent-acp-make-initialize-request
                 :protocol-version 1
                 :client-info `((name . "emagent")
                                (title . "Emacs Emagent")
                                (version . "0.1.0"))
                 :read-text-file-capability t
                 :write-text-file-capability t)
              (emagent-acp-make-initialize-request
               :protocol-version 1
               :client-info `((name . "emagent")
                              (title . "Emacs Emagent")
                              (version . "0.1.0"))))
   :on-success (lambda (response)
                 (map-put! state :initialized t)
                 (map-put! state :mcp-http (emagent-acp--mcp-http-capable-p response))
                 (emagent-acp--infer-external-tool-gate-from-agent state)
                 (emagent-acp--infer-external-tool-gate-from-initialize-response state response)
                 (emagent-acp--maybe-log-external-tool-gate-proactive state)
                 (let ((auth-methods (append (map-elt response 'authMethods) nil)))
                   (if-let ((method-id (map-elt (seq-find
                                                 (lambda (m) (map-elt m 'id))
                                                 auth-methods)
                                                'id)))
                       (emagent-acp--authenticate
                        :state state :method-id method-id :on-ready on-ready)
                     (emagent-acp--connect-session :state state :on-ready on-ready))))
   :on-failure (lambda (error _raw)
                 (emagent-acp--fail-connect
                  state
                  (format "emagent: initialize failed: %s"
                          (or (map-elt error 'message) (format "%s" error)))))))

(defun emagent-acp--mcp-http-capable-p (initialize-response)
  "Return non-nil when INITIALIZE-RESPONSE advertises http MCP support."
  (let ((value (map-nested-elt initialize-response
                               '(agentCapabilities mcpCapabilities http))))
    (and value (not (eq value :false)) (not (eq value :json-false)))))

(cl-defun emagent-acp--session-ready (&key state session-id on-ready resumed)
  (map-put! state :session-id session-id)
  (map-put! state :ready t)
  (emagent-acp--persist-session-id state session-id)
  (emagent-tools-set-project-directory (emagent-acp--session-cwd state))
  (emagent-acp--progress state (if resumed "resumed" "connected"))
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (pcase emagent-chat-provider
        ('cursor (emagent-chat-seed-cursor-slash-commands))
        ('claude
         (when (null emagent-chat-slash-commands)
           (emagent-log "loading slash commands from agent…"))))))
  (emagent-acp--start-rss-timer state)
  (emagent-acp--reveal-buffer state)
  (when on-ready (funcall on-ready)))

(cl-defun emagent-acp--new-session (&key state on-ready)
  (emagent-acp--progress state "creating session…")
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-new-request
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (map-elt state :mcp-http)
                                                       (emagent-acp--chat-buffer state))
             :meta `((systemPrompt . ((append . ,(emagent-acp--system-prompt))))))
   :on-success (lambda (response)
                 (emagent-acp--configure-model
                  :state state
                  :session-id (map-elt response 'sessionId)
                  :response response
                  :on-ready on-ready))
   :on-failure (lambda (error _raw)
                 (emagent-acp--fail-connect
                  state
                  (format "emagent: session/new failed: %s"
                          (or (map-elt error 'message) (format "%s" error)))))))

(cl-defun emagent-acp--load-session (&key state session-id on-ready)
  (emagent-acp--progress state "resuming session…")
  (map-put! state :replaying-history t)
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-load-request
             :session-id session-id
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (map-elt state :mcp-http)
                                                       (emagent-acp--chat-buffer state))
             :meta `((systemPrompt . ((append . ,(emagent-acp--system-prompt))))))
   :on-success (lambda (response)
                 (map-put! state :replaying-history nil)
                 (emagent-acp--configure-model
                  :state state
                  :session-id session-id
                  :response response
                  :on-ready on-ready
                  :resumed t))
   :on-failure (lambda (_error _raw)
                 (map-put! state :replaying-history nil)
                 (emagent-acp--progress state "resume failed, creating session…")
                 (when-let ((buf (emagent-acp--chat-buffer state)))
                   (with-current-buffer buf
                     (emagent-chat-clear-session-id)))
                 (emagent-acp--new-session :state state :on-ready on-ready))))

(cl-defun emagent-acp--connect-session (&key state on-ready)
  (emagent-acp--progress state "connecting session…")
  (let ((saved (emagent-acp--saved-session-id state)))
    (if (and saved (not (string-empty-p saved)))
        (emagent-acp--load-session :state state :session-id saved :on-ready on-ready)
      (emagent-acp--new-session :state state :on-ready on-ready))))

(cl-defun emagent-acp-start (&key client chat-buffer on-ready on-reveal callbacks)
  "Start an emagent ACP session in CHAT-BUFFER.

ON-REVEAL is called once when the chat buffer should be shown.
CALLBACKS is an alist of rendering callbacks keyed by:
  :cb-chunk, :cb-thought, :cb-finish, :cb-fail, :cb-slash-commands."
  (when (and emagent-acp-prefer-emacs (not emagent-acp-file-access))
    (emagent-log "prefer-Emacs mode works best with `emagent-acp-file-access'"))
  (when emagent-acp-trace
    (setq emagent-acp-logging-enabled t))
  (with-current-buffer chat-buffer
    (emagent-chat-clear-slash-commands)
    (setq emagent-acp--session (emagent-acp--make-state :client client
                                                        :chat-buffer chat-buffer
                                                        :on-reveal on-reveal))
    (dolist (cb callbacks)
      (map-put! emagent-acp--session (car cb) (cdr cb)))
    (emagent-mcp-register-session :token (emagent-mcp-buffer-token)
                                  :cwd (emagent-chat--session-directory)
                                  :buffer chat-buffer
                                  :prefer-emacs emagent-acp-prefer-emacs
                                  :acp t)
    (emagent-acp--progress emagent-acp--session "starting agent…")
    (emagent-acp--subscribe :state emagent-acp--session)
    (emagent-acp--initialize :state emagent-acp--session :on-ready on-ready)
    emagent-acp--session))

(defun emagent-acp-attach-context (text)
  "Attach TEXT to the next prompt in the current buffer."
  (let ((state (emagent-acp--session)))
    (map-put! state :extra-context
              (append (or (map-elt state :extra-context) nil) (list text)))))

(defun emagent-acp--image-media-type (ext)
  "Return the MIME type string for image extension EXT, or nil if not an image."
  (pcase (downcase (or ext ""))
    ("png"  "image/png")
    ("jpg"  "image/jpeg")
    ("jpeg" "image/jpeg")
    ("gif"  "image/gif")
    ("webp" "image/webp")
    (_      nil)))

(defun emagent-acp--extract-image-links (text)
  "Extract [[file:...]] image links from TEXT.

Scans for org file links whose paths end in PNG/JPEG/GIF/WebP, reads and
base64-encodes each file, and removes the link from the text.  Non-image
links and unreadable paths are left in place.

Returns (CLEANED-TEXT . IMAGES) where IMAGES is a list of
 ((media-type . TYPE) (data . BASE64)) plists."
  (let ((link-re "\\[\\[file:\\([^]\n]+\\)\\]\\(?:\\[[^]]*\\]\\)?\\]")
        images parts (pos 0))
    (while (string-match link-re text pos)
      (let* ((link-beg (match-beginning 0))
             (link-end (match-end 0))
             (path (match-string 1 text))
             (expanded (expand-file-name path))
             (media-type (emagent-acp--image-media-type
                          (file-name-extension expanded))))
        (push (substring text pos link-beg) parts)
        (if (and media-type (file-readable-p expanded))
            (let ((data (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally expanded)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string))))
              (push `((media-type . ,media-type) (data . ,data)) images))
          (push (substring text link-beg link-end) parts))
        (setq pos link-end)))
    (push (substring text pos) parts)
    (cons (string-trim (apply #'concat (nreverse parts)))
          (nreverse images))))

(defun emagent-acp--abort-compress-empty (state)
  "Close an empty /compress request with an error in the chat buffer."
  (emagent-acp--clear-prompt-watchdog state)
  (emagent-acp--cancel-prompt-render state)
  (map-put! state :busy nil)
  (map-put! state :assistant-text "")
  (map-put! state :thought-text "")
  (map-put! state :prompt-finalized t)
  (map-put! state :prompt-finishing nil)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (when-let ((cb (map-elt state :cb-fail)))
        (funcall cb "No conversation to compress"))))
  (emagent-acp--refresh-mode-line state))

(defun emagent-acp-send-prompt (user-text)
  "Send USER-TEXT to the current buffer's ACP session."
  (let* ((state (emagent-acp--session))
         (session-id (map-elt state :session-id)))
    (unless (map-elt state :ready)
      (user-error "Emagent is still connecting"))
    (when (map-elt state :busy)
      (user-error "Emagent is busy"))
    (when (and (eq emagent-chat-provider 'cursor)
               (fboundp 'emagent-cursor-normalize-slash-prompt))
      (setq user-text (emagent-cursor-normalize-slash-prompt user-text)))
    (let ((slash-command-p (emagent-chat--bare-slash-command-p user-text)))
      (when (and slash-command-p
                 (fboundp 'emagent-chat--compress-command-p)
                 (emagent-chat--compress-command-p user-text))
        (let ((history (with-current-buffer (emagent-acp--chat-buffer state)
                         (emagent-chat--conversation-history-text))))
          (if (string-empty-p history)
              (progn
                (emagent-acp--abort-compress-empty state)
                (cl-return-from emagent-acp-send-prompt))
            (setq user-text (emagent-chat--compress-prompt-text history))
            (map-put! state :compress-pending t)
            (setq slash-command-p nil))))
      (let* ((extra (map-elt state :extra-context))
             (full-prompt (if (or slash-command-p (map-elt state :compress-pending))
                              user-text
                            (emagent-context-build-prompt user-text extra)))
             (extracted (emagent-acp--extract-image-links
                         (substring-no-properties full-prompt)))
             (clean-text (car extracted))
             (images (cdr extracted))
             (blocks `[((type . "text") (text . ,clean-text))]))
        (map-put! state :extra-context nil)
        (cond
         ((map-elt state :compress-pending)
          (emagent-log "compressing conversation"))
         (slash-command-p
          (emagent-log "send slash command: %s" user-text)))
      (map-put! state :busy t)
      (when (fboundp 'emagent-chat--spinner-start)
        (emagent-chat--spinner-start))
      (map-put! state :assistant-text "")
      (map-put! state :thought-text "")
      (map-put! state :prompt-finalized nil)
      (map-put! state :prompt-finishing nil)
      (clrhash (map-elt state :tool-call-titles))
      (clrhash (map-elt state :tool-call-inputs))
      (clrhash (map-elt state :tool-call-labels))
      (clrhash (map-elt state :tool-call-pending))
      (map-put! state :cursor-tool-resolve-queue nil)
      (map-put! state :cursor-tool-resolve-worker nil)
      (clrhash (map-elt state :cursor-tool-resolve-attempts))
      (when-let ((timer (map-elt state :permission-drain-timer)))
        (cancel-timer timer)
        (map-put! state :permission-drain-timer nil))
      (map-put! state :permission-queue nil)
      (map-put! state :permission-busy nil)
      (map-put! state :deferred-complete-response nil)
      (emagent-acp--cancel-prompt-render state)
      (emagent-acp--clear-thought-buffer state)
      (emagent-acp--schedule-prompt-watchdog state)
      (let ((gen (map-elt state :prompt-generation)))
        (emagent-acp--send-request
         :state state
         :request (emagent-acp-make-session-prompt-request
                   :session-id session-id :prompt blocks :images images)
         :on-success
         (lambda (response)
           (when (eq (map-elt state :prompt-generation) gen)
             (emagent-acp--complete-prompt state response)))
         :on-failure
         (lambda (error _raw)
           (when (eq (map-elt state :prompt-generation) gen)
             (let ((message (or (map-elt error 'message) (format "%s" error))))
               (emagent-acp--abort-prompt state (format "prompt failed: %s" message))
               (emagent-acp--notify-user state (format "emagent: prompt failed: %s" message)))))))))))

(defun emagent-acp-interrupt ()
  "Interrupt the in-flight prompt and close the response block cleanly.

Appends a user-visible stop notice to whatever the agent has produced so far,
then finalizes the response as if it completed normally.  The pending ACP
request continues in the background but its result is ignored."
  (let ((state emagent-acp--session))
    (unless (or (map-elt state :busy) (map-elt state :prompt-finishing))
      (user-error "No active emagent prompt to interrupt"))
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    (emagent-acp--flush-thought-buffer state)
    (let* ((text (or (map-elt state :assistant-text) ""))
           (notice "/Stopped — awaiting new instructions./")
           (full (if (string-empty-p text)
                     notice
                   (concat text "\n\n" notice))))
      (map-put! state :assistant-text full))
    (map-put! state :prompt-generation (1+ (or (map-elt state :prompt-generation) 0)))
    (when-let ((client (map-elt state :client))
               (session-id (map-elt state :session-id)))
      (ignore-errors
        (emagent-acp-send-notification
         :client client
         :notification (emagent-acp-make-session-cancel-notification :session-id session-id))))
    (map-put! state :busy nil)
    (map-put! state :prompt-finishing t)
    (map-put! state :prompt-finalized nil)
    (emagent-acp--render-prompt-response state)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp-shutdown-buffer ()
  "Shut down the ACP session for the current buffer."
  (emagent-chat-clear-slash-commands)
  (when emagent-mcp--token
    (emagent-mcp-deregister-session emagent-mcp--token))
  (when-let ((state emagent-acp--session))
    (emagent-acp--stop-rss-timer state)
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    (when-let ((client (map-elt state :client)))
      (emagent-acp-shutdown :client client))
    (setq emagent-acp--session nil)))


(provide 'emagent-acp)

;;; emagent-acp.el ends here
