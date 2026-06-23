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

(defun emagent-acp-busy-p ()
  "Return non-nil when the current buffer's ACP session is processing a prompt."
  (and emagent-acp--session (map-elt emagent-acp--session :busy)))

(defun emagent-acp-waiting-permission-p ()
  "Return non-nil while permission requests are queued or being answered."
  (and emagent-acp--session
       (emagent-acp--permission-pending-p emagent-acp--session)))

(defun emagent-acp-ready-p ()
  "Return non-nil when the current buffer's ACP session is connected and idle."
  (and emagent-acp--session (map-elt emagent-acp--session :ready)))

(defun emagent-acp-current-tool ()
  "Return the name of the tool currently running, or nil."
  (and emagent-acp--session (map-elt emagent-acp--session :current-tool)))

(defun emagent-acp-current-tool-kind ()
  "Return the kind of the running tool (\"read\", \"write\", \"execute\"), or nil."
  (and emagent-acp--session (map-elt emagent-acp--session :current-tool-kind)))

(defun emagent-acp-agent-rss ()
  "Return the agent process RSS in MB, or nil."
  (and emagent-acp--session (map-elt emagent-acp--session :agent-rss)))

(defun emagent-acp-context-usage ()
  "Return (USED . SIZE) context token counts for the current session, or nil."
  (when-let* ((state emagent-acp--session)
              (usage (map-elt state :usage))
              (used (map-elt usage :context-used))
              (size (map-elt usage :context-size)))
    (cons used size)))

(defun emagent-acp-external-tool-gate-reasons ()
  "Return external tool-gate reason symbols for the current session, or nil.
See `emagent-acp-external-tool-gate-hints'."
  (and emagent-acp--session
       (map-elt emagent-acp--session :external-tool-gate-reasons)))

(defun emagent-acp--chat-buffer (state)
  "Return STATE's chat buffer if it is live, else nil.

A killed buffer must not be returned: timers and callbacks still hold STATE
after the user kills the chat buffer, and `with-current-buffer' on a dead
buffer signals \"Selecting deleted buffer\"."
  (let ((buf (map-elt state :chat-buffer)))
    (when (buffer-live-p buf)
      buf)))

(defun emagent-acp--session-cwd (state)
  (if-let ((buf (emagent-acp--chat-buffer state)))
      (with-current-buffer buf (emagent-chat--session-directory))
    (user-error "emagent chat buffer is no longer available")))

(defun emagent-acp--persist-session-id (state session-id)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (emagent-chat-set-session-id session-id))))

(defun emagent-acp--saved-session-id (state)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (emagent-chat-session-id))))

(defun emagent-acp--saved-model-id (state)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (emagent-chat-model))))

(defun emagent-acp--persist-model-id (state model-id)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (emagent-chat-set-model model-id)))
  (emagent-acp--refresh-mode-line state))

(defun emagent-acp--maybe-recover-stall (state)
  "Unstick a session that finished on the wire but left the buffer open."
  (when (and state (map-elt state :ready) (not (map-elt state :busy)))
    (emagent-acp--maybe-complete-deferred-prompt state)
    (when (map-elt state :permission-queue)
      (emagent-acp--drain-permission-queue state))))

(defun emagent-acp--refresh-mode-line (state)
  (emagent-acp--maybe-recover-stall state)
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (cond
       ((and (fboundp 'emagent-acp-busy-p)
             (not (emagent-acp-busy-p))
             (fboundp 'emagent-chat--refresh-mode-line))
        (emagent-chat--refresh-mode-line))
       ((fboundp 'emagent-chat--refresh-mode-line-soon)
        (emagent-chat--refresh-mode-line-soon))
       ((fboundp 'emagent-chat--refresh-mode-line)
        (emagent-chat--refresh-mode-line))
       (t (force-mode-line-update t))))))

(defun emagent-acp--usage-state (state)
  (or (map-elt state :usage)
      (let ((usage (make-hash-table :test 'eq)))
        (puthash :context-used nil usage)
        (puthash :context-size nil usage)
        (puthash :total-tokens 0 usage)
        (map-put! state :usage usage)
        usage)))

(defun emagent-acp--save-usage-from-response (state emagent-acp-usage)
  "Update STATE usage from a prompt response usage field."
  (let ((usage (emagent-acp--usage-state state)))
    (when-let ((total (map-elt emagent-acp-usage 'totalTokens)))
      (map-put! usage :total-tokens total))
    ;; Extract context usage — cursor may use different field names.
    (when-let ((used (or (map-elt emagent-acp-usage 'contextUsed)
                         (map-elt emagent-acp-usage 'inputTokens)
                         (map-elt emagent-acp-usage 'promptTokens))))
      (map-put! usage :context-used used))
    (when-let ((size (or (map-elt emagent-acp-usage 'contextSize)
                         (map-elt emagent-acp-usage 'contextLimit)
                         (map-elt emagent-acp-usage 'contextWindow))))
      (map-put! usage :context-size size))
    (map-put! state :usage usage)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp--update-usage-from-notification (state emagent-acp-update)
  "Update STATE usage from a session/update usage_update payload."
  (let ((usage (emagent-acp--usage-state state)))
    (when-let ((used (or (map-elt emagent-acp-update 'used)
                         (map-elt emagent-acp-update 'contextUsed)
                         (map-elt emagent-acp-update 'contextWindowUsed)
                         (map-elt emagent-acp-update 'tokensUsed)
                         (map-elt emagent-acp-update 'inputTokens))))
      (map-put! usage :context-used used))
    (when-let ((size (or (map-elt emagent-acp-update 'size)
                         (map-elt emagent-acp-update 'contextLimit)
                         (map-elt emagent-acp-update 'contextSize)
                         (map-elt emagent-acp-update 'contextWindow)
                         (map-elt emagent-acp-update 'maxTokens))))
      (map-put! usage :context-size size))
    (map-put! state :usage usage)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp--normalize-config-option (emagent-acp-option)
  `((:id . ,(map-elt emagent-acp-option 'id))
    (:name . ,(map-elt emagent-acp-option 'name))
    (:description . ,(map-elt emagent-acp-option 'description))
    (:category . ,(map-elt emagent-acp-option 'category))
    (:type . ,(map-elt emagent-acp-option 'type))
    (:current-value . ,(map-elt emagent-acp-option 'currentValue))
    (:options . ,(mapcar (lambda (emagent-acp-value)
                           `((:value . ,(map-elt emagent-acp-value 'value))
                             (:name . ,(map-elt emagent-acp-value 'name))
                             (:description . ,(map-elt emagent-acp-value 'description))))
                         (append (map-elt emagent-acp-option 'options) nil)))))

(defun emagent-acp--normalize-config-options (emagent-acp-config-options)
  (mapcar #'emagent-acp--normalize-config-option
          (append emagent-acp-config-options nil)))

(defun emagent-acp--save-config-options (state emagent-acp-config-options)
  (when emagent-acp-config-options
    (map-put! state :config-options
              (emagent-acp--normalize-config-options emagent-acp-config-options))))

(defun emagent-acp--config-options (state)
  (map-elt state :config-options))

(defun emagent-acp--config-option-by-category (state category)
  (seq-find (lambda (option)
              (equal category (map-elt option :category)))
            (emagent-acp--config-options state)))

(defun emagent-acp--model-config-option (state)
  (or (emagent-acp--config-option-by-category state "model")
      (seq-find (lambda (option)
                  (string= (map-elt option :id) "model"))
                (emagent-acp--config-options state))))

(defun emagent-acp--config-option-value-name (option value)
  (or (map-elt (seq-find (lambda (candidate)
                           (equal value (map-elt candidate :value)))
                         (map-elt option :options))
               :name)
      value))

(defun emagent-acp--config-option-set-value (state config-id value)
  (dolist (option (emagent-acp--config-options state))
    (when (equal config-id (map-elt option :id))
      (setf (map-elt option :current-value) value))))

(defun emagent-acp--model-entry-id (entry)
  (or (map-elt entry 'modelId) (map-elt entry 'model-id) (map-elt entry 'value)))

(defun emagent-acp--model-entry-name (entry)
  (or (map-elt entry 'name) (emagent-acp--model-entry-id entry)))

(defun emagent-acp--model-entries-from-response (response)
  "Return a list of ((:model-id . ID) (:name . NAME)) from a session/new RESPONSE."
  (when response
    (let* ((models (emagent-acp--models-from-response response))
           (entries (append (emagent-acp--available-model-entries models) nil)))
      (delq nil
            (mapcar (lambda (entry)
                      (let ((id (or (map-elt entry :model-id)
                                    (map-elt entry :value)
                                    (emagent-acp--model-entry-id entry)))
                            (name (or (map-elt entry :name)
                                      (emagent-acp--model-entry-name entry))))
                        (when id
                          `((:model-id . ,id) (:name . ,name)))))
                    entries)))))

(defun emagent-acp--models-from-response (response)
  (or (map-elt response 'models)
      (when-let* ((options (map-elt response 'configOptions))
                  (model-opt
                   (seq-find (lambda (option)
                               (or (string= (map-elt option 'category) "model")
                                   (string= (map-elt option 'id) "model")))
                             options)))
        (list (cons 'availableModels (map-elt model-opt 'options))
              (cons 'currentModelId (map-elt model-opt 'currentValue))))))

(defun emagent-acp--available-model-entries (models)
  (or (map-elt models 'availableModels) (map-elt models 'options) nil))

(defun emagent-acp--get-available-models (state models)
  (if-let ((model-option (emagent-acp--model-config-option state)))
      (mapcar (lambda (value)
                `((:model-id . ,(map-elt value :value))
                  (:name . ,(map-elt value :name))
                  (:description . ,(map-elt value :description))))
              (map-elt model-option :options))
    (emagent-acp--available-model-entries models)))

(defun emagent-acp--current-model-id (state models)
  (or (map-elt (emagent-acp--model-config-option state) :current-value)
      (and models (map-elt models 'currentModelId))
      (emagent-acp--saved-model-id state)))

(defun emagent-acp--model-display-name (state models model-id)
  (or (map-elt (seq-find (lambda (model)
                           (string= (or (map-elt model :model-id)
                                        (emagent-acp--model-entry-id model))
                                    model-id))
                         (emagent-acp--get-available-models state models))
               :name)
      (emagent-acp--model-entry-name
       (seq-find (lambda (model)
                   (string= (emagent-acp--model-entry-id model) model-id))
                 (emagent-acp--get-available-models state models)))
      model-id))

(defun emagent-acp--model-choices (state models)
  (mapcar (lambda (entry)
            (let ((id (or (map-elt entry :model-id)
                          (emagent-acp--model-entry-id entry)))
                  (name (or (map-elt entry :name)
                            (emagent-acp--model-entry-name entry))))
              (cons (format "%s (%s)" name id) id)))
          (emagent-acp--get-available-models state models)))

(defun emagent-acp--model-available-p (model-id state models)
  (and model-id (not (string-empty-p model-id))
       (seq-find (lambda (entry)
                   (string= model-id
                            (or (map-elt entry :model-id)
                                (emagent-acp--model-entry-id entry))))
                 (emagent-acp--get-available-models state models))))

(defun emagent-acp--resolve-model-id (state models saved-model-id)
  "Return a model id for session connect without prompting.

Prefers a saved buffer model, then \"auto\" when advertised, then the
agent's current model.  Claude agents omit \"auto\" and use their default."
  (let* ((available (emagent-acp--get-available-models state models))
         (current (and models (map-elt models 'currentModelId))))
    (cond
     ((and saved-model-id
           (emagent-acp--model-available-p saved-model-id state models))
      saved-model-id)
     ((emagent-acp--model-available-p emagent-acp-auto-model-id state models)
      emagent-acp-auto-model-id)
     ((and current (not (string-empty-p current))) current)
     ((= (length available) 1)
      (or (map-elt (car available) :model-id)
          (emagent-acp--model-entry-id (car available))))
     (t nil))))

(cl-defun emagent-acp--config-option-set-model-id (&key state session-id model-id
                                                        on-success on-failure)
  (if-let ((model-option (emagent-acp--model-config-option state)))
      (emagent-acp--send-request
       :state state
       :request (emagent-acp-make-session-set-config-option-request
                 :session-id session-id
                 :config-id (map-elt model-option :id)
                 :value model-id)
       :on-success (lambda (response)
                     (if (map-elt response 'configOptions)
                         (emagent-acp--save-config-options state
                                                           (map-elt response 'configOptions))
                       (emagent-acp--config-option-set-value state
                                                             (map-elt model-option :id)
                                                             model-id))
                     (emagent-acp--persist-model-id state model-id)
                     (emagent-acp--progress
                      state
                      (format "model %s"
                              (emagent-acp--config-option-value-name model-option model-id)))
                     (when on-success (funcall on-success)))
       :on-failure (lambda (error _raw)
                     (emagent-acp--notify-user
                      state
                      (format "emagent: model %s not applied: %s"
                              model-id
                              (or (map-elt error 'message) (format "%s" error))))
                     (when on-failure (funcall on-failure))))
    (emagent-acp--send-request
     :state state
     :request (emagent-acp-make-session-set-model-request
               :session-id session-id
               :model-id model-id)
     :on-success (lambda (_response)
                   (emagent-acp--persist-model-id state model-id)
                   (emagent-acp--notify-user
                    state
                    (format "emagent: model %s" model-id))
                   (when on-success (funcall on-success)))
     :on-failure (lambda (error _raw)
                   (emagent-acp--notify-user
                    state
                    (format "emagent: model %s not applied: %s"
                            model-id
                            (or (map-elt error 'message) (format "%s" error))))
                   (when on-failure (funcall on-failure))))))

(defun emagent-acp--finish-configure-model (state session-id on-ready resumed)
  (emagent-acp--session-ready
   :state state
   :session-id session-id
   :on-ready on-ready
   :resumed resumed))

(cl-defun emagent-acp--configure-model (&key state session-id response on-ready resumed)
  (emagent-acp--progress state "selecting model…")
  (emagent-acp--save-config-options state (map-elt response 'configOptions))
  (let* ((models (emagent-acp--models-from-response response))
         (current (emagent-acp--current-model-id state models))
         (choice (emagent-acp--resolve-model-id state models
                                                (emagent-acp--saved-model-id state))))
    (cond
     ((and choice session-id (not (string-empty-p choice))
           current (string= choice current))
      (emagent-acp--progress
       state
       (format "model %s"
               (emagent-acp--model-display-name state models choice)))
      (emagent-acp--persist-model-id state choice)
      (emagent-acp--finish-configure-model state session-id on-ready resumed))
     ((and choice session-id (not (string-empty-p choice)))
      (emagent-acp--progress
       state
       (format "setting model to %s…"
               (emagent-acp--model-display-name state models choice)))
      (emagent-acp--config-option-set-model-id
       :state state
       :session-id session-id
       :model-id choice
       :on-success (lambda ()
                     (emagent-acp--finish-configure-model state session-id on-ready resumed))
       :on-failure (lambda ()
                     (emagent-acp--finish-configure-model state session-id on-ready resumed))))
     (t
      (when current
        (emagent-acp--progress
         state
         (format "model %s"
                 (emagent-acp--model-display-name state models current)))
        (emagent-acp--persist-model-id state current))
      (emagent-acp--finish-configure-model state session-id on-ready resumed)))))

;;;###autoload
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

(defun emagent-acp--notify-user (_state message)
  "Append MESSAGE to `emagent-log-buffer-name'."
  (emagent-log "%s" message))

(defun emagent-acp--trace (format-string &rest args)
  "Append a trace line when `emagent-acp-trace' is non-nil."
  (when emagent-acp-trace
    (apply #'emagent-log (cons (concat "acp: " format-string) args))))

(defun emagent-acp--progress (state message)
  "Show init stage MESSAGE in the minibuffer and refresh the mode line."
  (emagent-acp--notify-user state (format "emagent: %s" message))
  (emagent-acp--refresh-mode-line state))

(defun emagent-acp--clear-prompt-watchdog (state)
  "Cancel any pending prompt stall watchdog for STATE."
  (when-let ((timer (map-elt state :prompt-watchdog-timer)))
    (cancel-timer timer))
  (map-put! state :prompt-watchdog nil)
  (map-put! state :prompt-watchdog-timer nil))

(defun emagent-acp--schedule-prompt-watchdog (state)
  "Abort a prompt that stays busy without ACP progress."
  (let* ((token (cl-gensym "emagent-prompt-watchdog"))
         (timer (run-with-timer
     emagent-acp-watchdog-timeout nil
     (lambda ()
       (when (and (eq (map-elt state :prompt-watchdog) token)
                  (map-elt state :busy))
         (let* ((client (map-elt state :client))
                (pending (and client (map-elt client :pending-requests))))
           (emagent-log "emagent: prompt stalled (no ACP completion in %ds)"
                        emagent-acp-watchdog-timeout)
           (when pending
             (emagent-log "emagent: pending ACP request count: %d"
                          (length pending)))
           (if (and (map-elt state :assistant-text)
                    (not (string-empty-p (map-elt state :assistant-text))))
               (progn
                 (emagent-log "emagent: prompt stalled; finalizing partial response")
                 (emagent-acp--complete-prompt state nil))
             (emagent-acp--abort-prompt
              state
              "prompt stalled — reconnect with M-x emagent-claude-start or kill and reopen the buffer"))))))))
  (map-put! state :prompt-watchdog token)
  (map-put! state :prompt-watchdog-timer timer)))

(defun emagent-acp--stream-to-buffer-p (state)
  "Return non-nil when agent chunks may update the chat buffer live."
  (and emagent-acp-stream-to-buffer
       (map-elt state :busy)
       (not (map-elt state :compress-pending))
       (not (map-elt state :prompt-finalized))
       (not (map-elt state :prompt-finishing))))

(defun emagent-acp--stream-thought-to-buffer-p (state)
  "Return non-nil when reasoning may stream into the chat buffer live."
  (and (memq emagent-acp-thought-progress '(buffer both))
       (map-elt state :busy)
       (not (map-elt state :compress-pending))
       (not (map-elt state :prompt-finalized))
       (not (map-elt state :prompt-finishing))))

(defun emagent-acp--cancel-prompt-render (state)
  "Cancel a pending debounced render for STATE."
  (when-let ((timer (map-elt state :finish-timer)))
    (cancel-timer timer))
  (map-put! state :finish-timer nil)
  (map-put! state :finish-token nil))

(defun emagent-acp--schedule-prompt-render (state)
  "Debounced render of the accumulated prompt into the chat buffer."
  (let ((token (cl-gensym "emagent-finish")))
    (emagent-acp--cancel-prompt-render state)
    (map-put! state :finish-token token)
    (map-put! state :finish-timer
              (run-with-timer
               emagent-acp-render-delay nil
               (lambda ()
                 (when (and (eq (map-elt state :finish-token) token)
                            (map-elt state :prompt-finishing))
                   (map-put! state :finish-timer nil)
                   (emagent-acp--render-prompt-response state)))))))

(defun emagent-acp--render-prompt-response (state)
  "Render accumulated prompt text into the chat buffer for STATE."
  (when (map-elt state :prompt-finishing)
    (when-let ((buffer (emagent-acp--chat-buffer state)))
      (if (map-elt state :compress-pending)
          (let ((summary (map-elt state :assistant-text)))
            (map-put! state :compress-pending nil)
            (with-current-buffer buffer
              (emagent-chat-apply-compression summary))
            (emagent-log "compressed session (%d chars)" (length (or summary "")))
            (emagent-acp--new-session :state state))
        (condition-case err
            (with-current-buffer buffer
              (when-let ((cb (map-elt state :cb-finish)))
                (funcall cb (map-elt state :assistant-text)
                         (map-elt state :thought-text))))
          (error
           (emagent-log "emagent: finish failed: %s" (error-message-string err))
           (with-current-buffer buffer
             (when-let ((cb (map-elt state :cb-fail)))
               (funcall cb (format "response finalize failed: %s"
                                   (error-message-string err)))))))))
    (map-put! state :prompt-finishing nil)
    (map-put! state :prompt-finalized t)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp--permission-pending-p (state)
  "Return non-nil when STATE has unanswered permission requests."
  (or (map-elt state :permission-busy)
      (map-elt state :permission-queue)))

(defun emagent-acp--maybe-complete-deferred-prompt (state)
  "Run a deferred `emagent-acp--complete-prompt' when permissions are clear."
  (when-let ((response (map-elt state :deferred-complete-response)))
    (unless (emagent-acp--permission-pending-p state)
      (map-put! state :deferred-complete-response nil)
      (emagent-acp--complete-prompt state response))))

(defun emagent-acp--complete-prompt (state response)
  "Finalize the in-flight prompt for STATE and close the chat response."
  (cond
   ((map-elt state :prompt-finalized)
    (when (map-elt state :busy)
      (map-put! state :busy nil)
      (emagent-acp--refresh-mode-line state)))
   ((not (map-elt state :busy))
    nil)
   ((emagent-acp--permission-pending-p state)
    (map-put! state :deferred-complete-response response))
   (t
    (map-put! state :prompt-finishing t)
    (map-put! state :busy nil)
    (map-put! state :current-tool nil)
    (map-put! state :current-tool-kind nil)
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--trace "prompt done (%d chars, %d thought)"
                        (length (or (map-elt state :assistant-text) ""))
                        (length (or (map-elt state :thought-text) "")))
    (emagent-acp--flush-thought-buffer state)
    (when (and response (map-elt response 'usage))
      (emagent-acp--save-usage-from-response state (map-elt response 'usage)))
    (emagent-acp--refresh-mode-line state)
    (emagent-acp--schedule-prompt-render state))))

(defun emagent-acp--log-thought-line (mode text)
  "Log one thought TEXT line according to MODE."
  (let ((line (string-trim text)))
    (unless (string-empty-p line)
      (pcase mode
        ('minimal
         (emagent-log "… %s" (emagent-log-truncate-line line 80)))
        ('trail
         (emagent-log "… %s" (emagent-log-truncate-line line 72 t)))
        (_ nil)))))

(defun emagent-acp--clear-thought-buffer (state)
  (map-put! state :thought-buffer ""))

(defun emagent-acp--flush-thought-buffer (state)
  "Log any trailing thought text for STATE and clear the buffer."
  (when-let ((mode emagent-acp-thought-progress))
    (when-let ((tail (string-trim (or (map-elt state :thought-buffer) ""))))
      (unless (string-empty-p tail)
        (emagent-acp--log-thought-line mode tail)))
    (emagent-acp--clear-thought-buffer state)))

(defun emagent-acp--thought-chunk (state text)
  "Accumulate thought TEXT for display and optional logging."
  (unless (string-empty-p text)
    (emagent-acp--detect-external-refusal-in-text state text)
    (map-put! state :thought-text
              (concat (or (map-elt state :thought-text) "") text))
    (when-let ((mode emagent-acp-thought-progress))
      (when (map-elt state :prompt-finishing)
        (emagent-acp--schedule-prompt-render state))
      (when (memq mode '(buffer both))
        (when-let ((buf (and (emagent-acp--stream-thought-to-buffer-p state)
                             (emagent-acp--chat-buffer state))))
          (with-current-buffer buf
            (when-let ((cb (map-elt state :cb-thought)))
              (funcall cb text)))))
      (when (memq mode '(minimal trail both))
        (let ((pending (concat (or (map-elt state :thought-buffer) "") text)))
          (while (string-match "\\`\\(.+?[.!?]\\)\\(?:[[:space:]]\\|\\'\\)" pending)
            (let ((end (match-end 0)))
              (emagent-acp--log-thought-line
               (if (eq mode 'both) 'minimal mode)
               (substring pending 0 end))
              (setq pending (substring pending end))))
          (map-put! state :thought-buffer pending))))))

(defun emagent-acp--run-reveal (reveal &optional now)
  (when reveal
    (if now
        (funcall reveal)
      (run-with-idle-timer 0 nil reveal))))

(defun emagent-acp--reveal-buffer (state &optional now)
  "Run the buffer reveal callback for STATE, if any.

When NOW is non-nil, show the buffer immediately for interactive prompts."
  (when-let ((reveal (map-elt state :on-reveal)))
    (map-put! state :on-reveal nil)
    (emagent-acp--run-reveal reveal now)))

(defun emagent-acp--prepare-interactive-context (state)
  "Show the chat buffer and select its window before a user prompt."
  (emagent-acp--reveal-buffer state t)
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (unless (get-buffer-window buffer)
      (pop-to-buffer buffer))
    (when-let ((window (get-buffer-window buffer)))
      (select-window window))))

(defun emagent-acp--fail-connect (state message)
  "Show MESSAGE, reveal the chat buffer, and stop connecting."
  (map-put! state :ready nil)
  (emagent-acp--notify-user state message)
  (emagent-acp--reveal-buffer state))

(defun emagent-acp--fatal-agent-error-p (message)
  "Return non-nil when MESSAGE should abort the in-flight prompt."
  (string-match-p "timed out\\|timeout\\|failed with status\\|ApiError\\|\\[31merror"
                  message))

(defun emagent-acp--abort-prompt (state message)
  "Abort the in-flight prompt for STATE and show MESSAGE."
  (when (or (map-elt state :busy) (map-elt state :prompt-finishing))
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    (map-put! state :busy nil)
    (map-put! state :prompt-finishing nil)
    (map-put! state :prompt-finalized nil)
    (map-put! state :assistant-text "")
    (map-put! state :compress-pending nil)
    (emagent-acp--trace "prompt aborted: %s" message)
    (emagent-acp--flush-thought-buffer state)
    (when-let ((buffer (emagent-acp--chat-buffer state)))
      (with-current-buffer buffer
        (when-let ((cb (map-elt state :cb-fail)))
          (funcall cb message))))
    (emagent-acp--refresh-mode-line state)))

(cl-defun emagent-acp--send-request (&key state request on-success on-failure)
  (let ((method (map-elt request :method)))
    (emagent-acp--trace "send %s" method)
    (emagent-acp-send-request
     :client (map-elt state :client)
     :request request
     :buffer (emagent-acp--chat-buffer state)
     :on-success
     (lambda (response)
       (emagent-acp--trace "recv %s ok" method)
       (when on-success (funcall on-success response)))
     :on-failure
     (lambda (error raw)
       (emagent-acp--trace "recv %s error: %s"
                           method
                           (or (map-elt error 'message) (format "%s" error)))
       (when on-failure (funcall on-failure error raw))))))

(defun emagent-acp--protected-fs-error (path)
  (emagent-acp-make-error
   :code -32603
   :message (format "Refusing Emacs access to %s (iCloud or another app's container)"
                    (emagent-tools--root-directory path))))

(defun emagent-acp--fs-unavailable-response (method)
  (emagent-acp-make-error
   :code -32601
   :message (format "%s disabled; use the external agent's project file tools"
                    method)))

(cl-defun emagent-acp--on-fs-read (&key state emagent-acp-request)
  (let ((client (map-elt state :client))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path))))
    (if (not emagent-acp-file-access)
        (emagent-acp-send-response
         :client client
         :response (emagent-acp-make-fs-read-text-file-response
                    :request-id request-id
                    :error (emagent-acp--fs-unavailable-response "fs/read_text_file")))
      (if (emagent-tools--protected-fs-path-p path)
          (emagent-acp-send-response
           :client client
           :response (emagent-acp-make-fs-read-text-file-response
                      :request-id request-id
                      :error (emagent-acp--protected-fs-error path)))
        (condition-case err
            (let* ((line (or (map-nested-elt emagent-acp-request '(params line)) 1))
                   (limit (map-nested-elt emagent-acp-request '(params limit)))
                   (content (emagent-tools--read-file-content path line limit)))
              (emagent-acp-send-response
               :client client
               :response (emagent-acp-make-fs-read-text-file-response
                          :request-id request-id
                          :content content)))
          (file-missing
           (emagent-acp-send-response
            :client client
            :response (emagent-acp-make-fs-read-text-file-response
                       :request-id request-id
                       :error (emagent-acp-make-error :code -32002
                                              :message "Resource not found"))))
          (error
           (emagent-acp-send-response
            :client client
            :response (emagent-acp-make-fs-read-text-file-response
                       :request-id request-id
                       :error (emagent-acp-make-error :code -32603
                                              :message (error-message-string err))))))))))

(cl-defun emagent-acp--on-fs-write (&key state emagent-acp-request)
  (let ((client (map-elt state :client))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path)))
        (resolved (emagent-tools--root-directory
                   (map-nested-elt emagent-acp-request '(params path)))))
    (if (not emagent-acp-file-access)
        (emagent-acp-send-response
         :client client
         :response (emagent-acp-make-fs-write-text-file-response
                    :request-id request-id
                    :error (emagent-acp--fs-unavailable-response "fs/write_text_file")))
      (if (emagent-tools--protected-fs-path-p path)
          (emagent-acp-send-response
           :client client
           :response (emagent-acp-make-fs-write-text-file-response
                      :request-id request-id
                      :error (emagent-acp--protected-fs-error path)))
        (progn
          (when emagent-acp-confirm-fs-writes
            (emagent-acp--prepare-interactive-context state))
          (condition-case err
              (if (and emagent-acp-confirm-fs-writes
                       (not (emagent-tools--confirm-write
                             'emagent-tool-write-file resolved
                             (or (map-nested-elt emagent-acp-request '(params content)) "")
                             (emagent-acp--chat-buffer state))))
                  (emagent-acp-send-response
                   :client client
                   :response (emagent-acp-make-fs-write-text-file-response
                              :request-id request-id
                              :error (emagent-acp-make-error :code -32603
                                     :message "Write denied by user")))
                (let ((written (emagent-tools--write-file-content
                                path (map-nested-elt emagent-acp-request '(params content)))))
                  (emagent-acp--notify-user
                   state (format "emagent: wrote %s (C-/ to undo in that buffer)"
                                 written))
                  (emagent-acp-send-response
                   :client client
                   :response (emagent-acp-make-fs-write-text-file-response
                              :request-id request-id))))
            (error
             (emagent-acp-send-response
              :client client
              :response (emagent-acp-make-fs-write-text-file-response
                         :request-id request-id
                         :error (emagent-acp-make-error :code -32603
                                :message (error-message-string err)))))))))))

(defun emagent-acp--permission-option-allow-p (opt)
  "Return non-nil when OPT is an allow-type ACP permission option."
  (let ((kind (downcase (or (map-elt opt 'kind) "")))
        (id (downcase (or (map-elt opt 'optionId) "")))
        (name (downcase (or (map-elt opt 'name) ""))))
    (or (member kind '("allow" "allow_once" "allow_always" "allow-once" "allow-always"))
        (member id '("allow_once" "allow-once" "allow_always" "allow-always" "allow" "yes" "run" "run_once"))
        (string-match-p "allow" id)
        (string-match-p "\\`\\(?:allow\\|yes\\|run\\)" name))))

(defun emagent-acp--permission-option-id (options)
  "Return an allow-type option id from OPTIONS.
Tries preferred optionIds, then `kind' = allow, then name/id matching allow.
Never returns a deny option.  OPTIONS may be a list or vector."
  (let ((prefer '("allow_once" "allow-once" "allow_always" "allow-always" "allow" "yes" "run" "run_once")))
    (or (map-elt (seq-find (lambda (opt)
                             (let ((id (map-elt opt 'optionId)))
                               (and id (member id prefer))))
                           options)
                 'optionId)
        (map-elt (seq-find #'emagent-acp--permission-option-allow-p options)
                 'optionId))))

(defconst emagent-acp--tool-call-detail-limit 120
  "Maximum detail length shown in Executing tool-call lines.")

(defun emagent-acp--update-put (update key value)
  "Return UPDATE alist with KEY bound to VALUE, replacing any prior binding."
  (cons (cons key value) (assoc-delete-all key update)))

(defun emagent-acp--cursor-agent-p (state)
  "Return non-nil when STATE's agent is Cursor's cursor-agent CLI."
  (when-let ((launch (emagent-acp--agent-launch-string state)))
    (string-match-p "cursor-agent" launch)))

(defconst emagent-acp--cursor-tool-resolve-max-attempts 8
  "Maximum store.db lookups while waiting for Cursor tool-call args.")

(defconst emagent-acp--cursor-tool-resolve-base-delay 0.05
  "Initial idle delay between Cursor store.db resolve retries.")

(defun emagent-acp--cursor-tool-resolve-active-p (state)
  "Return non-nil while Cursor store.db tool-call lookups are pending."
  (and (emagent-acp--cursor-agent-p state)
       (or (map-elt state :cursor-tool-resolve-worker)
           (map-elt state :cursor-tool-resolve-queue))))

(defconst emagent-acp--dangerous-command-regexps
  (mapconcat #'identity
             '("\\brm[[:space:]]+-[rf]+"
               "\\brm[[:space:]]+--recursive"
               "\\brm[[:space:]]+--force"
               "\\bdd[[:space:]]+"
               "\\bmkfs\\."
               "\\bmke2fs\\b"
               "\\bformat[[:space:]]+"
               "\\bshutdown\\b"
               "\\breboot\\b"
               "\\binit[[:space:]]+0\\b"
               "\\bsudo[[:space:]]+rm"
               "curl[[:space:]]+.*|.*sh\\b"
               "\\btrash\\b"
               "kill[[:space:]]+-9"
               ">*/dev/[sh]d[a-z]"
               "chmod[[:space:]]+-R[[:space:]]*777")
             "\\|")
  "Regexp matching shell commands that are destructive enough to prompt about.
Auto-approve in `safe' mode skips these and prompts for confirmation.")

(defun emagent-acp--tool-call-command-text (tool-call)
  "Extract the command string from a tool-call ACP object."
  (or (let ((raw (or (map-elt tool-call 'rawInput)
                     (map-elt tool-call 'arguments))))
        (when (stringp raw)
          (condition-case nil
              (let ((parsed (json-parse-string raw
                                               :object-type 'alist
                                               :array-type 'list
                                               :null-object nil
                                               :false-object nil)))
                (or (map-elt parsed 'command)
                    (map-elt parsed 'text)
                    (map-elt parsed 'cmd)))
            (error nil))))
      (map-elt tool-call 'subtitle)
      (map-elt tool-call 'title)))

(defun emagent-acp--tool-call-content-block (tool-call)
  "Return an org subsection string for the permission prompt, or nil.
For shell commands, returns a code block with the command.
For edits, returns the file path and content.
For reads, returns nil (the question line is enough)."
  (when (and (listp tool-call) (map-elt tool-call 'kind))
    (let* ((kind (downcase (or (map-elt tool-call 'kind) "")))
           (raw (or (map-elt tool-call 'rawInput)
                    (map-elt tool-call 'arguments)))
           (command (emagent-acp--tool-call-command-text tool-call))
           (detail (emagent-acp--tool-call-detail-from-tool-call tool-call))
           (path (and (member kind '("read" "write"))
                      (or (emagent-acp--tool-call-data-get
                           (emagent-acp--tool-call-normalize-data raw)
                           'path)
                          (emagent-acp--tool-call-data-get
                           (emagent-acp--tool-call-normalize-data raw)
                           'file_path)
                          detail))))
      (cond
       ((member kind '("execute" ""))
        (when command
          (format "** Allow execute\n#+BEGIN_SRC sh\n%s\n#+END_SRC" command)))
       ((equal kind "write")
        (if path
            (let ((content (emagent-acp--tool-call-data-get
                           (emagent-acp--tool-call-normalize-data raw)
                           'content))
                  (lang (if (string-match "\\.\\([a-z]+\\)\\'" path)
                            (match-string 1 path)
                          "text"))
                  (heading (if detail (format "Allow edit: %s" detail)
                             "Allow edit")))
              (if (and content (not (string-empty-p content)))
                  (format "** %s\n#+BEGIN_SRC %s\n%s\n#+END_SRC"
                          heading lang (substring content 0 (min (length content) 1000)))
                (format "** Allow edit\n= %s =" path)))
          (when command
            (format "** Allow edit\n#+BEGIN_SRC sh\n%s\n#+END_SRC" command))))
       (t nil)))))

(defun emagent-acp--tool-call-dangerous-p (tool-call)
  "Return non-nil when TOOL-CALL is a shell command with destructive intent.
Read and write tools are always safe.

For execute tools, analyses the command text against known dangerous
patterns: recursive deletes, disk writes, formatting, and similar.
Harmless commands like `mvn compile', `git status', or `ls' pass
through without prompting."
  (let ((kind (and (listp tool-call) (map-elt tool-call 'kind))))
    (when (and (stringp kind) (equal (downcase kind) "execute"))
      (let ((command (emagent-acp--tool-call-command-text tool-call)))
        (and (stringp command)
             (string-match-p emagent-acp--dangerous-command-regexps command))))))

(defun emagent-acp--permission-interactive-p (state)
  "Return non-nil when ACP permission prompts may need user input.
For `safe' auto-approve mode this returns t because the per-tool
decision is made in `emagent-acp--handle-one-permission'."
  (and (not (eq emagent-acp-auto-approve-permissions t))
       (not (map-elt state :session-auto-approve))))


(defun emagent-acp--schedule-permission-drain (state)
  "Run `emagent-acp--drain-permission-queue-now' outside the ACP process filter."
  (unless (or (map-elt state :permission-drain-timer)
              (map-elt state :permission-busy))
    (map-put! state :permission-drain-timer
              (run-at-time 0 nil
                           (lambda ()
                             (map-put! state :permission-drain-timer nil)
                             (emagent-acp--drain-permission-queue-now state))))))

(defun emagent-acp--enqueue-cursor-tool-resolve (state id &optional delay)
  "Queue ID for a serialized Cursor store.db lookup."
  (let ((queue (map-elt state :cursor-tool-resolve-queue)))
    (unless (member id queue)
      (map-put! state :cursor-tool-resolve-queue (append queue (list id)))))
  (unless (map-elt state :cursor-tool-resolve-worker)
    (emagent-acp--drain-cursor-tool-resolve-queue state delay)))

(defun emagent-acp--drain-cursor-tool-resolve-queue (state &optional delay)
  "Resolve one queued Cursor tool call via `run-at-time'."
  (unless (map-elt state :cursor-tool-resolve-worker)
    (if-let ((id (car (map-elt state :cursor-tool-resolve-queue))))
        (progn
          (map-put! state :cursor-tool-resolve-worker t)
          (run-at-time
           (or delay 0) nil
           (lambda ()
             (map-put! state :cursor-tool-resolve-worker nil)
             (map-put! state :cursor-tool-resolve-queue
                         (cdr (map-elt state :cursor-tool-resolve-queue)))
             (let ((retry-delay (emagent-acp--resolve-cursor-tool-from-store state id)))
               (if retry-delay
                   (emagent-acp--drain-cursor-tool-resolve-queue state retry-delay)
                 (progn
                   (emagent-acp--drain-cursor-tool-resolve-queue state 0)
                   (emagent-acp--maybe-complete-deferred-prompt state)))))))
      (progn
        (emagent-acp--drain-permission-queue state)
        (emagent-acp--maybe-complete-deferred-prompt state)))))

(defun emagent-acp--resolve-cursor-tool-from-store (state id)
  "Look up tool-call ID in Cursor store.db; return retry delay or nil when done."
  (when-let* ((pending-table (map-elt state :tool-call-pending))
              (merged (gethash id pending-table))
              (session-id (map-elt state :session-id))
              (fboundp 'emagent-cursor-enrich-tool-call-update))
    (let* ((enriched (emagent-cursor-enrich-tool-call-update session-id merged))
           (merged (if (equal enriched merged) merged
                     (emagent-acp--merged-tool-call-update state enriched)))
           (label (emagent-acp--tool-call-label merged))
           (status (map-elt merged 'status))
           (kind (map-elt merged 'kind))
           (attempts-table (map-elt state :cursor-tool-resolve-attempts))
           (attempts (gethash id attempts-table 0)))
      (cond
       ((emagent-acp--tool-call-meaningful-detail-p merged)
        (puthash id 0 attempts-table)
        (emagent-acp--emit-tool-call-display state id kind merged label status)
        (remhash id pending-table)
        nil)
       ((>= attempts emagent-acp--cursor-tool-resolve-max-attempts)
        (puthash id 0 attempts-table)
        (when (emagent-acp--tool-call-displayable-p merged)
          (emagent-acp--emit-tool-call-display state id kind merged label status))
        (remhash id pending-table)
        nil)
       (t
        (puthash id (1+ attempts) attempts-table)
        (let ((queue (map-elt state :cursor-tool-resolve-queue)))
          (unless (member id queue)
            (map-put! state :cursor-tool-resolve-queue (append queue (list id)))))
        (* emagent-acp--cursor-tool-resolve-base-delay (expt 2 attempts)))))))

(defun emagent-acp--tool-call-elisp-prin1-p (value)
  "Return non-nil when VALUE looks like a printed Elisp object."
  (and (stringp value)
       (string-match-p "\\`#s(" (string-trim value))))

(defun emagent-acp--tool-call-prin1-hash-detail (raw)
  "Extract a display string from a printed hash-table RAW, or nil."
  (when (emagent-acp--tool-call-elisp-prin1-p raw)
    (cl-loop for key in '(command path file pattern form directory args)
             when (string-match (format "(%s \\([^)]*\\))" key) raw)
             return (string-trim (match-string 1 raw)))))

(defun emagent-acp--tool-call-raw-input-empty-p (raw)
  "Return non-nil when tool-call rawInput carries no usable parameters."
  (or (null raw)
      (emagent-acp--tool-call-elisp-prin1-p raw)
      (and (stringp raw)
           (let ((trimmed (string-trim raw)))
             (or (string-empty-p trimmed)
                 (member trimmed '("{}" "[]" "null")))))
      (and (listp raw) (null raw))
      (and (hash-table-p raw) (zerop (hash-table-count raw)))))

(defun emagent-acp--tool-call-update-from-request (tool-call)
  "Return an ACP tool-call UPDATE alist from a permission TOOL-CALL object."
  (when-let ((id (map-elt tool-call 'toolCallId)))
    (append `((toolCallId . ,id))
            (cl-remove nil
                       (list (when-let ((v (map-elt tool-call 'title)))
                               (cons 'title v))
                             (when-let ((v (map-elt tool-call 'rawInput)))
                               (cons 'rawInput v))
                             (when-let ((v (map-elt tool-call 'arguments)))
                               (cons 'arguments v))
                             (when-let ((v (map-elt tool-call 'subtitle)))
                               (cons 'subtitle v))
                             (when-let ((v (map-elt tool-call 'kind)))
                               (cons 'kind v))
                             (when-let ((v (map-elt tool-call 'status)))
                               (cons 'status v)))))))

(defun emagent-acp--ingest-tool-call-request (state tool-call)
  "Merge TOOL-CALL from session/request_permission and refresh display."
  (when-let ((update (emagent-acp--tool-call-update-from-request tool-call)))
    (emagent-acp--on-tool-call state update)))

(defun emagent-acp--emit-tool-call-display (state id kind _merged label status)
  "Push TOOL-CALL LABEL to the chat buffer and update session UI."
  (let* ((labels (map-elt state :tool-call-labels))
         (prev (and id labels (gethash id labels)))
         (completed (member status '("completed" "failed")))
         (label-changed (and label (not (string-empty-p label))
                             (or (null prev) (not (string= prev label))))))
    (when label
      (emagent-acp--detect-external-refusal-in-text state label))
    (when label-changed
      (when id (puthash id label labels))
      (unless completed
        (emagent-acp--notify-user state (format "emagent: tool %s" label)))
      (when-let ((buf (emagent-acp--chat-buffer state)))
        (with-current-buffer buf
          (emagent-chat-show-tool-call id label))))
    (if completed
        (progn
          (map-put! state :current-tool nil)
          (map-put! state :current-tool-kind nil))
      (when label-changed
        (map-put! state :current-tool label)
        (when kind (map-put! state :current-tool-kind kind))
        (emagent-acp--schedule-prompt-watchdog state)))
    (when (or label-changed completed)
      (emagent-acp--refresh-mode-line state))))

(defun emagent-acp--tool-call-truncate (string)
  "Return STRING truncated for tool-call display."
  (when string
    (if (> (length string) emagent-acp--tool-call-detail-limit)
        (concat (substring string 0 emagent-acp--tool-call-detail-limit) "…")
      string)))

(defun emagent-acp--tool-call-data-get (data key)
  "Return KEY from ACP tool-call DATA alist or hash-table."
  (cond
   ((hash-table-p data)
    (or (gethash key data)
        (gethash (symbol-name key) data)
        (gethash (downcase (symbol-name key)) data)))
   ((listp data)
    (or (alist-get key data)
        (alist-get (symbol-name key) data)
        (cdr (assoc key data))
        (cdr (assoc (symbol-name key) data))
        (cdr (assoc (downcase (symbol-name key)) data))))
   (t nil)))

(defun emagent-acp--tool-call-value-string (value)
  "Return a display string for tool-call VALUE, or nil."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((null value) nil)
   ((hash-table-p value)
    (cl-loop for key in '(command path file file_path target_file filename
                               relativeWorkspacePath url query q search input
                               text pattern glob form directory dir name args)
             for v = (emagent-acp--tool-call-data-get value key)
             when (and (stringp v) (not (string-empty-p (string-trim v))))
             return (string-trim v)))
   (t (let ((text (prin1-to-string value)))
        (unless (string-empty-p text) text)))))

(defun emagent-acp--tool-call-normalize-data (raw)
  "Return RAW tool input as an alist/hash-table, parsing JSON strings."
  (cond
   ((or (hash-table-p raw) (listp raw)) raw)
   ((stringp raw)
    (condition-case nil
        (json-parse-string raw
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object nil)
      (error nil)))
   (t nil)))

(defun emagent-acp--tool-call-edits-detail (raw)
  "Extract a file-path summary from tool-call edit lists in RAW."
  (when-let ((data (emagent-acp--tool-call-normalize-data raw))
             (edits (emagent-acp--tool-call-data-get data 'edits)))
    (let* ((items (cond
                   ((vectorp edits) (append edits nil))
                   ((listp edits) edits)
                   (t nil)))
           (paths
            (delq nil
                  (mapcar
                   (lambda (item)
                     (emagent-acp--tool-call-value-string
                      (or (emagent-acp--tool-call-data-get item 'path)
                          (emagent-acp--tool-call-data-get item 'file_path)
                          (emagent-acp--tool-call-data-get item 'target_file)
                          (emagent-acp--tool-call-data-get item 'relativeWorkspacePath))))
                   items))))
      (when paths
        (if (= (length paths) 1)
            (car paths)
          (format "%s (+%d more)" (car paths) (1- (length paths))))))))

(defun emagent-acp--tool-call-raw-input-detail (raw)
  "Extract a concise detail string from tool-call rawInput RAW."
  (or
   (when-let* ((data (emagent-acp--tool-call-normalize-data raw))
               (key (seq-find (lambda (k)
                                (emagent-acp--tool-call-value-string
                                 (emagent-acp--tool-call-data-get data k)))
                              '(path file file_path target_file filename
                                    relativeWorkspacePath url command query q
                                    search input text pattern glob form
                                    directory dir name args description))))
     (emagent-acp--tool-call-value-string
      (emagent-acp--tool-call-data-get data key)))
   (emagent-acp--tool-call-prin1-hash-detail raw)))

(defun emagent-acp--tool-call-locations-detail (locations)
  "Extract a file-path summary from tool-call locations LOCATIONS."
  (when locations
    (let ((paths (delq nil
                       (mapcar (lambda (loc)
                                 (cond
                                  ((stringp loc) loc)
                                  ((listp loc) (map-elt loc 'path))
                                  ((hash-table-p loc)
                                   (or (gethash "path" loc) (gethash 'path loc)))))
                               (append locations nil)))))
      (when paths
        (if (= (length paths) 1)
            (car paths)
          (format "%s (+%d more)" (car paths) (1- (length paths))))))))

(defun emagent-acp--tool-call-content-detail (content)
  "Extract a concise detail string from tool-call content CONTENT."
  (when content
    (let* ((item (cond
                  ((vectorp content) (and (> (length content) 0) (aref content 0)))
                  ((listp content) (car content))
                  (t nil)))
           (text (or (map-nested-elt item '(content text))
                     (map-nested-elt item '(text))
                     (and (stringp item) item))))
      (when (and (stringp text) (not (string-empty-p (string-trim text))))
        (string-trim text)))))

(defun emagent-acp--tool-call-input (update)
  "Return raw tool input from ACP UPDATE.

Cursor often sends a useless `#s(hash-table …)' string in rawInput while the
real parameters live in arguments; prefer arguments when both are present."
  (let ((args (map-elt update 'arguments))
        (raw (map-elt update 'rawInput)))
    (cond
     ((and args (not (emagent-acp--tool-call-raw-input-empty-p args))) args)
     ((and raw (not (emagent-acp--tool-call-raw-input-empty-p raw))) raw)
     (t (or args raw)))))

(defun emagent-acp--tool-call-detail (update)
  "Return a concise detail string from ACP tool-call UPDATE, or nil."
  (let ((input (emagent-acp--tool-call-input update)))
    (or (emagent-acp--tool-call-raw-input-detail input)
        (emagent-acp--tool-call-edits-detail input)
        (emagent-acp--tool-call-locations-detail (map-elt update 'locations))
        (let ((subtitle (map-elt update 'subtitle)))
          (when (emagent-acp--human-tool-detail-p subtitle)
            subtitle))
        (emagent-acp--tool-call-content-detail (map-elt update 'content)))))

(defconst emagent-acp--tool-call-weak-details
  '("tool" "Tool" "running" "pending")
  "ACP tool-call detail strings too generic to display without store.db lookup.")

(defun emagent-acp--tool-call-meaningful-detail-p (update)
  "Return non-nil when UPDATE has useful path, command, or similar detail."
  (when-let ((detail (emagent-acp--tool-call-detail update)))
    (let ((trimmed (string-trim detail)))
      (and (not (string-empty-p trimmed))
           (not (member trimmed emagent-acp--tool-call-weak-details))))))

(defun emagent-acp--tool-call-generic-title-p (title)
  "Return non-nil when TITLE is too generic to show without detail."
  (and (fboundp 'emagent-cursor--generic-acp-title-p)
       (funcall #'emagent-cursor--generic-acp-title-p title)))

(defun emagent-acp--tool-call-redundant-detail-p (title detail)
  "Return non-nil when DETAIL adds nothing beyond generic TITLE."
  (when (and (stringp title) (stringp detail))
    (let* ((t0 (downcase (string-trim title)))
           (d0 (downcase (string-trim detail)))
           (t1 (replace-regexp-in-string "^emagent-" "" t0))
           (t2 (replace-regexp-in-string "^mcp_" "" t1)))
      (or (string= t0 d0)
          (string= t1 d0)
          (string= t2 d0)
          (and (string-match-p ":" t0)
               (string= (car (split-string t0 ":")) d0))))))

(defun emagent-acp--tool-call-displayable-p (update)
  "Return non-nil when UPDATE should appear in the Thinking block."
  (let* ((title (string-trim (or (map-elt update 'title) "")))
         (detail (emagent-acp--tool-call-detail update)))
    (cond
     ((and detail
           (emagent-acp--tool-call-meaningful-detail-p update)
           (not (emagent-acp--tool-call-redundant-detail-p title detail)))
      t)
     ((and (not (string-empty-p title))
           (not (emagent-acp--tool-call-generic-title-p title))
           (or (null detail) (string-empty-p detail)))
      t)
     (t nil))))

(defun emagent-acp--tool-call-label (update)
  "Return a display label for ACP tool-call UPDATE."
  (let* ((title (string-trim (or (map-elt update 'title) "tool")))
         (title (if (string-match-p "\\`MCP:? *tool\\'" title) "MCP" title))
         (detail (emagent-acp--tool-call-detail update)))
    (cond
     ((and detail (not (string-empty-p detail))
           (not (string-match-p (regexp-quote detail) title)))
      (format "%s: %s" title (emagent-acp--tool-call-truncate detail)))
     ((and detail (not (string-empty-p detail))) detail)
     (t title))))

(defun emagent-acp--merged-tool-call-update (state update)
  "Return UPDATE merged with stored title/rawInput for STATE."
  (let* ((id (map-elt update 'toolCallId))
         (titles (map-elt state :tool-call-titles))
         (inputs (map-elt state :tool-call-inputs))
         (stored-title (and id titles (gethash id titles)))
         (stored-input (and id inputs (gethash id inputs)))
         (title (or (map-elt update 'title) stored-title))
         (raw-input (or (map-elt update 'rawInput)
                        (map-elt update 'arguments)
                        stored-input))
         (merged update))
    (when (and id title)
      (puthash id title titles))
    (when (and id raw-input)
      (puthash id raw-input inputs))
    (when title
      (setq merged (emagent-acp--update-put merged 'title title)))
    (when (and raw-input (not (emagent-acp--tool-call-raw-input-empty-p raw-input)))
      (setq merged (emagent-acp--update-put merged 'rawInput raw-input)))
    (when (and id (map-elt merged 'rawInput))
      (puthash id (map-elt merged 'rawInput) inputs))
    merged))

(defun emagent-acp--on-tool-call (state update)
  "Display or refresh a tool-call line from ACP UPDATE."
  (unless (map-elt state :replaying-history)
    (let* ((id (map-elt update 'toolCallId))
           (status (map-elt update 'status))
           (kind (map-elt update 'kind))
           (merged (emagent-acp--merged-tool-call-update state update))
           (label (emagent-acp--tool-call-label merged))
           (pending-table (map-elt state :tool-call-pending))
           (has-detail (emagent-acp--tool-call-meaningful-detail-p merged))
           (defer (and (emagent-acp--cursor-agent-p state)
                       id
                       (not has-detail)))
           (show (and label (not (string-empty-p label)) (not defer)
                        (emagent-acp--tool-call-displayable-p merged))))
      (when defer
        (puthash id merged pending-table)
        (emagent-acp--enqueue-cursor-tool-resolve state id))
      (when show
        (emagent-acp--emit-tool-call-display state id kind merged label status)
        (when id (remhash id pending-table)))
      (when (map-elt state :permission-queue)
        (emagent-acp--drain-permission-queue state)))))

(defun emagent-acp--human-tool-detail-p (detail)
  "Return non-nil when DETAIL is safe to show in a permission prompt."
  (and (stringp detail)
       (let ((trimmed (string-trim detail)))
         (and (not (string-empty-p trimmed))
              (not (member trimmed emagent-acp--tool-call-weak-details))
              (not (emagent-acp--tool-call-elisp-prin1-p trimmed))))))

(defun emagent-acp--tool-call-detail-from-tool-call (tool-call)
  "Return a human-readable detail string from permission TOOL-CALL."
  (when tool-call
    (let ((update (emagent-acp--tool-call-update-from-request tool-call)))
      (or (and update (emagent-acp--tool-call-detail update))
          (emagent-acp--tool-call-raw-input-detail (map-elt tool-call 'arguments))
          (emagent-acp--tool-call-raw-input-detail (map-elt tool-call 'rawInput))))))

(defun emagent-acp--permission-question-line (emagent-acp-request)
  "Return the command or path to show on the permission ? line."
  (let* ((tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (detail (and tool-call (emagent-acp--tool-call-detail-from-tool-call tool-call)))
         (title (emagent-acp--permission-prompt-title emagent-acp-request)))
    (cond
     ((emagent-acp--human-tool-detail-p detail) detail)
     (title (replace-regexp-in-string "\\`Allow \\(.*\\)[?]\\'" "\\1" title))
     (t "Permission request"))))

(defun emagent-acp--permission-prompt-title (emagent-acp-request)
  "Return the primary permission question line from EMagent-ACP-REQUEST."
  (when-let ((raw (or (map-nested-elt emagent-acp-request '(params title))
                       (map-nested-elt emagent-acp-request '(params toolCall title))
                       "Permission request")))
    (car (split-string raw "\n" t))))

(defun emagent-acp--permission-prompt-text (emagent-acp-request)
  "Return user-facing permission prompt text for EMagent-ACP-REQUEST."
  (let* ((title (emagent-acp--permission-prompt-title emagent-acp-request))
         (tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (detail (emagent-acp--tool-call-detail-from-tool-call tool-call)))
    (if (and (emagent-acp--human-tool-detail-p detail)
             (not (string-match-p (regexp-quote detail) title)))
        (format "%s\n%s" title (emagent-acp--tool-call-truncate detail))
      title)))

(cl-defun emagent-acp--handle-one-permission (&key state emagent-acp-request)
  "Process a single queued permission request synchronously."
  (let ((tool-call (map-nested-elt emagent-acp-request '(params toolCall))))
    (when tool-call
      (emagent-acp--ingest-tool-call-request state tool-call))
    (let* ((options (map-nested-elt emagent-acp-request '(params options)))
           (question (emagent-acp--permission-question-line emagent-acp-request))
           (choices (mapcar (lambda (opt)
                              (cons (or (map-elt opt 'name) (map-elt opt 'optionId))
                                    (map-elt opt 'optionId)))
                            (append options nil)))
           (choice-list (append choices '(("Allow All (session)" . :allow-all))))
           (auto-approve
            (or (eq emagent-acp-auto-approve-permissions t)
                (map-elt state :session-auto-approve)
                (and (eq emagent-acp-auto-approve-permissions 'safe)
                     tool-call
                     (not (emagent-acp--tool-call-dangerous-p tool-call)))))
         (buf (emagent-acp--chat-buffer state))
         (choice
          (if auto-approve
              (emagent-acp--permission-option-id options)
            (progn
              (emagent-acp--prepare-interactive-context state)
              (emagent-acp--clear-prompt-watchdog state)
              (unwind-protect
                  (let ((raw
                         (if (and buf (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (emagent-chat--open-response-p)))
                             (with-current-buffer buf
                               (emagent-chat-permission-prompt question choice-list tool-call))
                           (emagent-tools--buttons-prompt
                            question choice-list buf))))
                    (if (eq raw :allow-all)
                        (progn
                          (map-put! state :session-auto-approve t)
                          (emagent-log "permission: Allow All (session) — auto-approving all future requests")
                          (emagent-acp--permission-option-id options))
                      raw))
                (when (map-elt state :busy)
                  (emagent-acp--schedule-prompt-watchdog state))
                (emagent-acp--refresh-mode-line state))))))
    (when auto-approve
      (emagent-log "permission auto-approve: %s → %s"
                   question (or choice "cancelled (no allow option)")))
    (emagent-acp-send-response
     :client (map-elt state :client)
     :response (if choice
                   (emagent-acp-make-session-request-permission-response
                    :request-id (map-elt emagent-acp-request 'id)
                    :option-id choice)
                 ;; C-g or empty options: send cancelled so the agent doesn't hang.
                 (emagent-acp-make-session-request-permission-response
                  :request-id (map-elt emagent-acp-request 'id)
                  :cancelled t))))))

(defun emagent-acp--drain-permission-queue-now (state)
  "Process one queued permission request synchronously."
  (unless (map-elt state :permission-busy)
    (when-let ((request (car (map-elt state :permission-queue))))
      (map-put! state :permission-queue (cdr (map-elt state :permission-queue)))
      (map-put! state :permission-busy t)
      (emagent-acp--refresh-mode-line state)
      (unwind-protect
          (condition-case err
              (emagent-acp--handle-one-permission :state state :emagent-acp-request request)
            (error
             (emagent-log "permission handler error: %s" (error-message-string err))))
        (map-put! state :permission-busy nil)
        (emagent-acp--refresh-mode-line state))
      (emagent-acp--maybe-complete-deferred-prompt state)
      (when (map-elt state :permission-queue)
        (if (emagent-acp--permission-interactive-p state)
            (emagent-acp--schedule-permission-drain state)
          (emagent-acp--drain-permission-queue-now state))))))

(defun emagent-acp--drain-permission-queue (state)
  "Process queued permission requests one at a time.

Interactive prompts are deferred to the next event cycle so
`recursive-edit' never runs inside the ACP process filter."
  (when (map-elt state :permission-queue)
    (if (emagent-acp--permission-interactive-p state)
        (emagent-acp--schedule-permission-drain state)
      (emagent-acp--drain-permission-queue-now state))))

(cl-defun emagent-acp--on-permission (&key state emagent-acp-request)
  (map-put! state :permission-queue
            (append (map-elt state :permission-queue) (list emagent-acp-request)))
  (emagent-acp--drain-permission-queue state))

(cl-defun emagent-acp--on-request (&key state emagent-acp-request)
  (pcase (map-elt emagent-acp-request 'method)
    ("fs/read_text_file"
     (emagent-acp--on-fs-read :state state :emagent-acp-request emagent-acp-request))
    ("fs/write_text_file"
     (emagent-acp--on-fs-write :state state :emagent-acp-request emagent-acp-request))
    ("session/request_permission"
     (emagent-acp--on-permission :state state :emagent-acp-request emagent-acp-request))
    (_
     (emagent-acp-send-response
      :client (map-elt state :client)
      :response `((:request-id . ,(map-elt emagent-acp-request 'id))
                  (:error . ,(emagent-acp-make-error
                              :code -32601
                              :message (format "Unsupported method: %s"
                                               (map-elt emagent-acp-request 'method)))))))))

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
