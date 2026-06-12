;;; emagent-acp.el --- ACP wire-up for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (acp "0.12.2"))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'acp)
(require 'emagent-acp-compat)
(require 'emagent-log)
(require 'emagent-chat)
(require 'emagent-context)
(require 'emagent-mcp)
(require 'emagent-prompts)

(declare-function emagent-chat-clear-slash-commands "emagent-chat")

;; Backward compatibility (aliases before their referents).
(define-obsolete-variable-alias 'emagent-acp-emacs-native 'emagent-acp-prefer-emacs "0.1.0")
(define-obsolete-variable-alias 'emagent-acp-emacs-only 'emagent-acp-prefer-emacs "0.1.0")

(defcustom emagent-acp-prefer-emacs t
  "Instruct the agent to prefer emagent and Emacs tools, with external fallback.

When non-nil (default), emagent tells the agent to reach for emagent MCP tools
and Emacs Lisp first, but still allows Claude Code built-in tools, plugin slash
commands, and forwarded MCP gateways when Emacs cannot do the job.  File search
uses pure Emacs grep rather than ripgrep.  Keep `emagent-acp-file-access'
enabled so ACP file read/write route through Emacs buffers."
  :type 'boolean
  :group 'emagent)

(defun emagent-acp--system-prompt ()
  "Return the system prompt for new ACP sessions."
  (concat emagent-acp-system-prompt
          (emagent-mcp-gateway-system-prompt)
          (when emagent-acp-prefer-emacs
            emagent-acp-system-prompt-prefer-emacs)))

(defun emagent-acp-prefer-emacs-p ()
  "Return non-nil when emagent instructs the agent to prefer Emacs tools."
  emagent-acp-prefer-emacs)

(define-obsolete-function-alias 'emagent-acp-emacs-native-p 'emagent-acp-prefer-emacs-p "0.1.0")
(define-obsolete-function-alias 'emagent-acp-emacs-only-p 'emagent-acp-prefer-emacs-p "0.1.0")

(defcustom emagent-acp-file-access t
  "Route ACP file read/write through Emacs file tools.

When non-nil (default), agent read_file and write_file calls run
`emagent-tools--read-file-content' and `emagent-tools--write-file-content'
instead of cursor-agent's own file tools.  That matches agent-shell and
avoids macOS \"access data from other apps\" prompts on normal project trees.

Emagent still refuses iCloud and ~/Library/Containers paths to avoid separate
iCloud Drive prompts.  Set to nil only if you prefer the agent's own file tools."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-auto-approve-permissions t
  "Automatically approve ACP session permission requests.

These gates do not replace the external agent's own tool permissions; they
only unblock the ACP session handshake."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-stream-to-buffer nil
  "When non-nil, stream agent chunks into the chat buffer while a prompt is busy.

Disabled by default because interleaved ACP notifications can finalize before
the full reply arrives.  Reasoning and answers are rendered once the prompt
completes."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-thought-progress 'buffer
  "How to surface agent reasoning from `agent_thought_chunk' updates.

- nil — silent in `emagent-log-buffer-name' (Reasoning still appears on finish)
- buffer — stream and show Reasoning in the chat buffer (default)
- minimal — truncated one-line log entries in `emagent-log-buffer-name'
- trail — log entries keeping the sentence tail visible
- both — Reasoning block in the chat buffer and minimal log lines"
  :type '(choice (const :tag "Silent" nil)
                 (const :tag "Chat buffer" buffer)
                 (const :tag "Log per sentence" minimal)
                 (const :tag "Log sentence tail" trail)
                 (const :tag "Chat buffer and log" both))
  :group 'emagent)

(defcustom emagent-acp-render-delay 0.05
  "Seconds to wait after the last prompt chunk before rendering the response."
  :type 'number
  :group 'emagent)

(defcustom emagent-log-agent-stderr nil
  "When non-nil, log filtered cursor-agent stderr to `emagent-log-buffer-name'."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-watchdog-timeout 300
  "Seconds of inactivity before the prompt watchdog fires.

The watchdog resets on each tool-call notification, so this measures idle
time since the last tool call, not total prompt duration.  Increase if your
agent regularly makes long chains of tool calls."
  :type 'integer
  :group 'emagent)

(defcustom emagent-acp-trace nil
  "Log ACP wire events to `emagent-log-buffer-name'.

Shows outgoing methods, each `session/update' type with payload size, and
when `session/prompt' completes.  Also enables `acp-logging-enabled' for
the full wire log in the ACP logs buffer (`acp-logs-buffer')."
  :type 'boolean
  :group 'emagent)

(defconst emagent-acp-auto-model-id "auto"
  "Model id used by cursor-agent-acp for automatic model selection.

Claude and other agents that do not advertise this id fall back to the
session's current model instead.")

(defvar-local emagent-acp--session nil
  "ACP session state for the current emagent buffer.")

(defun emagent-acp--strip-pino-colors (string)
  "Remove literal pino color tokens like [32m from STRING."
  (replace-regexp-in-string "\\[[0-9]+m" "" string))

(defun emagent-acp--agent-log-line-p (line)
  "Return non-nil when LINE is cursor-agent info/warn stderr."
  (let ((line (string-trim (emagent-acp--strip-pino-colors line))))
    (or (string-empty-p line)
        (string-match-p "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] .*\\[ *\\(info\\|warn\\) *\\]:" line))))

(defun emagent-acp--stderr-notify-p (acp-error)
  "Return non-nil when ACP-ERROR should be shown to the user."
  (let ((message (string-trim (or (map-elt acp-error 'message) (format "%s" acp-error)))))
    (cond
     ((string-empty-p message) nil)
     ((string-match-p "\\`\\(?:finished\\|Process\\|acp-client(\\)" message) nil)
     ((string-match-p "\\[32minfo\\|\\[33mwarn" message) nil)
     ((string-match-p "ApiError\\|failed with status\\|\\[31merror" message) t)
     (t
      (let ((lines (split-string message "\n" t)))
        (not (and lines (seq-every-p #'emagent-acp--agent-log-line-p lines))))))))

(defun emagent-acp--log-agent-stderr (message)
  (when emagent-log-agent-stderr
    (emagent-log "agent: %s" (string-trim message))))

(defun emagent-acp--session ()
  (or emagent-acp--session
      (error "No active emagent session for this buffer")))

(defun emagent-acp--agent-rss-mb (state)
  "Return the agent process RSS in MB via `process-attributes', or nil."
  (when-let* ((client (map-elt state :client))
              (proc (and client (map-elt client :process)))
              ((processp proc))
              (pid (process-id proc))
              ((> pid 0))
              (attrs (ignore-errors (process-attributes pid)))
              (rss-kb (alist-get 'rss attrs)))
    (round (/ (float rss-kb) 1024))))

(defun emagent-acp--start-rss-timer (state)
  "Start a repeating timer that refreshes :agent-rss in STATE every 15 s."
  (when-let ((old (map-elt state :agent-rss-timer)))
    (cancel-timer old))
  (map-put! state :agent-rss-timer
            (run-with-timer
             5 15
             (lambda ()
               (let ((mb (emagent-acp--agent-rss-mb state)))
                 (map-put! state :agent-rss mb)
                 (emagent-acp--refresh-mode-line state))))))

(defun emagent-acp--stop-rss-timer (state)
  "Cancel the RSS polling timer for STATE."
  (when-let ((timer (and state (map-elt state :agent-rss-timer))))
    (cancel-timer timer)
    (map-put! state :agent-rss-timer nil)))

(defun emagent-acp--connected-p ()
  "Return non-nil when the current buffer has a live, ready ACP session."
  (and emagent-acp--session
       (map-elt emagent-acp--session :ready)
       (let ((client (map-elt emagent-acp--session :client)))
         (and client (acp--client-started-p client)))))

(defun emagent-acp--teardown-stale-session ()
  "Shut down a dead or incomplete ACP session without clearing persisted ids."
  (when-let* ((state emagent-acp--session)
              (client (map-elt state :client)))
    (ignore-errors (acp-shutdown :client client)))
  (setq emagent-acp--session nil))

(cl-defun emagent-acp--make-state (&key client chat-buffer on-reveal)
  "Return a mutable session state table for Emacs 30 `map-put!'.

Plain alists cannot grow via `map-put!' on Emacs 30; hash tables can."
  (let ((state (make-hash-table :test 'eq))
        (usage (make-hash-table :test 'eq)))
    (puthash :context-used nil usage)
    (puthash :context-size nil usage)
    (puthash :total-tokens 0 usage)
    (puthash :client client state)
    (puthash :chat-buffer chat-buffer state)
    (puthash :session-id nil state)
    (puthash :config-options nil state)
    (puthash :usage usage state)
    (puthash :initialized nil state)
    (puthash :mcp-http nil state)
    (puthash :ready nil state)
    (puthash :busy nil state)
    (puthash :assistant-text "" state)
    (puthash :thought-text "" state)
    (puthash :thought-buffer "" state)
    (puthash :prompt-finalized nil state)
    (puthash :prompt-finishing nil state)
    (puthash :finish-token nil state)
    (puthash :finish-timer nil state)
    (puthash :prompt-watchdog nil state)
    (puthash :extra-context nil state)
    (puthash :replaying-history nil state)
    (puthash :current-tool nil state)
    ;; Rendering callbacks — set by the integration layer (emagent.el).
    ;; :cb-chunk fn(text)       — streaming assistant text chunk
    ;; :cb-thought fn(text)     — streaming reasoning text chunk
    ;; :cb-finish fn(text,thought) — complete response rendered
    ;; :cb-fail fn(message)     — error/abort rendered
    ;; :cb-slash-commands fn(commands) — available-commands list received
    (puthash :cb-chunk nil state)
    (puthash :cb-thought nil state)
    (puthash :cb-finish nil state)
    (puthash :cb-fail nil state)
    (puthash :cb-slash-commands nil state)
    (puthash :agent-rss nil state)
    (puthash :agent-rss-timer nil state)
    (puthash :on-reveal on-reveal state)
    state))

;;;; Public session state accessors (for use by emagent-chat.el)

(defun emagent-acp-busy-p ()
  "Return non-nil when the current buffer's ACP session is processing a prompt."
  (and emagent-acp--session (map-elt emagent-acp--session :busy)))

(defun emagent-acp-ready-p ()
  "Return non-nil when the current buffer's ACP session is connected and idle."
  (and emagent-acp--session (map-elt emagent-acp--session :ready)))

(defun emagent-acp-current-tool ()
  "Return the name of the tool currently running, or nil."
  (and emagent-acp--session (map-elt emagent-acp--session :current-tool)))

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

(defun emagent-acp--chat-buffer (state)
  (map-elt state :chat-buffer))

(defun emagent-acp--session-cwd (state)
  (with-current-buffer (emagent-acp--chat-buffer state)
    (emagent-chat--session-directory)))

(defun emagent-acp--persist-session-id (state session-id)
  (with-current-buffer (emagent-acp--chat-buffer state)
    (emagent-chat-set-session-id session-id)))

(defun emagent-acp--saved-session-id (state)
  (with-current-buffer (emagent-acp--chat-buffer state)
    (emagent-chat-session-id)))

(defun emagent-acp--saved-model-id (state)
  (with-current-buffer (emagent-acp--chat-buffer state)
    (emagent-chat-model)))

(defun emagent-acp--persist-model-id (state model-id)
  (with-current-buffer (emagent-acp--chat-buffer state)
    (emagent-chat-set-model model-id))
  (emagent-acp--refresh-mode-line state))

(defun emagent-acp--refresh-mode-line (state)
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (force-mode-line-update t))))

(defun emagent-acp--usage-state (state)
  (or (map-elt state :usage)
      (let ((usage (make-hash-table :test 'eq)))
        (puthash :context-used nil usage)
        (puthash :context-size nil usage)
        (puthash :total-tokens 0 usage)
        (map-put! state :usage usage)
        usage)))

(defun emagent-acp--save-usage-from-response (state acp-usage)
  "Update STATE usage from a prompt response usage field."
  (let ((usage (emagent-acp--usage-state state)))
    (when-let ((total (map-elt acp-usage 'totalTokens)))
      (map-put! usage :total-tokens total))
    (map-put! state :usage usage)))

(defun emagent-acp--update-usage-from-notification (state acp-update)
  "Update STATE usage from a session/update usage_update payload."
  (let ((usage (emagent-acp--usage-state state)))
    (when-let ((used (or (map-elt acp-update 'used)
                         (map-elt acp-update 'contextUsed))))
      (map-put! usage :context-used used))
    (when-let ((size (or (map-elt acp-update 'size)
                         (map-elt acp-update 'contextLimit)
                         (map-elt acp-update 'contextSize))))
      (map-put! usage :context-size size))
    (map-put! state :usage usage)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp--normalize-config-option (acp-option)
  `((:id . ,(map-elt acp-option 'id))
    (:name . ,(map-elt acp-option 'name))
    (:description . ,(map-elt acp-option 'description))
    (:category . ,(map-elt acp-option 'category))
    (:type . ,(map-elt acp-option 'type))
    (:current-value . ,(map-elt acp-option 'currentValue))
    (:options . ,(mapcar (lambda (acp-value)
                            `((:value . ,(map-elt acp-value 'value))
                              (:name . ,(map-elt acp-value 'name))
                              (:description . ,(map-elt acp-value 'description))))
                          (append (map-elt acp-option 'options) nil)))))

(defun emagent-acp--normalize-config-options (acp-config-options)
  (mapcar #'emagent-acp--normalize-config-option
          (append acp-config-options nil)))

(defun emagent-acp--save-config-options (state acp-config-options)
  (when acp-config-options
    (map-put! state :config-options
              (emagent-acp--normalize-config-options acp-config-options))))

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
       :request (acp-make-session-set-config-option-request
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
     :request (acp-make-session-set-model-request
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
  (map-put! state :prompt-watchdog nil))

(defun emagent-acp--schedule-prompt-watchdog (state)
  "Abort a prompt that stays busy without ACP progress."
  (let ((token (cl-gensym "emagent-prompt-watchdog")))
    (map-put! state :prompt-watchdog token)
    (run-with-timer
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

(defun emagent-acp--stream-to-buffer-p (state)
  "Return non-nil when agent chunks may update the chat buffer live."
  (and emagent-acp-stream-to-buffer
       (map-elt state :busy)
       (not (map-elt state :prompt-finalized))
       (not (map-elt state :prompt-finishing))))

(defun emagent-acp--stream-thought-to-buffer-p (state)
  "Return non-nil when reasoning may stream into the chat buffer live."
  (and (memq emagent-acp-thought-progress '(buffer both))
       (map-elt state :busy)
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
                                 (error-message-string err))))))))
    (map-put! state :prompt-finishing nil)
    (map-put! state :prompt-finalized t)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp--complete-prompt (state response)
  "Finalize the in-flight prompt for STATE and close the chat response."
  (cond
   ((map-elt state :prompt-finalized)
    (when (map-elt state :busy)
      (map-put! state :busy nil)
      (emagent-acp--refresh-mode-line state)))
   ((not (map-elt state :busy))
    nil)
   (t
    (map-put! state :prompt-finishing t)
    (map-put! state :busy nil)
    (map-put! state :current-tool nil)
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
    (map-put! state :thought-text
              (concat (or (map-elt state :thought-text) "") text))
    (when-let ((mode emagent-acp-thought-progress))
      (when (map-elt state :prompt-finishing)
        (emagent-acp--schedule-prompt-render state))
      (when (memq mode '(buffer both))
        (when (and (emagent-acp--stream-thought-to-buffer-p state)
                   (buffer-live-p (emagent-acp--chat-buffer state)))
          (with-current-buffer (emagent-acp--chat-buffer state)
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
    (acp-send-request
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
  (acp-make-error
   :code -32603
   :message (format "Refusing Emacs access to %s (iCloud or another app's container)"
                    (emagent-tools--root-directory path))))

(defun emagent-acp--fs-unavailable-response (method)
  (acp-make-error
   :code -32601
   :message (format "%s disabled; use the external agent's project file tools"
                    method)))

(cl-defun emagent-acp--on-fs-read (&key state acp-request)
  (let ((client (map-elt state :client))
        (request-id (map-elt acp-request 'id))
        (path (map-nested-elt acp-request '(params path))))
    (if (not emagent-acp-file-access)
        (acp-send-response
         :client client
         :response (acp-make-fs-read-text-file-response
                    :request-id request-id
                    :error (emagent-acp--fs-unavailable-response "fs/read_text_file")))
      (if (emagent-tools--protected-fs-path-p path)
          (acp-send-response
           :client client
           :response (acp-make-fs-read-text-file-response
                      :request-id request-id
                      :error (emagent-acp--protected-fs-error path)))
        (condition-case err
            (let* ((line (or (map-nested-elt acp-request '(params line)) 1))
                   (limit (map-nested-elt acp-request '(params limit)))
                   (content (emagent-tools--read-file-content path line limit)))
              (acp-send-response
               :client client
               :response (acp-make-fs-read-text-file-response
                          :request-id request-id
                          :content content)))
          (file-missing
           (acp-send-response
            :client client
            :response (acp-make-fs-read-text-file-response
                       :request-id request-id
                       :error (acp-make-error :code -32002
                                              :message "Resource not found"))))
          (error
           (acp-send-response
            :client client
            :response (acp-make-fs-read-text-file-response
                       :request-id request-id
                       :error (acp-make-error :code -32603
                                              :message (error-message-string err))))))))))

(cl-defun emagent-acp--on-fs-write (&key state acp-request)
  (let ((client (map-elt state :client))
        (request-id (map-elt acp-request 'id))
        (path (map-nested-elt acp-request '(params path)))
        (resolved (emagent-tools--root-directory
                   (map-nested-elt acp-request '(params path)))))
    (if (not emagent-acp-file-access)
        (acp-send-response
         :client client
         :response (acp-make-fs-write-text-file-response
                    :request-id request-id
                    :error (emagent-acp--fs-unavailable-response "fs/write_text_file")))
      (if (emagent-tools--protected-fs-path-p path)
          (acp-send-response
           :client client
           :response (acp-make-fs-write-text-file-response
                      :request-id request-id
                      :error (emagent-acp--protected-fs-error path)))
        (progn
          (emagent-acp--prepare-interactive-context state)
          (condition-case err
              (if (y-or-n-p (format "Allow emagent to write %s? " resolved))
                  (let ((written (emagent-tools--write-file-content
                                   path (map-nested-elt acp-request '(params content)))))
                    (emagent-acp--notify-user
                     state (format "emagent: wrote %s (C-/ to undo in that buffer)"
                                   written))
                    (acp-send-response
                     :client client
                     :response (acp-make-fs-write-text-file-response
                                :request-id request-id)))
                (acp-send-response
                 :client client
                 :response (acp-make-fs-write-text-file-response
                            :request-id request-id
                            :error (acp-make-error :code -32603
                                                   :message "Write denied by user"))))
            (error
             (acp-send-response
              :client client
              :response (acp-make-fs-write-text-file-response
                         :request-id request-id
                         :error (acp-make-error :code -32603
                                                :message (error-message-string err)))))))))))

(defun emagent-acp--permission-option-id (options)
  "Return a permissive option id from OPTIONS."
  (let ((prefer '("allow_once" "allow-once" "allow_always" "allow-always" "allow" "yes")))
    (or (map-elt (seq-find (lambda (opt)
                             (let ((id (map-elt opt 'optionId)))
                               (and id (member id prefer))))
                           options)
                'optionId)
        (map-elt (car options) 'optionId))))

(cl-defun emagent-acp--on-permission (&key state acp-request)
  (let* ((options (map-nested-elt acp-request '(params options)))
         (title (or (map-nested-elt acp-request '(params toolCall title))
                    (map-nested-elt acp-request '(params title))
                    "Permission request"))
         (choice
          (if emagent-acp-auto-approve-permissions
              (emagent-acp--permission-option-id options)
            (progn
              (emagent-acp--prepare-interactive-context state)
              (completing-read
               (format "emagent: %s " title)
               (mapcar (lambda (opt)
                         (cons (or (map-elt opt 'name) (map-elt opt 'optionId))
                               (map-elt opt 'optionId)))
                       (append options nil))
               nil t)))))
    (acp-send-response
     :client (map-elt state :client)
     :response (acp-make-session-request-permission-response
                :request-id (map-elt acp-request 'id)
                :option-id choice))))

(cl-defun emagent-acp--on-request (&key state acp-request)
  (pcase (map-elt acp-request 'method)
    ("fs/read_text_file"
     (emagent-acp--on-fs-read :state state :acp-request acp-request))
    ("fs/write_text_file"
     (emagent-acp--on-fs-write :state state :acp-request acp-request))
    ("session/request_permission"
     (emagent-acp--on-permission :state state :acp-request acp-request))
    (_
     (acp-send-response
      :client (map-elt state :client)
      :response `((:request-id . ,(map-elt acp-request 'id))
                  (:error . ,(acp-make-error
                              :code -32601
                              :message (format "Unsupported method: %s"
                                               (map-elt acp-request 'method)))))))))

(defun emagent-acp--trace-update (update-type acp-notification)
  "Log UPDATE-TYPE and a short payload summary when tracing."
  (let ((text (or (map-nested-elt acp-notification '(params update content text)) ""))
        (title (map-nested-elt acp-notification '(params update title))))
    (pcase update-type
      ((or "agent_message_chunk" "agent_thought_chunk")
       (emagent-acp--trace "recv %s +%d" update-type (length text)))
      ("tool_call"
       (emagent-acp--trace "recv tool_call %s"
                          (or title
                              (map-nested-elt acp-notification '(params update toolCallId))
                              "running")))
      (_
       (emagent-acp--trace "recv %s" (or update-type "session/update"))))))

(cl-defun emagent-acp--on-notification (&key state acp-notification)
  (when (equal (map-elt acp-notification 'method) "session/update")
    (let ((update-type (map-nested-elt acp-notification '(params update sessionUpdate))))
      (emagent-acp--trace-update update-type acp-notification)
      (pcase update-type
        ("agent_message_chunk"
         (let ((text (or (map-nested-elt acp-notification '(params update content text)) "")))
           (unless (map-elt state :replaying-history)
             (map-put! state :assistant-text (concat (map-elt state :assistant-text) text))
             (when (map-elt state :prompt-finishing)
               (emagent-acp--schedule-prompt-render state))
             (when (and (emagent-acp--stream-to-buffer-p state)
                        (buffer-live-p (emagent-acp--chat-buffer state)))
               (with-current-buffer (emagent-acp--chat-buffer state)
                 (when-let ((cb (map-elt state :cb-chunk)))
                   (funcall cb text))))))
        ("agent_thought_chunk"
         (let ((text (or (map-nested-elt acp-notification '(params update content text)) "")))
           (emagent-acp--thought-chunk state text)))
        ("tool_call"
         (let ((title (map-nested-elt acp-notification '(params update title))))
           (emagent-acp--notify-user state (format "emagent: tool %s" (or title "running")))
           (map-put! state :current-tool (or title "running"))
           (emagent-acp--refresh-mode-line state)
           (emagent-acp--schedule-prompt-watchdog state)))
        ("config_option_update"
         (emagent-acp--save-config-options
          state
          (map-nested-elt acp-notification '(params update configOptions)))
         (when-let ((model-id (emagent-acp--current-model-id state nil)))
           (emagent-acp--persist-model-id state model-id)))
        ("usage_update"
         (emagent-acp--update-usage-from-notification
          state
          (map-nested-elt acp-notification '(params update))))
        ("available_commands_update"
         (let ((commands (map-nested-elt acp-notification
                                       '(params update availableCommands))))
           (when-let* ((buffer (emagent-acp--chat-buffer state))
                       (cb (map-elt state :cb-slash-commands)))
             (with-current-buffer buffer
               (funcall cb commands)))))
        (_ nil)))))

(cl-defun emagent-acp--subscribe (&key state)
  (let ((buffer (emagent-acp--chat-buffer state)))
    (acp-subscribe-to-errors
     :client (map-elt state :client)
     :buffer buffer
     :on-error
     (lambda (acp-error)
       (let ((message (or (map-elt acp-error 'message)
                          (format "%s" acp-error))))
         (emagent-acp--log-agent-stderr message)
         (when (and (map-elt state :busy)
                    (emagent-acp--fatal-agent-error-p message))
           (emagent-acp--abort-prompt state message))
         (when (emagent-acp--stderr-notify-p acp-error)
           (emagent-acp--notify-user state (format "emagent error: %s" message))))))
    (acp-subscribe-to-notifications
     :client (map-elt state :client)
     :buffer buffer
     :on-notification
     (lambda (notification)
       (emagent-acp--on-notification :state state
                                    :acp-notification notification)))
    (acp-subscribe-to-requests
     :client (map-elt state :client)
     :buffer buffer
     :on-request
     (lambda (request)
       (emagent-acp--on-request :state state :acp-request request)))))

(cl-defun emagent-acp--initialize (&key state on-ready)
  (emagent-acp--progress state "initializing ACP…")
  (emagent-acp--send-request
   :state state
   :request (if emagent-acp-file-access
               (acp-make-initialize-request
                :protocol-version 1
                :client-info `((name . "emagent")
                               (title . "Emacs Emagent")
                               (version . "0.1.0"))
                :read-text-file-capability t
                :write-text-file-capability t)
             (acp-make-initialize-request
              :protocol-version 1
              :client-info `((name . "emagent")
                             (title . "Emacs Emagent")
                             (version . "0.1.0"))))
   :on-success (lambda (response)
                 (map-put! state :initialized t)
                 (map-put! state :mcp-http (emagent-acp--mcp-http-capable-p response))
                 (emagent-acp--connect-session :state state :on-ready on-ready))
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
      (when (and (eq emagent-chat-provider 'claude)
                 (null emagent-chat-slash-commands))
        (emagent-log "loading slash commands from agent…"))))
  (emagent-acp--start-rss-timer state)
  (emagent-acp--reveal-buffer state)
  (when on-ready (funcall on-ready)))

(cl-defun emagent-acp--new-session (&key state on-ready)
  (emagent-acp--progress state "creating session…")
  (emagent-acp--send-request
   :state state
   :request (acp-make-session-new-request
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
   :request (acp-make-session-load-request
             :session-id session-id
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (map-elt state :mcp-http)
                                              (emagent-acp--chat-buffer state)))
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
                 (with-current-buffer (emagent-acp--chat-buffer state)
                   (emagent-chat-clear-session-id))
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
    (setq acp-logging-enabled t))
  (unless (and (boundp 'emagent-acp-compat--installed)
               emagent-acp-compat--installed)
    (require 'emagent-acp-compat))
  (when (and (boundp 'emagent-acp-compat--installed)
             emagent-acp-compat--installed)
    (emagent-log "emagent: ACP queue workaround active"))
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
                                 :prefer-emacs emagent-acp-prefer-emacs)
    (emagent-acp--progress emagent-acp--session "starting agent…")
    (emagent-acp--subscribe :state emagent-acp--session)
    (emagent-acp--initialize :state emagent-acp--session :on-ready on-ready)
    emagent-acp--session))

(defun emagent-acp-attach-context (text)
  "Attach TEXT to the next prompt in the current buffer."
  (let ((state (emagent-acp--session)))
    (map-put! state :extra-context
              (append (or (map-elt state :extra-context) nil) (list text)))))

(defun emagent-acp-send-prompt (user-text)
  "Send USER-TEXT to the current buffer's ACP session."
  (let* ((state (emagent-acp--session))
         (session-id (map-elt state :session-id)))
    (unless (map-elt state :ready)
      (user-error "Emagent is still connecting"))
    (when (map-elt state :busy)
      (user-error "Emagent is busy"))
    (let* ((extra (map-elt state :extra-context))
           (prompt (emagent-context-build-prompt user-text extra))
           (blocks `[((type . "text") (text . ,(substring-no-properties prompt)))]))
      (map-put! state :extra-context nil)
      (map-put! state :busy t)
      (map-put! state :assistant-text "")
      (map-put! state :thought-text "")
      (map-put! state :prompt-finalized nil)
      (map-put! state :prompt-finishing nil)
      (emagent-acp--cancel-prompt-render state)
      (emagent-acp--clear-thought-buffer state)
      (emagent-acp--schedule-prompt-watchdog state)
      (emagent-acp--send-request
       :state state
       :request (acp-make-session-prompt-request :session-id session-id :prompt blocks)
       :on-success
       (lambda (response)
         (emagent-acp--complete-prompt state response))
       :on-failure
       (lambda (error _raw)
         (let ((message (or (map-elt error 'message) (format "%s" error))))
           (emagent-acp--abort-prompt state (format "prompt failed: %s" message))
           (emagent-acp--notify-user state (format "emagent: prompt failed: %s" message))))))))

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
    (when-let ((client (map-elt state :client)))
      (acp-shutdown :client client))
    (setq emagent-acp--session nil)))

(defun emagent-acp-shutdown ()
  "Shut down the ACP session for the current buffer."
  (emagent-acp-shutdown-buffer))

(provide 'emagent-acp)

;;; emagent-acp.el ends here
