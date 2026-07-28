;;; emagent-acp-lifecycle.el --- ACP lifecycle module  -*- lexical-binding: t; -*-

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
;;
;; Session lifecycle, system prompt assembly, and ACP notifications.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-custom)
(require 'emagent-acp-provider)
(require 'emagent-acp-model)
(require 'emagent-acp-permit)
(require 'emagent-acp-prompt)
(require 'emagent-acp-protocol)
(require 'emagent-acp-request)
(require 'emagent-acp-state)
(require 'emagent-acp-tool-call)
(require 'emagent-acp-usage)
(require 'emagent-log)
(require 'emagent-mcp)
(require 'emagent-prompts)

(defun emagent-acp--system-prompt ()
  "Return the system prompt for new ACP sessions."
  (concat emagent-acp-system-prompt
          (emagent-mcp-gateway-system-prompt)
          (when emagent-acp-prefer-emacs
            (emagent-prompts--prefer-emacs-prompt))
          (when emagent-acp-prefer-emacs
            (emagent-prompts--structural-policy))))

(defun emagent-acp--session-system-prompt (&optional compressed-context)
  "Return the system prompt for session/new, optionally with COMPRESSED-CONTEXT."
  (let ((summary (string-trim (or compressed-context ""))))
    (if (string-empty-p summary)
        (emagent-acp--system-prompt)
      (concat (emagent-acp--system-prompt)
              (format "\n\n[Compressed prior conversation context]\n%s"
                      summary)))))

(defun emagent-acp--trace-update (update-type emagent-acp-notification)
  "Log UPDATE-TYPE and a short payload summary when tracing.

Arguments: EMAGENT-ACP-NOTIFICATION."
  (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) ""))
        (title (map-nested-elt emagent-acp-notification '(params update title))))
    (pcase update-type
      ((or "agent_message_chunk" "agent_thought_chunk")
       (emagent-acp--trace "recv %s +%d" update-type (length text)))
      ((or "tool_call" "tool_call_update")
       (let* ((update (map-nested-elt emagent-acp-notification '(params update)))
              (raw (or (map-elt update 'rawInput) (map-elt update 'arguments)))
              (subtitle (map-elt update 'subtitle))
              (locations (map-elt update 'locations))
              (id (map-elt update 'toolCallId))
              (raw-summary
               (cond
                ((or (null raw) (equal raw :null) (equal raw "")) nil)
                ((hash-table-p raw)
                 (format "keys(%s)"
                         (string-join (hash-table-keys raw) ",")))
                ((listp raw)
                 (format "keys(%s)"
                         (string-join (mapcar (lambda (p) (format "%s" (car p))) raw) ",")))
                ((stringp raw)
                 (format "str(%d)" (length raw)))
                (t "?")))
              (detail (or raw-summary
                          (when subtitle (format "sub=%s" (truncate-string-to-width subtitle 40 nil nil "…")))
                          (when locations (format "locs=%d" (length (append locations nil))))
                          "no-detail")))
         (emagent-acp--trace "recv %s %s [%s] %s"
                             update-type
                             (or title id "?")
                             (or (map-elt update 'status) "")
                             detail)))
      (_
       (emagent-acp--trace "recv %s" (or update-type "session/update"))))))

(cl-defun emagent-acp--on-notification (&key state emagent-acp-notification)
  
  "Internal helper for STATE and EMAGENT-ACP-NOTIFICATION."
  (when (equal (map-elt emagent-acp-notification 'method) "session/update")
    (let ((update-type (map-nested-elt emagent-acp-notification '(params update sessionUpdate))))
      (emagent-acp--trace-update update-type emagent-acp-notification)
      (pcase update-type
        ("agent_message_chunk"
         (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) "")))
           (unless (emagent-acp-state-replaying-history state)
             (when (and (not (string-empty-p text))
                        (emagent-acp-state-tool-call-since-last-chunk state)
                        (not (string-empty-p (or (emagent-acp-state-assistant-text state) ""))))
               (setq text (concat "\n\n" text)))
             (setf (emagent-acp-state-tool-call-since-last-chunk state) nil)
             (emagent-acp--detect-external-refusal-in-text state text)
             (setf (emagent-acp-state-assistant-text state) (concat (emagent-acp-state-assistant-text state) text))
             (when (emagent-acp-state-prompt-finishing state)
               (emagent-acp--schedule-prompt-render state))
             (when-let ((buf (and (emagent-acp--stream-to-buffer-p state)
                                 (emagent-acp--chat-buffer state))))
               (with-current-buffer buf
                 (when-let ((cb (emagent-acp-state-cb-chunk state)))
                   (funcall cb text)))))))
        ("agent_thought_chunk"
         (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) "")))
           (emagent-acp--thought-chunk state text)))
        ("tool_call"
         (setf (emagent-acp-state-tool-call-since-last-chunk state) t)
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
                       (cb (emagent-acp-state-cb-slash-commands state)))
             (with-current-buffer buffer
               (funcall cb commands)))))
        ("current_mode_update"
         (let* ((update (map-nested-elt emagent-acp-notification
                                        '(params update)))
                (mode-id (or (map-elt update 'currentModeId)
                             (map-elt update 'modeId)
                             (map-elt update :currentModeId)
                             (map-elt update :modeId))))
           (when (and (stringp mode-id) (not (string-empty-p mode-id)))
             (setf (emagent-acp-state-session-mode-id state) mode-id)
             (emagent-acp--refresh-mode-line state))))
        (_ nil)))))

(cl-defun emagent-acp--subscribe (&key state)
  "Subscribe STATE's client to ACP errors, notifications, and requests."
  (let ((buffer (emagent-acp--chat-buffer state)))
    (emagent-acp-subscribe-to-errors
     :client (emagent-acp-state-client state)
     :buffer buffer
     :on-error
     (lambda (emagent-acp-error)
       (let ((message (or (map-elt emagent-acp-error 'message)
                          (format "%s" emagent-acp-error))))
         (emagent-acp--log-agent-stderr message)
         (when (and (emagent-acp--fatal-agent-error-p message)
                    (not (emagent-acp--prompt-retry-pending-p state))
                    (or (emagent-acp-state-busy state)
                        (emagent-acp-state-prompt-finishing state)
                        (emagent-acp--quota-error-p message)))
           (emagent-acp--abort-prompt state message))
         (when (emagent-acp--stderr-notify-p emagent-acp-error)
           (emagent-acp--notify-user state (format "emagent error: %s" message))))))
    (emagent-acp-subscribe-to-notifications
     :client (emagent-acp-state-client state)
     :buffer buffer
     :on-notification
     (lambda (notification)
       (emagent-acp--on-notification :state state
                                     :emagent-acp-notification notification)))
    (emagent-acp-subscribe-to-requests
     :client (emagent-acp-state-client state)
     :buffer buffer
     :on-request
     (lambda (request)
       (emagent-acp--on-request :state state :emagent-acp-request request)))))

(cl-defun emagent-acp--authenticate (&key state method-id on-ready)
  "Send an authenticate request with METHOD-ID, then connect the session.

Called when `initialize' returns authMethods (e.g. cursor_login).
The authenticate call completes the credential handshake so the agent
grants full plan access (including Auto model) to this ACP session.

Arguments: STATE, ON-READY."
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
  
  "Internal helper for STATE and ON-READY."
  (emagent-acp--progress state "initializing ACP…")
  (emagent-acp--send-request
   :state state
   :request (if emagent-acp-file-access
                (emagent-acp-make-initialize-request
                 :protocol-version 1
                 :client-info `((name . "emagent")
                                (title . "Emacs Emagent")
                                (version . "1.0.2"))
                 :read-text-file-capability t
                 :write-text-file-capability t)
              (emagent-acp-make-initialize-request
               :protocol-version 1
               :client-info `((name . "emagent")
                              (title . "Emacs Emagent")
                              (version . "1.0.2"))))
   :on-success (lambda (response)
                 (setf (emagent-acp-state-initialized state) t)
                 (setf (emagent-acp-state-mcp-http state) (emagent-acp--mcp-http-capable-p response))
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
  
  "Internal helper for STATE and SESSION-ID and ON-READY and RESUMED."
  (setf (emagent-acp-state-session-id state) session-id)
  (setf (emagent-acp-state-ready state) t)
  (emagent-acp--persist-session-id state session-id)
  (emagent-acp--hydrate-session-permissions state session-id)
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

(cl-defun emagent-acp--new-session (&key state on-ready compressed-context)
  
  "Internal helper for STATE and ON-READY and COMPRESSED-CONTEXT."
  (emagent-acp--progress state "creating session…")
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-new-request
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (emagent-acp-state-mcp-http state)
                                                       (emagent-acp--chat-buffer state))
             :meta `((systemPrompt . ((append . ,(emagent-acp--session-system-prompt
                                                  compressed-context))))))
   :on-success (lambda (response)
                 (unless (fboundp 'emagent-acp--configure-model)
                   (require 'emagent-acp-model))
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
  "Resume SESSION-ID for STATE, falling back to session/new on failure."
  (emagent-acp--progress state "resuming session…")
  (setf (emagent-acp-state-replaying-history state) t)
  (emagent-acp--set-suppress-history-updates
   (emagent-acp-state-client state) t)
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-load-request
             :session-id session-id
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (emagent-acp-state-mcp-http state)
                                                       (emagent-acp--chat-buffer state))
             :meta `((systemPrompt . ((append . ,(emagent-acp--system-prompt))))))
   :on-success (lambda (response)
                 (emagent-acp--set-suppress-history-updates
                  (emagent-acp-state-client state) nil)
                 (setf (emagent-acp-state-replaying-history state) nil)
                 (unless (fboundp 'emagent-acp--configure-model)
                   (require 'emagent-acp-model))
                 (emagent-acp--configure-model
                  :state state
                  :session-id session-id
                  :response response
                  :on-ready on-ready
                  :resumed t))
   :on-failure (lambda (error _raw)
                 (emagent-acp--set-suppress-history-updates
                  (emagent-acp-state-client state) nil)
                 (setf (emagent-acp-state-replaying-history state) nil)
                 (emagent-log "session/load failed for %s: %s"
                              session-id
                              (or (map-elt error 'message) (format "%s" error)))
                 (emagent-acp--progress state "resume failed, creating session…")
                 (when-let ((buf (emagent-acp--chat-buffer state)))
                   (with-current-buffer buf
                     (let ((was-modified (buffer-modified-p)))
                       (unwind-protect
                           (emagent-session-clear-id)
                         (set-buffer-modified-p was-modified)))))
                 (emagent-acp--new-session :state state :on-ready on-ready))))

(cl-defun emagent-acp--connect-session (&key state on-ready)
  
  "Internal helper for STATE and ON-READY."
  (emagent-acp--progress state "connecting session…")
  (let ((saved (emagent-acp--saved-session-id state)))
    (if (and saved (not (string-empty-p saved)))
        (emagent-acp--load-session :state state :session-id saved :on-ready on-ready)
      (emagent-acp--new-session :state state :on-ready on-ready))))

(cl-defun emagent-acp-start (&key client chat-buffer on-ready on-reveal callbacks)
  "Start an emagent ACP session in CHAT-BUFFER.

ON-REVEAL is called once when the chat buffer should be shown.
CALLBACKS is an alist of rendering callbacks keyed by:
  :cb-chunk, :cb-thought, :cb-finish, :cb-fail, :cb-slash-commands.

Arguments: CLIENT, ON-READY."
  (when (and emagent-acp-prefer-emacs (not emagent-acp-file-access))
    (emagent-log "prefer-Emacs mode works best with `emagent-acp-file-access'"))
  (when emagent-acp-trace
    (setq emagent-acp-logging-enabled t))
  (with-current-buffer chat-buffer
    (emagent-chat-clear-slash-commands)
    ;; Cursor built-ins are local; keep them available while the agent
    ;; connects so TAB completion does not go empty mid-reconnect.
    (emagent-chat-seed-cursor-slash-commands)
    (setq emagent-acp--session (emagent-acp--make-state :client client
                                                        :chat-buffer chat-buffer
                                                        :on-reveal on-reveal))
    (setf (emagent-acp-state-provider emagent-acp--session) (or emagent-chat-provider 'cursor))
    (dolist (cb callbacks)
      (emagent-acp--set-callback emagent-acp--session (car cb) (cdr cb)))
    (emagent-mcp-register-session :token (emagent-mcp-buffer-token)
                                  :cwd (emagent-chat--session-directory)
                                  :buffer chat-buffer
                                  :prefer-emacs emagent-acp-prefer-emacs
                                  :acp t)
    (emagent-acp--progress emagent-acp--session "starting agent…")
    (emagent-acp--subscribe :state emagent-acp--session)
    (emagent-acp--initialize :state emagent-acp--session :on-ready on-ready)
    emagent-acp--session))

(provide 'emagent-acp-lifecycle)
;;; emagent-acp-lifecycle.el ends here
