;;; emagent-acp-notify.el --- ACP notify module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin
(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-protocol)

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Code:
(defun emagent-acp--trace-update (update-type emagent-acp-notification)
  "Log UPDATE-TYPE and a short payload summary when tracing."
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
  (when (equal (map-elt emagent-acp-notification 'method) "session/update")
    (let ((update-type (map-nested-elt emagent-acp-notification '(params update sessionUpdate))))
      (emagent-acp--trace-update update-type emagent-acp-notification)
      (pcase update-type
        ("agent_message_chunk"
         (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) "")))
           (unless (map-elt state :replaying-history)
             (when (and (not (string-empty-p text))
                        (map-elt state :tool-call-since-last-chunk)
                        (not (string-empty-p (or (map-elt state :assistant-text) ""))))
               (setq text (concat "\n\n" text)))
             (map-put! state :tool-call-since-last-chunk nil)
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
         (map-put! state :tool-call-since-last-chunk t)
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

(provide 'emagent-acp-notify)
;;; emagent-acp-notify.el ends here
(declare-function emagent-acp--trace "emagent-acp-prompt")
(declare-function emagent-acp--detect-external-refusal-in-text "emagent-acp-gate")
(declare-function emagent-acp--schedule-prompt-render "emagent-acp-prompt")
(declare-function emagent-acp--stream-to-buffer-p "emagent-acp-prompt")
(declare-function emagent-acp--chat-buffer "emagent-acp-usage")
(declare-function emagent-acp--thought-chunk "emagent-acp-prompt")
(declare-function emagent-acp--on-tool-call "emagent-acp-tool-call")
(declare-function emagent-acp--save-config-options "emagent-acp-model")
(declare-function emagent-acp--current-model-id "emagent-acp-model")
(declare-function emagent-acp--persist-model-id "emagent-acp-usage")
(declare-function emagent-acp--update-usage-from-notification "emagent-acp-usage")
(declare-function emagent-acp--fatal-agent-error-p "emagent-acp-prompt")
(declare-function emagent-acp--abort-prompt "emagent-acp-prompt")
(declare-function emagent-acp--notify-user "emagent-acp-prompt")
(declare-function emagent-acp--on-request "emagent-acp-request")
