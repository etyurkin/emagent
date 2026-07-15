;;; emagent-acp-notify.el --- ACP notify module  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6

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

;; Handle ACP session/update and related notifications.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-protocol)

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
        (_ nil)))))

(cl-defun emagent-acp--subscribe (&key state)
  
  "Internal helper for STATE."
  (let ((buffer (emagent-acp--chat-buffer state)))
    (emagent-acp-subscribe-to-errors
     :client (emagent-acp-state-client state)
     :buffer buffer
     :on-error
     (lambda (emagent-acp-error)
       (let ((message (or (map-elt emagent-acp-error 'message)
                          (format "%s" emagent-acp-error))))
         (emagent-acp--log-agent-stderr message)
         (when (and (emagent-acp-state-busy state)
                    (emagent-acp--fatal-agent-error-p message)
                    (not (emagent-acp--prompt-retry-pending-p state)))
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
(declare-function emagent-acp--prompt-retry-pending-p "emagent-acp-prompt")
(declare-function emagent-acp--abort-prompt "emagent-acp-prompt")
(declare-function emagent-acp--notify-user "emagent-acp-prompt")
(declare-function emagent-acp--on-request "emagent-acp-request")
