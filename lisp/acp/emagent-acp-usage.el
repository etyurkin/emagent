;;; emagent-acp-usage.el --- Session state queries and usage tracking  -*- lexical-binding: t; -*-

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
;; Token/usage tracking and thin ACP wire progress helpers.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-custom)
(require 'emagent-acp-protocol)
(require 'emagent-acp-provider)
(require 'emagent-acp-state)
(require 'emagent-log)
(require 'emagent-session)

(defun emagent-acp-busy-p ()
  "Return non-nil when the current buffer's ACP session is processing a prompt."
  (and emagent-acp--session (emagent-acp-state-busy emagent-acp--session)))

(defun emagent-acp-turn-in-flight-p ()
  "Return non-nil while the session is busy or finishing a prompt.

Used to defer expensive org font-lock until the turn settles."
  (and emagent-acp--session
       (or (emagent-acp-state-busy emagent-acp--session)
           (emagent-acp-state-prompt-finishing emagent-acp--session))))

(defun emagent-acp-waiting-permission-p ()
  "Return non-nil while permission requests are queued or being answered."
  (and emagent-acp--session
       (emagent-acp--permission-pending-p emagent-acp--session)))

(defun emagent-acp-ready-p ()
  "Return non-nil when the current buffer's ACP session is connected and idle."
  (and emagent-acp--session (emagent-acp-state-ready emagent-acp--session)))

(defun emagent-acp-current-tool ()
  "Return the name of the tool currently running, or nil."
  (and emagent-acp--session (emagent-acp-state-current-tool emagent-acp--session)))

(defun emagent-acp-current-tool-kind ()
  "Return the kind of the running tool (\"read\", \"write\", \"execute\"), or nil."
  (and emagent-acp--session (emagent-acp-state-current-tool-kind emagent-acp--session)))

(defun emagent-acp-agent-rss ()
  "Return the agent process RSS in MB, or nil."
  (and emagent-acp--session (emagent-acp-state-agent-rss emagent-acp--session)))

(defun emagent-acp-context-usage ()
  "Return (USED . SIZE) context token counts for the current session, or nil."
  (when-let* ((state emagent-acp--session)
              (usage (emagent-acp-state-usage state))
              (used (map-elt usage :context-used))
              (size (map-elt usage :context-size)))
    (cons used size)))

(defun emagent-acp-context-usage-unavailable-p ()
  "Return non-nil when a connected session cannot report context usage.
Cursor does not expose context-window figures over ACP, so emagent has no data
to compute a percentage and the mode line shows `ctx:n/a' instead."
  (and emagent-acp--session
       (or (emagent-acp-state-busy emagent-acp--session)
           (emagent-acp-state-ready emagent-acp--session))
       (emagent-acp--provider-context-usage-unavailable-p emagent-acp--session)))

(defun emagent-acp-external-tool-gate-reasons ()
  "Return external tool-gate reason symbols for the current session, or nil.
See `emagent-acp-external-tool-gate-hints'."
  (and emagent-acp--session
       (emagent-acp-state-external-tool-gate-reasons emagent-acp--session)))

(defun emagent-acp--chat-buffer (state)
  "Return STATE's chat buffer if it is live, else nil.

A killed buffer must not be returned: timers and callbacks still hold STATE
after the user kills the chat buffer, and `with-current-buffer' on a dead
buffer signals \"Selecting deleted buffer\"."
  (let ((buf (emagent-acp-state-chat-buffer state)))
    (when (buffer-live-p buf)
      buf)))

(defun emagent-acp--session-cwd (state)
  
  "Internal helper for STATE."
  (if-let ((buf (emagent-acp--chat-buffer state)))
      (with-current-buffer buf (emagent-chat--session-directory))
    (user-error "Emagent chat buffer is no longer available")))

(defun emagent-acp--persist-session-id (state session-id)
  
  "Internal helper for STATE and SESSION-ID."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (let ((was-modified (buffer-modified-p)))
        (unwind-protect
            (emagent-session-set-id session-id)
          (set-buffer-modified-p was-modified))))))

(defun emagent-acp--saved-session-id (state)
  
  "Internal helper for STATE."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (emagent-session-id))))

(defun emagent-acp--saved-model-id (state)
  
  "Internal helper for STATE."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (emagent-session-model))))

(defun emagent-acp--persist-model-id (state model-id)
  
  "Internal helper for STATE and MODEL-ID."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (let ((was-modified (buffer-modified-p)))
        (unwind-protect
            (emagent-session-set-model model-id)
          (set-buffer-modified-p was-modified)))))
  ;; The status push from --refresh-mode-line re-renders the model label.
  (emagent-acp--refresh-mode-line state))

(defun emagent-acp--current-model-id (state models)
  "Return the current model id for STATE.

Prefer the session config-option current value, then MODELS'
`currentModelId', then the chat buffer's saved model."
  (or (map-elt
       (or (seq-find (lambda (option)
                       (equal "model" (map-elt option :category)))
                     (emagent-acp-state-config-options state))
           (seq-find (lambda (option)
                       (string= (map-elt option :id) "model"))
                     (emagent-acp-state-config-options state)))
       :current-value)
      (and models (map-elt models 'currentModelId))
      (emagent-acp--saved-model-id state)))

(defun emagent-acp--maybe-recover-stall (state)
  "Unstick a session that finished on the wire but left the buffer open.

Lazy-loads prompt/permission-queue so this leaf need not require them
\(those modules require this file\).

Arguments: STATE."
  (when (and state
             (emagent-acp-state-ready state)
             (not (emagent-acp-state-busy state)))
    (unless (fboundp 'emagent-acp--maybe-complete-deferred-prompt)
      (require 'emagent-acp-prompt))
    (emagent-acp--maybe-complete-deferred-prompt state)
    (when (emagent-acp-state-permission-queue state)
      (unless (fboundp 'emagent-acp--drain-permission-queue)
        (require 'emagent-acp-permission-queue))
      (emagent-acp--drain-permission-queue state))))

(defun emagent-acp--status-snapshot (state)
  "Return a mode-line status plist computed from STATE.

Built entirely from STATE so it does not depend on the current buffer; the UI
renders from this snapshot instead of pulling session state back out of the ACP
layer (see `emagent-chat-set-status')."
  (let ((usage (emagent-acp-state-usage state)))
    (list :busy (and (emagent-acp-state-busy state) t)
          :waiting-permission (and (emagent-acp--permission-pending-p state) t)
          :ready (and (emagent-acp-state-ready state) t)
          :prompt-finishing (and (emagent-acp-state-prompt-finishing state) t)
          :tool (emagent-acp-state-current-tool state)
          :tool-kind (emagent-acp-state-current-tool-kind state)
          :rss (emagent-acp-state-agent-rss state)
          :model-id (and (emagent-acp-state-ready state)
                         (emagent-acp--current-model-id state nil))
          :ctx-usage (when-let ((used (and usage (map-elt usage :context-used)))
                                (size (map-elt usage :context-size)))
                       (cons used size))
          :ctx-unavailable (and (or (emagent-acp-state-busy state)
                                    (emagent-acp-state-ready state))
                                (emagent-acp--provider-context-usage-unavailable-p
                                 state))
          :mode-id (emagent-acp-state-session-mode-id state))))

(defun emagent-acp--refresh-mode-line (state)
  
  "Internal helper for STATE."
  (emagent-acp--maybe-recover-stall state)
  (when-let ((buffer (emagent-acp--chat-buffer state))
             (cb (emagent-acp-state-cb-status state)))
    (let ((snapshot (emagent-acp--status-snapshot state)))
      (with-current-buffer buffer
        (funcall cb snapshot)))))

(defun emagent-acp--agent-rss-mb (state)
  "Return the agent process RSS in MB via `process-attributes', or nil.

Arguments: STATE."
  (when-let* ((client (emagent-acp-state-client state))
              (proc (and client (map-elt client :process)))
              ((processp proc))
              (pid (process-id proc))
              ((> pid 0))
              (attrs (ignore-errors (process-attributes pid)))
              (rss-kb (alist-get 'rss attrs)))
    (round (/ (float rss-kb) 1024))))

(defun emagent-acp--start-rss-timer (state)
  "Start a repeating timer that refreshes :agent-rss in STATE every 15 s."
  (when-let ((old (emagent-acp-state-agent-rss-timer state)))
    (cancel-timer old))
  (setf (emagent-acp-state-agent-rss-timer state)
        (run-with-timer
         5 15
         (lambda ()
           (if (buffer-live-p (emagent-acp-state-chat-buffer state))
               (let ((mb (emagent-acp--agent-rss-mb state)))
                 (setf (emagent-acp-state-agent-rss state) mb)
                 (emagent-acp--refresh-mode-line state))
             (emagent-acp--stop-rss-timer state))))))

(defun emagent-acp--stop-rss-timer (state)
  "Cancel the RSS polling timer for STATE."
  (when-let ((timer (and state (emagent-acp-state-agent-rss-timer state))))
    (cancel-timer timer)
    (setf (emagent-acp-state-agent-rss-timer state) nil)))

(defun emagent-acp--usage-state (state)
  
  "Internal helper for STATE."
  (or (emagent-acp-state-usage state)
      (let ((usage (make-hash-table :test 'eq)))
        (puthash :context-used nil usage)
        (puthash :context-size nil usage)
        (puthash :total-tokens 0 usage)
        (setf (emagent-acp-state-usage state) usage)
        usage)))

(defun emagent-acp--usage-context-used (data)
  "Return cumulative context-window fill from DATA, or nil.
Per-turn input/prompt token counts are not context fill."
  (or (map-elt data 'contextUsed)
      (map-elt data 'used)
      (map-elt data 'contextWindowUsed)
      (map-elt data 'tokensUsed)))

(defun emagent-acp--usage-context-size (data)
  "Return context window size/limit from DATA, or nil."
  (or (map-elt data 'contextSize)
      (map-elt data 'contextLimit)
      (map-elt data 'contextWindow)
      (map-elt data 'size)
      (map-elt data 'maxTokens)))

(defun emagent-acp--save-usage-from-response (state emagent-acp-usage)
  "Update STATE usage from a prompt response usage field.

Arguments: EMAGENT-ACP-USAGE."
  (let ((usage (emagent-acp--usage-state state)))
    (when-let ((total (map-elt emagent-acp-usage 'totalTokens)))
      (map-put! usage :total-tokens total))
    (when-let ((used (emagent-acp--usage-context-used emagent-acp-usage)))
      (map-put! usage :context-used used))
    (when-let ((size (emagent-acp--usage-context-size emagent-acp-usage)))
      (map-put! usage :context-size size))
    (setf (emagent-acp-state-usage state) usage)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp--update-usage-from-notification (state emagent-acp-update)
  "Update STATE usage from a session/update usage_update payload.

Arguments: EMAGENT-ACP-UPDATE."
  (let ((usage (emagent-acp--usage-state state)))
    (when-let ((used (emagent-acp--usage-context-used emagent-acp-update)))
      (map-put! usage :context-used used))
    (when-let ((size (emagent-acp--usage-context-size emagent-acp-update)))
      (map-put! usage :context-size size))
    (setf (emagent-acp-state-usage state) usage)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp--notify-user (_state message)
  "Append MESSAGE to `emagent-log-buffer-name'."
  (emagent-log "%s" message))

(defun emagent-acp--trace (format-string &rest args)
  "Append a trace line when `emagent-acp-trace' is non-nil.

Arguments: FORMAT-STRING, ARGS."
  (when emagent-acp-trace
    (apply #'emagent-log (cons (concat "acp: " format-string) args))))

(defun emagent-acp--progress (state message)
  "Show init stage MESSAGE in the minibuffer and refresh the mode line.

Arguments: STATE."
  (emagent-acp--notify-user state (format "emagent: %s" message))
  (emagent-acp--refresh-mode-line state))

(cl-defun emagent-acp--send-request (&key state request on-success on-failure)

  "Internal helper for STATE and REQUEST and ON-SUCCESS and ON-FAILURE."
  (let ((method (map-elt request :method)))
    (emagent-acp--trace "send %s" method)
    (emagent-acp-send-request
     :client (emagent-acp-state-client state)
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

(provide 'emagent-acp-usage)
;;; emagent-acp-usage.el ends here
