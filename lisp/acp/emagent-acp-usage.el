;;; emagent-acp-usage.el --- Session state queries and usage tracking  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public session state accessors, session utility helpers, and usage
;; tracking for the ACP layer.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-session)

(declare-function emagent-chat--session-directory "emagent-chat-header")
(declare-function emagent-acp--permission-pending-p "emagent-acp")
(declare-function emagent-acp--maybe-complete-deferred-prompt "emagent-acp")
(declare-function emagent-acp--drain-permission-queue "emagent-acp")

;;; -------------------------------------------------------------------------
;;; Public session state accessors (for use by emagent-chat.el)
;;; -------------------------------------------------------------------------

(defun emagent-acp-busy-p ()
  "Return non-nil when the current buffer's ACP session is processing a prompt."
  (and emagent-acp--session (emagent-acp-state-busy emagent-acp--session)))

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
       (eq (or (emagent-acp-state-provider emagent-acp--session) 'cursor) 'cursor)))

(defun emagent-acp-external-tool-gate-reasons ()
  "Return external tool-gate reason symbols for the current session, or nil.
See `emagent-acp-external-tool-gate-hints'."
  (and emagent-acp--session
       (emagent-acp-state-external-tool-gate-reasons emagent-acp--session)))

;;; -------------------------------------------------------------------------
;;; Session utility helpers
;;; -------------------------------------------------------------------------

(defun emagent-acp--chat-buffer (state)
  "Return STATE's chat buffer if it is live, else nil.

A killed buffer must not be returned: timers and callbacks still hold STATE
after the user kills the chat buffer, and `with-current-buffer' on a dead
buffer signals \"Selecting deleted buffer\"."
  (let ((buf (emagent-acp-state-chat-buffer state)))
    (when (buffer-live-p buf)
      buf)))

(defun emagent-acp--session-cwd (state)
  (if-let ((buf (emagent-acp--chat-buffer state)))
      (with-current-buffer buf (emagent-chat--session-directory))
    (user-error "emagent chat buffer is no longer available")))

(defun emagent-acp--persist-session-id (state session-id)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (let ((was-modified (buffer-modified-p)))
        (unwind-protect
            (emagent-session-set-id session-id)
          (set-buffer-modified-p was-modified))))))

(defun emagent-acp--saved-session-id (state)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (emagent-session-id))))

(defun emagent-acp--saved-model-id (state)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (emagent-session-model))))

(defun emagent-acp--persist-model-id (state model-id)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (let ((was-modified (buffer-modified-p)))
        (unwind-protect
            (emagent-session-set-model model-id)
          (set-buffer-modified-p was-modified)))))
  ;; The status push from --refresh-mode-line re-renders the model label.
  (emagent-acp--refresh-mode-line state))

(defun emagent-acp--maybe-recover-stall (state)
  "Unstick a session that finished on the wire but left the buffer open."
  (when (and state (emagent-acp-state-ready state) (not (emagent-acp-state-busy state)))
    (emagent-acp--maybe-complete-deferred-prompt state)
    (when (emagent-acp-state-permission-queue state)
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
          :ctx-usage (when-let ((used (and usage (map-elt usage :context-used)))
                                (size (map-elt usage :context-size)))
                       (cons used size))
          :ctx-unavailable (and (or (emagent-acp-state-busy state) (emagent-acp-state-ready state))
                                (eq (or (emagent-acp-state-provider state) 'cursor) 'cursor)))))

(defun emagent-acp--refresh-mode-line (state)
  (emagent-acp--maybe-recover-stall state)
  (when-let ((buffer (emagent-acp--chat-buffer state))
             (cb (emagent-acp-state-cb-status state)))
    (let ((snapshot (emagent-acp--status-snapshot state)))
      (with-current-buffer buffer
        (funcall cb snapshot)))))

;;; -------------------------------------------------------------------------
;;; Usage tracking
;;; -------------------------------------------------------------------------

(defun emagent-acp--usage-state (state)
  (or (emagent-acp-state-usage state)
      (let ((usage (make-hash-table :test 'eq)))
        (puthash :context-used nil usage)
        (puthash :context-size nil usage)
        (puthash :total-tokens 0 usage)
        (setf (emagent-acp-state-usage state) usage)
        usage)))

(defun emagent-acp--save-usage-from-response (state emagent-acp-usage)
  "Update STATE usage from a prompt response usage field."
  (let ((usage (emagent-acp--usage-state state)))
    (when-let ((total (map-elt emagent-acp-usage 'totalTokens)))
      (map-put! usage :total-tokens total))
    ;; Extract context usage -- cursor may use different field names.
    (when-let ((used (or (map-elt emagent-acp-usage 'contextUsed)
                         (map-elt emagent-acp-usage 'inputTokens)
                         (map-elt emagent-acp-usage 'promptTokens))))
      (map-put! usage :context-used used))
    (when-let ((size (or (map-elt emagent-acp-usage 'contextSize)
                         (map-elt emagent-acp-usage 'contextLimit)
                         (map-elt emagent-acp-usage 'contextWindow))))
      (map-put! usage :context-size size))
    (setf (emagent-acp-state-usage state) usage)
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
    (setf (emagent-acp-state-usage state) usage)
    (emagent-acp--refresh-mode-line state)))

(provide 'emagent-acp-usage)
;;; emagent-acp-usage.el ends here
