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
(declare-function emagent-chat-set-session-id "emagent-chat")
(declare-function emagent-chat-set-model "emagent-chat")
(declare-function emagent-chat--refresh-mode-line "emagent-chat-mode-line")
(declare-function emagent-chat--refresh-mode-line-soon "emagent-chat-mode-line")
(declare-function emagent-acp--permission-pending-p "emagent-acp")
(declare-function emagent-acp--maybe-complete-deferred-prompt "emagent-acp")
(declare-function emagent-acp--drain-permission-queue "emagent-acp")

;;; -------------------------------------------------------------------------
;;; Public session state accessors (for use by emagent-chat.el)
;;; -------------------------------------------------------------------------

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

(defun emagent-acp-context-usage-unavailable-p ()
  "Return non-nil when a connected session cannot report context usage.
Cursor does not expose context-window figures over ACP, so emagent has no data
to compute a percentage and the mode line shows `ctx:n/a' instead."
  (and emagent-acp--session
       (or (map-elt emagent-acp--session :busy)
           (map-elt emagent-acp--session :ready))
       (eq (or (map-elt emagent-acp--session :provider) 'cursor) 'cursor)))

(defun emagent-acp-external-tool-gate-reasons ()
  "Return external tool-gate reason symbols for the current session, or nil.
See `emagent-acp-external-tool-gate-hints'."
  (and emagent-acp--session
       (map-elt emagent-acp--session :external-tool-gate-reasons)))

;;; -------------------------------------------------------------------------
;;; Session utility helpers
;;; -------------------------------------------------------------------------

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
      (let ((was-modified (buffer-modified-p)))
        (unwind-protect
            (emagent-chat-set-session-id session-id)
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
            (emagent-chat-set-model model-id)
          (set-buffer-modified-p was-modified)))))
  (emagent-acp--refresh-mode-line state))

(defun emagent-acp--maybe-recover-stall (state)
  "Unstick a session that finished on the wire but left the buffer open."
  (when (and state (map-elt state :ready) (not (map-elt state :busy)))
    (emagent-acp--maybe-complete-deferred-prompt state)
    (when (map-elt state :permission-queue)
      (emagent-acp--drain-permission-queue state))))

(declare-function emagent-chat--spinner-ensure-running "emagent-chat-mode-line")
(declare-function emagent-chat--maybe-force-mode-line-update "emagent-chat")

(defun emagent-acp--refresh-mode-line (state)
  (emagent-acp--maybe-recover-stall state)
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (when (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p)
                 (fboundp 'emagent-chat--spinner-ensure-running))
        (emagent-chat--spinner-ensure-running))
      (cond
       ((and (fboundp 'emagent-acp-busy-p)
             (not (emagent-acp-busy-p))
             (fboundp 'emagent-chat--refresh-mode-line))
        (emagent-chat--refresh-mode-line))
       ((fboundp 'emagent-chat--refresh-mode-line-soon)
        (emagent-chat--refresh-mode-line-soon))
       ((fboundp 'emagent-chat--refresh-mode-line)
        (emagent-chat--refresh-mode-line))
       (t (emagent-chat--maybe-force-mode-line-update))))))

;;; -------------------------------------------------------------------------
;;; Usage tracking
;;; -------------------------------------------------------------------------

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
    ;; Extract context usage -- cursor may use different field names.
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

(provide 'emagent-acp-usage)
;;; emagent-acp-usage.el ends here
