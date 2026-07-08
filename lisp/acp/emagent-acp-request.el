;;; emagent-acp-request.el --- ACP permission request dispatch  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Handles session/request_permission ACP method: display, user interaction,
;; queue drain, and response sending.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-protocol)
(require 'emagent-tools)
(require 'emagent-chat)
(require 'emagent-acp-permit)

(declare-function emagent-acp--send-response "emagent-acp-protocol")
(declare-function emagent-acp--tool-call-elisp-prin1-p "emagent-acp-tool-call")
(declare-function emagent-acp--tool-call-update-from-request "emagent-acp-tool-call")
(declare-function emagent-acp--tool-call-detail "emagent-acp-tool-call")
(declare-function emagent-acp--tool-call-raw-input-detail "emagent-acp-tool-call")
(declare-function emagent-acp--refresh-mode-line "emagent-acp-usage")
(declare-function emagent-acp--maybe-complete-deferred-prompt "emagent-acp")
(declare-function emagent-acp--permission-interactive-p "emagent-acp-permit")
(declare-function emagent-acp--permission-acp-allow-id "emagent-acp-permit")
(declare-function emagent-acp--permission-acp-deny-id "emagent-acp-permit")
(declare-function emagent-acp--permission-fingerprint "emagent-acp-permit")
(declare-function emagent-acp--permission-validate "emagent-acp-permit")
(declare-function emagent-acp--permission-gate-auto-approve-p "emagent-acp-permit")
(declare-function emagent-acp--permission-auto-allowed-p "emagent-acp-permit")
(declare-function emagent-acp--permission-apply-choice "emagent-acp-permit")
(declare-function emagent-acp--permission-approved-choice-p "emagent-acp-permit")
(declare-function emagent-acp--permission-stored-auto-choice "emagent-acp-permit")
(declare-function emagent-acp--show-permission-decision "emagent-acp-permit")
(declare-function emagent-acp--schedule-permission-drain "emagent-acp-permit")
(declare-function emagent-tools--buttons-prompt "emagent-tools" (prompt choices chat-buffer callback &optional preamble))

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

(cl-defun emagent-acp--handle-one-permission (&key state emagent-acp-request on-complete)
  "Show permission dialog for EMAGENT-ACP-REQUEST in STATE's chat buffer.

For auto-deny/auto-approve: sends the ACP response synchronously and calls
ON-COMPLETE immediately.  For interactive prompts: inserts the dialog
non-blockingly and returns; ON-COMPLETE is called after the user responds."
  (let* ((raw-tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (tool-call (and raw-tool-call
                         (emagent-acp--permission-tool-call state raw-tool-call)))
         (options (map-nested-elt emagent-acp-request '(params options)))
         (request-id (map-elt emagent-acp-request 'id))
         (question (emagent-acp--permission-question-line emagent-acp-request))
         (fingerprint (and tool-call (emagent-acp--permission-fingerprint tool-call)))
         (validation (and tool-call (emagent-acp--permission-validate tool-call)))
         (buf (emagent-acp--chat-buffer state))
         (allow-id (emagent-acp--permission-acp-allow-id options))
         (deny-id (emagent-acp--permission-acp-deny-id options)))
    (when raw-tool-call
      (emagent-acp--ingest-tool-call-request state raw-tool-call))
    (let ((respond
           (lambda (choice)
             (when (emagent-acp--permission-approved-choice-p choice)
               (emagent-acp--permission-apply-choice state fingerprint buf choice))
             (let* ((response
                     (cond
                      ((and validation (eq (car validation) :deny))
                       (if deny-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id deny-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      ((emagent-acp--permission-approved-choice-p choice)
                       (if allow-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id allow-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      ((eq choice :deny)
                       (if deny-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id deny-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      (t
                       (emagent-acp-make-session-request-permission-response
                        :request-id request-id :cancelled t))))
                    (outcome (map-nested-elt response '(:result outcome))))
               (emagent-log "permission response: question=%s outcome=%s choice=%s"
                            question (or outcome "?") choice)
               (emagent-acp-send-response :client (map-elt state :client) :response response))
             (when on-complete (funcall on-complete)))))
      (cond
       ((and validation (eq (car validation) :deny))
        (emagent-log "permission denied by emagent gate: %s — %s" question (cdr validation))
        (emagent-acp--show-permission-decision state tool-call :deny)
        (funcall respond :deny))
       ((emagent-acp--permission-gate-auto-approve-p state tool-call validation fingerprint buf)
        (let ((stored (emagent-acp--permission-stored-auto-choice state fingerprint buf)))
          (emagent-log "permission auto-approve: %s (fingerprint %s)" question (or fingerprint "none"))
          (emagent-acp--show-permission-decision state tool-call (or stored :allow))
          (funcall respond :allow-once)))
       (t
        (emagent-acp--prepare-interactive-context state)
        (emagent-acp--clear-prompt-watchdog state)
        (let ((after-response
               (lambda (choice)
                 (when choice
                   (emagent-acp--show-permission-decision state tool-call choice))
                 (when (map-elt state :busy)
                   (emagent-acp--schedule-prompt-watchdog state))
                 (emagent-acp--refresh-mode-line state)
                 (funcall respond (or choice :cancel)))))
          (if (and buf (buffer-live-p buf)
                   (map-elt state :cb-permission)
                   (with-current-buffer buf (emagent-chat--open-response-p)))
              (with-current-buffer buf
                (funcall (map-elt state :cb-permission)
                         question emagent-acp--permission-emagent-choices
                         after-response tool-call))
            (emagent-tools--buttons-prompt
             question emagent-acp--permission-emagent-choices buf after-response))))))))

(defun emagent-acp--cancel-permission-request (state request)
  "Reply `cancelled' to permission REQUEST so the waiting agent does not hang.
Used when a request is abandoned by an error, interrupt, or teardown rather
than by a user decision."
  (when-let ((request-id (map-elt request 'id)))
    (ignore-errors
      (emagent-acp-send-response
       :client (map-elt state :client)
       :response (emagent-acp-make-session-request-permission-response
                  :request-id request-id :cancelled t)))))

(defun emagent-acp--cancel-outstanding-permissions (state)
  "Reply `cancelled' to every queued permission request, then clear the queue.
Leaves `:permission-busy' untouched: an in-flight interactive prompt owns its
own response.  Call from interrupt/teardown so abandoned requests never leave
the agent blocked."
  (dolist (request (map-elt state :permission-queue))
    (emagent-acp--cancel-permission-request state request))
  (map-put! state :permission-queue nil))

(defun emagent-acp--drain-permission-queue-now (state)
  "Process one queued permission request.

For auto-deny/auto-approve: synchronous.  For interactive prompts: inserts
the dialog non-blockingly and returns; the response is sent from the button
callback when the user decides."
  (if (and (map-elt state :permission-queue)
           (active-minibuffer-window))
      ;; Minibuffer is active — inserting a dialog would conflict.  Poll.
      (unless (or (map-elt state :permission-drain-timer)
                  (map-elt state :permission-busy))
        (map-put! state :permission-drain-timer
                  (run-at-time 0.3 nil
                               (lambda ()
                                 (map-put! state :permission-drain-timer nil)
                                 (emagent-acp--drain-permission-queue-now state)))))
    (unless (map-elt state :permission-busy)
      (when-let ((request (car (map-elt state :permission-queue))))
        (map-put! state :permission-queue (cdr (map-elt state :permission-queue)))
        (map-put! state :permission-busy t)
        (emagent-acp--refresh-mode-line state)
        (condition-case err
            (emagent-acp--handle-one-permission
             :state state
             :emagent-acp-request request
             :on-complete
             (lambda ()
               (map-put! state :permission-busy nil)
               (emagent-acp--refresh-mode-line state)
               (emagent-acp--maybe-complete-deferred-prompt state)
               (when (map-elt state :permission-queue)
                 (if (emagent-acp--permission-interactive-p state)
                     (emagent-acp--schedule-permission-drain state)
                   (emagent-acp--drain-permission-queue-now state)))))
          ((error quit)
           ;; The request was already popped; reply `cancelled' so the agent is
           ;; not left blocked, release busy, and drain whatever remains.
           (emagent-log "permission handler error: %s" (error-message-string err))
           (emagent-acp--cancel-permission-request state request)
           (map-put! state :permission-busy nil)
           (emagent-acp--refresh-mode-line state)
           (when (map-elt state :permission-queue)
             (emagent-acp--schedule-permission-drain state))))))))

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

(provide 'emagent-acp-request)
;;; emagent-acp-request.el ends here
(declare-function emagent-acp--tool-call-truncate "emagent-acp-tool-call")
(declare-function emagent-acp--ingest-tool-call-request "emagent-acp-tool-call")
(declare-function emagent-acp--chat-buffer "emagent-acp-usage")
(declare-function emagent-acp--prepare-interactive-context "emagent-acp-prompt")
(declare-function emagent-acp--clear-prompt-watchdog "emagent-acp-prompt")
(declare-function emagent-acp--schedule-prompt-watchdog "emagent-acp-prompt")
(declare-function emagent-acp--on-fs-read "emagent-acp-file")
(declare-function emagent-acp--on-fs-write "emagent-acp-file")
(declare-function emagent-acp--tool-call-weak-details "emagent-acp-tool-call")
