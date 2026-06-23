;;; emagent-acp-request.el --- ACP permission request dispatch  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(declare-function emagent-acp--send-response "emagent-acp-protocol")
(declare-function emagent-acp--tool-call-elisp-prin1-p "emagent-acp-tool-call")
(declare-function emagent-acp--tool-call-update-from-request "emagent-acp-tool-call")
(declare-function emagent-acp--tool-call-detail "emagent-acp-tool-call")
(declare-function emagent-acp--tool-call-raw-input-detail "emagent-acp-tool-call")
(declare-function emagent-acp--refresh-mode-line "emagent-acp-usage")
(declare-function emagent-acp--maybe-complete-deferred-prompt "emagent-acp")
(declare-function emagent-acp--permission-interactive-p "emagent-acp-permit")
(declare-function emagent-acp--permission-option-id "emagent-acp-permit")
(declare-function emagent-acp--schedule-permission-drain "emagent-acp-permit")

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

(provide 'emagent-acp-request)
;;; emagent-acp-request.el ends here
(declare-function emagent-acp--tool-call-truncate "emagent-acp-tool-call")
(declare-function emagent-acp--ingest-tool-call-request "emagent-acp-permit")
(declare-function emagent-acp--tool-call-dangerous-p "emagent-acp-permit")
(declare-function emagent-acp--chat-buffer "emagent-acp-usage")
(declare-function emagent-acp--prepare-interactive-context "emagent-acp-prompt")
(declare-function emagent-acp--clear-prompt-watchdog "emagent-acp-prompt")
(declare-function emagent-acp--schedule-prompt-watchdog "emagent-acp-prompt")
(declare-function emagent-acp--on-fs-read "emagent-acp-file")
(declare-function emagent-acp--on-fs-write "emagent-acp-file")
(declare-function emagent-acp--tool-call-weak-details "emagent-acp-tool-call")
