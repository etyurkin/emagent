;;; emagent-acp-permission-dialog.el --- ACP permission dialog UI  -*- lexical-binding: t; -*-

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

;; Leaf owning one permission dialog: prompt text helpers and
;; `emagent-acp--handle-one-permission'.  Required by
;; `emagent-acp-permission-queue' so the drain loop can call the handler
;; directly without a soft cycle through `emagent-acp-request'.  Must not
;; top-level require `emagent-acp-prompt' (lazy inside the interactive path).

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-protocol)
(require 'emagent-tools)
(require 'emagent-chat-ui)
(require 'emagent-chat)
(require 'emagent-acp-permit)
(require 'emagent-acp-tool-call)
(require 'emagent-acp-usage)

(defun emagent-acp--permission-question-line (emagent-acp-request)
  "Return the command or path to show on the permission ? line.

Arguments: EMAGENT-ACP-REQUEST."
  (let* ((tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (detail (and tool-call (emagent-acp--tool-call-detail-from-tool-call tool-call)))
         (title (emagent-acp--permission-prompt-title emagent-acp-request))
         (name (and title (replace-regexp-in-string "\\`Allow \\(.*\\)[?]\\'" "\\1" title))))
    (cond
     ((emagent-acp--human-tool-detail-p detail)
      ;; A shell detail is self-explanatory ("make test"), but an MCP
      ;; tool's raw-input fragment ("--oneline -10") is meaningless
      ;; without the tool's name — prepend it.
      (if (and name (string-match-p "\\`mcp__" name)
               (not (string-match-p (regexp-quote name) detail)))
          (format "%s %s" name detail)
        detail))
     (name name)
     (t "Permission request"))))

(defun emagent-acp--permission-prompt-title (emagent-acp-request)
  "Return the primary permission question line from EMAGENT-ACP-REQUEST."
  (let* ((tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (raw (or (map-nested-elt emagent-acp-request '(params title))
                  (map-nested-elt emagent-acp-request '(params toolCall title))
                  "Permission request"))
         (title (car (split-string raw "\n" t))))
    (if (and tool-call (emagent-acp--switch-mode-tool-p tool-call))
        (emagent-acp--switch-mode-display-title
         (if (map-elt tool-call 'title)
             tool-call
           (cons (cons 'title title) tool-call)))
      title)))

(defun emagent-acp--permission-prompt-text (emagent-acp-request)
  "Return user-facing permission prompt text for EMAGENT-ACP-REQUEST."
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
non-blockingly and returns; ON-COMPLETE is called after the user responds.

`switch_mode' permissions (Claude ExitPlanMode, Cursor SwitchMode) never
auto-approve: the agent's optionIds are mode ids and must be returned
unchanged."
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
         (deny-id (emagent-acp--permission-acp-deny-id options))
         (switch-mode (emagent-acp--switch-mode-tool-p tool-call))
         (switch-choices (and switch-mode
                              (emagent-acp--switch-mode-choices options))))
    (when raw-tool-call
      (emagent-acp--ingest-tool-call-request state raw-tool-call))
    (let ((respond
           (lambda (choice)
             (when (and (not switch-mode)
                        (emagent-acp--permission-approved-choice-p choice))
               (emagent-acp--permission-apply-choice state fingerprint buf choice))
             (let* ((response
                     (cond
                      ((and (stringp choice) (not (string-empty-p choice)))
                       (emagent-acp-make-session-request-permission-response
                        :request-id request-id :option-id choice))
                      ((and (not switch-mode)
                            validation (eq (car validation) :deny))
                       (if deny-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id deny-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      ((and (not switch-mode)
                            (emagent-acp--permission-approved-choice-p choice))
                       (if allow-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id allow-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      ((and (not switch-mode) (eq choice :deny))
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
               (emagent-acp-send-response :client (emagent-acp-state-client state) :response response))
             (when on-complete (funcall on-complete)))))
      (cond
       (switch-mode
        (unless (fboundp 'emagent-acp--prepare-interactive-context)
          (require 'emagent-acp-prompt))
        (emagent-acp--prepare-interactive-context state)
        (emagent-acp--clear-prompt-watchdog state)
        (let ((after-response
               (lambda (choice)
                 (when choice
                   (emagent-acp--show-permission-decision state tool-call choice))
                 (when (emagent-acp-state-busy state)
                   (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
                     (require 'emagent-acp-prompt))
                   (emagent-acp--schedule-prompt-watchdog state))
                 (emagent-acp--refresh-mode-line state)
                 (funcall respond (or choice :cancel))))
              (choices (or switch-choices
                           '(("Cancel" . :cancel))))
              (prompt (or (and tool-call
                               (emagent-acp--switch-mode-display-title tool-call))
                          question))
              (preamble (emagent-acp--switch-mode-preamble
                         (or raw-tool-call tool-call))))
          (emagent-tools--buttons-prompt
           prompt choices buf after-response preamble)))
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
        (unless (fboundp 'emagent-acp--prepare-interactive-context)
          (require 'emagent-acp-prompt))
        (emagent-acp--prepare-interactive-context state)
        (emagent-acp--clear-prompt-watchdog state)
        (let ((after-response
               (lambda (choice)
                 (when choice
                   (emagent-acp--show-permission-decision state tool-call choice))
                 (when (emagent-acp-state-busy state)
                   (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
                     (require 'emagent-acp-prompt))
                   (emagent-acp--schedule-prompt-watchdog state))
                 (emagent-acp--refresh-mode-line state)
                 (funcall respond (or choice :cancel)))))
          (if (and buf (buffer-live-p buf)
                   (emagent-acp-state-cb-permission state)
                   (with-current-buffer buf (emagent-chat--open-response-p)))
              (with-current-buffer buf
                (funcall (emagent-acp-state-cb-permission state)
                         question emagent-acp--permission-emagent-choices
                         after-response tool-call))
            (emagent-tools--buttons-prompt
             question emagent-acp--permission-emagent-choices buf after-response))))))))

(provide 'emagent-acp-permission-dialog)
;;; emagent-acp-permission-dialog.el ends here
