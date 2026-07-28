;;; emagent-acp-permission-queue.el --- ACP permission queue mechanics  -*- lexical-binding: t; -*-

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

;; Permission-queue drain loop: batching, minibuffer-busy poll, and
;; cancellation.  Requires `emagent-acp-permission-dialog' and calls
;; `emagent-acp--handle-one-permission' directly.  Send/tool-call drive
;; the queue via lazy or top-level require of this file without needing
;; `emagent-acp-request'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-protocol)
(require 'emagent-acp-usage)
(require 'emagent-acp-permission-dialog)

(defun emagent-acp--permission-interactive-p (state)
  "Return non-nil when ACP permission dialogue may need user input.

Arguments: STATE."
  (and (not (emagent-acp-state-session-auto-approve state))
       (not (eq emagent-acp-auto-approve-permissions t))))

(defun emagent-acp--cancel-permission-request (state request)
  "Reply `cancelled' to permission REQUEST so the waiting agent does not hang.
Used when a request is abandoned by an error, interrupt, or teardown rather
than by a user decision.

Arguments: STATE."
  (when-let ((request-id (map-elt request 'id)))
    (ignore-errors
      (emagent-acp-send-response
       :client (emagent-acp-state-client state)
       :response (emagent-acp-make-session-request-permission-response
                  :request-id request-id :cancelled t)))))

(defun emagent-acp--cancel-outstanding-permissions (state)
  "Reply `cancelled' to every queued permission request, then clear the queue.
Leaves `:permission-busy' untouched: an in-flight interactive prompt owns its
own response.  Call from interrupt/teardown so abandoned requests never leave
the agent blocked.

Arguments: STATE."
  (dolist (request (emagent-acp-state-permission-queue state))
    (emagent-acp--cancel-permission-request state request))
  (setf (emagent-acp-state-permission-queue state) nil))

(defun emagent-acp--schedule-permission-drain (state)
  "Run `emagent-acp--drain-permission-queue-now' outside the ACP process filter.

Arguments: STATE."
  (unless (or (emagent-acp-state-permission-drain-timer state)
              (emagent-acp-state-permission-busy state))
    (setf (emagent-acp-state-permission-drain-timer state)
              (run-at-time 0 nil
                           (lambda ()
                             (setf (emagent-acp-state-permission-drain-timer state) nil)
                             (emagent-acp--drain-permission-queue-now state))))))

(defun emagent-acp--drain-permission-queue-now (state)
  "Process queued permission requests without recursive auto-approve nesting.

For auto-deny/auto-approve: handle at most
`emagent-acp-permission-drain-batch-size' requests, then reschedule so a flood
of MCP permissions cannot peg the Emacs command loop.  For interactive
prompts: insert one dialog and return; the button callback schedules the
next drain.

Arguments: STATE."
  (if (and (emagent-acp-state-permission-queue state)
           (active-minibuffer-window))
      ;; Minibuffer is active — inserting a dialog would conflict.  Poll.
      (unless (or (emagent-acp-state-permission-drain-timer state)
                  (emagent-acp-state-permission-busy state))
        (setf (emagent-acp-state-permission-drain-timer state)
              (run-at-time 0.3 nil
                           (lambda ()
                             (setf (emagent-acp-state-permission-drain-timer state) nil)
                             (emagent-acp--drain-permission-queue-now state)))))
    (let ((batch 0)
          (limit (max 1 emagent-acp-permission-drain-batch-size))
          (interactive (emagent-acp--permission-interactive-p state)))
      (while (and (emagent-acp-state-permission-queue state)
                  (not (emagent-acp-state-permission-busy state))
                  (or interactive (< batch limit)))
        (setq batch (1+ batch))
        (let ((request (car (emagent-acp-state-permission-queue state))))
          (setf (emagent-acp-state-permission-queue state)
                (cdr (emagent-acp-state-permission-queue state)))
          (setf (emagent-acp-state-permission-busy state) t)
          (emagent-acp--refresh-mode-line state)
          (condition-case err
              (emagent-acp--handle-one-permission
               :state state
               :emagent-acp-request request
               :on-complete
               (lambda ()
                 ;; Auto-approve runs this before handle-one returns (busy
                 ;; clears; the while loop continues).  Interactive prompts
                 ;; run this later from the button callback.
                 (setf (emagent-acp-state-permission-busy state) nil)
                 (emagent-acp--refresh-mode-line state)
                 (condition-case cont-err
                     (progn
                       (unless (fboundp 'emagent-acp--maybe-complete-deferred-prompt)
                         (require 'emagent-acp-prompt))
                       (emagent-acp--maybe-complete-deferred-prompt state)
                       (when (and (emagent-acp-state-permission-queue state)
                                  (emagent-acp--permission-interactive-p state))
                         (emagent-acp--schedule-permission-drain state)))
                   ((error quit)
                    (emagent-log "permission on-complete error: %s"
                                 (error-message-string cont-err))))))
            ((error quit)
             ;; Request was popped but not answered: cancel so the agent is
             ;; not left blocked, then continue the iterative drain.
             (emagent-log "permission handler error: %s" (error-message-string err))
             (emagent-acp--cancel-permission-request state request)
             (setf (emagent-acp-state-permission-busy state) nil)
             (emagent-acp--refresh-mode-line state)
             (when (and (emagent-acp-state-permission-queue state)
                        (emagent-acp--permission-interactive-p state))
               (emagent-acp--schedule-permission-drain state))))))
      ;; Yield after an auto-approve batch so redisplay can run.
      (when (and (not interactive)
                 (emagent-acp-state-permission-queue state)
                 (not (emagent-acp-state-permission-busy state)))
        (emagent-acp--schedule-permission-drain state)))))

(defun emagent-acp--drain-permission-queue (state)
  "Process queued permission requests one at a time.

Interactive prompts are deferred to the next event cycle so
`recursive-edit' never runs inside the ACP process filter.

Arguments: STATE."
  (when (emagent-acp-state-permission-queue state)
    (if (emagent-acp--permission-interactive-p state)
        (emagent-acp--schedule-permission-drain state)
      (emagent-acp--drain-permission-queue-now state))))

(provide 'emagent-acp-permission-queue)
;;; emagent-acp-permission-queue.el ends here
