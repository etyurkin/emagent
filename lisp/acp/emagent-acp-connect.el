;;; emagent-acp-connect.el --- ACP connect and send composition  -*- lexical-binding: t; -*-

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

;; Buffer-facing ACP connect/send composition, kept out of `emagent.el' so the
;; root file only wires user-facing commands to public entry points:
;;
;; - `emagent-acp-ensure-connected' turns a chat buffer's saved session state
;;   into a running ACP session (client + callbacks wired to the public chat
;;   rendering API).
;; - `emagent-acp-send' sends the current prompt through it, switching to a
;;   per-turn `/model' override transiently first when one is set.

;;; Code:

(require 'cl-lib)
(require 'emagent-log)
(require 'emagent-tools)
(require 'emagent-chat)
(require 'emagent-acp)
(require 'emagent-cursor)
(require 'emagent-claude)

(defun emagent-acp--make-client (provider buffer)
  "Create an ACP client for PROVIDER using BUFFER as context."
  (let ((process-directory (and (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (emagent-chat--session-directory)))))
    (pcase provider
      ('cursor (emagent-cursor-make-client :context-buffer buffer
                                           :process-directory process-directory))
      ('claude (emagent-claude-make-client :context-buffer buffer
                                           :process-directory process-directory))
      (_ (user-error "Unknown emagent provider: %s" provider)))))

(defvar emagent-default-provider)

;;;###autoload
(cl-defun emagent-acp-ensure-connected (&key on-ready on-reveal)
  "Connect the current emagent buffer to its ACP provider if needed.

When the agent process died but buffer-local state remains, tear it down and
reconnect (resuming the saved session id when present).  Optional ON-READY runs
once the session is ready; ON-REVEAL runs when the chat buffer should be shown.
While a connection is already in flight, ON-READY is queued instead of tearing
the session down and starting over."
  (when on-ready (push on-ready emagent-acp--when-connected-queue))
  (cond
   ((emagent-acp--connected-p)
    (emagent-acp--run-when-connected-queue))
   ((emagent-acp--connecting-p)
    nil)
   (t
    (emagent-acp--teardown-stale-session)
    (let* ((provider (or emagent-chat-provider emagent-default-provider))
           (client (emagent-acp--make-client provider (current-buffer))))
      (emagent-acp-start :client client
                        :chat-buffer (current-buffer)
                        :on-ready #'emagent-acp--run-when-connected-queue
                        :on-reveal on-reveal
                        :callbacks
                        `((:cb-chunk          . ,#'emagent-chat-append-assistant)
                          (:cb-thought        . ,#'emagent-chat-append-thought)
                          (:cb-finish         . ,(lambda (&rest args)
                                                    (apply #'emagent-chat-finish-assistant args)
                                                    (emagent-acp--restore-turn-model)))
                          (:cb-fail           . ,(lambda (&rest args)
                                                    (apply #'emagent-chat-fail-assistant args)
                                                    (emagent-acp--turn-model-on-failure
                                                     (car args))))
                          (:cb-slash-commands . ,#'emagent-chat-set-slash-commands)
                          (:cb-tool-call      . ,#'emagent-chat-show-tool-call)
                          (:cb-permission     . ,#'emagent-chat-permission-prompt)
                          (:cb-status         . ,#'emagent-chat-set-status)))))))

(defun emagent-acp--send-prompt-safe (buffer user-text &optional compress)
  "Send USER-TEXT from BUFFER, logging and surfacing failures in the chat.
COMPRESS is forwarded to `emagent-acp-send-prompt'."
  (with-current-buffer buffer
    (condition-case err
        (emagent-acp-send-prompt user-text compress)
      (error
       (let ((msg (error-message-string err)))
         (when (fboundp 'emagent-chat--send-pending-end)
           (emagent-chat--send-pending-end))
         (emagent-log "emagent: send failed: %s" msg)
         (when (fboundp 'emagent-chat-fail-assistant)
           (emagent-chat-fail-assistant msg)))))))

;;;###autoload
(defun emagent-acp-send (user-text &optional compress)
  "Ensure connection and send USER-TEXT from the current buffer.

When a per-turn model override (`emagent-chat--turn-model', set by `/model') is
active and differs from the session model, switch to it transiently first, then
send; the buffer model is restored when the turn ends (see
`emagent-chat-finish-assistant' / `emagent-chat-fail-assistant').

COMPRESS is forwarded to `emagent-acp-send-prompt': set by
`emagent-chat--dispatch-compress' when USER-TEXT is already a compression
summary prompt rather than ordinary chat input."
  (let ((buf (current-buffer))
        (turn-model emagent-chat--turn-model)
        (token emagent-chat--send-token))
    (emagent-acp-ensure-connected
     :on-ready
     (lambda ()
       (with-current-buffer buf
         (when (emagent-chat--send-active-p token)
           (let* ((state emagent-acp--session)
                  (current (and state (emagent-acp-current-model-id)))
                  (target (and turn-model state
                                (emagent-acp--match-model-id turn-model state nil))))
             (if (and target current (not (string= target current)))
                 (progn
                   ;; Remember the real session model to restore to (only the
                   ;; first time, so a sticky post-failure override still points
                   ;; back at the original global model).
                   (unless emagent-chat--turn-model-base
                     (setq emagent-chat--turn-model-base current))
                   (when (fboundp 'emagent-acp--progress)
                     (emagent-acp--progress
                      state
                      (format "switching model to %s for this turn…"
                              (if (fboundp 'emagent-acp--model-display-name)
                                  (emagent-acp--model-display-name state nil target)
                                target))))
                   (emagent-acp-set-model-transient
                    target
                    (lambda ()
                      (when (emagent-chat--send-active-p token)
                        (emagent-acp--send-prompt-safe buf user-text compress)))))
               (emagent-acp--send-prompt-safe buf user-text compress)))))))))

(defun emagent-acp--restore-turn-model ()
  "Restore the session model overridden by `/model' and clear the override.
Called on a successful turn: switches back to the captured base model and clears
`emagent-chat--turn-model' so the next prompt uses the buffer model again."
  (when emagent-chat--turn-model
    (when emagent-chat--turn-model-base
      (emagent-acp-set-model-transient emagent-chat--turn-model-base #'ignore))
    (setq emagent-chat--turn-model nil
          emagent-chat--turn-model-base nil)))

(defun emagent-acp--turn-model-on-failure (&optional message)
  "After a failed `/model' turn, keep or restore the per-turn override.

MESSAGE is the failure text used to classify transient vs permanent errors.
Only transient network failures (after retries are exhausted) ask whether to
keep the override for a manual `retry'.  Permanent errors such as context
overflow restore the buffer model immediately."
  (when emagent-chat--turn-model
    (if (and message (fboundp 'emagent-acp--retriable-prompt-error-p)
         (emagent-acp--retriable-prompt-error-p message))
        (emagent-tools--buttons-prompt
         (format "Continue with %s for the next prompt?" emagent-chat--turn-model)
         '(("Yes, keep it" . keep) ("No, use the buffer model" . restore))
         (current-buffer)
         (lambda (choice)
           (when (eq choice 'restore)
             (emagent-acp--restore-turn-model))))
      (emagent-acp--restore-turn-model))))

(provide 'emagent-acp-connect)
;;; emagent-acp-connect.el ends here
