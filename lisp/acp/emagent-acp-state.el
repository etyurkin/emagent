;;; emagent-acp-state.el --- ACP session state for emagent  -*- lexical-binding: t; -*-

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

;; ACP session state hash table, stderr filtering helpers, RSS monitoring
;; timers, and the state constructor.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-protocol-client)


(defvar-local emagent-acp--session nil
  "ACP session state for the current emagent buffer.")

(defvar-local emagent-acp--when-connected-queue nil
  "Callbacks waiting for `emagent-acp--connected-p' in this buffer.")

(defun emagent-acp--make-usage ()
  "Return a fresh usage hash table."
  (let ((u (make-hash-table :test 'eq)))
    (puthash :context-used nil u)
    (puthash :context-size nil u)
    (puthash :total-tokens 0 u)
    u))

;; Defined before any code that reads or `setf's its slots: the accessors' gv
;; setter expanders must be registered at compile time, else a `setf' on a slot
;; earlier in the file falls back to a nonexistent `(setf ...)' function.
(cl-defstruct (emagent-acp-state
               (:constructor emagent-acp--state-create)
               (:copier nil))
  "Mutable per-buffer ACP session state.

Replaces the former untyped hash table so field access is checked
at byte-compile time.  Slots that are themselves maps (the usage
and tool-call/tool-resolve tables, keyed by id) stay hash tables."
  ;; Connection
  client chat-buffer on-reveal provider mcp-http initialized
  ;; Session
  session-id config-options (usage (emagent-acp--make-usage))
  session-auto-approve permission-auto-allow
  external-tool-gate-reasons external-tool-gate-logged
  external-tool-refusal-logged
  agent-rss agent-rss-timer
  ;; Turn
  ready busy
  (assistant-text "") (thought-text "") (thought-buffer "")
  prompt-finalized prompt-finishing (prompt-generation 0)
  prompt-retry-gen
  finish-token finish-timer prompt-watchdog prompt-watchdog-timer
  extra-context compress-pending quiet-prompt replaying-history
  continue-attempts deferred-complete-response
  current-tool current-tool-kind tool-call-since-last-chunk
  (tool-call-titles (make-hash-table :test 'equal))
  (tool-call-inputs (make-hash-table :test 'equal))
  (tool-call-labels (make-hash-table :test 'equal))
  (tool-call-decisions (make-hash-table :test 'equal))
  (tool-call-pending (make-hash-table :test 'equal))
  tool-resolve-queue tool-resolve-worker
  (tool-resolve-attempts (make-hash-table :test 'equal))
  ;; Permission gate
  permission-queue permission-busy permission-drain-timer
  ;; Agent-scheduled wakeup (ScheduleWakeup tool)
  wakeup-request wakeup-timer
  ;; Callbacks (wired by the app; see emagent.el)
  cb-chunk cb-thought cb-finish cb-fail cb-slash-commands
  cb-tool-call cb-permission cb-status)

(defun emagent-acp--strip-pino-colors (string)
  "Remove literal pino color tokens like [32m from STRING."
  (replace-regexp-in-string "\\[[0-9]+m" "" string))

(defun emagent-acp--agent-log-line-p (line)
  "Return non-nil when LINE is cursor-agent info/warn stderr."
  (let ((line (string-trim (emagent-acp--strip-pino-colors line))))
    (or (string-empty-p line)
        (string-match-p "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] .*\\[ *\\(info\\|warn\\) *\\]:" line))))

(defun emagent-acp--stderr-notify-p (emagent-acp-error)
  "Return non-nil when ACP-ERROR should be shown to the user.

Arguments: EMAGENT-ACP-ERROR."
  (let ((message (string-trim (or (map-elt emagent-acp-error 'message) (format "%s" emagent-acp-error)))))
    (cond
     ((string-empty-p message) nil)
     ((string-match-p "\\`\\(?:finished\\|Process\\|acp-client(\\)" message) nil)
     ((string-match-p "\\[32minfo\\|\\[33mwarn" message) nil)
     ((string-match-p "RetriableError" message) nil)
     ((string-match-p "ApiError\\|failed with status\\|\\[31merror" message) t)
     (t
      (let ((lines (split-string message "\n" t)))
        (not (and lines (seq-every-p #'emagent-acp--agent-log-line-p lines))))))))

(defun emagent-acp--log-agent-stderr (message)
  
  "Internal helper for MESSAGE."
  (when emagent-log-agent-stderr
    (emagent-log "agent: %s" (string-trim message))))

(defun emagent-acp--session ()
  
  "Internal helper."
  (or emagent-acp--session
      (error "No active emagent session for this buffer")))


(defun emagent-acp--turn-phase (state)
  "Return the lifecycle phase of STATE's current turn.

One of:
  `idle'        no turn in flight;
  `streaming'   a prompt is in flight (`:busy'), receiving output and possibly
                paused on a permission prompt;
  `finalizing'  streaming ended, the response is being rendered;
  `done'        the response has been fully rendered.

This derives the phase from the turn flags so callers share one
vocabulary for the turn state machine.  The flags remain the
underlying representation for now."
  (cond
   ((emagent-acp-state-busy state) 'streaming)
   ((emagent-acp-state-prompt-finishing state)
    (if (emagent-acp-state-prompt-finalized state) 'done 'finalizing))
   (t 'idle)))

(defun emagent-acp--connecting-p ()
  "Return non-nil when an ACP session is starting but not yet ready."
  (and emagent-acp--session
       (not (emagent-acp-state-ready emagent-acp--session))
       ;; From first `emagent-acp-start' until `session-ready': keep any
       ;; additional `ensure-connected' calls from tearing the attempt down.
       ;; After init, require a live client so a dead half-session can reconnect.
       (or (not (emagent-acp-state-initialized emagent-acp--session))
           (and (emagent-acp-state-client emagent-acp--session)
                (emagent-acp--client-started-p
                 (emagent-acp-state-client emagent-acp--session))))))

(defun emagent-acp--run-when-connected-queue ()
  "Run and clear `emagent-acp--when-connected-queue'."
  (while emagent-acp--when-connected-queue
    (let ((fn (pop emagent-acp--when-connected-queue)))
      (condition-case err
          (funcall fn)
        (error
         (emagent-log "connect callback failed: %s"
                      (error-message-string err)))))))

(defun emagent-acp--clear-when-connected-queue ()
  "Drop queued `emagent-acp-ensure-connected' callbacks without running them."
  (setq emagent-acp--when-connected-queue nil))

(defun emagent-acp--connected-p ()
  "Return non-nil when the current buffer has a live, ready ACP session."
  (and emagent-acp--session
       (emagent-acp-state-ready emagent-acp--session)
       (let ((client (emagent-acp-state-client emagent-acp--session)))
         (and client (emagent-acp--client-started-p client)))))

(defun emagent-acp--permission-pending-p (state)
  "Return non-nil when STATE has unanswered permission requests."
  (or (emagent-acp-state-permission-busy state)
      (emagent-acp-state-permission-queue state)))

(defun emagent-acp--cancel-wakeup (state)
  "Cancel a pending or armed agent wakeup (ScheduleWakeup) for STATE."
  (when-let ((timer (emagent-acp-state-wakeup-timer state)))
    (cancel-timer timer))
  (setf (emagent-acp-state-wakeup-timer state) nil)
  (setf (emagent-acp-state-wakeup-request state) nil))

(defun emagent-acp--cancel-state-timers (state)
  "Cancel every timer stored in STATE and clear its slot.
Prevents a reconnect or shutdown from leaving repeating/pending timers
\(RSS poll, watchdog, finish, permission drain, wakeup) pointed at dead
state."
  (dolist (timer (list (emagent-acp-state-agent-rss-timer state)
                       (emagent-acp-state-prompt-watchdog-timer state)
                       (emagent-acp-state-finish-timer state)
                       (emagent-acp-state-permission-drain-timer state)
                       (emagent-acp-state-wakeup-timer state)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (emagent-acp-state-agent-rss-timer state) nil
        (emagent-acp-state-prompt-watchdog-timer state) nil
        (emagent-acp-state-finish-timer state) nil
        (emagent-acp-state-permission-drain-timer state) nil
        (emagent-acp-state-wakeup-timer state) nil
        (emagent-acp-state-wakeup-request state) nil))

(defun emagent-acp--teardown-stale-session ()
  "Shut down a dead or incomplete ACP session without clearing persisted ids."
  (when-let* ((state emagent-acp--session))
    (emagent-acp--cancel-state-timers state)
    (when-let ((client (emagent-acp-state-client state)))
      (ignore-errors (emagent-acp-shutdown :client client))))
  (setq emagent-acp--session nil))

(cl-defun emagent-acp--make-state (&key client chat-buffer on-reveal)
  "Return a fresh `emagent-acp-state' for a session.

Arguments: CLIENT, CHAT-BUFFER, ON-REVEAL."
  (emagent-acp--state-create :client client
                             :chat-buffer chat-buffer
                             :on-reveal on-reveal))

(defun emagent-acp--set-callback (state key value)
  "Set the :cb-* callback slot KEY on STATE to VALUE.
Bridges the keyword-keyed callback alist wired by the app to typed slots."
  (pcase key
    (:cb-chunk          (setf (emagent-acp-state-cb-chunk state) value))
    (:cb-thought        (setf (emagent-acp-state-cb-thought state) value))
    (:cb-finish         (setf (emagent-acp-state-cb-finish state) value))
    (:cb-fail           (setf (emagent-acp-state-cb-fail state) value))
    (:cb-slash-commands (setf (emagent-acp-state-cb-slash-commands state) value))
    (:cb-tool-call      (setf (emagent-acp-state-cb-tool-call state) value))
    (:cb-permission     (setf (emagent-acp-state-cb-permission state) value))
    (:cb-status         (setf (emagent-acp-state-cb-status state) value))
    (_ (emagent-log "unknown callback key %S" key))))

(defconst emagent-acp--agent-error-signature-re
  (concat "RetriableError\\|getaddrinfo\\|ENOTFOUND\\|EAI_AGAIN"
          "\\|ECONNRESET\\|ECONNREFUSED\\|ConnectionRefused"
          "\\|ETIMEDOUT\\|EPIPE"
          "\\|\\[unavailable\\]\\|socket hang up\\|WritableIterable is closed")
  "Machine-generated markers of a transient error emitted as agent output.
Deliberately stricter than `emagent-acp--retriable-prompt-error-p': it must
not match prose such as \"network error\" or \"timeout\" that can legitimately
appear inside a real answer.")

(defun emagent-acp--turn-did-no-work-p (state)
  "Return non-nil when STATE's turn did no real work.
No tool invocations and little text means replaying the prompt is safe."
  (let ((text (string-trim (or (emagent-acp-state-assistant-text state) "")))
        (titles (emagent-acp-state-tool-call-titles state)))
    (and (or (null titles) (zerop (hash-table-count titles)))
         (< (length text) 400))))

(defun emagent-acp--agent-error-only-response-p (state)
  "Return non-nil when STATE's finished turn is only a transient agent error.

Some agents (e.g. cursor-agent-acp) accept the prompt, then hit a transient
network failure and emit the error as the whole turn's output instead of
failing the request.  Such a turn carries no real content and no tool calls
\(`emagent-acp--turn-did-no-work-p'), so it is safe for emagent to re-issue the
prompt with backoff rather than surface the error.  Matching uses
`emagent-acp--agent-error-signature-re', which only recognises
machine-generated error markers."
  (let ((text (string-trim (or (emagent-acp-state-assistant-text state) ""))))
    (and (not (emagent-acp-state-compress-pending state))
         (not (emagent-acp-state-quiet-prompt state))
         (emagent-acp--turn-did-no-work-p state)
         (not (string-empty-p text))
         (string-match-p emagent-acp--agent-error-signature-re text))))

(defun emagent-acp--turn-hit-transient-error-p (state)
  "Return non-nil when STATE's finished turn ended on a transient error marker.

Unlike `emagent-acp--agent-error-only-response-p' this does not require the
turn to be empty: it is true even when tool calls ran or real content was
produced.  Such a turn must NOT be replayed (that would repeat side effects
like commits or pushes); instead emagent resumes it by sending \"continue\",
mirroring what a user does by hand."
  (let ((text (or (emagent-acp-state-assistant-text state) "")))
    (and (not (emagent-acp-state-compress-pending state))
         (not (emagent-acp-state-quiet-prompt state))
         (string-match-p emagent-acp--agent-error-signature-re text))))

(provide 'emagent-acp-state)
;;; emagent-acp-state.el ends here
