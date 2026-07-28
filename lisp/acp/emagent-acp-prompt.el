;;; emagent-acp-prompt.el --- Prompt lifecycle for emagent  -*- lexical-binding: t; -*-

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

;; Watchdog timer, streaming toggles, render scheduling, prompt completion,
;; thought buffer, reveal, interactive context, and error handling.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-mcp)
(require 'emagent-chat)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-usage)
(require 'emagent-acp-permit)
(require 'emagent-acp-model)
(require 'emagent-acp-gate)
(require 'emagent-acp-send)
(require 'emagent-acp-wire)

(defvar emagent-chat--finish-close)

(defun emagent-acp--clear-prompt-watchdog (state)
  "Cancel any pending prompt stall watchdog for STATE."
  (when-let ((timer (emagent-acp-state-prompt-watchdog-timer state)))
    (cancel-timer timer))
  (setf (emagent-acp-state-prompt-watchdog state) nil)
  (setf (emagent-acp-state-prompt-watchdog-timer state) nil))

(defun emagent-acp--schedule-prompt-watchdog (state)
  "Abort a prompt that stays busy without ACP progress.

Cancel any existing watchdog first: this is re-invoked on every displayed tool
call and permission answer, and without the cancel each call would leak a live
timer (token-guarded no-ops that still pin STATE for the whole timeout).

When ACP work is still outstanding (pending RPC, permission prompt, or
tool-resolve), extend the watchdog instead of finalizing: otherwise the UI
closes the Response while the agent keeps working and logging."
  (when-let ((old (emagent-acp-state-prompt-watchdog-timer state)))
    (cancel-timer old))
  (let* ((token (cl-gensym "emagent-prompt-watchdog"))
         (timer (run-with-timer
                 emagent-acp-watchdog-timeout nil
                 (lambda ()
                   (when (and (eq (emagent-acp-state-prompt-watchdog state) token)
                              (emagent-acp-state-busy state))
                     (let* ((client (emagent-acp-state-client state))
                            (pending (and client (map-elt client :pending-requests)))
                            (waiting
                             (or pending
                                 (emagent-acp--permission-pending-p state)
                                 (and (fboundp 'emagent-acp--provider-tool-resolve-active-p)
                                      (emagent-acp--provider-tool-resolve-active-p state)))))
                       (emagent-log "emagent: prompt stalled (no ACP completion in %ds)"
                                    emagent-acp-watchdog-timeout)
                       (when pending
                         (emagent-log "emagent: pending ACP request count: %d"
                                      (length pending)))
                       (cond
                        (waiting
                         (emagent-log "emagent: prompt still waiting on agent work; extending watchdog")
                         (emagent-acp--schedule-prompt-watchdog state))
                        ((and (emagent-acp-state-assistant-text state)
                              (not (string-empty-p
                                    (emagent-acp-state-assistant-text state))))
                         (emagent-log "emagent: prompt stalled; finalizing partial response")
                         (emagent-acp--complete-prompt state nil))
                        (t
                         (emagent-acp--abort-prompt
                          state
                          "prompt stalled — reconnect with M-x emagent-mode or kill and reopen the buffer")))))))))
    (setf (emagent-acp-state-prompt-watchdog state) token)
    (setf (emagent-acp-state-prompt-watchdog-timer state) timer)))

(defun emagent-acp--stream-to-buffer-p (state)
  "Return non-nil when agent chunks may update the chat buffer live.

Arguments: STATE.

Chunks may stream while the prompt is busy or while a finish render is
still settling (`prompt-finishing'), so late agent text is not stranded
after an early stub.  Once `prompt-finalized' is set, streaming stops."
  (and emagent-acp-stream-to-buffer
       (not (emagent-acp-state-compress-pending state))
       (not (emagent-acp-state-quiet-prompt state))
       (not (emagent-acp-state-prompt-finalized state))
       (or (emagent-acp-state-busy state)
           (emagent-acp-state-prompt-finishing state))))

(defun emagent-acp--stream-thought-to-buffer-p (state)
  "Return non-nil when reasoning may stream into the chat buffer live.

Arguments: STATE."
  (and (memq emagent-acp-thought-progress '(buffer both))
       (not (emagent-acp-state-compress-pending state))
       (not (emagent-acp-state-quiet-prompt state))
       (not (emagent-acp-state-prompt-finalized state))
       (or (emagent-acp-state-busy state)
           (emagent-acp-state-prompt-finishing state))))

(defun emagent-acp--cancel-prompt-render (state)
  "Cancel a pending debounced render for STATE."
  (when-let ((timer (emagent-acp-state-finish-timer state)))
    (cancel-timer timer))
  (setf (emagent-acp-state-finish-timer state) nil)
  (setf (emagent-acp-state-finish-token state) nil))

(defun emagent-acp--schedule-prompt-render (state)
  "Debounced render of the accumulated prompt into the chat buffer.

Arguments: STATE."
  (let ((token (cl-gensym "emagent-finish")))
    (emagent-acp--cancel-prompt-render state)
    (setf (emagent-acp-state-finish-token state) token)
    (setf (emagent-acp-state-finish-timer state)
              (run-with-timer
               emagent-acp-render-delay nil
               (lambda ()
                 (when (and (eq (emagent-acp-state-finish-token state) token)
                            (emagent-acp-state-prompt-finishing state))
                   (setf (emagent-acp-state-finish-timer state) nil)
                   (emagent-acp--render-prompt-response state)))))))

(defun emagent-acp--render-prompt-response (state)
  "Render accumulated prompt text into the chat buffer for STATE.

For a normal finish, rewrite the open response without closing it, then
close only when assistant/thought text is still the snapshot that was
rendered.  Late chunks that arrive during the debounce or the finish
callback update state and reschedule; an early stub must not land before
the final text is stable."
  (when (emagent-acp-state-prompt-finishing state)
    (when-let ((buffer (emagent-acp--chat-buffer state)))
      (cond
       ((emagent-acp-state-quiet-prompt state)
        (setf (emagent-acp-state-quiet-prompt state) nil)
        (setf (emagent-acp-state-assistant-text state) "")
        (setf (emagent-acp-state-thought-text state) "")
        (emagent-acp--clear-thought-buffer state)
        (emagent-acp--cancel-prompt-render state)
        (setf (emagent-acp-state-prompt-finishing state) nil)
        (setf (emagent-acp-state-prompt-finalized state) t)
        (emagent-log "compacted session materialized")
        (emagent-acp--progress state "connected")
        (emagent-acp--refresh-mode-line state))
       ((emagent-acp-state-compress-pending state)
        (let ((summary (string-trim (or (emagent-acp-state-assistant-text state) ""))))
          (setf (emagent-acp-state-compress-pending state) nil)
          (if (string-empty-p summary)
              (progn
                (emagent-log "compression aborted: empty summary")
                (with-current-buffer buffer
                  (when-let ((cb (emagent-acp-state-cb-fail state)))
                    (funcall cb "Compression produced no summary; conversation left intact"))))
            (with-current-buffer buffer
              (when-let ((cb (emagent-acp-state-cb-finish state)))
                (funcall cb
                         (format "*Context compacted.* Agent session reset; the summary below is its only memory of the prior conversation.\n\n%s"
                                 summary))))
            (emagent-log "compressed session (%d chars)" (length summary))
            (unless (fboundp 'emagent-acp--new-session)
              (require 'emagent-acp-lifecycle))
            (emagent-acp--new-session
             :state state
             :compressed-context summary
             :on-ready
             (lambda ()
               (unless (fboundp 'emagent-acp--materialize-session)
                 (require 'emagent-acp-send))
               (emagent-acp--materialize-session state))))
          (setf (emagent-acp-state-prompt-finishing state) nil)
          (setf (emagent-acp-state-prompt-finalized state) t)
          (with-current-buffer buffer
            (emagent-chat--flush-deferred-font-lock))
          (emagent-acp--refresh-mode-line state)))
       (t
        (let ((token (emagent-acp-state-finish-token state))
              (assistant (emagent-acp-state-assistant-text state))
              (thought (emagent-acp-state-thought-text state))
              (failed nil))
          (condition-case err
              (with-current-buffer buffer
                (when-let ((cb (emagent-acp-state-cb-finish state)))
                  (let ((emagent-chat--finish-close nil))
                    (funcall cb assistant thought))))
            (error
             (setq failed t)
             (emagent-log "emagent: finish failed: %s" (error-message-string err))
             (with-current-buffer buffer
               (when-let ((cb (emagent-acp-state-cb-fail state)))
                 (funcall cb (format "response finalize failed: %s"
                                     (error-message-string err)))))))
          (cond
           (failed
            (setf (emagent-acp-state-prompt-finishing state) nil)
            (setf (emagent-acp-state-prompt-finalized state) t)
            (with-current-buffer buffer
              (emagent-chat--flush-deferred-font-lock))
            (emagent-acp--refresh-mode-line state))
           ((and (emagent-acp-state-prompt-finishing state)
                 (eq (emagent-acp-state-finish-token state) token)
                 (eq (emagent-acp-state-assistant-text state) assistant)
                 (eq (emagent-acp-state-thought-text state) thought))
            ;; Finalize before close so a reentrant chunk cannot stream or
            ;; schedule another render against a half-closed response.
            (setf (emagent-acp-state-prompt-finalized state) t)
            (setf (emagent-acp-state-prompt-finishing state) nil)
            (emagent-acp--cancel-prompt-render state)
            (with-current-buffer buffer
              (emagent-chat--close-finished-response))
            (emagent-acp--refresh-mode-line state))
           ((and (emagent-acp-state-prompt-finishing state)
                 (eq (emagent-acp-state-finish-token state) token))
            ;; Text changed during finish but no newer timer was scheduled.
            (emagent-acp--schedule-prompt-render state)))))))))

(defun emagent-acp--maybe-complete-deferred-prompt (state)
  "Run a deferred `emagent-acp--complete-prompt' when permissions are clear.

Arguments: STATE."
  (when-let ((response (emagent-acp-state-deferred-complete-response state)))
    (unless (emagent-acp--permission-pending-p state)
      (setf (emagent-acp-state-deferred-complete-response state) nil)
      (emagent-acp--complete-prompt state response))))

(defun emagent-acp--complete-prompt (state response)
  "Finalize the in-flight prompt for STATE using RESPONSE and close chat."
  (cond
   ((emagent-acp-state-prompt-finalized state)
    (when (emagent-acp-state-busy state)
      (setf (emagent-acp-state-busy state) nil)
      (emagent-acp--refresh-mode-line state)))
   ((not (emagent-acp-state-busy state))
    nil)
   ((emagent-acp--permission-pending-p state)
    (setf (emagent-acp-state-deferred-complete-response state) response))
   (t
    (setf (emagent-acp-state-prompt-retry-gen state) nil)
    (setf (emagent-acp-state-prompt-finishing state) t)
    (setf (emagent-acp-state-busy state) nil)
    (setf (emagent-acp-state-current-tool state) nil)
    (setf (emagent-acp-state-current-tool-kind state) nil)
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--trace "prompt done (%d chars, %d thought)"
                        (length (or (emagent-acp-state-assistant-text state) ""))
                        (length (or (emagent-acp-state-thought-text state) "")))
    (emagent-acp--flush-thought-buffer state)
    (when (and response (map-elt response 'usage))
      (emagent-acp--save-usage-from-response state (map-elt response 'usage)))
    (emagent-acp--refresh-mode-line state)
    (emagent-acp--schedule-prompt-render state)
    (emagent-acp--arm-wakeup state)
    (emagent-acp--arm-plan-build state))))

(defun emagent-acp--arm-wakeup (state)
  "Start the ScheduleWakeup timer for STATE after this turn completes.
Called when the turn completes: the agent has ended its reply and now
waits to be re-invoked.  The wakeup prompt is sent as a regular user
turn so the transcript records what re-started the agent."
  (when-let ((request (and emagent-acp-honor-schedule-wakeup
                           (emagent-acp-state-wakeup-request state))))
    (emagent-acp--cancel-wakeup state)
    (let ((delay (plist-get request :delay))
          (text (or (plist-get request :prompt)
                    (if-let ((reason (plist-get request :reason)))
                        (format "Wake up: %s" reason)
                      "Wake up: continue the scheduled task."))))
      (emagent-acp--notify-user
       state (format "emagent: wakeup armed in %ds%s" delay
                     (if-let ((reason (plist-get request :reason)))
                         (format " — %s" reason)
                       "")))
      (setf (emagent-acp-state-wakeup-timer state)
            (run-with-timer delay nil #'emagent-acp--fire-wakeup state text)))))

(defun emagent-acp--fire-wakeup (state text)
  "Send TEXT as a new user turn for STATE's chat buffer.
Skips silently when the buffer is gone or a prompt is already running
\(a manual turn superseded the loop)."
  (setf (emagent-acp-state-wakeup-timer state) nil)
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (cond
       ((emagent-acp-state-busy state)
        (emagent-log "wakeup: skipped — a prompt is already running"))
       ((not (and (fboundp 'emagent-chat--insert-user-heading-with-text)
                  emagent-chat--on-send))
        (emagent-log "wakeup: skipped — chat send unavailable"))
       (t
        (emagent-log "wakeup: %s" (emagent-log-truncate-line text 80))
        (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
          (emagent-chat--begin-response response-pos))
        (emagent-chat--ensure-follow-window buffer)
        ;; emagent-acp-send drops the turn unless a send token is armed
        ;; (manual C-c C-c calls send-pending-begin; Build/wakeup must too).
        (emagent-chat--send-pending-begin)
        (funcall emagent-chat--on-send text))))))

(defun emagent-acp--set-session-mode (state mode-id)
  "Best-effort `session/set_mode' to MODE-ID for STATE."
  (when-let ((session-id (emagent-acp-state-session-id state)))
    (unless (fboundp 'emagent-acp--send-request)
      (require 'emagent-acp-wire))
    (emagent-acp--send-request
     :state state
     :request (emagent-acp-make-session-set-mode-request
               :session-id session-id
               :mode-id mode-id)
     :on-success
     (lambda (_response)
       (setf (emagent-acp-state-session-mode-id state) mode-id)
       (emagent-acp--refresh-mode-line state))
     :on-failure
     (lambda (err _raw)
       (emagent-log "session/set_mode %s failed: %s"
                    mode-id
                    (or (map-elt err 'message) err))))))

(defun emagent-acp--ensure-agent-mode (state)
  "Best-effort `session/set_mode' to agent for STATE before Build."
  (emagent-acp--set-session-mode state "agent"))

(defun emagent-acp--fire-plan-build (state text)
  "Send TEXT as the Build follow-up for STATE without a user heading.

Build instructions are agent-internal: open Thinking/Response for the
work, but do not invent a synthetic `* user>' line in the transcript."
  (setf (emagent-acp-state-plan-build-timer state) nil)
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (cond
       ((emagent-acp-state-busy state)
        (emagent-log "plan-build: skipped — a prompt is already running"))
       ((not emagent-chat--on-send)
        (emagent-log "plan-build: skipped — chat send unavailable"))
       (t
        (emagent-log "plan-build: %s" (emagent-log-truncate-line text 80))
        ;; Build owns the next turn; allow a normal stub after it finishes.
        (setq emagent-chat--defer-user-stub nil)
        (emagent-chat--begin-response (emagent-chat--user-zone-start))
        (emagent-chat--ensure-follow-window buffer)
        (emagent-chat--send-pending-begin)
        (funcall emagent-chat--on-send text))))))

(defun emagent-acp--arm-plan-build (state)
  "Arm a Build turn when create_plan queued one on STATE."
  (when-let ((text (emagent-acp-state-plan-build-prompt state)))
    (setf (emagent-acp-state-plan-build-prompt state) nil)
    (when-let ((timer (emagent-acp-state-plan-build-timer state)))
      (when (timerp timer) (cancel-timer timer))
      (setf (emagent-acp-state-plan-build-timer state) nil))
    (emagent-log "cursor/create_plan: arming Build turn")
    (emagent-acp--ensure-agent-mode state)
    (setf (emagent-acp-state-plan-build-timer state)
          (run-with-timer 0.35 nil
                          #'emagent-acp--fire-plan-build state text))))

(defun emagent-acp--log-thought-line (mode text)
  "Log one thought TEXT line according to MODE."
  (let ((line (string-trim text)))
    (unless (string-empty-p line)
      (pcase mode
        ('minimal
         (emagent-log "… %s" (emagent-log-truncate-line line 80)))
        ('trail
         (emagent-log "… %s" (emagent-log-truncate-line line 72 t)))
        (_ nil)))))

(defun emagent-acp--clear-thought-buffer (state)
  
  "Internal helper for STATE."
  (setf (emagent-acp-state-thought-buffer state) ""))

(defun emagent-acp--flush-thought-buffer (state)
  "Log any trailing thought text for STATE and clear the buffer."
  (when-let ((mode emagent-acp-thought-progress))
    (when-let ((tail (string-trim (or (emagent-acp-state-thought-buffer state) ""))))
      (unless (string-empty-p tail)
        (emagent-acp--log-thought-line mode tail)))
    (emagent-acp--clear-thought-buffer state)))

(defun emagent-acp--thought-chunk (state text)
  "Accumulate thought TEXT for display and optional logging.

Arguments: STATE."
  (unless (string-empty-p text)
    (emagent-acp--detect-external-refusal-in-text state text)
    (setf (emagent-acp-state-thought-text state)
              (concat (or (emagent-acp-state-thought-text state) "") text))
    (when-let ((mode emagent-acp-thought-progress))
      (when (emagent-acp-state-prompt-finishing state)
        (emagent-acp--schedule-prompt-render state))
      (when (memq mode '(buffer both))
        (when-let ((buf (and (emagent-acp--stream-thought-to-buffer-p state)
                             (emagent-acp--chat-buffer state))))
          (with-current-buffer buf
            (when-let ((cb (emagent-acp-state-cb-thought state)))
              (funcall cb text)))))
      (when (memq mode '(minimal trail both))
        (let ((pending (concat (or (emagent-acp-state-thought-buffer state) "") text)))
          (while (string-match "\\`\\(.+?[.!?]\\)\\(?:[[:space:]]\\|\\'\\)" pending)
            (let ((end (match-end 0)))
              (emagent-acp--log-thought-line
               (if (eq mode 'both) 'minimal mode)
               (substring pending 0 end))
              (setq pending (substring pending end))))
          (setf (emagent-acp-state-thought-buffer state) pending))))))

(defun emagent-acp--run-reveal (reveal &optional now)
  
  "Internal helper for REVEAL and NOW."
  (when reveal
    (if now
        (funcall reveal)
      (run-with-idle-timer 0 nil reveal))))

(defun emagent-acp--reveal-buffer (state &optional now)
  "Run the buffer reveal callback for STATE, if any.

When NOW is non-nil, show the buffer immediately for interactive prompts."
  (when-let ((reveal (emagent-acp-state-on-reveal state)))
    (setf (emagent-acp-state-on-reveal state) nil)
    (emagent-acp--run-reveal reveal now)))

(defun emagent-acp--prepare-interactive-context (state)
  "Focus the chat buffer's window before a user prompt, without rearranging.

Selects the chat window only when it is already visible in the selected
frame, so permission shortcuts (y/n/…) work when the user is looking at
the session.  Never pops the buffer into a window or touches other
frames: with several sessions across frames, stealing a window would
flip an unrelated frame to this session's project.  Background prompts
are surfaced by `emagent-chat--notify-inactive-update' instead.

Arguments: STATE."
  (emagent-acp--reveal-buffer state t)
  (when-let* ((buffer (emagent-acp--chat-buffer state))
              (window (get-buffer-window buffer)))
    (select-window window)))

(defun emagent-acp--fail-connect (state message)
  "Show MESSAGE, reveal the chat buffer, and stop connecting.

Arguments: STATE."
  (setf (emagent-acp-state-ready state) nil)
  (emagent-acp--notify-user state message)
  (emagent-acp--reveal-buffer state))

(defun emagent-acp--quota-error-p (message)
  "Return non-nil when MESSAGE is a session/rate/usage quota error."
  (and (stringp message)
       (string-match-p
        (concat "session limit\\|rate limit\\|usage limit\\|spend limit"
                "\\|You've hit your\\|hit your limit\\|out of credits"
                "\\|quota exceeded\\|quota limit")
        message)))

(defun emagent-acp--fatal-agent-error-p (message)
  "Return non-nil when MESSAGE should abort the in-flight prompt.

RetriableError and other transient network failures are excluded: those are
retried by `emagent-acp--schedule-prompt-retry' and must not be double-handled
via stderr subscription (which would clear `:busy' before the retry fires).

Session/rate quota errors are fatal so they surface in the chat buffer even
when they arrive only on agent stderr."
  (and (stringp message)
       (not (string-match-p "RetriableError" message))
       (not (emagent-acp--retriable-prompt-error-p message))
       (or (emagent-acp--quota-error-p message)
           (string-match-p
            "timed out\\|timeout\\|failed with status\\|ApiError\\|API Error\\|\\[31merror"
            message))))

(defun emagent-acp--prompt-retry-pending-p (state)
  "Return non-nil when STATE is waiting to replay a failed prompt."
  (and state
       (emagent-acp-state-prompt-retry-gen state)
       (eq (emagent-acp-state-prompt-retry-gen state)
           (emagent-acp-state-prompt-generation state))
       (emagent-acp-state-busy state)))

(defun emagent-acp--retriable-prompt-error-p (message)
  "Return non-nil when a failed prompt MESSAGE is a transient network error.

Covers Cursor's own RetriableError wrapper and the common DNS/connection
failures underneath it (getaddrinfo ENOTFOUND api2.cursor.sh, connection
resets, timeouts).  These usually recover on a second attempt, so emagent
retries them before surfacing the error (`emagent-acp-prompt-retry-attempts')."
  (and (stringp message)
       (string-match-p
        (concat "RetriableError\\|getaddrinfo\\|ENOTFOUND\\|EAI_AGAIN"
                "\\|ECONNRESET\\|ECONNREFUSED\\|ConnectionRefused"
                "\\|ETIMEDOUT\\|EPIPE"
                "\\|\\[unavailable\\]\\|socket hang up\\|network error"
                "\\|Unable to connect to API")
        message)))

(defun emagent-acp--prompt-retry-delay (attempt)
  "Return backoff seconds to wait before the next retry after ATTEMPT (1-based)."
  (* emagent-acp-prompt-retry-base-delay (expt 2 (max 0 (1- attempt)))))


(defun emagent-acp--abort-prompt (state message)
  "Abort the in-flight prompt for STATE and show MESSAGE.

Quota/session-limit errors are always shown in the chat buffer, even when the
watchdog already finalized a partial Response (busy cleared) while the agent
was still working."
  (setf (emagent-acp-state-prompt-retry-gen state) nil)
  (let ((quiet (emagent-acp-state-quiet-prompt state))
        (in-flight (or (emagent-acp-state-busy state)
                       (emagent-acp-state-prompt-finishing state)))
        (force (and (not (emagent-acp-state-quiet-prompt state))
                    (emagent-acp--quota-error-p message))))
    (when (or in-flight force)
      ;; Do not arm a ScheduleWakeup captured during a failed/aborted turn.
      (emagent-acp--cancel-wakeup state)
      (emagent-acp--cancel-plan-build state)
      (when in-flight
        (emagent-acp--clear-prompt-watchdog state)
        (emagent-acp--cancel-prompt-render state)
        (setf (emagent-acp-state-busy state) nil)
        (setf (emagent-acp-state-prompt-finishing state) nil)
        (setf (emagent-acp-state-prompt-finalized state) nil)
        (setf (emagent-acp-state-assistant-text state) "")
        (setf (emagent-acp-state-compress-pending state) nil)
        (setf (emagent-acp-state-quiet-prompt state) nil)
        (emagent-acp--flush-thought-buffer state))
      (emagent-acp--trace "prompt aborted: %s" message)
      (cond
       (quiet
        (emagent-log "compacted session materialize failed: %s" message))
       (t
        (when-let ((buffer (emagent-acp--chat-buffer state)))
          (with-current-buffer buffer
            (when-let ((cb (emagent-acp-state-cb-fail state)))
              (funcall cb message))))))
      (emagent-acp--refresh-mode-line state))))

(provide 'emagent-acp-prompt)
;;; emagent-acp-prompt.el ends here
