;;; emagent-acp-prompt.el --- Prompt lifecycle for emagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

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

(declare-function emagent-acp-send-prompt "emagent-acp-send")
(declare-function emagent-acp--on-tool-call "emagent-acp-tool-call")
(declare-function emagent-acp--abort-compress-empty "emagent-acp-send")
(declare-function emagent-acp--save-config-options "emagent-acp-model")
(declare-function emagent-acp--current-model-id "emagent-acp-model")
(declare-function emagent-acp--persist-model-id "emagent-acp-usage")
(declare-function emagent-acp--update-usage-from-notification "emagent-acp-usage")
(declare-function emagent-acp--detect-external-refusal-in-text "emagent-acp-gate")
(declare-function emagent-acp--chat-buffer "emagent-acp-usage")
(declare-function emagent-chat-set-slash-commands "emagent-chat-slash")
(declare-function emagent-chat--refresh-mode-line "emagent-chat-mode-line")
(declare-function emagent-chat--spinner-start "emagent-chat-mode-line")

(defun emagent-acp--notify-user (_state message)
  "Append MESSAGE to `emagent-log-buffer-name'."
  (emagent-log "%s" message))

(defun emagent-acp--trace (format-string &rest args)
  "Append a trace line when `emagent-acp-trace' is non-nil."
  (when emagent-acp-trace
    (apply #'emagent-log (cons (concat "acp: " format-string) args))))

(defun emagent-acp--progress (state message)
  "Show init stage MESSAGE in the minibuffer and refresh the mode line."
  (emagent-acp--notify-user state (format "emagent: %s" message))
  (emagent-acp--refresh-mode-line state))

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
timer (token-guarded no-ops that still pin STATE for the whole timeout)."
  (when-let ((old (emagent-acp-state-prompt-watchdog-timer state)))
    (cancel-timer old))
  (let* ((token (cl-gensym "emagent-prompt-watchdog"))
         (timer (run-with-timer
     emagent-acp-watchdog-timeout nil
     (lambda ()
       (when (and (eq (emagent-acp-state-prompt-watchdog state) token)
                  (emagent-acp-state-busy state))
         (let* ((client (emagent-acp-state-client state))
                (pending (and client (map-elt client :pending-requests))))
           (emagent-log "emagent: prompt stalled (no ACP completion in %ds)"
                        emagent-acp-watchdog-timeout)
           (when pending
             (emagent-log "emagent: pending ACP request count: %d"
                          (length pending)))
           (if (and (emagent-acp-state-assistant-text state)
                    (not (string-empty-p (emagent-acp-state-assistant-text state))))
               (progn
                 (emagent-log "emagent: prompt stalled; finalizing partial response")
                 (emagent-acp--complete-prompt state nil))
             (emagent-acp--abort-prompt
              state
              "prompt stalled — reconnect with M-x emagent-mode or kill and reopen the buffer"))))))))
  (setf (emagent-acp-state-prompt-watchdog state) token)
  (setf (emagent-acp-state-prompt-watchdog-timer state) timer)))

(defun emagent-acp--stream-to-buffer-p (state)
  "Return non-nil when agent chunks may update the chat buffer live."
  (and emagent-acp-stream-to-buffer
       (emagent-acp-state-busy state)
       (not (emagent-acp-state-compress-pending state))
       (not (emagent-acp-state-prompt-finalized state))
       (not (emagent-acp-state-prompt-finishing state))))

(defun emagent-acp--stream-thought-to-buffer-p (state)
  "Return non-nil when reasoning may stream into the chat buffer live."
  (and (memq emagent-acp-thought-progress '(buffer both))
       (emagent-acp-state-busy state)
       (not (emagent-acp-state-compress-pending state))
       (not (emagent-acp-state-prompt-finalized state))
       (not (emagent-acp-state-prompt-finishing state))))

(defun emagent-acp--cancel-prompt-render (state)
  "Cancel a pending debounced render for STATE."
  (when-let ((timer (emagent-acp-state-finish-timer state)))
    (cancel-timer timer))
  (setf (emagent-acp-state-finish-timer state) nil)
  (setf (emagent-acp-state-finish-token state) nil))

(defun emagent-acp--schedule-prompt-render (state)
  "Debounced render of the accumulated prompt into the chat buffer."
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
  "Render accumulated prompt text into the chat buffer for STATE."
  (when (emagent-acp-state-prompt-finishing state)
    (when-let ((buffer (emagent-acp--chat-buffer state)))
      (if (emagent-acp-state-compress-pending state)
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
              (emagent-acp--new-session :state state :compressed-context summary)))
        (condition-case err
            (with-current-buffer buffer
              (when-let ((cb (emagent-acp-state-cb-finish state)))
                (funcall cb (emagent-acp-state-assistant-text state)
                         (emagent-acp-state-thought-text state))))
          (error
           (emagent-log "emagent: finish failed: %s" (error-message-string err))
           (with-current-buffer buffer
             (when-let ((cb (emagent-acp-state-cb-fail state)))
               (funcall cb (format "response finalize failed: %s"
                                   (error-message-string err)))))))))
    (setf (emagent-acp-state-prompt-finishing state) nil)
    (setf (emagent-acp-state-prompt-finalized state) t)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp--permission-pending-p (state)
  "Return non-nil when STATE has unanswered permission requests."
  (or (emagent-acp-state-permission-busy state)
      (emagent-acp-state-permission-queue state)))

(defun emagent-acp--maybe-complete-deferred-prompt (state)
  "Run a deferred `emagent-acp--complete-prompt' when permissions are clear."
  (when-let ((response (emagent-acp-state-deferred-complete-response state)))
    (unless (emagent-acp--permission-pending-p state)
      (setf (emagent-acp-state-deferred-complete-response state) nil)
      (emagent-acp--complete-prompt state response))))

(defun emagent-acp--complete-prompt (state response)
  "Finalize the in-flight prompt for STATE and close the chat response."
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
    (emagent-acp--schedule-prompt-render state))))

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
  (setf (emagent-acp-state-thought-buffer state) ""))

(defun emagent-acp--flush-thought-buffer (state)
  "Log any trailing thought text for STATE and clear the buffer."
  (when-let ((mode emagent-acp-thought-progress))
    (when-let ((tail (string-trim (or (emagent-acp-state-thought-buffer state) ""))))
      (unless (string-empty-p tail)
        (emagent-acp--log-thought-line mode tail)))
    (emagent-acp--clear-thought-buffer state)))

(defun emagent-acp--thought-chunk (state text)
  "Accumulate thought TEXT for display and optional logging."
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
are surfaced by `emagent-chat--notify-inactive-update' instead."
  (emagent-acp--reveal-buffer state t)
  (when-let* ((buffer (emagent-acp--chat-buffer state))
              (window (get-buffer-window buffer)))
    (select-window window)))

(defun emagent-acp--fail-connect (state message)
  "Show MESSAGE, reveal the chat buffer, and stop connecting."
  (setf (emagent-acp-state-ready state) nil)
  (emagent-acp--notify-user state message)
  (emagent-acp--reveal-buffer state))

(defun emagent-acp--fatal-agent-error-p (message)
  "Return non-nil when MESSAGE should abort the in-flight prompt.

RetriableError and other transient network failures are excluded: those are
retried by `emagent-acp--schedule-prompt-retry' and must not be double-handled
via stderr subscription (which would clear `:busy' before the retry fires)."
  (and (stringp message)
       (not (string-match-p "RetriableError" message))
       (not (emagent-acp--retriable-prompt-error-p message))
       (string-match-p "timed out\\|timeout\\|failed with status\\|ApiError\\|API Error\\|\\[31merror"
                       message)))

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
  "Return non-nil when STATE's turn ran no tool calls and produced little text.
Such a turn has no side effects, so replaying its prompt is safe."
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
         (string-match-p emagent-acp--agent-error-signature-re text))))

(defun emagent-acp--abort-prompt (state message)
  "Abort the in-flight prompt for STATE and show MESSAGE."
  (setf (emagent-acp-state-prompt-retry-gen state) nil)
  (when (or (emagent-acp-state-busy state) (emagent-acp-state-prompt-finishing state))
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    (setf (emagent-acp-state-busy state) nil)
    (setf (emagent-acp-state-prompt-finishing state) nil)
    (setf (emagent-acp-state-prompt-finalized state) nil)
    (setf (emagent-acp-state-assistant-text state) "")
    (setf (emagent-acp-state-compress-pending state) nil)
    (emagent-acp--trace "prompt aborted: %s" message)
    (emagent-acp--flush-thought-buffer state)
    (when-let ((buffer (emagent-acp--chat-buffer state)))
      (with-current-buffer buffer
        (when-let ((cb (emagent-acp-state-cb-fail state)))
          (funcall cb message))))
    (emagent-acp--refresh-mode-line state)))

(cl-defun emagent-acp--send-request (&key state request on-success on-failure)
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

(provide 'emagent-acp-prompt)
;;; emagent-acp-prompt.el ends here
