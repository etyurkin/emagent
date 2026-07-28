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
;;
;; Prompt construction/sending and in-flight ACP prompt control.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-protocol)
(require 'emagent-acp-usage)
(require 'emagent-acp-permit)
(require 'emagent-chat)
(require 'emagent-chat-ui)
(require 'emagent-log)
(require 'emagent-mcp)
(require 'emagent-session)

(defun emagent-acp-attach-context (text)
  "Attach TEXT to the next prompt in the current buffer."
  (let ((state (emagent-acp--session)))
    (setf (emagent-acp-state-extra-context state)
              (append (emagent-acp-state-extra-context state) (list text)))))

(defun emagent-acp--image-media-type (ext)
  "Return the MIME type string for image extension EXT, or nil if not an image."
  (pcase (downcase (or ext ""))
    ("png"  "image/png")
    ("jpg"  "image/jpeg")
    ("jpeg" "image/jpeg")
    ("gif"  "image/gif")
    ("webp" "image/webp")
    (_      nil)))

(defun emagent-acp--extract-image-links (text)
  "Extract [[file:...]] image links from TEXT.

Scans for org file links whose paths end in PNG/JPEG/GIF/WebP, reads and
base64-encodes each file, and removes the link from the text.  Non-image
links and unreadable paths are left in place.

Returns (CLEANED-TEXT . IMAGES) where IMAGES is a list of
 ((media-type . TYPE) (data . BASE64)) plists."
  (let ((link-re "\\[\\[file:\\([^]\n]+\\)\\]\\(?:\\[[^]]*\\]\\)?\\]")
        images parts (pos 0))
    (while (string-match link-re text pos)
      (let* ((link-beg (match-beginning 0))
             (link-end (match-end 0))
             (path (match-string 1 text))
             (expanded (expand-file-name path))
             (media-type (emagent-acp--image-media-type
                          (file-name-extension expanded))))
        (push (substring text pos link-beg) parts)
        (if (and media-type (file-readable-p expanded))
            (let ((data (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally expanded)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string))))
              (push `((media-type . ,media-type) (data . ,data)) images))
          (push (substring text link-beg link-end) parts))
        (setq pos link-end)))
    (push (substring text pos) parts)
    (cons (string-trim (apply #'concat (nreverse parts)))
          (nreverse images))))

(defconst emagent-acp--materialize-prompt-text
  (concat "Acknowledge that this compacted session is ready. "
          "Reply with exactly: ready. Do not use tools.")
  "Quiet prompt text that forces the agent to persist a new session.

Cursor ACP creates only meta.json until the first session/prompt; without
this turn, compact then restart fails session/load.")

(defun emagent-acp--materialize-session (state)
  "Send a quiet prompt so STATE's new session is durable across restarts.

Called after /compact creates a fresh session/new.  The reply is not
rendered into the chat buffer."
  (when-let ((session-id (emagent-acp-state-session-id state)))
    (when (and (emagent-acp-state-ready state)
               (not (emagent-acp-state-busy state)))
      (emagent-log "materializing compacted session…")
      (emagent-acp--progress state "materializing compacted session…")
      (setf (emagent-acp-state-quiet-prompt state) t)
      (emagent-acp--turn-begin state)
      (emagent-acp--dispatch-prompt-request
       :state state
       :session-id session-id
       :blocks `[((type . "text")
                  (text . ,emagent-acp--materialize-prompt-text))]
       :images nil
       :gen (emagent-acp-state-prompt-generation state)
       :attempt 1))))

(defun emagent-acp--schedule-prompt-retry (state session-id blocks images gen attempt reason)
  "Re-dispatch the in-flight prompt after exponential backoff.

REASON is a short human-readable phrase describing why the retry fires; it is
shown to the user together with the attempt count.  The GEN guard prevents a
stale retry from firing after the prompt was superseded or interrupted.

Arguments: STATE, SESSION-ID, BLOCKS, IMAGES."
  (let* ((delay (emagent-acp--prompt-retry-delay attempt))
         (next (1+ attempt)))
    (setf (emagent-acp-state-prompt-retry-gen state) gen)
    (emagent-acp--notify-user
     state
     (format "emagent: %s; retrying prompt (%d/%d) in %.1fs"
             reason next emagent-acp-prompt-retry-attempts delay))
    (emagent-acp--schedule-prompt-watchdog state)
    (run-with-timer
     delay nil
     (lambda ()
       (setf (emagent-acp-state-prompt-retry-gen state) nil)
       (if (and (eq (emagent-acp-state-prompt-generation state) gen)
                (emagent-acp-state-busy state))
           (emagent-acp--dispatch-prompt-request
            :state state :session-id session-id
            :blocks blocks :images images
            :gen gen :attempt next)
         (emagent-log "emagent: prompt retry skipped (busy=%s gen=%s/%s)"
                      (if (emagent-acp-state-busy state) "yes" "no")
                      (emagent-acp-state-prompt-generation state)
                      gen))))))

(defun emagent-acp--log-transient-error (state &optional message)
  "Log MESSAGE and STATE's partial assistant output to `emagent-log-buffer-name'.

Used when a transient error ends an in-flight turn: the details are recorded in
the log instead of the chat buffer, and the turn is then resumed with
\"continue\" (see `emagent-acp--schedule-continue')."
  (when (and message (not (string-empty-p message)))
    (emagent-log "transient error: %s" message))
  (let ((text (string-trim (or (emagent-acp-state-assistant-text state) ""))))
    (unless (string-empty-p text)
      (emagent-log "partial output before auto-continue:\n%s" text))))

(defun emagent-acp--schedule-continue (state session-id images gen reason)
  "Resume an errored in-flight turn by re-dispatching a \"continue\" prompt.

Unlike `emagent-acp--schedule-prompt-retry' (which replays the ORIGINAL prompt
and is only safe when the turn did no work), this sends a fresh \"continue\"
turn so tool side effects such as commits or pushes are never repeated.  The
open response block is kept, so the continued output renders into it; the
transient error itself is only logged (see `emagent-acp--log-transient-error'),
never rendered into the chat buffer.  REASON is logged with the attempt count;
the `:continue-attempts' counter bounds the number of resumes and the GEN guard
cancels a stale resume after an interrupt or new prompt.

Arguments: STATE, SESSION-ID, IMAGES."
  (let* ((attempt (1+ (or (emagent-acp-state-continue-attempts state) 0)))
         (delay (emagent-acp--prompt-retry-delay attempt)))
    (setf (emagent-acp-state-continue-attempts state) attempt)
    (emagent-acp--notify-user
     state
     (format "emagent: %s; auto-continuing (%d/%d) in %.1fs"
             reason attempt emagent-acp-prompt-retry-attempts delay))
    (emagent-acp--schedule-prompt-watchdog state)
    (run-with-timer
     delay nil
     (lambda ()
       (when (and (eq (emagent-acp-state-prompt-generation state) gen)
                  (emagent-acp-state-busy state))
         (emagent-acp--dispatch-prompt-request
          :state state :session-id session-id
          :blocks [((type . "text") (text . "continue"))]
          :images images
          :gen gen :attempt 1))))))

(cl-defun emagent-acp--dispatch-prompt-request (&key state session-id blocks images gen attempt)
  "Send the session/prompt request, recovering from transient network failures.

ATTEMPT is the 1-based try count.  Recovery depends on how the failure arrives
and whether the turn already did work:

- Pure transient failure with no tool calls or content
  (`emagent-acp--agent-error-only-response-p' /
  `emagent-acp--turn-did-no-work-p') is replayed with exponential backoff up to
  `emagent-acp-prompt-retry-attempts' via `emagent-acp--schedule-prompt-retry'.

- A turn that already ran tool calls or produced content but ended on a
  transient error (`emagent-acp--turn-hit-transient-error-p') is resumed by
  auto-sending \"continue\" via `emagent-acp--schedule-continue', so side
  effects such as commits or pushes are never repeated.  The error is logged to
  `emagent-log-buffer-name' rather than rendered into the chat buffer.

GEN guards against a stale retry firing after the prompt was superseded or
interrupted.

Arguments: STATE, SESSION-ID, BLOCKS, IMAGES."
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-prompt-request
             :session-id session-id :prompt blocks :images images)
   :on-success
   (lambda (response)
     (when (eq (emagent-acp-state-prompt-generation state) gen)
       (cond
        ((and (emagent-acp-state-busy state)
              (< attempt emagent-acp-prompt-retry-attempts)
              (emagent-acp--agent-error-only-response-p state))
         (let ((message (string-trim (or (emagent-acp-state-assistant-text state) ""))))
           (setf (emagent-acp-state-assistant-text state) "")
           (setf (emagent-acp-state-thought-text state) "")
           (emagent-acp--clear-thought-buffer state)
           (emagent-acp--cancel-prompt-render state)
           (emagent-acp--schedule-prompt-retry
            state session-id blocks images gen attempt
            (format "agent returned a transient error (%s)" message))))
        ((and (emagent-acp-state-busy state)
              (< (or (emagent-acp-state-continue-attempts state) 0)
                 emagent-acp-prompt-retry-attempts)
              (emagent-acp--turn-hit-transient-error-p state))
         (emagent-acp--log-transient-error state)
         (setf (emagent-acp-state-assistant-text state) "")
         (setf (emagent-acp-state-thought-text state) "")
         (emagent-acp--clear-thought-buffer state)
         (emagent-acp--cancel-prompt-render state)
         (emagent-acp--schedule-continue
          state session-id images gen "agent turn ended on a transient error"))
        (t
         (emagent-acp--complete-prompt state response)))))
   :on-failure
   (lambda (error _raw)
     (when (eq (emagent-acp-state-prompt-generation state) gen)
       (let ((message (or (map-elt error 'message) (format "%s" error))))
         (cond
          ((and (emagent-acp-state-busy state)
                (< attempt emagent-acp-prompt-retry-attempts)
                (emagent-acp--retriable-prompt-error-p message)
                (emagent-acp--turn-did-no-work-p state))
           (emagent-acp--schedule-prompt-retry
            state session-id blocks images gen attempt
            (format "prompt failed (%s)" message)))
          ((and (emagent-acp-state-busy state)
                (emagent-acp--retriable-prompt-error-p message)
                (< (or (emagent-acp-state-continue-attempts state) 0)
                   emagent-acp-prompt-retry-attempts))
           (emagent-acp--log-transient-error state message)
           (setf (emagent-acp-state-assistant-text state) "")
           (setf (emagent-acp-state-thought-text state) "")
           (emagent-acp--clear-thought-buffer state)
           (emagent-acp--cancel-prompt-render state)
           (emagent-acp--schedule-continue
            state session-id images gen (format "prompt interrupted (%s)" message)))
          (t
           (emagent-acp--abort-prompt state (format "prompt failed: %s" message))
           (emagent-acp--notify-user
            state (format "emagent: prompt failed: %s" message)))))))))

(defun emagent-acp--reset-permission-gate (state)
  "Cancel STATE's pending permission drain and clear the permission gate.
Replies `cancelled' to any outstanding requests so the agent does not hang.
Shared by the two turn-boundary owners (`--turn-begin' and finalize)."
  (when-let ((timer (emagent-acp-state-permission-drain-timer state)))
    (cancel-timer timer)
    (setf (emagent-acp-state-permission-drain-timer state) nil))
  (emagent-acp--cancel-outstanding-permissions state)
  (setf (emagent-acp-state-permission-busy state) nil)
  (setf (emagent-acp-state-deferred-complete-response state) nil))

(defun emagent-acp--turn-begin (state)
  "Enter the streaming phase of a new turn for STATE.

Mints a fresh turn generation (so a late response from a previous turn fails
the GEN guard instead of finalizing this one) and resets all turn-scoped state:
resume budget, streamed text, finalize flags, the tool-call display tables, the
provider tool-resolve queue, and any outstanding permission requests.  This is
the single entry point for turn start; the terminal paths (`--complete-prompt',
`--abort-prompt', `--finalize-in-flight-prompt') own turn end."
  (setf (emagent-acp-state-busy state) t)
  (setf (emagent-acp-state-prompt-generation state) (1+ (or (emagent-acp-state-prompt-generation state) 0)))
  (setf (emagent-acp-state-continue-attempts state) 0)
  (setf (emagent-acp-state-assistant-text state) "")
  (setf (emagent-acp-state-thought-text state) "")
  (setf (emagent-acp-state-prompt-finalized state) nil)
  (setf (emagent-acp-state-prompt-finishing state) nil)
  (clrhash (emagent-acp-state-tool-call-titles state))
  (clrhash (emagent-acp-state-tool-call-inputs state))
  (clrhash (emagent-acp-state-tool-call-labels state))
  (clrhash (emagent-acp-state-tool-call-decisions state))
  (clrhash (emagent-acp-state-tool-call-pending state))
  ;; A new turn supersedes any agent-scheduled wakeup: a stale request must
  ;; not arm after an unrelated prompt, and a pending timer must not fire
  ;; into the middle of this turn's conversation.
  (emagent-acp--cancel-wakeup state)
  (emagent-acp--cancel-plan-build state)
  (emagent-acp--provider-reset-tool-resolve state)
  (emagent-acp--reset-permission-gate state)
  (emagent-acp--cancel-prompt-render state)
  (emagent-acp--clear-thought-buffer state)
  (emagent-acp--schedule-prompt-watchdog state)
  (unless (emagent-acp-state-quiet-prompt state)
    (when (fboundp 'emagent-chat--send-pending-end)
      (when-let ((buf (emagent-acp--chat-buffer state)))
        (with-current-buffer buf
          (emagent-chat--send-pending-end))))
    (when (fboundp 'emagent-chat--promote-transient-to-thinking)
      (when-let ((buf (emagent-acp--chat-buffer state)))
        (with-current-buffer buf
          (emagent-chat--promote-transient-to-thinking)))))
  ;; Push the now-busy status; the mode line starts the spinner from it.
  (emagent-acp--refresh-mode-line state))

(cl-defun emagent-acp-send-prompt (user-text &optional compress)
  "Send USER-TEXT to the current buffer's ACP session.

When COMPRESS is non-nil, USER-TEXT is already a compression summary prompt
assembled by `emagent-chat--dispatch-compress': context injection is skipped
and the turn is marked so `emagent-acp--render-prompt-response' resets the
session with the summary once it finishes.  MCP and /compress detection live
in the chat send path (`emagent-chat-send'); by the time a prompt reaches
here it is always the final text to dispatch."
  (let* ((state (emagent-acp--session))
         (session-id (emagent-acp-state-session-id state)))
    (unless (emagent-acp-state-ready state)
      (user-error "Emagent is still connecting"))
    (when (emagent-acp-state-busy state)
      (user-error "Emagent is busy"))
    (setq user-text (emagent-acp--provider-normalize-slash-prompt state user-text))
    (when compress
      (setf (emagent-acp-state-compress-pending state) t))
    (let* ((slash-command-p (and (not compress) (emagent-chat--bare-slash-command-p user-text)))
           (extra (emagent-acp-state-extra-context state))
           (full-prompt (if (or slash-command-p compress)
                            user-text
                          (emagent-context-build-prompt user-text extra)))
           (extracted (emagent-acp--extract-image-links
                       (substring-no-properties full-prompt)))
           (clean-text (car extracted))
           (images (cdr extracted))
           (blocks `[((type . "text") (text . ,clean-text))]))
      (setf (emagent-acp-state-extra-context state) nil)
      (cond
       (compress (emagent-log "compressing conversation"))
       (slash-command-p (emagent-log "send slash command: %s" user-text)))
      (emagent-log "dispatch prompt (%d chars)" (length clean-text))
      (emagent-acp--turn-begin state)
      (emagent-acp--dispatch-prompt-request
       :state state :session-id session-id
       :blocks blocks :images images
       :gen (emagent-acp-state-prompt-generation state) :attempt 1))))

(cl-defun emagent-acp--finalize-in-flight-prompt (&optional stop-notice)
  "Finalize the in-flight prompt and cancel it on the agent side.

When STOP-NOTICE is non-nil, append it to any partial assistant text
before closing the response block.  Returns non-nil when a prompt was
finalized."
  (let ((state emagent-acp--session))
    (unless (and state
                 (or (emagent-acp-state-busy state)
                     (emagent-acp-state-prompt-finishing state)))
      (cl-return-from emagent-acp--finalize-in-flight-prompt nil))
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    ;; Interrupt/stop must not leave a ScheduleWakeup to arm later.
    (emagent-acp--cancel-wakeup state)
    (emagent-acp--cancel-plan-build state)
    (emagent-acp--flush-thought-buffer state)
    (when (and stop-notice (not (string-empty-p stop-notice)))
      (let* ((text (or (emagent-acp-state-assistant-text state) ""))
             (full (if (string-empty-p text)
                       stop-notice
                     (concat text "\n\n" stop-notice))))
        (setf (emagent-acp-state-assistant-text state) full)))
    (setf (emagent-acp-state-prompt-generation state) (1+ (or (emagent-acp-state-prompt-generation state) 0)))
    (when-let ((client (emagent-acp-state-client state))
               (session-id (emagent-acp-state-session-id state)))
      (ignore-errors
        (emagent-acp-send-notification
         :client client
         :notification (emagent-acp-make-session-cancel-notification
                        :session-id session-id))))
    (emagent-acp--reset-permission-gate state)
    (setf (emagent-acp-state-busy state) nil)
    (setf (emagent-acp-state-prompt-finishing state) t)
    (setf (emagent-acp-state-prompt-finalized state) nil)
    (emagent-acp--render-prompt-response state)
    (emagent-acp--refresh-mode-line state)
    t))

(defun emagent-acp-interrupt ()
  "Interrupt the in-flight prompt and close the response block cleanly.

Appends a user-visible stop notice to whatever the agent has produced so far,
then finalizes the response as if it completed normally.  The pending ACP
request continues in the background but its result is ignored."
  (interactive)
  (if (emagent-acp--finalize-in-flight-prompt
       "/Stopped — awaiting new instructions./")
      (message "emagent: interrupted")
    (user-error "No active emagent prompt to interrupt")))

(defun emagent-acp-shutdown-buffer ()
  "Shut down the ACP session for the current buffer."
  (emagent-chat-clear-slash-commands)
  (when emagent-mcp--token
    (emagent-mcp-deregister-session emagent-mcp--token))
  (when-let ((state emagent-acp--session))
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    (emagent-acp--cancel-state-timers state)
    (when-let ((client (emagent-acp-state-client state)))
      (emagent-acp-shutdown :client client))
    (setq emagent-acp--session nil)))

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
       ((not (fboundp 'emagent-chat--insert-user-heading-with-text))
        (emagent-log "wakeup: skipped — chat send unavailable"))
       (t
        (emagent-log "wakeup: %s" (emagent-log-truncate-line text 80))
        (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
          (emagent-chat--begin-response response-pos))
        (emagent-chat--ensure-follow-window buffer)
        ;; emagent-acp-send drops the turn unless a send token is armed
        ;; (manual C-c C-c calls send-pending-begin; Build/wakeup must too).
        (emagent-chat--send-pending-begin)
        (unless (fboundp 'emagent-acp-send)
          (require 'emagent-acp))
        (emagent-acp-send text))))))

(defun emagent-acp--set-session-mode (state mode-id)
  "Best-effort `session/set_mode' to MODE-ID for STATE."
  (when-let ((session-id (emagent-acp-state-session-id state)))
    (unless (fboundp 'emagent-acp--send-request)
      (require 'emagent-acp-usage))
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
       (t
        (emagent-log "plan-build: %s" (emagent-log-truncate-line text 80))
        ;; Build owns the next turn; allow a normal stub after it finishes.
        (setq emagent-chat--defer-user-stub nil)
        (emagent-chat--begin-response (emagent-chat--user-zone-start))
        (emagent-chat--ensure-follow-window buffer)
        (emagent-chat--send-pending-begin)
        (unless (fboundp 'emagent-acp-send)
          (require 'emagent-acp))
        (emagent-acp-send text))))))

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
