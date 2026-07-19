;;; emagent-acp-send.el --- ACP send module  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6

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

;; Send prompts and manage in-flight ACP requests.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-provider)
(require 'emagent-acp-protocol)
(require 'emagent-chat-compress)
(require 'emagent-chat-mcp)

;; Defined in emagent-acp-prompt.el, which declares this file's functions the
;; same way; declaring here avoids a require cycle between the two modules.
(declare-function emagent-acp--cancel-outstanding-permissions "emagent-acp-request")
(declare-function emagent-acp--refresh-mode-line "emagent-acp-usage")
(declare-function emagent-acp--agent-error-only-response-p "emagent-acp-prompt")
(declare-function emagent-acp--turn-hit-transient-error-p "emagent-acp-prompt")
(declare-function emagent-acp--turn-did-no-work-p "emagent-acp-prompt")
(declare-function emagent-chat-begin-thought "emagent-chat-thought")
(declare-function emagent-chat--open-response-p "emagent-chat")
(declare-function emagent-chat--send-pending-end "emagent-chat")
(declare-function emagent-chat--promote-transient-to-thinking "emagent-chat-reasoning")
(declare-function emagent-acp--chat-buffer "emagent-acp-usage")

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

(defun emagent-acp--abort-compress-empty (state)
  "Close an empty /compress request with an error in the chat buffer.

Arguments: STATE."
  (emagent-acp--clear-prompt-watchdog state)
  (emagent-acp--cancel-prompt-render state)
  (setf (emagent-acp-state-busy state) nil)
  (setf (emagent-acp-state-assistant-text state) "")
  (setf (emagent-acp-state-thought-text state) "")
  (setf (emagent-acp-state-prompt-finalized state) t)
  (setf (emagent-acp-state-prompt-finishing state) nil)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (when-let ((cb (emagent-acp-state-cb-fail state)))
        (funcall cb "No conversation to compress"))))
  (emagent-acp--refresh-mode-line state))

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

(cl-defun emagent-acp-send-prompt (user-text)
  "Send USER-TEXT to the current buffer's ACP session."
  (let* ((state (emagent-acp--session))
         (session-id (emagent-acp-state-session-id state)))
    (unless (emagent-acp-state-ready state)
      (user-error "Emagent is still connecting"))
    (when (emagent-acp-state-busy state)
      (user-error "Emagent is busy"))
    (setq user-text (emagent-acp--provider-normalize-slash-prompt state user-text))
    ;; Safety net if `/mcp' reaches ACP send (primary intercept is chat-send).
    (when (emagent-chat--mcp-command-p user-text)
      (emagent-chat--slash-mcp-apply user-text)
      (cl-return-from emagent-acp-send-prompt))
    (let ((slash-command-p (emagent-chat--bare-slash-command-p user-text)))
      (when (and slash-command-p
                 (fboundp 'emagent-chat--compress-command-p)
                 (emagent-chat--compress-command-p user-text))
        (let ((history (with-current-buffer (emagent-acp--chat-buffer state)
                         (emagent-chat--conversation-history-text))))
          (if (string-empty-p history)
              (progn
                (emagent-acp--abort-compress-empty state)
                (cl-return-from emagent-acp-send-prompt))
            (setq user-text (emagent-chat--compress-prompt-text history))
            (setf (emagent-acp-state-compress-pending state) t)
            (setq slash-command-p nil))))
      (let* ((extra (emagent-acp-state-extra-context state))
             (full-prompt (if (or slash-command-p (emagent-acp-state-compress-pending state))
                              user-text
                            (emagent-context-build-prompt user-text extra)))
             (extracted (emagent-acp--extract-image-links
                         (substring-no-properties full-prompt)))
             (clean-text (car extracted))
             (images (cdr extracted))
             (blocks `[((type . "text") (text . ,clean-text))]))
        (setf (emagent-acp-state-extra-context state) nil)
        (cond
         ((emagent-acp-state-compress-pending state)
          (emagent-log "compressing conversation"))
         (slash-command-p
          (emagent-log "send slash command: %s" user-text)))
        (emagent-log "dispatch prompt (%d chars)" (length clean-text))
        (emagent-acp--turn-begin state)
        (emagent-acp--dispatch-prompt-request
         :state state :session-id session-id
         :blocks blocks :images images
         :gen (emagent-acp-state-prompt-generation state) :attempt 1)))))

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


(provide 'emagent-acp-send)
;;; emagent-acp-send.el ends here
