;;; emagent-acp-send.el --- ACP send module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin
(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-provider)
(require 'emagent-acp-protocol)

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Code:
(defun emagent-acp-attach-context (text)
  "Attach TEXT to the next prompt in the current buffer."
  (let ((state (emagent-acp--session)))
    (map-put! state :extra-context
              (append (or (map-elt state :extra-context) nil) (list text)))))

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
  "Close an empty /compress request with an error in the chat buffer."
  (emagent-acp--clear-prompt-watchdog state)
  (emagent-acp--cancel-prompt-render state)
  (map-put! state :busy nil)
  (map-put! state :assistant-text "")
  (map-put! state :thought-text "")
  (map-put! state :prompt-finalized t)
  (map-put! state :prompt-finishing nil)
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (when-let ((cb (map-elt state :cb-fail)))
        (funcall cb "No conversation to compress"))))
  (emagent-acp--refresh-mode-line state))

(cl-defun emagent-acp--dispatch-prompt-request (&key state session-id blocks images gen attempt)
  "Send the session/prompt request, retrying transient network failures.

ATTEMPT is the 1-based try count.  A failure whose message matches
`emagent-acp--retriable-prompt-error-p' is retried with exponential
backoff until `emagent-acp-prompt-retry-attempts' is reached; only then is
the error surfaced via `emagent-acp--abort-prompt'.  GEN guards against a
stale retry firing after the prompt was superseded or interrupted."
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-prompt-request
             :session-id session-id :prompt blocks :images images)
   :on-success
   (lambda (response)
     (when (eq (map-elt state :prompt-generation) gen)
       (emagent-acp--complete-prompt state response)))
   :on-failure
   (lambda (error _raw)
     (when (eq (map-elt state :prompt-generation) gen)
       (let ((message (or (map-elt error 'message) (format "%s" error))))
         (if (and (map-elt state :busy)
                  (< attempt emagent-acp-prompt-retry-attempts)
                  (emagent-acp--retriable-prompt-error-p message))
             (let ((delay (emagent-acp--prompt-retry-delay attempt)))
               (emagent-acp--notify-user
                state
                (format "emagent: prompt failed (%s); retrying (%d/%d) in %.1fs"
                        message attempt emagent-acp-prompt-retry-attempts delay))
               (emagent-acp--schedule-prompt-watchdog state)
               (run-with-timer
                delay nil
                (lambda ()
                  (when (and (eq (map-elt state :prompt-generation) gen)
                             (map-elt state :busy))
                    (emagent-acp--dispatch-prompt-request
                     :state state :session-id session-id
                     :blocks blocks :images images
                     :gen gen :attempt (1+ attempt))))))
           (emagent-acp--abort-prompt state (format "prompt failed: %s" message))
           (emagent-acp--notify-user
            state (format "emagent: prompt failed: %s" message))))))))

(cl-defun emagent-acp-send-prompt (user-text)
  "Send USER-TEXT to the current buffer's ACP session."
  (let* ((state (emagent-acp--session))
         (session-id (map-elt state :session-id)))
    (unless (map-elt state :ready)
      (user-error "Emagent is still connecting"))
    (when (map-elt state :busy)
      (user-error "Emagent is busy"))
    (setq user-text (emagent-acp--provider-normalize-slash-prompt state user-text))
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
            (map-put! state :compress-pending t)
            (setq slash-command-p nil))))
      (let* ((extra (map-elt state :extra-context))
             (full-prompt (if (or slash-command-p (map-elt state :compress-pending))
                              user-text
                            (emagent-context-build-prompt user-text extra)))
             (extracted (emagent-acp--extract-image-links
                         (substring-no-properties full-prompt)))
             (clean-text (car extracted))
             (images (cdr extracted))
             (blocks `[((type . "text") (text . ,clean-text))]))
        (map-put! state :extra-context nil)
        (cond
         ((map-elt state :compress-pending)
          (emagent-log "compressing conversation"))
         (slash-command-p
          (emagent-log "send slash command: %s" user-text)))
      (map-put! state :busy t)
      (when (fboundp 'emagent-chat--spinner-start)
        (emagent-chat--spinner-start))
      (map-put! state :assistant-text "")
      (map-put! state :thought-text "")
      (map-put! state :prompt-finalized nil)
      (map-put! state :prompt-finishing nil)
      (clrhash (map-elt state :tool-call-titles))
      (clrhash (map-elt state :tool-call-inputs))
      (clrhash (map-elt state :tool-call-labels))
      (clrhash (map-elt state :tool-call-decisions))
      (clrhash (map-elt state :tool-call-pending))
      (emagent-acp--provider-reset-tool-resolve state)
      (when-let ((timer (map-elt state :permission-drain-timer)))
        (cancel-timer timer)
        (map-put! state :permission-drain-timer nil))
      (map-put! state :permission-queue nil)
      (map-put! state :permission-busy nil)
      (map-put! state :deferred-complete-response nil)
      (emagent-acp--cancel-prompt-render state)
      (emagent-acp--clear-thought-buffer state)
      (emagent-acp--schedule-prompt-watchdog state)
      (emagent-acp--dispatch-prompt-request
       :state state :session-id session-id
       :blocks blocks :images images
       :gen (map-elt state :prompt-generation) :attempt 1)))))

(defun emagent-acp--finalize-in-flight-prompt (&optional stop-notice)
  "Finalize the in-flight prompt and cancel it on the agent side.

When STOP-NOTICE is non-nil, append it to any partial assistant text
before closing the response block.  Returns non-nil when a prompt was
finalized."
  (let ((state emagent-acp--session))
    (unless (or (map-elt state :busy) (map-elt state :prompt-finishing))
      (cl-return-from emagent-acp--finalize-in-flight-prompt nil))
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    (emagent-acp--flush-thought-buffer state)
    (when (and stop-notice (not (string-empty-p stop-notice)))
      (let* ((text (or (map-elt state :assistant-text) ""))
             (full (if (string-empty-p text)
                       stop-notice
                     (concat text "\n\n" stop-notice))))
        (map-put! state :assistant-text full)))
    (map-put! state :prompt-generation (1+ (or (map-elt state :prompt-generation) 0)))
    (when-let ((client (map-elt state :client))
               (session-id (map-elt state :session-id)))
      (ignore-errors
        (emagent-acp-send-notification
         :client client
         :notification (emagent-acp-make-session-cancel-notification
                        :session-id session-id))))
    (when-let ((timer (map-elt state :permission-drain-timer)))
      (cancel-timer timer)
      (map-put! state :permission-drain-timer nil))
    (map-put! state :permission-queue nil)
    (map-put! state :permission-busy nil)
    (map-put! state :deferred-complete-response nil)
    (map-put! state :busy nil)
    (map-put! state :prompt-finishing t)
    (map-put! state :prompt-finalized nil)
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
    (emagent-acp--stop-rss-timer state)
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    (when-let ((client (map-elt state :client)))
      (emagent-acp-shutdown :client client))
    (setq emagent-acp--session nil)))


(provide 'emagent-acp-send)
;;; emagent-acp-send.el ends here
