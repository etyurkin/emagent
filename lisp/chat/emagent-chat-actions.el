;;; emagent-chat-actions.el --- actions module  -*- lexical-binding: t; -*-

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

;; Interactive chat actions (copy, retry, approve, etc.).

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'map)
(require 'emagent-log)
(require 'emagent-chat-header)
(require 'emagent-chat-markup)
(require 'emagent-chat-render)
(require 'emagent-chat-mode-line)
(require 'emagent-chat-input)
(require 'emagent-chat-response)
(require 'emagent-chat-reasoning)
(require 'emagent-chat-compress)
(require 'emagent-acp-usage)
(require 'emagent-acp-state)
(require 'emagent-chat-send-state)
(require 'emagent-chat-response-state)
(require 'emagent-chat-model-ui)
(require 'emagent-chat-mcp)

(defun emagent-chat--acp-send (text &optional compress)
  "Send TEXT via `emagent-acp-send', loading connect on first use.
Optional COMPRESS is forwarded for `/compress' session reset."
  (unless (fboundp 'emagent-acp-send)
    (require 'emagent-acp-connect))
  (emagent-acp-send text compress))

(defun emagent-chat--acp-quit ()
  "Shut down this buffer's ACP session, loading send on first use."
  (unless (fboundp 'emagent-acp-shutdown-buffer)
    (require 'emagent-acp-send))
  (emagent-acp-shutdown-buffer))

(defun emagent-chat--operation-active-p ()
  "Return non-nil when the buffer has work Esc-Esc should stop."
  (or emagent-chat--send-pending
      (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))))

(defun emagent-chat--abort-open-response ()
  "Delete the in-flight response scaffold opened before dispatch."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (when (emagent-chat--open-response-p)
      (when-let* ((beg (emagent-chat--open-response-begin))
                  (end (emagent-chat--response-region-end beg)))
        (save-excursion
          (goto-char beg)
          (while (and (> (point) (point-min))
                      (progn (forward-line -1)
                             (string-empty-p
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))))
            (setq beg (line-beginning-position)))
          (delete-region beg end)))))
  (emagent-chat--reset-response-state)
  (emagent-chat--sync-user-zone-marker))

(defun emagent-chat--stop-operation ()
  "Stop any in-flight emagent work in the current buffer.
Return non-nil when something was stopped."
  (when (emagent-chat--operation-active-p)
    (when (and emagent-chat--send-pending
               (not (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))))
      (setq emagent-chat--send-token nil)
      (when (fboundp 'emagent-acp--clear-when-connected-queue)
        (emagent-acp--clear-when-connected-queue))
      (emagent-chat--abort-open-response))
    (when (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
      (progn
        (unless (fboundp 'emagent-acp--finalize-in-flight-prompt)
          (require 'emagent-acp-send))
        (emagent-acp--finalize-in-flight-prompt
         "/Stopped — awaiting new instructions./")))
    (emagent-chat--send-pending-end)
    (when (fboundp 'emagent-chat--refresh-mode-line)
      (emagent-chat--refresh-mode-line))
    (when (fboundp 'emagent-chat--spinner-ensure-running)
      (emagent-chat--spinner-ensure-running))
    t))

(defun emagent-chat--insert-user-heading-with-text (text)
  "Insert TEXT as a complete `* username> TEXT' heading and return point after it."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (goto-char (emagent-chat--user-zone-start))
    (if (emagent-chat--user-heading-at-point-p)
        (delete-region (line-beginning-position) (line-end-position))
      (unless (bolp) (insert "\n")))
    (insert (emagent-chat--user-heading-prefix) text)
    (unless (= (char-before) ?\n) (insert "\n"))
    (point)))

(defun emagent-btw (text)
  "Send TEXT to the agent immediately as a btw side note (C-c C-e b).

When the agent is still thinking, cancels the in-flight turn, keeps any
partial response, and sends `btw, TEXT' as a new prompt."
  (interactive "sBTW: ")
  (when (string-empty-p (string-trim text))
    (user-error "BTW message is empty"))
  (let ((text (format "btw, %s" (string-trim text))))
    (when (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
      (progn
        (unless (fboundp 'emagent-acp--finalize-in-flight-prompt)
          (require 'emagent-acp-send))
        (emagent-acp--finalize-in-flight-prompt)))
    (emagent-log "btw send: %s" (emagent-log-truncate-line text 80))
    (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
      (emagent-chat--begin-response response-pos))
    (emagent-chat--ensure-follow-window)
    (emagent-chat--send-pending-begin)
    (emagent-chat--acp-send text)))

(defun emagent-chat--dispatch-compress ()
  "Handle a /compress-family slash command from `emagent-chat-send'.

Summarizes the prior conversation and forwards the summary to
`emagent-acp-send' with the compress flag, so ACP resets the session with
it once the turn finishes (see `emagent-acp-send-prompt').  With no prior
conversation, fails the just-opened response instead of dispatching."
  (let ((history (emagent-chat--conversation-history-text)))
    (if (string-empty-p history)
        (emagent-chat-fail-assistant "No conversation to compress")
      (emagent-chat--acp-send (emagent-chat--compress-prompt-text history) t))))

(defun emagent-chat-send ()
  "Send the `* user>' prompt at point to the agent (C-c C-c).

Point must be on the prompt's heading line or in its body lines.  The
prompt is sent as-is (heading prefix stripped); nothing in the buffer
is rewritten, so text properties like the `/model' stamp survive.
Sending a previous prompt replaces its old response."
  (interactive)
  (let ((bounds (emagent-chat--send-bounds)))
    (unless bounds
      (user-error "Not on a user prompt"))
    (let* ((raw (string-trim (buffer-substring-no-properties
                              (car bounds) (cdr bounds))))
           ;; The `/model' link overrides the buffer model for this turn; it
           ;; is client UI, stripped from what the agent receives.  With no
           ;; link, keep whatever override is sticky (a prior failure may
           ;; have chosen to keep one).
           (input (emagent-chat--strip-model-links
                   (string-trim (emagent-chat--strip-user-heading raw))))
           (override (emagent-chat--region-turn-model (car bounds) (cdr bounds))))
      (when (string-empty-p input)
        (user-error "Prompt is empty"))
      ;; Client `/mcp' never goes to the agent (Claude or Cursor).
      (if (emagent-chat--mcp-command-p input)
          (emagent-chat--slash-mcp-apply input)
        (when override
          (setq emagent-chat--turn-model override))
        (let ((response-pos
               (save-excursion
                 (goto-char (cdr bounds))
                 (end-of-line)
                 (if (eobp)
                     (let ((inhibit-read-only t)) (insert "\n"))
                   (forward-line 1))
                 (point))))
          (emagent-chat--delete-following-response response-pos)
          (emagent-log "send: %s" (emagent-log-truncate-line input 80))
          (emagent-chat--send-pending-begin)
          (emagent-chat--begin-response response-pos)
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (if emagent-chat--turn-model
                (emagent-chat--insert-switching-scaffold)
              (emagent-chat--insert-preparing-scaffold)))
          ;; Preparing is inserted outside streaming-view; pin the window
          ;; to the live end so the first thought chunk does not clear
          ;; sticky follow when that end is briefly off-screen.
          (emagent-chat--ensure-follow-window)
          (if (and (emagent-chat--bare-slash-command-p input)
                   (emagent-chat--compress-command-p input))
              (emagent-chat--dispatch-compress)
            (emagent-chat--acp-send input)))))))

(defun emagent-chat-interrupt ()
  "Stop any in-flight emagent work (ESC ESC).

Closes a streaming response, cancels a pre-dispatch send, or clears a
connect/model-switch wait.  When idle, falls through to `keyboard-quit'."
  (interactive)
  (if (emagent-chat--stop-operation)
      (message "emagent: stopped")
    (keyboard-quit)))

(defun emagent-chat-new-prompt ()
  "Insert a '* username>' heading at point for a new prompt (C-c C-n).

Useful when the heading stub was accidentally deleted.  If point is
above the user zone, jumps to the end of the buffer first."
  (interactive)
  (let ((inhibit-read-only t)
        (zone-start (emagent-chat--user-zone-start)))
    (when (< (point) zone-start)
      (goto-char (point-max)))
    (emagent-chat--writable)
    (unless (bolp) (insert "\n"))
    (insert (emagent-chat--user-heading-prefix))))

(defun emagent-chat-quit ()
  "Disconnect this buffer's ACP agent and bury the window."
  (interactive)
  (emagent-chat--acp-quit)
  (bury-buffer))

(provide 'emagent-chat-actions)
;;; emagent-chat-actions.el ends here
