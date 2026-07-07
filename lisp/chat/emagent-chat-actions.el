;;; emagent-chat-actions.el --- actions module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'map)
(require 'emagent-log)
(require 'emagent-chat-header)
(require 'emagent-chat-markup)
(require 'emagent-chat-render)
(require 'emagent-chat-mode-line)

(declare-function emagent-acp--finalize-in-flight-prompt "emagent-acp-send")
(declare-function emagent-acp-busy-p "emagent-acp-usage")
(declare-function emagent-chat--user-heading-at-point-p "emagent-chat-input")

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
      (emagent-acp--finalize-in-flight-prompt))
    (emagent-log "btw send: %s" (emagent-log-truncate-line text 80))
    (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
      (emagent-chat--begin-response response-pos))
    (when emagent-chat--on-send
      (funcall emagent-chat--on-send text))))

(defun emagent-chat-send ()
  "Send region or line at point to the agent (C-c C-c).

With an active region, send the selection.  Otherwise send the current
line (or nearest preceding sendable line in the user zone).  The text is
formatted as a '* username> ' org heading in the buffer; the heading
prefix is stripped before the text is sent to the agent."
  (interactive)
  (let* ((bounds (emagent-chat--send-bounds))
         (raw (string-trim (buffer-substring-no-properties
                            (car bounds) (cdr bounds)))))
    (when (string-empty-p raw)
      (user-error "No sendable text at point"))
    (let* ((response-pos (emagent-chat--format-as-user-heading bounds raw))
           (input (string-trim (emagent-chat--strip-user-heading raw))))
      (emagent-chat--delete-following-response response-pos)
      (emagent-log "send: %s" (emagent-log-truncate-line input 80))
      (emagent-chat--begin-response response-pos)
      (when emagent-chat--on-send
        (funcall emagent-chat--on-send input)))))

(declare-function emagent-acp-interrupt "emagent-acp")

(defun emagent-chat-interrupt ()
  "Interrupt the running agent response (ESC ESC).

When the agent is busy, closes the response block with a stop notice and
returns the session to idle.  When idle, falls through to `keyboard-quit'."
  (interactive)
  (if (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
      (emagent-acp-interrupt)
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
  (when emagent-chat--on-quit
    (funcall emagent-chat--on-quit))
  (bury-buffer))

(provide 'emagent-chat-actions)
;;; emagent-chat-actions.el ends here
