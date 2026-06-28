;;; emagent-chat-actions.el --- actions module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'map)
(require 'emagent-log)
(require 'emagent-chat-header)
(require 'emagent-chat-markup)
(require 'emagent-chat-render)
(require 'emagent-chat-mode-line)

(defun emagent-chat--clear-btw-indicator ()
  "Remove the btw queued indicator line from the buffer, if present."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (get-text-property (point) 'emagent-btw)
            (delete-region (line-beginning-position)
                           (min (1+ (line-end-position)) (point-max)))
          (forward-line 1))))))

(defun emagent-chat--insert-user-heading-with-text (text)
  "Insert TEXT as a complete `* username> TEXT' heading and return point after it."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (goto-char (emagent-chat--user-zone-start))
    (unless (bolp) (insert "\n"))
    (insert (emagent-chat--user-heading-prefix) text)
    (unless (= (char-before) ?\n) (insert "\n"))
    (point)))

(defun emagent-chat--flush-pending-prompt ()
  "Send the pending btw prompt if one exists.  Called after agent finishes."
  (when emagent-chat--pending-prompt
    (let ((text emagent-chat--pending-prompt))
      (setq emagent-chat--pending-prompt nil)
      (emagent-chat--clear-btw-indicator)
      (emagent-chat--refresh-mode-line)
      (when emagent-chat--on-send
        (emagent-log "btw send: %s" (emagent-log-truncate-line text 80))
        (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
          (emagent-chat--begin-response response-pos))
        (funcall emagent-chat--on-send text)))))

(defun emagent-btw (text)
  "Queue TEXT to send after the current agent response finishes (C-c b).

When the agent is idle, sends immediately.  When busy, stores TEXT and
shows a `# [btw]' indicator; it is removed and TEXT is sent automatically
when the agent finishes."
  (interactive "sBTW: ")
  (when (string-empty-p (string-trim text))
    (user-error "BTW message is empty"))
  (let ((text (format "btw, %s" (string-trim text))))
  (if (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
      (progn
        (setq emagent-chat--pending-prompt text)
        (let ((inhibit-read-only t))
          (emagent-chat--writable)
          (emagent-chat--clear-btw-indicator)
          (save-excursion
            (goto-char (emagent-chat--user-zone-start))
            (unless (bolp) (insert "\n"))
            (insert (propertize (format "# [btw] %s\n" text)
                                'face 'shadow
                                'emagent-btw t))))
        (emagent-chat--refresh-mode-line)
        (message "emagent: btw queued — will send when agent finishes"))
    ;; Agent is idle: send immediately.
    (emagent-log "btw send: %s" (emagent-log-truncate-line text 80))
    (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
      (emagent-chat--begin-response response-pos))
    (when emagent-chat--on-send
      (funcall emagent-chat--on-send text)))))

(defun emagent-chat-send ()
  "Send region or line at point to the agent (C-c C-c).

With an active region, send the selection.  Otherwise send the current
line (or nearest preceding sendable line in the user zone).  The text is
formatted as a '* username> ' org heading in the buffer; the heading
prefix is stripped before the text is sent to the agent.

If the text starts with /btw, the remainder is queued and sent after
the current agent response finishes.  An empty /btw opens a minibuffer."
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
  "Interrupt the running agent response (C-g C-g).

When the agent is busy, closes the response block with a stop notice and
returns the session to idle.  When idle, falls through to `keyboard-quit'."
  (interactive)
  (if (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
      (progn
        (emagent-acp-interrupt)
        (message "emagent: interrupted"))
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
