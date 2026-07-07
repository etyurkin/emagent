;;; emagent-chat-compress.el --- Conversation compression for emagent  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; /compress, /compact, /summarize slash command support.
;; Manages conversation boundary detection, history extraction,
;; compression prompt assembly, and session context reset via ACP.

;;; Code:

(require 'cl-lib)
(require 'map)

(declare-function emagent-chat--with-stable-view "emagent-chat")
(declare-function emagent-chat--writable "emagent-chat")
(declare-function emagent-chat--metadata-end "emagent-chat-header")
(declare-function emagent-chat--user-heading-prefix "emagent-chat")
(declare-function emagent-chat--reset-response-state "emagent-chat")
(declare-function emagent-chat--sync-user-zone-marker "emagent-chat")
(declare-function emagent-chat--find-open-response-begin "emagent-chat")
(declare-function emagent-chat--user-heading-re "emagent-chat")
(declare-function cl-position-if "cl-lib")

(defun emagent-chat--bare-slash-command-p (text)
  "Return non-nil when TEXT is a single-line slash command."
  (let ((trimmed (string-trim text)))
    (and (not (string-empty-p trimmed))
         (string-prefix-p "/" trimmed)
         (not (string-match-p "\n" trimmed))
         (let* ((body (substring trimmed 1))
                (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
                (cmd (if space (substring body 0 space) body)))
           (and (> (length cmd) 0)
                (string-match-p "\\`[-a-z0-9:]+\\'" cmd))))))

(defun emagent-chat--compress-command-p (text)
  "Return non-nil when TEXT is a conversation compression slash command."
  (let ((trimmed (string-trim text)))
    (when (string-prefix-p "/" trimmed)
      (let* ((body (substring trimmed 1))
             (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
             (cmd (if space (substring body 0 space) body)))
        (member cmd '("compress" "compact" "summarize"))))))

(defconst emagent-chat--compress-history-limit 200000
  "Maximum conversation chars included in a /compress request.")

(defun emagent-chat--compress-boundary ()
  "Return point at the user heading before an open response, or nil."
  (save-excursion
    (when-let ((resp (emagent-chat--find-open-response-begin)))
      (goto-char resp)
      (when (re-search-backward (emagent-chat--user-heading-re) nil t)
        (line-beginning-position)))))

(defun emagent-chat--conversation-history-text ()
  "Return prior conversation text for /compress, or \"\"."
  (save-excursion
    (let* ((zone (emagent-chat--metadata-end))
           (end (or (emagent-chat--compress-boundary) (point))))
      (when (and end (> end zone))
        (string-trim (buffer-substring-no-properties zone end))))))

(defun emagent-chat--compress-prompt-text (history)
  "Return a summarization prompt for compression using HISTORY."
  (let ((body (if (> (length history) emagent-chat--compress-history-limit)
                  (concat (substring history 0 emagent-chat--compress-history-limit)
                          "\n\n[...truncated for compression request...]")
                history)))
    (format "Summarize the conversation below for context compression. Preserve key decisions, file paths, errors, and open tasks. Output only the summary.\n\n<conversation>\n%s\n</conversation>"
            body)))

;;;###autoload


(provide 'emagent-chat-compress)
;;; emagent-chat-compress.el ends here
