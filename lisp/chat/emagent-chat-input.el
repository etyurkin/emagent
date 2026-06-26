;;; emagent-chat-input.el --- input module  -*- lexical-binding: t; -*-

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

(defun emagent-chat--writable ()
  "Remove read-only state left by older emagent chat UIs."
  (setq buffer-read-only nil)
  (when (< (point-min) (point-max))
    (remove-text-properties (point-min) (point-max) '(read-only t))))

(defun emagent-chat--insert-initial-comment ()
  "Insert the scratch-style intro and initial user heading in a new buffer."
  (when (= (point-min) (point-max))
    (insert emagent-chat-initial-comment)
    (goto-char (point-max))
    (insert (emagent-chat--user-heading-prefix))))

(defun emagent-chat--line-text ()
  (string-trim (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))

(defun emagent-chat--user-heading-prefix ()
  "Return the org heading prefix for user turns, e.g. \"* etyurkin> \"."
  (format "* %s> " (user-login-name)))

(defun emagent-chat--user-heading-re ()
  "Return a regexp matching the user heading prefix at start of line."
  (format "^\\* %s> ?" (regexp-quote (user-login-name))))

(defun emagent-chat--user-prompt-input-pos ()
  "Return point after the user heading prefix on the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at (emagent-chat--user-heading-re))
      (match-end 0))))

(defun emagent-chat-beginning-of-line ()
  "On a user prompt heading, first \\[emagent-chat-beginning-of-line] jumps after \">\"."
  (interactive)
  (let ((input (emagent-chat--user-prompt-input-pos)))
    (cond
     ((and input (= (point) input))
      (move-beginning-of-line 1))
     ((and input (not (= (point) (line-beginning-position))))
      (goto-char input))
     (t
      (move-beginning-of-line 1)))))

(defun emagent-chat--strip-user-heading (text)
  "Strip the '* username> ' prefix from the first line of TEXT."
  (let* ((re (emagent-chat--user-heading-re))
         (lines (split-string text "\n" nil))
         (first (car lines)))
    (if (string-match re first)
        (string-join (cons (substring first (match-end 0)) (cdr lines)) "\n")
      text)))

(defun emagent-chat--format-as-user-heading (bounds raw)
  "Replace text at BOUNDS with RAW formatted as a user org heading.
Returns the buffer position after the formatted heading."
  (let* ((inhibit-read-only t)
         (prefix (emagent-chat--user-heading-prefix))
         (already (string-match-p "^\\* " raw))
         (lines (and (not already) (split-string raw "\n" t)))
         (formatted (if already
                        raw
                      (if (cdr lines)
                          (concat prefix (car lines) "\n"
                                  (string-join (cdr lines) "\n"))
                        (concat prefix (car lines))))))
    (emagent-chat--writable)
    (goto-char (car bounds))
    (delete-region (car bounds) (cdr bounds))
    (insert formatted)
    (unless (= (char-before) ?\n)
      (insert "\n"))
    (point)))

(defun emagent-chat--delete-following-response (pos)
  "Delete the response block after POS, stopping before the next user heading.

Deletes from the first `emagent-chat-response-headline' or
`# --- emagent ---' after POS up to (but not including) the next
`* user>' heading, whether bare or with content."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t\n")
    (unless (or (looking-at emagent-chat--response-headline-re)
                (looking-at emagent-chat--response-begin-re))
      (or (re-search-forward emagent-chat--response-headline-re nil t)
          (re-search-forward emagent-chat--response-begin-re nil t)))
    (when (or (looking-at emagent-chat--response-headline-re)
              (looking-at emagent-chat--response-begin-re))
      (let ((start (line-beginning-position))
            (inhibit-read-only t))
        (emagent-chat--writable)
        (when (re-search-forward emagent-chat--response-end-re nil t)
          (forward-line 1)
          (skip-chars-forward " \t\n")
          (delete-region start (point)))))))

(defun emagent-chat--user-heading-follows-p ()
  "Return non-nil when a '* user>' heading immediately follows point."
  (save-excursion
    (end-of-line)
    (skip-chars-forward " \t\n")
    (looking-at (emagent-chat--user-heading-re))))

(defun emagent-chat--insert-user-heading-stub ()
  "Insert a user heading stub unless one already follows the user zone."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (goto-char (emagent-chat--user-zone-start))
    (unless (emagent-chat--user-heading-follows-p)
      (unless (bolp) (insert "\n"))
      (insert (emagent-chat--user-heading-prefix)))
    (point)))

(defun emagent-chat--sendable-line-p (line)
  (or (string-empty-p line)
      (and (not (string-match-p "^#\\+" line))
           (not (string-match-p "^# " line))
           (not (string-match-p "^\\* Emagent\\b" line))
           (not (string-match-p emagent-chat--response-headline-re line))
           (not (string-match-p emagent-chat--response-begin-re line))
           (not (string-match-p emagent-chat--response-end-re line))
           (not (string-match-p "^#\\+BEGIN_SRC" line))
           (not (string-match-p "^#\\+END_SRC" line))
           ;; Bare stub "* user> " with no text after it is not sendable
           (not (string-match-p (concat (emagent-chat--user-heading-re) "$") line)))))

(defun emagent-chat--sendable-text-p (text)
  "Return non-nil when TEXT is user prompt material, not buffer metadata."
  (and (not (string-empty-p text))
       (seq-every-p #'emagent-chat--sendable-line-p (split-string text "\n" t))))

(defun emagent-chat--skip-header ()
  (goto-char (point-min))
  (while (and (not (eobp))
              (or (looking-at "#\\+")
                  (looking-at "# ")
                  (looking-at "#$")))
    (forward-line 1))
  (skip-chars-forward "\n")
  (point))

(defun emagent-chat--after-last-response ()
  "Return the position after the last closed emagent response."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward emagent-chat--response-end-re nil t)
        (progn
          (while (re-search-forward emagent-chat--response-end-re nil t)
            nil)
          (goto-char (match-end 0))
          (skip-chars-forward "\n")
          (line-beginning-position))
      (if (re-search-forward "^\\* Emagent\\b" nil t)
          (progn
            (goto-char (point-max))
            (skip-chars-forward "\n")
            (point))
        (emagent-chat--skip-header)))))

(defun emagent-chat--sync-user-zone-marker ()
  "Update the user-zone marker from the buffer, without insertion-type."
  (let ((pos (emagent-chat--after-last-response)))
    (if (and emagent-chat--user-zone-start-marker
             (marker-position emagent-chat--user-zone-start-marker))
        (set-marker emagent-chat--user-zone-start-marker pos)
      (setq emagent-chat--user-zone-start-marker (copy-marker pos nil)))))

(defun emagent-chat--user-zone-start ()
  "Return the buffer position where the next user prompt may begin."
  (emagent-chat--after-last-response))

(defun emagent-chat--send-bounds-backward (end zone-start)
  "Return bounds for the nearest preceding sendable line before END."
  (save-excursion
    (goto-char end)
    (let (found)
      (while (and (not found) (>= (line-beginning-position) zone-start))
        (let ((text (emagent-chat--line-text)))
          (when (emagent-chat--sendable-text-p text)
            (setq found (cons (line-beginning-position) (line-end-position)))))
        (unless found
          (forward-line -1)))
      found)))

(defun emagent-chat--user-block-bounds (zone-start)
  "Return (START . END) for the '* username>' block enclosing point, or nil.
Captures the heading line and all body lines up to the next heading or
response delimiter."
  (let ((user-re (emagent-chat--user-heading-re)))
    (save-excursion
      (let ((heading-pos
             (or (and (looking-at "\\* ") (line-beginning-position))
                 (and (re-search-backward "^\\* " zone-start t)
                      (line-beginning-position)))))
        (when (and heading-pos (>= heading-pos zone-start)
                   (save-excursion
                     (goto-char heading-pos)
                     (looking-at user-re)))
          (let* ((start heading-pos)
                 (end (progn
                        (goto-char heading-pos)
                        (forward-line 1)
                        (if (re-search-forward
                             (concat "^\\* \\|"
                                     emagent-chat--response-headline-re
                                     "\\|"
                                     emagent-chat--response-begin-re)
                             (point-max) t)
                            (match-beginning 0)
                          (point-max)))))
            (cons start (save-excursion
                          (goto-char end)
                          (skip-chars-backward " \t\n")
                          (point)))))))))

(defun emagent-chat--send-bounds ()
  "Return (BEG . END) of text to send at point.

When point is inside a '* user>' heading (anywhere in the buffer), the zone
check is skipped so the user can re-evaluate any previous prompt."
  (cond
   ((region-active-p)
    (cons (region-beginning) (region-end)))
   (t
    (let* ((zone-start (emagent-chat--user-zone-start))
           (point0 (point))
           ;; Re-eval: cursor is on/inside a user heading anywhere in buffer.
           (re-eval (emagent-chat--user-block-bounds (point-min))))
      (if re-eval
          re-eval
        (when (< (line-beginning-position) zone-start)
          (user-error "Move point below the latest emagent response"))
        (or (emagent-chat--send-bounds-backward point0 zone-start)
            (user-error "No sendable text at point")))))))




(provide 'emagent-chat-input)
;;; emagent-chat-input.el ends here
