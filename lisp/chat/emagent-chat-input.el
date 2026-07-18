;;; emagent-chat-input.el --- input module  -*- lexical-binding: t; -*-

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

;; Chat input region helpers and send plumbing.

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
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) '(read-only t)))))

(defun emagent-chat--insert-initial-comment ()
  "Insert the scratch-style intro and initial user heading in a new buffer."
  (when (= (point-min) (point-max))
    (insert emagent-chat-initial-comment)
    (goto-char (point-max))
    (insert (emagent-chat--user-heading-prefix))))

(defun emagent-chat--user-heading-prefix ()
  "Return the org heading prefix for user messages, e.g. \"* etyurkin> \"."
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

(defvar-local emagent-chat--history-items nil
  "Most recent user prompts, newest first, for history navigation.")

(defvar-local emagent-chat--history-index nil
  "Current history index while navigating, or nil when inactive.")

(defvar-local emagent-chat--history-base-input nil
  "Original input line text before history navigation started.")

(defvar-local emagent-chat--history-origin-line nil
  "Line start position where history navigation was initiated.")

(defun emagent-chat--on-user-input-line-p ()
  "Return non-nil when point is after `* user> ' on the current line."
  (when-let ((input-pos (emagent-chat--user-prompt-input-pos)))
    (>= (point) input-pos)))

(defun emagent-chat--current-user-input ()
  "Return current line input after the `* user> ' prefix, or nil."
  (when-let ((input-pos (emagent-chat--user-prompt-input-pos)))
    (buffer-substring-no-properties input-pos (line-end-position))))

(defun emagent-chat--replace-current-user-input (text)
  "Replace current line input after `* user> ' with TEXT."
  (when-let ((input-pos (emagent-chat--user-prompt-input-pos)))
    (let ((inhibit-read-only t))
      (goto-char input-pos)
      (delete-region input-pos (line-end-position))
      (insert (or text ""))
      (goto-char (line-end-position)))))

(defun emagent-chat--collect-history-items ()
  "Return previous user heading inputs, newest first, excluding current line."
  (let ((current-line (line-beginning-position))
        (user-re (emagent-chat--user-heading-re))
        items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward user-re nil t)
        (let ((line-start (line-beginning-position)))
          (when (< line-start current-line)
            (let ((text (string-trim
                         (buffer-substring-no-properties (match-end 0)
                                                         (line-end-position)))))
              (unless (string-empty-p text)
                (push text items)))))))
    items))

(defun emagent-chat--history-reset ()
  "Reset transient prompt-history navigation state for current buffer."
  (setq emagent-chat--history-items nil
        emagent-chat--history-index nil
        emagent-chat--history-base-input nil
        emagent-chat--history-origin-line nil))

(defun emagent-chat--history-ensure-state ()
  "Initialize prompt-history navigation state for the current input line."
  (let ((line-start (line-beginning-position)))
    (unless (and (integerp emagent-chat--history-index)
                 (equal emagent-chat--history-origin-line line-start))
      (setq emagent-chat--history-origin-line line-start
            emagent-chat--history-base-input (or (emagent-chat--current-user-input) "")
            emagent-chat--history-items (emagent-chat--collect-history-items)
            emagent-chat--history-index -1))))

(defun emagent-chat-history-previous ()
  "Replace current `* user> ' input with the previous prompt from history."
  (interactive)
  (unless (emagent-chat--on-user-input-line-p)
    (user-error "Place point after '* user> ' to use prompt history"))
  (emagent-chat--history-ensure-state)
  (if (null emagent-chat--history-items)
      (message "No prompt history")
    (if (< emagent-chat--history-index (1- (length emagent-chat--history-items)))
        (setq emagent-chat--history-index (1+ emagent-chat--history-index))
      (message "Start of prompt history"))
    (emagent-chat--replace-current-user-input
     (nth emagent-chat--history-index emagent-chat--history-items))))

(defun emagent-chat-history-next ()
  "Replace current `* user> ' input with the newer prompt from history."
  (interactive)
  (unless (emagent-chat--on-user-input-line-p)
    (user-error "Place point after '* user> ' to use prompt history"))
  (emagent-chat--history-ensure-state)
  (cond
   ((not (integerp emagent-chat--history-index))
    (message "No prompt history"))
   ((> emagent-chat--history-index 0)
    (setq emagent-chat--history-index (1- emagent-chat--history-index))
    (emagent-chat--replace-current-user-input
     (nth emagent-chat--history-index emagent-chat--history-items)))
   ((= emagent-chat--history-index 0)
    (setq emagent-chat--history-index -1)
    (emagent-chat--replace-current-user-input emagent-chat--history-base-input))
   (t
    (message "End of prompt history"))))

(defun emagent-chat-history-previous-or-previous-line ()
  "Prompt history on user input; otherwise delegate to `previous-line'."
  (interactive)
  (if (emagent-chat--on-user-input-line-p)
      (emagent-chat-history-previous)
    (call-interactively #'previous-line)))

(defun emagent-chat-history-next-or-next-line ()
  "Prompt history on user input; otherwise delegate to `next-line'."
  (interactive)
  (if (emagent-chat--on-user-input-line-p)
      (emagent-chat-history-next)
    (call-interactively #'next-line)))

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

(defun emagent-chat--delete-following-response (pos)
  "Delete the response subsections after POS, before the next user heading.

Deletes from the first `** Thinking'/`** Response' subsection headline after POS
up to (but not including) the next `* user>' heading."
  (save-excursion
    (goto-char pos)
    (let ((limit (save-excursion
                   (if (re-search-forward "^\\* " nil t)
                       (line-beginning-position)
                     (point-max)))))
      (when (re-search-forward emagent-chat--subsection-headline-re limit t)
        (let ((start (line-beginning-position))
              (inhibit-read-only t))
          (emagent-chat--writable)
          (delete-region start limit))))))

(defun emagent-chat--user-heading-at-point-p ()
  "Return non-nil when point is on a `* user>' heading line."
  (looking-at (emagent-chat--user-heading-re)))

(defun emagent-chat--user-heading-follows-p ()
  "Return non-nil when a '* user>' heading immediately follows point."
  (save-excursion
    (end-of-line)
    (skip-chars-forward " \t\n")
    (looking-at (emagent-chat--user-heading-re))))

(defun emagent-chat--insert-user-heading-stub ()
  "Insert a user heading stub unless one already follows the user zone.

Leave point after the `* user> ' prefix so the user can type immediately."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (goto-char (emagent-chat--user-zone-start))
    (unless (emagent-chat--user-heading-follows-p)
      (unless (bolp) (insert "\n"))
      (insert (emagent-chat--user-heading-prefix)))
    ;; When a stub already exists, zone-start is its bol — move to the
    ;; input position after the prefix rather than leaving point on `*'.
    (goto-char (or (emagent-chat--user-prompt-input-pos) (point)))
    (point)))

(defun emagent-chat--skip-header ()
  
  "Internal helper."
  (goto-char (point-min))
  (while (and (not (eobp))
              (or (looking-at "#\\+")
                  (looking-at "# ")
                  (looking-at "#$")))
    (forward-line 1))
  (skip-chars-forward "\n")
  (point))

(defun emagent-chat--after-last-response ()
  "Return the position where the trailing user zone begins.

That is the `* user>' heading after the last `** Thinking'/`** Response'
subsection, or the start of the conversation when no response exists yet."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward emagent-chat--subsection-headline-re nil t)
        (progn
          (goto-char (match-beginning 0))
          (if (re-search-forward "^\\* " nil t)
              (line-beginning-position)
            (point-max)))
      (emagent-chat--skip-header))))

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

(defun emagent-chat--user-block-bounds ()
  "Return (START . END) for the `* username>' block above point, or nil.
Captures the heading line and all body lines up to the next heading or
response delimiter, trailing whitespace trimmed.  Does not check that
point is within the block; `emagent-chat--send-bounds' does."
  (let ((user-re (emagent-chat--user-heading-re)))
    (save-excursion
      (let ((heading-pos
             (or (and (looking-at "\\* ") (line-beginning-position))
                 (and (re-search-backward "^\\* " (point-min) t)
                      (line-beginning-position)))))
        (when (and heading-pos
                   (save-excursion
                     (goto-char heading-pos)
                     (looking-at user-re)))
          (let* ((start heading-pos)
                 (end (progn
                        (goto-char heading-pos)
                        (forward-line 1)
                        (if (re-search-forward
                             (concat "^\\* \\|"
                                     emagent-chat--subsection-headline-re)
                             (point-max) t)
                            (match-beginning 0)
                          (point-max)))))
            (cons start (save-excursion
                          (goto-char end)
                          (skip-chars-backward " \t\n")
                          (point)))))))))

(defun emagent-chat--send-bounds ()
  "Return (BEG . END) of the `* user>' prompt at point, or nil.

Point must be on the prompt's heading line or within its direct body,
above its response subsections.  Anywhere else there is nothing to
send; then fall through to org."
  (let ((block (emagent-chat--user-block-bounds)))
    (when (and block
               (>= (point) (car block))
               (<= (line-beginning-position) (cdr block)))
      block)))




(provide 'emagent-chat-input)
;;; emagent-chat-input.el ends here
