;;; emagent-chat-tools-ui.el --- tool lines and permission UI module  -*- lexical-binding: t; -*-

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
;; Tool-call UI blocks and desktop/session notifications.
;;
;;; Code:

(require 'cl-lib)
(require 'emagent-chat-header)
(require 'emagent-chat-reasoning)
(require 'emagent-chat-response-state)
(require 'emagent-chat-thought)
(require 'emagent-chat-tools-fontify)
(require 'emagent-chat-ui)
(require 'emagent-chat-view)
(require 'emagent-tools)

(defcustom emagent-chat-inactive-bell t
  "When non-nil, ring bell when agent output arrives in an inactive buffer."
  :type 'boolean
  :group 'emagent-chat)

(defcustom emagent-chat-inactive-bell-cooldown 1.0
  "Minimum seconds between inactive-buffer bell notifications."
  :type 'number
  :group 'emagent-chat)

(defcustom emagent-chat-inactive-osx-notification t
  "When non-nil on macOS, show a notification for background attention."
  :type 'boolean
  :group 'emagent-chat)

(defcustom emagent-chat-inactive-notification-title "emagent needs attention"
  "Title for macOS background attention notifications."
  :type 'string
  :group 'emagent-chat)

(defcustom emagent-chat-macos-activate-bundle-id "org.gnu.Emacs"
  "Bundle id used by terminal-notifier to foreground Emacs on click.

Only used when terminal-notifier is installed."
  :type 'string
  :group 'emagent-chat)

(defvar-local emagent-chat--last-inactive-bell-time 0.0
  "Last `float-time' when inactive attention notifications were emitted.")

(defvar emagent-chat--emacs-focused-p t
  "Non-nil when Emacs currently has OS-level input focus.")

(defun emagent-chat--sync-focus-state ()
  "Update `emagent-chat--emacs-focused-p' from `selected-frame' focus."
  (setq emagent-chat--emacs-focused-p
        (if (fboundp 'frame-focus-state)
            (frame-focus-state)
          t)))

(condition-case nil
    (progn
      (unless (advice-member-p #'emagent-chat--sync-focus-state after-focus-change-function)
        (add-function :after after-focus-change-function
                      #'emagent-chat--sync-focus-state))
      (emagent-chat--sync-focus-state))
  (error nil))

(defun emagent-chat--inactive-attention-needed-p ()
  "Return non-nil when background attention notifications should fire."
  (and (not emagent-chat--emacs-focused-p)
       (null (get-buffer-window (current-buffer) 0))))

(defun emagent-chat--notify-macos-inactive-update ()
  "Show a macOS notification for background emagent attention.

Uses terminal-notifier when available (click can activate Emacs),
otherwise falls back to osascript notifications.  Any launcher error is
ignored so chat rendering never stalls on OS notifications."
  (when (and emagent-chat-inactive-osx-notification
             (eq system-type 'darwin)
             (not noninteractive))
    (let* ((title emagent-chat-inactive-notification-title)
           (message (or (buffer-name) "emagent"))
           (notifier (executable-find "terminal-notifier"))
           (osascript (executable-find "osascript")))
      (condition-case nil
          (if notifier
              (start-process
               "emagent-inactive-notify" nil notifier
               "-title" title
               "-message" message
               "-group" "emagent-attention"
               "-activate" emagent-chat-macos-activate-bundle-id)
            (when osascript
              (start-process
               "emagent-inactive-notify" nil osascript "-e"
               (format "display notification %s with title %s"
                       (prin1-to-string message)
                       (prin1-to-string title)))))
        (error nil)))))

(defun emagent-chat--notify-inactive-update ()
  "Emit throttled attention notifications for background permission dialogue."
  (when (emagent-chat--inactive-attention-needed-p)
    (let ((now (float-time)))
      (when (>= (- now emagent-chat--last-inactive-bell-time)
                emagent-chat-inactive-bell-cooldown)
        (setq emagent-chat--last-inactive-bell-time now)
        (condition-case nil
            (progn
              (when emagent-chat-inactive-bell
                (ding t))
              (emagent-chat--notify-macos-inactive-update))
          (error nil))))))

(defvar-local emagent-chat--permission-pending nil
  "Non-nil while a permission dialog is active in the current buffer.
New tool-call lines are suppressed while a dialog awaits user input so the
thinking block stays stable until the user responds.")

(defun emagent-chat--insert-permission-newline-if-needed ()
  "Insert a separating newline unless point is already on a fresh line."
  (unless (bolp)
    (insert "\n")))

(defun emagent-chat--ensure-reasoning-for-tool ()
  "Ensure the open response can accept tool annotations in Reasoning."
  (when (emagent-chat--open-response-p)
    (emagent-chat--ensure-reasoning-scaffold)))

(defun emagent-chat--separate-before-tool ()
  "Ensure point is on a fresh line before inserting a tool line.
Consecutive tool lines and src blocks stay adjacent; a blank line is added
only before the first tool line after prose."
  (unless (bolp) (insert "\n"))
  (unless (or (bobp)
              (save-excursion
                (forward-line -1)
                (or (looking-at-p "[ \t]*$")
                    (looking-at-p "→ ")
                    (looking-at-p "#\\+[Ee][Nn][Dd]_[Ss][Rr][Cc]")
                    (looking-at emagent-chat--thinking-headline-re))))
    (insert "\n")))

(defun emagent-chat--append-tool-line (label &optional id lang code)
  "Append tool LABEL to the open Reasoning block.
When ID is non-nil, remember the span for later in-place updates.  When CODE
is non-empty, render it as an Org src block in LANG instead of a single →
line, with LABEL's trailing decision/(Emacs) annotation beneath."
  (when (and label (not (string-empty-p label))
               (emagent-chat--open-response-p)
               (not emagent-chat--permission-pending))
    (emagent-chat--end-send-pending-if-active)
    (emagent-chat--with-stable-view
     (lambda ()
       (with-current-buffer (current-buffer)
         (let ((inhibit-read-only t))
           (emagent-chat--writable)
           ;; Write any buffered reasoning first so the tool line lands after
           ;; the prose received so far, never splitting a pending sentence.
           ;; Force a final flush so a held inline-code span is emitted before
           ;; the tool line rather than stranded after it.
           (emagent-chat--flush-thought-pending t)
           (emagent-chat--ensure-response-markers)
           (emagent-chat--ensure-reasoning-for-tool)
           (unless (and id (emagent-chat--update-tool-call-line id label lang code))
             (when (and emagent-chat--thought-open-p
                        emagent-chat--thought-marker
                        (marker-position emagent-chat--thought-marker))
               (save-excursion
                 (goto-char emagent-chat--thought-marker)
                 (emagent-chat--separate-before-tool)
                 (let ((line-start (line-beginning-position))
                       (blockp (and code (not (string-empty-p code)))))
                   (insert (if blockp
                               (if (and (equal lang "text")
                                        (not (string-match-p "\n" (or code ""))))
                                   ;; Text block = file path: arrow with display path, no block.
                                   (let* ((annotation (emagent-chat--tool-label-annotation label))
                                          (base (if annotation
                                                    (string-trim
                                                     (replace-regexp-in-string
                                                      (concat " *" (regexp-quote annotation) "\\'")
                                                      "" label))
                                                  label))
                                          (verb (car (split-string base "[ :/]" t)))
                                          (full-label (concat (or verb base)
                                                              ": "
                                                              (emagent-chat--display-path code)
                                                              (if annotation (concat " " annotation) ""))))
                                     (emagent-chat--format-tool-line full-label))
                                 ;; Non-text blocks: arrow + block.
                                 (concat (emagent-chat--format-tool-line
                                          (emagent-chat--combined-arrow-label label code))
                                         "\n"
                                         (emagent-chat--format-tool-block code lang nil)))
                             (emagent-chat--format-tool-line label)))
                   (let ((line-end (line-end-position)))
                     (when id
                       (puthash id (cons (copy-marker line-start nil)
                                         (copy-marker line-end nil))
                                emagent-chat--tool-call-lines))
                     (if blockp
                         (emagent-chat--fontify-tool-block line-start line-end)
                       (emagent-chat--fontify-tool-line line-start line-end)))
                   (emagent-chat--finish-tool-line-in-reasoning)))))))))))

(defun emagent-chat--update-tool-call-line (id label &optional lang code)
  "Replace the displayed tool-call span for ID with LABEL.
When CODE is non-empty, render an Org src block in LANG instead of a line.
Return non-nil when a span was updated."
  (let ((entry (gethash id emagent-chat--tool-call-lines)))
    (when (and entry
               (markerp (car entry)) (marker-position (car entry))
               (markerp (cdr entry)) (marker-position (cdr entry)))
      (let* ((start (car entry))
             (end (cdr entry))
             (blockp (and code (not (string-empty-p code))))
             (annotation (emagent-chat--tool-label-annotation label))
             ;; When transitioning from an arrow line to a block, keep the
             ;; arrow line (without annotation) and append the block below.
             ;; The annotation moves into the block comment so it appears once.
             (current (buffer-substring-no-properties start end))
             ;; Arrow-only: single → line with no block appended yet.
             ;; Arrow-with-block: already combined → line + #+begin_src block.
             (was-arrow-only (string-match-p "\\`→ [^\n]*\\'" current))
             (was-arrow-with-block (and (string-match-p "\\`→ " current)
                                        (not was-arrow-only)))
             (display (cond
                       ((and blockp (or was-arrow-only was-arrow-with-block)
                             (equal lang "text")
                             (not (string-match-p "\n" (or code ""))))
                        ;; Text block = file path: show the display path on the
                        ;; arrow (no block) by reconstructing the label from
                        ;; the untruncated code.
                        (let* ((base (if annotation
                                         (string-trim
                                          (replace-regexp-in-string
                                           (concat " *" (regexp-quote annotation) "\\'")
                                           "" label))
                                       label))
                               (verb (car (split-string base "[ :/]" t)))
                               (full-label (concat (or verb base)
                                                   ": "
                                                   (emagent-chat--display-path code)
                                                   (if annotation (concat " " annotation) ""))))
                          (emagent-chat--format-tool-line full-label)))
                       ((and blockp (or was-arrow-only was-arrow-with-block))
                        ;; Arrow carries annotation; abbreviate if label==code.
                        (concat (emagent-chat--format-tool-line
                                 (emagent-chat--combined-arrow-label label code))
                                "\n"
                                (emagent-chat--format-tool-block code lang nil)))
                       (blockp
                        (emagent-chat--format-tool-block
                         code lang
                         (if (equal lang "text")
                             (emagent-chat--tool-label-title-annotation label)
                           annotation)))
                       (t (emagent-chat--format-tool-line label)))))
        (unless (string= (buffer-substring-no-properties start end) display)
          (save-excursion
            (delete-region start end)
            (goto-char start)
            (insert display)
            (set-marker end (point))
            (if blockp
                (emagent-chat--fontify-tool-block (marker-position start)
                                                  (marker-position end))
              (emagent-chat--fontify-tool-line (marker-position start)
                                               (marker-position end)))
            (when emagent-chat--thought-open-p
              (emagent-chat--sync-thought-marker-after-tool end))))
        t))))

(defun emagent-chat-show-tool-call (id label &optional lang code)
  "Show or update a tool-call display for ACP toolCallId ID with LABEL.
When CODE is non-empty, render it as an Org src block in LANG instead of a
single → line."
  (emagent-chat--append-tool-line label id lang code))

(defun emagent-chat-permission-prompt (question choices callback &optional tool-call)
  "Show permission UI for QUESTION at the end of `** Thinking'.

When TOOL-CALL carries a shell command or edit payload, inserts that content,
then CHOICES as buttons.  Otherwise inserts a ? question line before the
buttons.  Skips that content/question line when it would just repeat
TOOL-CALL's already-rendered pending tool-call line.

CHOICES is a list of (LABEL . VALUE) pairs.  Non-blocking: inserts the dialog
and returns immediately.  CALLBACK is called with the chosen VALUE when a
button is clicked.

Keyboard shortcuts (via keymap text property on the buttons line):
  y / RET  — Allow once    s — Allow for session
  w        — Allow always  a — Allow all (session)
  n        — Deny."
  (when (emagent-chat--open-response-p)
    (let* ((buf (current-buffer))
           (raw-content-block (emagent-chat--permission-content-block tool-call))
           (redundant (emagent-chat--permission-redundant-p
                       tool-call raw-content-block question))
           (content-block (unless redundant raw-content-block))
           (responded nil)
           btn-keymap
           question-beg question-end
           content-beg content-end
           buttons-beg buttons-end
           first-button)
      (let ((cleanup
             (lambda ()
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (emagent-chat--writable)
                   (when (and question-beg question-end
                              (marker-buffer question-beg) (marker-buffer question-end))
                     (delete-region (marker-position question-beg) (marker-position question-end)))
                   (when (and buttons-beg buttons-end
                              (marker-buffer buttons-beg) (marker-buffer buttons-end))
                     (delete-region (marker-position buttons-beg) (marker-position buttons-end)))
                   (when (and content-beg content-end
                              (marker-buffer content-beg) (marker-buffer content-end))
                     (delete-region (marker-position content-beg) (marker-position content-end)))
                   (when-let ((stream (emagent-chat--reasoning-stream-marker)))
                     (setq emagent-chat--thought-marker stream))
                   (setq emagent-chat--permission-pending nil))))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat--ensure-response-markers)
            (emagent-chat--ensure-reasoning-scaffold)
            (if-let ((insert-at (emagent-chat--reasoning-block-tail)))
                (progn
                  (goto-char insert-at)
                  ;; Normalize: keep at most 1 blank line before the dialog.
                  ;; reasoning-block-tail may point past trailing \n\n from the
                  ;; response body; strip the excess so the dialog stays tight.
                  (let ((content-end-pos (save-excursion
                                           (skip-chars-backward
                                            "\n"
                                            (or (emagent-chat--open-response-begin)
                                                (point-min)))
                                           (point))))
                    (when (> (- insert-at content-end-pos) 2)
                      (delete-region (+ content-end-pos 2) insert-at)
                      (goto-char (+ content-end-pos 2))))
                  (when content-block
                    (setq content-beg (copy-marker (point) nil))
                    (emagent-chat--insert-permission-newline-if-needed)
                    (insert content-block "\n")
                    (setq content-end (copy-marker (point) nil)))
                  (goto-char (or (and content-end (marker-position content-end))
                                 insert-at))
                  (unless (or content-block redundant)
                    (setq question-beg (copy-marker (point) nil))
                    (emagent-chat--insert-permission-newline-if-needed)
                    (insert (emagent-chat--format-permission-line question))
                    (put-text-property (marker-position question-beg) (point)
                                       'face 'emagent-permission-prompt)
                    (emagent-chat--repair-tool-line-faces (marker-position question-beg) (point))
                    (insert "\n")
                    (setq question-end (copy-marker (point) nil)))
                  (goto-char (or (and question-end (marker-position question-end))
                                 (and content-end (marker-position content-end))
                                 insert-at))
                  (setq buttons-beg (copy-marker (point) nil))
                  (emagent-chat--insert-permission-newline-if-needed)
                  (setq btn-keymap (make-sparse-keymap))
                  (set-keymap-parent btn-keymap button-map)
                  ;; Build key-hints alist and populate btn-keymap first
                  (let* ((allow-once-shown nil) (allow-always-shown nil) (deny-shown nil)
                         (hints
                          (mapcar
                           (lambda (choice)
                             (let* ((val (cdr choice))
                                    (id (and (stringp val) (downcase val)))
                                    (kh (cond
                                         ((eq val :allow-once) (setq allow-once-shown t) "y")
                                         ((eq val :allow-session) "s")
                                         ((eq val :allow-always) (setq allow-always-shown t) "w")
                                         ((eq val :allow-all) "a")
                                         ((eq val :deny) (setq deny-shown t) "n")
                                         ((and (not allow-always-shown) id
                                               (string-match-p "allow_always\\|always" id))
                                          (setq allow-always-shown t) "w")
                                         ((and (not allow-once-shown) id
                                               (string-match-p "allow\\|yes\\|run" id))
                                          (setq allow-once-shown t) "y")
                                         ((and (not deny-shown) id
                                               (string-match-p "deny\\|no\\|reject" id))
                                          (setq deny-shown t) "n")
                                         (t nil))))
                               (when kh
                                 (define-key btn-keymap (kbd kh)
                                             (let ((v val))
                                               (lambda ()
                                                 (interactive)
                                                 (unless responded
                                                   (setq responded t)
                                                   (funcall cleanup)
                                                   (funcall callback v))))))
                               kh))
                           choices)))
                    ;; Now insert buttons with btn-keymap as their keymap
                    (cl-mapc
                     (lambda (choice kh)
                       (let ((val (cdr choice)))
                         (unless first-button
                           (setq first-button (copy-marker (point) nil)))
                         (insert-button
                          (concat "[" (car choice) "]")
                          'keymap btn-keymap
                          'action
                          (let ((v val))
                            (lambda (_b)
                              (unless responded
                                (setq responded t)
                                (funcall cleanup)
                                (funcall callback v))))
                          'follow-link t)
                         (when kh
                           (insert (propertize (format " [%s]" kh) 'face 'shadow)))
                         (insert "  ")))
                     choices hints))
                  (insert "\n")
                  (setq buttons-end (copy-marker (point) nil))
                  (when first-button
                    (emagent-tools--apply-button-line-keymap
                     (marker-position first-button)
                     (marker-position buttons-end)
                     btn-keymap))
                  (setq emagent-chat--permission-pending t))
              (setq question-beg nil content-beg nil buttons-beg nil))))
        (emagent-chat--notify-inactive-update)
        (if (not buttons-beg)
            (let ((content-block (or content-block raw-content-block))
                  (preamble (concat
                             "\n** Request permissions\n"
                             (when content-block
                               (concat content-block "\n")))))
              (emagent-tools--buttons-prompt
               (if content-block "" question)
               choices buf callback preamble))
          (emagent-tools--focus-inline-buttons buf first-button))))))

(provide 'emagent-chat-tools-ui)
;;; emagent-chat-tools-ui.el ends here
