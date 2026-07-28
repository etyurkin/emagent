;;; emagent-chat-ui.el --- Inline permission-button UI for emagent chat  -*- lexical-binding: t; -*-

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
;; Shared chat UI helpers, send-in-flight state, and buffer utilities.
;;
;;; Code:

(require 'cl-lib)

(defun emagent-tools--apply-button-line-keymap (beg end keymap)
  "Attach KEYMAP to the button line spanning BEG through END (exclusive).
Shortcuts then work anywhere on that line, including at line beginning."
  (when (and beg end keymap (< beg end))
    (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position))))
      (put-text-property line-beg (1- end) 'keymap keymap))))

(defun emagent-tools--goto-first-button (pos)
  "Move point to the first button at or after POS; return non-nil on success."
  (when pos
    (goto-char pos)
    (or (button-at (point))
        (when-let ((btn (next-button (max (1- pos) (point-min)))))
          (goto-char (button-start btn))
          t))))

(defun emagent-tools--focus-inline-buttons (chat-buffer button-pos)
  "Move point to BUTTON-POS in CHAT-BUFFER so button keymaps accept shortcuts."
  (when (and chat-buffer (buffer-live-p chat-buffer) button-pos)
    (when-let ((pos (if (markerp button-pos)
                        (marker-position button-pos)
                      button-pos)))
      (if-let ((win (get-buffer-window chat-buffer)))
          (progn
            (select-window win)
            (with-current-buffer chat-buffer
              (emagent-tools--goto-first-button pos)
              (recenter -3)))
        (with-current-buffer chat-buffer
          (emagent-tools--goto-first-button pos))))))

(defun emagent-tools--choice-shortcut (value)
  "Return a single-character keyboard shortcut for VALUE, or nil."
  (cond
   ((memq value '(yes :allow-once :accept)) "y")
   ((memq value '(no :deny :reject)) "n")
   ((eq value :allow-session) "s")
   ((eq value :allow-always) "w")
   ((memq value '(all :allow-all)) "a")
   (t nil)))

(defun emagent-tools--buttons-prompt (prompt choices chat-buffer callback &optional preamble)
  "Insert optional PREAMBLE, PROMPT, and CHOICES as buttons in CHAT-BUFFER.
CHOICES is a list of (LABEL . VALUE) pairs.  Non-blocking: inserts the dialog
and returns immediately.  CALLBACK is called with the VALUE when a button is
clicked.  Falls back to `completing-read' (synchronous) when CHAT-BUFFER is
nil or dead, calling CALLBACK with the chosen value.

Accept/reject choices bind both lower- and upper-case Y/N.  Labels show the
shortcut in parentheses.  When a trailing `* user>' stub is present, the
dialog is inserted above it rather than after it."
  (if (not (and chat-buffer (buffer-live-p chat-buffer)))
      (let* ((labels (mapcar #'car choices))
             (label (completing-read (concat prompt " ") labels nil t)))
        (funcall callback (cdr (assoc label choices))))
    (let (start-mark end-mark first-button (responded nil))
      (let ((do-respond
             (lambda (v)
               (unless responded
                 (setq responded t)
                 (when (and start-mark end-mark
                            (marker-buffer start-mark)
                            (marker-buffer end-mark))
                   (with-current-buffer chat-buffer
                     (let ((inhibit-read-only t))
                       (when (fboundp 'emagent-chat--writable)
                         (funcall #'emagent-chat--writable))
                       (delete-region (marker-position start-mark)
                                      (marker-position end-mark)))))
                 (funcall callback v)))))
        (with-current-buffer chat-buffer
          (let ((inhibit-read-only t))
            (when (fboundp 'emagent-chat--writable)
              (funcall #'emagent-chat--writable))
            (goto-char
             ;; Only park above a real trailing * user> stub.  Bare
             ;; user-zone-start can be point-min when no response exists
             ;; yet, which would put the dialog at the buffer head.
             (let ((zone (and (fboundp 'emagent-chat--user-zone-start)
                              (emagent-chat--user-zone-start))))
               (if (and zone
                        (fboundp 'emagent-chat--user-heading-at-point-p)
                        (save-excursion
                          (goto-char zone)
                          (emagent-chat--user-heading-at-point-p)))
                   zone
                 (point-max))))
            (unless (bolp) (insert "\n"))
            (setq start-mark (copy-marker (point) nil))
            (when preamble (insert preamble))
            (insert "\n" prompt "\n")
            ;; Build keymap with all shortcuts BEFORE inserting buttons,
            ;; then pass it to each insert-button so the button's own
            ;; overlay keymap contains our shortcuts (higher priority than
            ;; any external overlay we add afterward).
            (let ((btn-keymap (make-sparse-keymap)))
              (set-keymap-parent btn-keymap button-map)
              ;; First pass: define all shortcuts in btn-keymap
              (dolist (choice choices)
                (when-let ((key (emagent-tools--choice-shortcut (cdr choice))))
                  (let ((handler
                         (let ((vv (cdr choice)))
                           (lambda ()
                             (interactive)
                             (funcall do-respond vv)))))
                    (define-key btn-keymap (kbd key) handler)
                    (define-key btn-keymap (kbd (upcase key)) handler))))
              ;; Second pass: insert buttons with btn-keymap as their keymap
              (dolist (choice choices)
                (let* ((v (cdr choice))
                       (key (emagent-tools--choice-shortcut v))
                       (label (if key
                                  (format "[%s (%s)]" (car choice) key)
                                (concat "[" (car choice) "]"))))
                  (unless first-button
                    (setq first-button (copy-marker (point) nil)))
                  (insert-button
                   label
                   'keymap btn-keymap
                   'action (lambda (_b) (funcall do-respond v))
                   'follow-link t)
                  (insert "  ")))
              (insert "\n")
              (setq end-mark (copy-marker (point) nil))
              (when first-button
                (emagent-tools--apply-button-line-keymap
                 (marker-position first-button)
                 (marker-position end-mark)
                 btn-keymap))
              ;; Stop sticky follow so later tool/stream inserts do not
              ;; yank point off the dialog (Y/N keymap needs point here).
              (when (boundp 'emagent-chat--follow-output)
                (setq emagent-chat--follow-output nil)))))
        (emagent-tools--focus-inline-buttons chat-buffer first-button)))))

(defvar-local emagent-chat--send-pending nil
  "Non-nil from send until `emagent-acp-send-prompt' dispatches the turn.

Covers connecting, per-turn model switches (`/model'), and other pre-dispatch
work.  The mode line shows a spinner during this window so large resumed
sessions do not look idle while the agent re-hydrates context for a new model.")

(defvar-local emagent-chat--send-token nil
  "Token for the in-flight pre-dispatch send; cleared on cancel or dispatch.")

(defun emagent-chat--send-active-p (token)
  "Return non-nil when TOKEN is still the active pre-dispatch send."
  (and emagent-chat--send-pending (eq emagent-chat--send-token token)))

(defun emagent-chat--send-pending-begin ()
  "Mark the buffer as preparing a send and refresh the mode line."
  (setq emagent-chat--send-pending t
        emagent-chat--send-token (cl-gensym "emagent-send"))
  (when (fboundp 'emagent-chat--refresh-mode-line)
    (emagent-chat--refresh-mode-line))
  (when (fboundp 'emagent-chat--spinner-ensure-running)
    (emagent-chat--spinner-ensure-running)))

(defun emagent-chat--send-pending-end ()
  "Clear the pre-dispatch send marker and refresh the mode line."
  (when emagent-chat--send-pending
    (setq emagent-chat--send-pending nil
          emagent-chat--send-token nil)
    (when (fboundp 'emagent-chat--refresh-mode-line)
      (emagent-chat--refresh-mode-line))))

(defvar emagent-chat--live-buffers (make-hash-table :weakness 'key :test 'eq)
  "Weak set of live `emagent-mode' buffers.

Used by focus/spinner refresh paths instead of scanning `buffer-list'.")

(defun emagent-chat--register-live-buffer (&optional buffer)
  "Register BUFFER (default current) as a live emagent chat buffer."
  (puthash (or buffer (current-buffer)) t emagent-chat--live-buffers))

(defun emagent-chat--unregister-live-buffer (&optional buffer)
  "Unregister BUFFER (default current) from the live emagent set."
  (remhash (or buffer (current-buffer)) emagent-chat--live-buffers))

(defun emagent-chat--map-live-buffers (fn)
  "Call FN with each live registered emagent buffer."
  (maphash (lambda (buf _)
             (when (buffer-live-p buf)
               (funcall fn buf)))
           emagent-chat--live-buffers))

(defun emagent-chat--buffer-active-p (&optional buffer)
  "Return non-nil when BUFFER is displayed in the selected window."
  (let ((buf (or buffer (current-buffer))))
    (and (window-live-p (selected-window))
         (eq buf (window-buffer (selected-window))))))

(defalias 'emagent-chat--buffer-visible-p 'emagent-chat--buffer-active-p)

(defun emagent-chat--buffer-displayed-p (&optional buffer)
  "Return non-nil when BUFFER is shown in a window on a visible frame.

Unlike `emagent-chat--buffer-active-p', this is true even when the buffer is
not in the selected window (e.g. side-by-side with another buffer, or while
Emacs itself is unfocused).  It is nil only when no visible frame displays
the buffer (every window hidden or the frame iconified)."
  (and (get-buffer-window-list (or buffer (current-buffer)) nil 'visible) t))

(provide 'emagent-chat-ui)
;;; emagent-chat-ui.el ends here
