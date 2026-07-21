;;; emagent-chat-view.el --- Window scroll preservation for emagent chat  -*- lexical-binding: t; -*-

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

;; Runs a thunk while preserving each window's scroll position (or letting it
;; follow to the end when it was already there), used by the response/thought
;; streaming code so inserts do not yank the user's scroll position around.
;; Kept out of the facade `emagent-chat' — required by both `emagent-acp' and
;; `emagent-acp-connect' — so `emagent-chat-response'/`emagent-chat-thought'
;; (required by `emagent-chat-actions', which the facade requires) can call it
;; without a load cycle.

;;; Code:

(require 'cl-lib)

(defun emagent-chat--window-at-bottom-p (window)
  "Return non-nil when WINDOW's point is at the end of the current buffer.

Used to decide whether streaming inserts should follow.  Checking only whether
`point-max' is visible is wrong for short chats: the whole buffer fits in the
window, so the end stays visible even after the user moves point earlier to
read, and every thought chunk would jump the cursor back to EOB."
  (and window (window-live-p window)
       (eq (window-buffer window) (current-buffer))
       (= (window-point window) (point-max))))

(defun emagent-chat--save-window-views ()
  "Return saved scroll state for windows displaying the current buffer."
  (let (views)
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (push `(:window ,win
              :start ,(window-start win)
              :at-bottom ,(emagent-chat--window-at-bottom-p win))
            views))
    views))

(defun emagent-chat--restore-window-views (views)
  "Restore scroll state from VIEWS returned by `emagent-chat--save-window-views'.

Windows that were at the buffer end keep following newly inserted text by
moving their `window-point' to `point-max', matching `emagent-log'."
  (dolist (view views)
    (let ((win (plist-get view :window)))
      (when (window-live-p win)
        (if (plist-get view :at-bottom)
            (progn
              (set-window-point win (point-max))
              (with-selected-window win
                (recenter -1)))
          (set-window-start win (plist-get view :start) t))))))

(defun emagent-chat--with-stable-view (fn)
  "Run FN while preserving window scroll unless already at buffer end."
  (let* ((saved-point (point-marker))
         (saved-windows (emagent-chat--save-window-views))
         (selected (selected-window))
         (follow (cl-some (lambda (v)
                            (and (eq (plist-get v :window) selected)
                                 (plist-get v :at-bottom)))
                          saved-windows)))
    (unwind-protect
        (funcall fn)
      (emagent-chat--restore-window-views saved-windows)
      (unless follow
        (when (marker-position saved-point)
          (goto-char saved-point)))
      (set-marker saved-point nil))))

(defun emagent-chat--with-streaming-view (fn)
  "Run FN during live streaming, following windows already at buffer end.

Windows scrolled away from the end keep their `window-start'; windows that
were showing `point-max' are scrolled to the new end after FN returns.
Inserts use `save-excursion', so this explicit follow is required — Emacs
does not auto-scroll when `window-point' is not at the insertion point."
  (let ((views (emagent-chat--save-window-views)))
    (unwind-protect
        (funcall fn)
      (emagent-chat--restore-window-views views))))

(provide 'emagent-chat-view)
;;; emagent-chat-view.el ends here