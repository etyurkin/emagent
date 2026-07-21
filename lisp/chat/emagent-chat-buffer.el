;;; emagent-chat-buffer.el --- Live buffer registry for emagent chat  -*- lexical-binding: t; -*-

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

;; Weak registry of live `emagent-mode' buffers, plus visibility predicates
;; (`emagent-chat--buffer-active-p' / `--buffer-displayed-p').  Shared by the
;; mode-line spinner/refresh paths and `emagent-chat-mode' so neither needs
;; to require the other (that would cycle through the facade `emagent-chat').

;;; Code:

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

(provide 'emagent-chat-buffer)
;;; emagent-chat-buffer.el ends here
