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

(defvar-local emagent-chat--follow-output nil
  "Non-nil when this buffer should keep the live response in view.

Set when the user sends a prompt or the window sits on the live tail;
cleared when they scroll so the follow position is off-screen or move
point into earlier history.")

(defun emagent-chat--follow-output-pos ()
  "Return the buffer position streaming output should keep in view."
  (or (when (fboundp 'emagent-chat--open-response-body-bounds)
        (when-let ((bounds (emagent-chat--open-response-body-bounds)))
          (cdr bounds)))
      (point-max)))

(defun emagent-chat--ensure-follow-window (&optional buffer)
  "Arm sticky follow and scroll BUFFER's window to the live output end.

Call after opening a response (send or quiet Build).  Preparing/Thinking
are inserted without `emagent-chat--with-streaming-view', so without this
the first stream chunk can see the follow position off-screen and clear
sticky follow before any recenter runs."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (setq emagent-chat--follow-output t)
      (let ((pos (emagent-chat--follow-output-pos)))
        (goto-char pos)
        (when-let ((win (get-buffer-window buf 'visible)))
          (set-window-point win pos)
          (when (eq win (selected-window))
            (recenter -1)))))))

(defun emagent-chat--live-tail-start ()
  "Return start of the live exchange (prompt + open response), or nil."
  (when (and (fboundp 'emagent-chat--open-response-begin)
             (fboundp 'emagent-chat--user-heading-re))
    (when-let ((begin (emagent-chat--open-response-begin)))
      (save-excursion
        (goto-char begin)
        (if (re-search-backward (emagent-chat--user-heading-re) nil t)
            (line-beginning-position)
          begin)))))

(defun emagent-chat--window-at-bottom-p (window)
  "Return non-nil when WINDOW should follow newly inserted chat output.

Follow when point is on the live prompt/response and either sticky follow
is armed or the window sits on the live end.  Exact `point-max' alone is
not enough: after send, point often remains on the prompt while the
Preparing/Thinking scaffold grows past it.

Sticky follow survives the end briefly leaving the window (Preparing is
inserted outside streaming-view).  It clears when point leaves the live
exchange.  Mid-buffer points without sticky do not re-arm follow."
  (and window (window-live-p window)
       (eq (window-buffer window) (current-buffer))
       (let* ((wp (window-point window))
              (follow-pos (emagent-chat--follow-output-pos))
              (tail (emagent-chat--live-tail-start))
              (end-visible (or noninteractive
                              (pos-visible-in-window-p follow-pos window)))
              (in-live-tail (if tail (>= wp tail) (= wp (point-max)))))
         (cond
          ((not in-live-tail)
           (when (eq window (selected-window))
             (setq emagent-chat--follow-output nil))
           nil)
          ;; Sticky send/Build follow: keep tracking even if the end left
          ;; the window before the first recenter could run.
          (emagent-chat--follow-output t)
          ((not end-visible) nil)
          ((= wp follow-pos)
           (setq emagent-chat--follow-output t)
           t)
          ((= wp (point-max))
           (setq emagent-chat--follow-output t)
           t)
          (t nil)))))

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

Windows marked for follow keep newly inserted text in view by moving
their `window-point' to `emagent-chat--follow-output-pos'."
  (dolist (view views)
    (let ((win (plist-get view :window)))
      (when (window-live-p win)
        (if (plist-get view :at-bottom)
            (let ((pos (emagent-chat--follow-output-pos)))
              (set-window-point win pos)
              (with-selected-window win
                (goto-char pos)
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