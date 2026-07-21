;;; emagent-chat-markup-fontify.el --- Deferred font-lock and table align  -*- lexical-binding: t; -*-

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

;; Deferred org font-lock and idle org-table alignment for chat buffers.
;; Depends on buffer visibility predicates and markup conversion helpers.

;;; Code:

(require 'org)
(require 'emagent-acp-usage)
(require 'emagent-chat-buffer)
(require 'emagent-chat-markup-convert)

(defvar-local emagent-chat--font-lock-deferred-p nil
  "When non-nil, defer org font-lock until the emagent buffer is active.")

(defun emagent-chat--align-org-tables-in-region (start end)
  "Align every org table between START and END.

Uses a line-shape check instead of `org-at-table-p' so we never invoke
`org-element-at-point' on every `|' match (that pegged Emacs at 100% CPU
when deferred align ran from redisplay hooks on large chat buffers)."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "^|" nil t)
        (beginning-of-line)
        ;; Cheap shape check — never `org-at-table-p' (org-element).
        (if (looking-at-p "^|.*|")
            (condition-case nil
                (progn
                  (org-table-align)
                  (goto-char (or (org-table-end nil) (point-max))))
              (error (forward-line 1)))
          (forward-line 1))))))

(defun emagent-chat--font-lock-region-start ()
  "Return a start position for incremental font-lock in the current buffer.

When a response is open, only re-fontify a trailing window of that response.
Long tool-heavy turns accumulate large `#+begin_src' blocks; re-fontifying
them all on every stream/tool update blocked the event loop (the \"hang\" on
large sessions).  Between turns, fall back to the user zone — never the
whole session log."
  (let* ((response-start
          (and (boundp 'emagent-chat--response-body-start)
               emagent-chat--response-body-start
               (marker-position emagent-chat--response-body-start)))
         (user-start
          (and (fboundp 'emagent-chat--user-zone-start)
               (emagent-chat--user-zone-start)))
         (floor (or response-start user-start (point-min)))
         ;; ~12k chars covers recent thought/tool/response chunks without
         ;; redoing earlier src blocks in the same turn.
         (window 12000))
    (if response-start
        (max floor (- (point-max) window))
      floor)))

(defun emagent-chat--font-lock-response-tail ()
  "Re-fontify the response tail without flushing the entire session buffer."
  (when font-lock-mode
    (save-excursion
      (let ((start (emagent-chat--font-lock-region-start))
            (end (point-max)))
        (when (< start end)
          (condition-case nil
              (font-lock-fontify-region start end)
            (error nil)))))))

(defun emagent-chat--maybe-font-lock-flush ()
  "Run org font-lock on the response tail when safe; defer otherwise.

Defer when the buffer is not selected, and also while an ACP turn is
busy or finishing — fontifying large Thinking/tool regions on every
chunk pegs the command loop for both Cursor and Claude."
  (if (and (emagent-chat--buffer-active-p)
           (not (emagent-acp-turn-in-flight-p)))
      (progn
        (setq emagent-chat--font-lock-deferred-p nil)
        (emagent-chat--font-lock-response-tail))
    (setq emagent-chat--font-lock-deferred-p t)))

(defun emagent-chat--flush-deferred-font-lock ()
  "Font-lock the response tail when a deferred flush was requested.

Skipped while an ACP turn is still in flight so settle happens once."
  (when (and emagent-chat--font-lock-deferred-p
             (emagent-chat--buffer-active-p)
             (not (emagent-acp-turn-in-flight-p)))
    (setq emagent-chat--font-lock-deferred-p nil)
    (emagent-chat--font-lock-response-tail)))

(defvar-local emagent-chat--table-align-start nil
  "Marker for the start of a pending idle org-table align region, or nil.")

(defvar-local emagent-chat--table-align-end nil
  "Marker for the end of a pending idle org-table align region, or nil.")

(defvar-local emagent-chat--table-align-timer nil
  "Idle timer that aligns `emagent-chat--table-align-start'..end, or nil.")

(defun emagent-chat--cancel-scheduled-table-align ()
  "Cancel any pending idle org-table alignment for this buffer."
  (when emagent-chat--table-align-timer
    (cancel-timer emagent-chat--table-align-timer)
    (setq emagent-chat--table-align-timer nil))
  (when (markerp emagent-chat--table-align-start)
    (set-marker emagent-chat--table-align-start nil))
  (when (markerp emagent-chat--table-align-end)
    (set-marker emagent-chat--table-align-end nil))
  (setq emagent-chat--table-align-start nil
        emagent-chat--table-align-end nil))

(defun emagent-chat--run-scheduled-table-align ()
  "Align the pending org-table region, if any, without dirtying the buffer."
  (setq emagent-chat--table-align-timer nil)
  (when-let* ((start emagent-chat--table-align-start)
              (end emagent-chat--table-align-end)
              (s (marker-position start))
              (e (marker-position end))
              ((< s e)))
    (setq emagent-chat--table-align-start nil
          emagent-chat--table-align-end nil)
    (set-marker start nil)
    (set-marker end nil)
    (let ((was-modified (buffer-modified-p)))
      (unwind-protect
          (save-excursion
            (emagent-chat--align-org-tables-in-region s e))
        (set-buffer-modified-p was-modified)))))

(defun emagent-chat--schedule-align-org-tables (start end)
  "Schedule one idle alignment of org tables between START and END.

Replaces any previous pending region for this buffer.  Never runs from
redisplay/`window-configuration-change-hook' — that path hung Emacs on
large chats via `org-element' parses."
  (when (and (integer-or-marker-p start)
             (integer-or-marker-p end)
             (< (if (markerp start) (marker-position start) start)
                (if (markerp end) (marker-position end) end)))
    (emagent-chat--cancel-scheduled-table-align)
    (setq emagent-chat--table-align-start (copy-marker start t)
          emagent-chat--table-align-end (copy-marker end nil))
    (let ((buf (current-buffer)))
      (setq emagent-chat--table-align-timer
            (run-with-idle-timer
             0 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (emagent-chat--run-scheduled-table-align)))))))))

(provide 'emagent-chat-markup-fontify)
;;; emagent-chat-markup-fontify.el ends here
