;;; emagent-chat-thought.el --- thought streaming module  -*- lexical-binding: t; -*-

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

;; Thought/reasoning streaming facade.  Format/hide, fence, and insert helpers
;; live in sibling modules; callers keep requiring `emagent-chat-thought'.

;;; Code:

(require 'emagent-chat-thought-hide)
(require 'emagent-chat-thought-fence)
(require 'emagent-chat-thought-insert)
(require 'emagent-chat-view)

(defun emagent-chat--cancel-thought-flush ()
  "Flush any pending reasoning content and cancel the flush timer.
Empty-lang fences (reasoning notes) are emitted as plain text; language-tagged
fences are closed as org src blocks so buffered content stays readable."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer)
    (setq emagent-chat--thought-flush-timer nil))
  (let ((fence emagent-chat--fence-state)
        (pending emagent-chat--thought-pending))
    (setq emagent-chat--fence-state nil
          emagent-chat--thought-pending "")
    (let ((to-insert
           (concat
            (when fence
              (let* ((raw-lang (car fence))
                     (body (string-trim-right (cdr fence))))
                (cond
                 ((string-empty-p body) "")
                 ((string-empty-p raw-lang) (concat body "\n"))
                 (t (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n"
                            (emagent-chat--lang-from-src-tag raw-lang) body)))))
            pending)))
      (when (and (not (string-empty-p to-insert))
                 (emagent-chat--open-response-p))
        (emagent-chat--insert-thought-now to-insert)))))

(defun emagent-chat--schedule-thought-flush ()
  "Debounce reasoning insertion using `emagent-chat-thought-stream-delay'."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer))
  (setq emagent-chat--thought-flush-timer
        (run-with-timer emagent-chat-thought-stream-delay nil
                        (lambda ()
                          (setq emagent-chat--thought-flush-timer nil)
                          (emagent-chat--flush-thought-pending)))))

(defun emagent-chat--flush-thought-pending (&optional final)
  "Insert any batched reasoning text into the open Thinking block.
Converts complete markdown code fences to org src blocks; buffers incomplete
fences in `emagent-chat--fence-state' until the closing ``` arrives, and
buffers a trailing partial inline span (code, bold, or link) in
`emagent-chat--thought-pending' until its closing delimiter arrives, so markup
split across streaming chunks never renders raw.  With FINAL non-nil (thought
close or an interrupting tool call) everything buffered is emitted as-is."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer)
    (setq emagent-chat--thought-flush-timer nil))
  (let ((text emagent-chat--thought-pending))
    (setq emagent-chat--thought-pending "")
    (when (not (string-empty-p text))
      (let* ((result (emagent-chat--feed-fences text emagent-chat--fence-state))
             (to-insert (car result))
             (new-fence (cdr result)))
        (setq emagent-chat--fence-state new-fence)
        ;; With no open fence and more chunks still coming, hold back a
        ;; trailing partial markup span (inline code, bold, or link) whose
        ;; closing delimiter may arrive in the next chunk.
        (unless (or final new-fence)
          (let ((split (emagent-chat--split-open-markup to-insert)))
            (setq to-insert (car split)
                  emagent-chat--thought-pending (cdr split))))
        (when (not (string-empty-p to-insert))
          (emagent-chat--with-streaming-view
           (lambda ()
             (emagent-chat--insert-thought-now to-insert))))))))

(defun emagent-chat-begin-thought ()
  "Resume or open the Thinking block in the in-flight emagent response."
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (when (emagent-chat--open-response-p)
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat--ensure-response-markers)
            (emagent-chat--ensure-reasoning-scaffold)))))))

(defun emagent-chat-append-thought (text)
  "Append reasoning TEXT to the open Reasoning block."
  (when (not (string-empty-p text))
    (setq emagent-chat--thought-pending
          (concat emagent-chat--thought-pending text))
    (if (or noninteractive (<= emagent-chat-thought-stream-delay 0))
        (emagent-chat--flush-thought-pending)
      (emagent-chat--schedule-thought-flush))))

(defun emagent-chat-close-thought ()
  "Close the open `** Thinking' subsection, if any, and schedule folding."
  (emagent-chat--flush-thought-pending t)
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (when emagent-chat--thought-open-p
          (let ((inhibit-read-only t)
                (hide-at (emagent-chat--open-reasoning-begin)))
            (emagent-chat--writable)
            (when-let ((tail (emagent-chat--reasoning-block-tail)))
              ;; Only reset assistant-marker to the reasoning tail when response
              ;; text hasn't started yet.  If the Response section already exists
              ;; (streaming started), keep the marker tracking the response content.
              (unless (emagent-chat--response-body-bounds)
                (setq emagent-chat--assistant-marker (copy-marker tail nil))))
            (setq emagent-chat--thought-open-p nil
                  emagent-chat--thought-marker nil)
            (emagent-chat--maybe-font-lock-flush)
            (when hide-at
              (emagent-chat--hide-reasoning-deferred hide-at))))))))

(defun emagent-chat--finish-tool-line-in-reasoning ()
  "Leave `emagent-chat--thought-marker' on a fresh line after a tool line."
  (goto-char (line-end-position))
  (unless (or (eobp) (eq (char-after) ?\n))
    (insert "\n"))
  (setq emagent-chat--thought-marker (copy-marker (point) nil)))

(defun emagent-chat--sync-thought-marker-after-tool (end)
  "Place `emagent-chat--thought-marker' after tool display ending at END."
  (goto-char (marker-position end))
  (unless (bolp)
    (goto-char (line-end-position)))
  (unless (or (eobp) (eq (char-after) ?\n))
    (insert "\n"))
  (setq emagent-chat--thought-marker (copy-marker (point) nil)))

(provide 'emagent-chat-thought)
;;; emagent-chat-thought.el ends here
