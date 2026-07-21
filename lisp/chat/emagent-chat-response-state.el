;;; emagent-chat-response-state.el --- Open-response tracking for emagent chat  -*- lexical-binding: t; -*-

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

;; Tracks whether an emagent response is open in the current buffer, where
;; its body begins and ends, and the marker bookkeeping shared by the
;; response and thought streaming modules.  Kept out of the facade
;; `emagent-chat' — required by both `emagent-acp' and `emagent-acp-connect'
;; — so sibling chat modules (mode line, compression, reasoning, tool UI,
;; response/thought streaming) can require it directly without a load cycle.

;;; Code:

(require 'emagent-chat-input)

;; The markers below stay defvar-local/defconst in the facade `emagent-chat';
;; forward-declared here so this leaf never requires it back.
(defvar emagent-chat--assistant-marker)

(defvar emagent-chat--response-content-marker)

(defvar emagent-chat--response-headline-re)

(defvar-local emagent-chat--response-body-start nil
  "Start of the in-flight emagent response body.")

(defvar-local emagent-chat--response-end-marker nil
  "End of the open response region.
A live marker at the following exchange's user heading (re-evaluating an earlier
prompt), the symbol `point-max' when the response is last in the buffer, or nil
before a response is open.  Owning it avoids re-scanning to the next heading on
every streamed chunk.")

(defun emagent-chat--open-response-p ()
  "Return non-nil when an emagent response is in flight.

A response is open from `emagent-chat--begin-response' until it is
finalized or failed; openness is tracked by the live body-start marker."
  (and emagent-chat--response-body-start
       (marker-buffer emagent-chat--response-body-start)
       (marker-position emagent-chat--response-body-start)
       t))

(defun emagent-chat--open-response-begin ()
  "Return the buffer position where the in-flight response begins, or nil."
  (when (emagent-chat--open-response-p)
    (marker-position emagent-chat--response-body-start)))

(defun emagent-chat--find-open-response-begin ()
  "Return the start of the in-flight response (the `** Thinking' line), or nil."
  (emagent-chat--open-response-begin))

(defun emagent-chat--response-region-end (begin)
  "Return the buffer position that ends the response region starting at BEGIN.

Read from the owned `emagent-chat--response-end-marker' (set once when the
response is opened): a live marker at the following exchange's user heading, or
`point-max' when the response is last.  Falls back to a forward scan only when
the marker was not set (e.g. a re-opened session).  Bounding the region here
keeps finalizing a mid-buffer response from deleting the exchanges below it."
  (cond
   ((markerp emagent-chat--response-end-marker)
    (marker-position emagent-chat--response-end-marker))
   ((eq emagent-chat--response-end-marker 'point-max)
    (point-max))
   (t
    (save-excursion
      (goto-char begin)
      (if (re-search-forward (emagent-chat--user-heading-re) nil t)
          (line-beginning-position)
        (point-max))))))

(defun emagent-chat--open-response-body-bounds ()
  "Return (BEG . END) for the open response body.

BEG is the response body start; END is the next user heading after it (the next
exchange), or `point-max' when this is the last exchange.  Returns nil when no
response is open."
  (when-let ((begin (emagent-chat--open-response-begin)))
    (cons begin (emagent-chat--response-region-end begin))))

(defun emagent-chat--ensure-response-markers ()
  "Set body markers for the open response when they were lost."
  (unless (and emagent-chat--response-body-start
               (marker-position emagent-chat--response-body-start)
               emagent-chat--assistant-marker
               (marker-position emagent-chat--assistant-marker))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (setq emagent-chat--response-body-start (copy-marker (car bounds) nil)
            emagent-chat--assistant-marker (copy-marker (cdr bounds) nil)))))

(defun emagent-chat--response-body-bounds ()
  "Return (CONTENT-START . END) for the `** Response' body, or nil.
CONTENT-START comes from the owned `emagent-chat--response-content-marker' once
the headline exists; otherwise the headline is located by search and cached."
  (if (and emagent-chat--response-content-marker
           (marker-position emagent-chat--response-content-marker)
           (emagent-chat--open-response-p))
      (cons (marker-position emagent-chat--response-content-marker)
            (emagent-chat--response-region-end
             (emagent-chat--open-response-begin)))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--response-headline-re (cdr bounds) t)
          (forward-line 1)
          (setq emagent-chat--response-content-marker (copy-marker (point) nil))
          (cons (point) (cdr bounds)))))))

(provide 'emagent-chat-response-state)
;;; emagent-chat-response-state.el ends here