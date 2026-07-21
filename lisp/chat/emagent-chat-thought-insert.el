;;; emagent-chat-thought-insert.el --- insert/inject reasoning helpers  -*- lexical-binding: t; -*-

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

;; Insert, inject, and collapse helpers for streamed reasoning text.

;;; Code:

(require 'cl-lib)
(require 'emagent-chat-markup)
(require 'emagent-chat-reasoning)
(require 'emagent-chat-response-state)
(require 'emagent-chat-send-state)
(require 'emagent-chat-input)

(defun emagent-chat--end-send-pending-if-active ()
  "End the pre-dispatch phase once agent output begins arriving."
  (emagent-chat--send-pending-end))

(defun emagent-chat--insert-thought-now (text)
  "Insert reasoning TEXT at the open Thinking marker."
  (emagent-chat--end-send-pending-if-active)
  (when (and (not (string-empty-p text))
             (emagent-chat--open-response-p))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (emagent-chat--ensure-response-markers)
      (emagent-chat--ensure-thought-stream)
      (when (and emagent-chat--thought-marker
                 (marker-position emagent-chat--thought-marker))
        (let ((inhibit-modification-hooks t))
          (save-excursion
            (goto-char emagent-chat--thought-marker)
            (emagent-chat--insert-reasoning-text text)
            (setq emagent-chat--thought-marker (point-marker)
                  emagent-chat--assistant-marker (point-marker)
                  emagent-chat--reasoning-streamed-p t)))))))

(defun emagent-chat--reasoning-after-tool-artifact-p ()
  "Return non-nil when point follows a tool line or src block, modulo blanks."
  (save-excursion
    (skip-chars-backward " \t\n")
    (beginning-of-line)
    (looking-at-p "\\(?:→ \\|#\\+[Ee][Nn][Dd]_[Ss][Rr][Cc]\\)")))

(defun emagent-chat--thinking-leading-blank-p ()
  "Return non-nil when point sits in the blank run after `** Thinking'."
  (when-let* ((beg (emagent-chat--open-reasoning-begin))
              (pos (point)))
    (save-excursion
      (goto-char beg)
      (forward-line 1)
      (let ((start (point)))
        (and (> pos start)
             (string-match-p "\\`[ \t\n]*\\'"
                             (buffer-substring-no-properties start pos)))))))

(defun emagent-chat--collapse-thinking-leading-blanks ()
  "Delete the blank run after `** Thinking' up to point."
  (when-let ((beg (emagent-chat--open-reasoning-begin)))
    (save-excursion
      (goto-char beg)
      (forward-line 1)
      (let ((start (point)))
        (when (< start (point))
          (delete-region start (point))
          (goto-char start))))))

(defun emagent-chat--newlines-before-point ()
  "Return the count of consecutive newlines immediately before point."
  (save-excursion
    (let ((n 0))
      (while (eq (char-before) ?\n)
        (setq n (1+ n))
        (backward-char))
      n)))

(defun emagent-chat--newlines-after-point ()
  "Return the count of consecutive newlines immediately after point."
  (save-excursion
    (let ((n 0))
      (while (eq (char-after) ?\n)
        (setq n (1+ n))
        (forward-char))
      n)))

(defun emagent-chat--insert-reasoning-text (text)
  "Insert TEXT at `emagent-chat--thought-marker' in the open Thinking content.
Prose that resumes after a tool line or src block is separated from it by
exactly one blank line, so the two never glue onto the same line.  Some
agents stream bare paragraph-break deltas (chunks that are only newlines)
while still composing; a run of those is collapsed to at most one blank
line rather than piling up as a growing blank tail.

The streaming marker re-syncs to the content tail by skipping back over
trailing newlines (see `emagent-chat--reasoning-stream-marker'), which
strands the previous chunk's own trailing newlines after point.  That run is
folded into TEXT's leading separation, so a thought that resumes after a
close/reopen cycle (each assistant chunk closes the thought) neither glues
onto the previous one nor grows the blank tail two lines per cycle.  When
the run separates the content from following text — the `** Response'
headline, or a tool line's trailing newline — it is structural and is
re-inserted after TEXT, leaving the marker at the true content end."
  (cl-block emagent-chat--insert-reasoning-text
    (let* ((safe (replace-regexp-in-string
                "\n\\{3,\\}" "\n\n"
                (emagent-chat--escape-reasoning-text text (not (bolp)))))
         (before (emagent-chat--newlines-before-point))
         (after (emagent-chat--newlines-after-point))
         (tail-sep (when (> after 0)
                     (save-excursion
                       (forward-char after)
                       (unless (eobp) (min after 2))))))
    (when (> after 0)
      (delete-char after)
      (setq safe (replace-regexp-in-string
                  "\\`\n\\{3,\\}" "\n\n"
                  (concat (make-string after ?\n) safe))))
    (when (emagent-chat--thinking-leading-blank-p)
      (setq safe (replace-regexp-in-string "\\`[\n\r]+" "" safe))
      (if (string-empty-p (string-trim safe))
          (progn
            (emagent-chat--collapse-thinking-leading-blanks)
            (cl-return-from emagent-chat--insert-reasoning-text nil))
        (emagent-chat--collapse-thinking-leading-blanks)
        (setq before (emagent-chat--newlines-before-point)
              after (emagent-chat--newlines-after-point)
              tail-sep (when (> after 0)
                         (save-excursion
                           (forward-char after)
                           (unless (eobp) (min after 2)))))))
    (cond
     ;; Resuming after a tool line/block: keep exactly one blank line of
     ;; separation.
     ((emagent-chat--reasoning-after-tool-artifact-p)
      (setq safe (concat (make-string (max 0 (- 2 before)) ?\n)
                         (replace-regexp-in-string "\\`[\n\r]+" "" safe))))
     ;; TEXT itself opens with blank line(s): only trim when that run,
     ;; combined with the newlines already before point, would exceed one
     ;; blank line — a lone paragraph break is left untouched.
     ((string-match "\\`\n+" safe)
      (let ((leading (match-end 0)))
        (when (> (+ before leading) 2)
          (setq safe (concat (make-string (max 0 (- 2 before)) ?\n)
                             (substring safe leading)))))))
    (when (string-empty-p safe)
      (cl-return-from emagent-chat--insert-reasoning-text nil))
    (if (not tail-sep)
        (insert safe)
      ;; Structural separation follows: share it with SAFE's own trailing
      ;; newlines (a paragraph break at the tail and the separation are the
      ;; same blank line) and keep point before it, at the content end.
      (let ((trailing 0))
        (when (string-match "\n+\\'" safe)
          (setq trailing (- (match-end 0) (match-beginning 0))
                safe (substring safe 0 (match-beginning 0))))
        (insert safe)
        (save-excursion
          (insert (make-string (max tail-sep (min trailing 2)) ?\n))))))))

(defun emagent-chat--inject-reasoning-thought (thought-text)
  "Insert THOUGHT-TEXT under `** Thinking' when reasoning was not streamed.

Create the `** Thinking' subsection on demand, since responses no longer
open one eagerly."
  (let ((trimmed (string-trim (or thought-text ""))))
    (when (not (string-empty-p trimmed))
      (unless (emagent-chat--open-reasoning-begin)
        (emagent-chat--insert-reasoning-scaffold))
      (when-let ((beg (emagent-chat--open-reasoning-begin)))
        (save-excursion
          (goto-char beg)
          (forward-line 1)
          (insert (emagent-chat--escape-reasoning-text trimmed))
          (unless (bolp) (insert "\n")))))))

(defun emagent-chat--remove-empty-thinking ()
  "Remove the open `** Thinking' headline when its content is blank.

Scoped to the in-flight response so it never deletes a `** Thinking'
subsection that belongs to an earlier response."
  (when-let ((beg (emagent-chat--open-reasoning-begin)))
    (let ((content-start (save-excursion (goto-char beg) (line-end-position)))
          (content-end (emagent-chat--reasoning-block-tail)))
      (when (and content-end
                 (string-empty-p
                  (string-trim
                   (buffer-substring-no-properties content-start content-end))))
        (save-excursion
          (goto-char beg)
          (delete-region (line-beginning-position) content-end))
        t))))

(provide 'emagent-chat-thought-insert)
;;; emagent-chat-thought-insert.el ends here
