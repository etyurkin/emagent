;;; emagent-chat-reasoning.el --- reasoning module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'map)
(require 'emagent-log)
(require 'emagent-chat-header)
(require 'emagent-chat-markup)

(defun emagent-chat--open-reasoning-begin ()
  "Return point at the last Reasoning opener in the open response body."
  (when-let ((bounds (emagent-chat--open-response-body-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (let (last)
        (while (re-search-forward emagent-chat--reasoning-begin-re (cdr bounds) t)
          (setq last (match-beginning 0)))
        last))))

(defun emagent-chat--last-reasoning-end-quote-pos (begin limit)
  "Return buffer position of the last #+end_quote line after BEGIN before LIMIT."
  (save-excursion
    (goto-char begin)
    (let (last)
      (while (re-search-forward "^#\\+end_quote\\s-*$" limit t)
        (setq last (match-beginning 0)))
      last)))

(defun emagent-chat--insert-reasoning-scaffold ()
  "Insert an empty Thinking block at `emagent-chat--response-body-start'."
  (when (and emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (insert (format "#+begin_quote %s\n" emagent-chat--thinking-block-label))
    (setq emagent-chat--thought-marker (point-marker))
    (insert "\n#+end_quote\n\n")
    (setq emagent-chat--thought-open-p t
          emagent-chat--assistant-marker (point-marker))
    (emagent-chat--maybe-font-lock-flush)))

(defun emagent-chat--ensure-reasoning-end-quote ()
  "Insert #+end_quote when the open Thinking block has no closing line."
  (when-let* ((beg (emagent-chat--open-reasoning-begin))
              (bounds (emagent-chat--open-response-body-bounds))
              (limit (cdr bounds))
              (search-from (save-excursion (goto-char beg) (line-end-position))))
    (unless (emagent-chat--last-reasoning-end-quote-pos search-from limit)
      (let ((insert-at (or (and emagent-chat--thought-marker
                                (marker-position emagent-chat--thought-marker))
                           (save-excursion
                             (goto-char beg)
                             (forward-line 1)
                             (point)))))
        (save-excursion
          (goto-char insert-at)
          (unless (bolp) (insert "\n"))
          (insert "#+end_quote\n\n")
          (setq emagent-chat--assistant-marker (point-marker)))
        (emagent-chat--maybe-font-lock-flush)
        t))))

(defun emagent-chat--ensure-reasoning-scaffold ()
  "Ensure the open response has a Thinking block with #+end_quote present."
  (when (emagent-chat--open-response-p)
    (cond
     (emagent-chat--thought-open-p
      (emagent-chat--ensure-reasoning-end-quote)
      (emagent-chat--sync-thought-marker))
     ((emagent-chat--reasoning-stream-marker)
      (setq emagent-chat--thought-marker (emagent-chat--reasoning-stream-marker)
            emagent-chat--thought-open-p t))
     ((not (emagent-chat--open-reasoning-begin))
      (emagent-chat--insert-reasoning-scaffold))
     (t
      (emagent-chat--ensure-reasoning-end-quote)
      (let ((stream (emagent-chat--reasoning-stream-marker)))
        (setq emagent-chat--thought-marker stream
              emagent-chat--thought-open-p t)
        (unless stream
          (emagent-log "emagent: cannot find reasoning stream marker after ensure")))))))

(defun emagent-chat--reasoning-stream-marker ()
  "Return insert marker before the closing #+end_quote in the open Reasoning block.

Uses the last #+end_quote after the Reasoning opener so streamed text that
contains a literal #+end_quote line cannot steal the insertion point."
  (when-let* ((bounds (emagent-chat--open-response-body-bounds))
              (beg (emagent-chat--open-reasoning-begin))
              (end-quote (emagent-chat--last-reasoning-end-quote-pos
                           (save-excursion (goto-char beg) (line-end-position))
                           (cdr bounds))))
    (save-excursion
      (goto-char end-quote)
      (beginning-of-line)
      (when (and (> (point) (point-min))
                 (= (char-before) ?\n))
        (backward-char 1))
      (point-marker))))

(defun emagent-chat--reasoning-block-tail ()
  "Return point after the last Reasoning block in the open response, or nil."
  (when-let* ((bounds (emagent-chat--open-response-body-bounds))
              (beg (emagent-chat--open-reasoning-begin))
              (end-quote (emagent-chat--last-reasoning-end-quote-pos
                           (save-excursion (goto-char beg) (line-end-position))
                           (cdr bounds))))
    (save-excursion
      (goto-char end-quote)
      (goto-char (line-end-position))
      (skip-chars-forward "\n")
      (point))))

(defun emagent-chat--sync-thought-marker ()
  "Realign `emagent-chat--thought-marker' before the true Reasoning tail."
  (when emagent-chat--thought-open-p
    (when-let ((stream (emagent-chat--reasoning-stream-marker)))
      (let ((cur (and emagent-chat--thought-marker
                      (marker-position emagent-chat--thought-marker))))
        (when (or (not cur) (< cur (marker-position stream)))
          (setq emagent-chat--thought-marker stream))))))

(defun emagent-chat--can-resume-reasoning-p ()
  "Return non-nil when streaming can continue in an existing Reasoning block."
  (when-let* ((tail (emagent-chat--reasoning-block-tail))
              (bounds (emagent-chat--open-response-body-bounds))
              ((>= tail (car bounds))))
    (string-empty-p
     (string-trim (buffer-substring-no-properties tail (cdr bounds))))))

(defun emagent-chat--ensure-thought-stream ()
  "Open or resume the streaming Reasoning block in the in-flight response."
  (emagent-chat--ensure-reasoning-scaffold))

(provide 'emagent-chat-reasoning)
;;; emagent-chat-reasoning.el ends here
