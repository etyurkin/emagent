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
  "Return point at the `** Thinking' headline in the open response body."
  (when-let ((bounds (emagent-chat--open-response-body-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (when (re-search-forward emagent-chat--thinking-headline-re (cdr bounds) t)
        (match-beginning 0)))))

(defun emagent-chat--thinking-content-end (begin limit)
  "Return where the Thinking content ends after BEGIN, before LIMIT.

That is the start of the `** Response' headline when present, otherwise LIMIT."
  (save-excursion
    (goto-char begin)
    (if (re-search-forward emagent-chat--response-headline-re limit t)
        (match-beginning 0)
      limit)))

(defun emagent-chat--insert-reasoning-scaffold ()
  "Insert an empty `** Thinking' subsection at the response body start."
  (when (and emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (insert emagent-chat-thinking-headline "\n")
    (setq emagent-chat--thought-marker (point-marker)
          emagent-chat--thought-open-p t
          emagent-chat--assistant-marker (point-marker))
    (emagent-chat--maybe-font-lock-flush)))

(defun emagent-chat--ensure-reasoning-scaffold ()
  "Ensure the open response has a `** Thinking' subsection ready to stream."
  (when (emagent-chat--open-response-p)
    (cond
     (emagent-chat--thought-open-p
      (emagent-chat--sync-thought-marker))
     ((not (emagent-chat--open-reasoning-begin))
      (emagent-chat--insert-reasoning-scaffold))
     (t
      (let ((stream (emagent-chat--reasoning-stream-marker)))
        (setq emagent-chat--thought-marker stream
              emagent-chat--thought-open-p t)
        (unless stream
          (emagent-log "emagent: cannot find reasoning stream marker after ensure")))))))

(defun emagent-chat--reasoning-stream-marker ()
  "Return the insert marker at the end of the open Thinking content.

Streamed reasoning is inserted here, before any `** Response' headline."
  (when-let ((tail (emagent-chat--reasoning-block-tail)))
    (save-excursion
      (goto-char tail)
      (skip-chars-backward "\n" (or (emagent-chat--open-response-begin) (point-min)))
      (point-marker))))

(defun emagent-chat--reasoning-block-tail ()
  "Return point at the end of the open Thinking content, or nil.

This is the start of the `** Response' headline when present, otherwise the
end of the open response body."
  (when-let* ((bounds (emagent-chat--open-response-body-bounds))
              (beg (emagent-chat--open-reasoning-begin)))
    (emagent-chat--thinking-content-end
     (save-excursion (goto-char beg) (line-end-position))
     (cdr bounds))))

(defun emagent-chat--sync-thought-marker ()
  "Realign `emagent-chat--thought-marker' to the true Thinking tail."
  (when emagent-chat--thought-open-p
    (when-let ((stream (emagent-chat--reasoning-stream-marker)))
      (let ((cur (and emagent-chat--thought-marker
                      (marker-position emagent-chat--thought-marker))))
        (when (or (not cur) (< cur (marker-position stream)))
          (setq emagent-chat--thought-marker stream))))))

(defun emagent-chat--ensure-thought-stream ()
  "Open or resume the streaming Thinking subsection in the in-flight response."
  (emagent-chat--ensure-reasoning-scaffold))

(provide 'emagent-chat-reasoning)
;;; emagent-chat-reasoning.el ends here
