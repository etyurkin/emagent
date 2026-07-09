;;; emagent-chat-reasoning.el --- reasoning module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'map)
(require 'emagent-log)
(require 'emagent-chat-header)
(require 'emagent-chat-markup)

(defun emagent-chat--open-reasoning-begin ()
  "Return point at the `** Thinking' headline in the open response body.
Read from the owned `emagent-chat--thinking-headline-marker' when set; otherwise
locate the headline by search and cache it."
  (if (and emagent-chat--thinking-headline-marker
           (marker-position emagent-chat--thinking-headline-marker)
           (emagent-chat--open-response-p))
      (marker-position emagent-chat--thinking-headline-marker)
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--thinking-headline-re (cdr bounds) t)
          (setq emagent-chat--thinking-headline-marker
                (copy-marker (match-beginning 0) nil))
          (match-beginning 0))))))

(defun emagent-chat--thinking-content-end (begin limit)
  "Return where the Thinking content ends after BEGIN, before LIMIT.

That is the start of the `** Response' headline when present, otherwise LIMIT.
Uses the owned `emagent-chat--response-content-marker' (the Response headline
sits on the line just above it) so it does not scan to LIMIT for a Response
headline that has not been created yet — that scan was O(reasoning^2) while
reasoning streamed with no Response below."
  (if (and emagent-chat--response-content-marker
           (marker-position emagent-chat--response-content-marker)
           (< begin (marker-position emagent-chat--response-content-marker)))
      ;; Response headline exists (owned marker sits on the line below it).
      (save-excursion
        (goto-char emagent-chat--response-content-marker)
        (forward-line -1)
        (line-beginning-position))
    ;; No Response headline: reasoning owns the marker whenever the headline is
    ;; created, so a nil marker in this streaming path means Thinking runs to
    ;; the end of the region.
    limit))

(defun emagent-chat--insert-reasoning-scaffold ()
  "Insert an empty `** Thinking' subsection at the response body start."
  (when (and emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (setq emagent-chat--thinking-headline-marker (copy-marker (point) nil))
    (insert (if emagent-chat--turn-model
                (concat emagent-chat-thinking-headline " ("
                        (propertize emagent-chat--turn-model
                                    emagent-chat--turn-model-property
                                    emagent-chat--turn-model)
                        ")")
              emagent-chat-thinking-headline)
            "\n")
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
