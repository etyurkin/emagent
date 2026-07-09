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

(declare-function emagent-chat--model-link "emagent-chat")

(defun emagent-chat--open-reasoning-begin ()
  "Return point at the `** Thinking' headline in the open response body.
Read from the owned `emagent-chat--thinking-headline-marker' when set; otherwise
locate the headline by search and cache it."
  (if (and emagent-chat--thinking-headline-marker
           (marker-position emagent-chat--thinking-headline-marker)
           (emagent-chat--open-response-p))
      (save-excursion
        (goto-char emagent-chat--thinking-headline-marker)
        (beginning-of-line)
        (when (looking-at emagent-chat--thinking-headline-re)
          (marker-position emagent-chat--thinking-headline-marker)))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--thinking-headline-re (cdr bounds) t)
          (setq emagent-chat--thinking-headline-marker
                (copy-marker (match-beginning 0) nil))
          (match-beginning 0))))))

(defun emagent-chat--open-switching-begin ()
  "Return point at the `** Switching model' headline, or nil."
  (if (and emagent-chat--thinking-headline-marker
           (marker-position emagent-chat--thinking-headline-marker)
           emagent-chat--switching-model-p
           (emagent-chat--open-response-p))
      (save-excursion
        (goto-char emagent-chat--thinking-headline-marker)
        (beginning-of-line)
        (when (looking-at emagent-chat--switching-headline-re)
          (marker-position emagent-chat--thinking-headline-marker)))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--switching-headline-re (cdr bounds) t)
          (setq emagent-chat--thinking-headline-marker
                (copy-marker (match-beginning 0) nil)
                emagent-chat--switching-model-p t)
          (match-beginning 0))))))

(defun emagent-chat--thinking-headline-text (&optional model-id)
  "Return the `** Thinking' headline, optionally suffixing MODEL-ID link."
  (if model-id
      (concat emagent-chat-thinking-headline " "
              (emagent-chat--model-link model-id))
    emagent-chat-thinking-headline))

(defun emagent-chat--switching-headline-text (model-id)
  "Return the `** Switching model' headline for per-turn MODEL-ID."
  (concat emagent-chat-switching-headline " to "
          (emagent-chat--model-link model-id) "…"))

(defun emagent-chat--insert-switching-scaffold ()
  "Insert a `** Switching model' subsection at the response body start."
  (when (and emagent-chat--turn-model
             emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (setq emagent-chat--thinking-headline-marker (copy-marker (point) nil)
          emagent-chat--switching-model-p t
          emagent-chat--thought-open-p nil
          emagent-chat--thought-marker nil)
    (insert (emagent-chat--switching-headline-text emagent-chat--turn-model) "\n")
    (setq emagent-chat--assistant-marker (copy-marker (point) nil))))

(defun emagent-chat--promote-switching-to-thinking ()
  "Replace a `** Switching model' headline with `** Thinking'."
  (when-let ((beg (emagent-chat--open-switching-begin)))
    (goto-char beg)
    (delete-region (line-beginning-position) (line-end-position))
    (insert (emagent-chat--thinking-headline-text emagent-chat--turn-model))
    (unless (bolp) (insert "\n"))
    (setq emagent-chat--switching-model-p nil
          emagent-chat--thought-open-p t
          emagent-chat--thought-marker (copy-marker (point) nil)
          emagent-chat--assistant-marker (copy-marker (point) nil))))

(defun emagent-chat--clear-switching-scaffold ()
  "Remove a `** Switching model' headline from the open response, if present."
  (when-let ((beg (emagent-chat--open-switching-begin)))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (goto-char beg)
      (delete-region (line-beginning-position)
                     (min (point-max) (1+ (line-end-position)))))
    (setq emagent-chat--switching-model-p nil
          emagent-chat--thinking-headline-marker nil)))

(defun emagent-chat--insert-reasoning-scaffold ()
  "Insert an empty `** Thinking' subsection at the response body start."
  (when (and emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (setq emagent-chat--thinking-headline-marker (copy-marker (point) nil))
    (insert (emagent-chat--thinking-headline-text emagent-chat--turn-model) "\n")
    (setq emagent-chat--thought-marker (point-marker)
          emagent-chat--thought-open-p t
          emagent-chat--assistant-marker (point-marker))))

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

(defun emagent-chat--ensure-reasoning-scaffold ()
  "Ensure the open response has a `** Thinking' subsection ready to stream."
  (when (emagent-chat--open-response-p)
    (when (emagent-chat--open-switching-begin)
      (emagent-chat--promote-switching-to-thinking))
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
