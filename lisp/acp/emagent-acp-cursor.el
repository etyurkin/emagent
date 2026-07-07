;;; emagent-acp-cursor.el --- Cursor ACP provider adapter  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Cursor-specific ACP behavior: store.db arg enrichment, deferred tool-call
;; display, and serialized resolve retries.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-provider)
(require 'emagent-acp-gate)

(declare-function emagent-cursor-enrich-tool-call-update "emagent-cursor")
(declare-function emagent-cursor--generic-acp-title-p "emagent-cursor")
(declare-function emagent-cursor-normalize-slash-prompt "emagent-cursor")
(declare-function emagent-acp--merged-tool-call-update "emagent-acp-tool-call")
(declare-function emagent-acp--tool-call-label "emagent-acp-tool-call")
(declare-function emagent-acp--tool-call-meaningful-detail-p "emagent-acp-tool-call")
(declare-function emagent-acp--tool-call-displayable-p "emagent-acp-tool-call")
(declare-function emagent-acp--emit-tool-call-display "emagent-acp-tool-call")
(declare-function emagent-acp--drain-permission-queue "emagent-acp-request")
(declare-function emagent-acp--maybe-complete-deferred-prompt "emagent-acp-prompt")

(defconst emagent-acp-cursor--tool-resolve-max-attempts 8
  "Maximum store.db lookups while waiting for Cursor tool-call args.")

(defconst emagent-acp-cursor--tool-resolve-base-delay 0.05
  "Initial idle delay between Cursor store.db resolve retries.")

(defun emagent-acp-cursor--detect-p (state)
  "Return non-nil when STATE's agent is Cursor's cursor-agent CLI."
  (when-let ((launch (emagent-acp--agent-launch-string state)))
    (string-match-p "cursor-agent" launch)))

(defun emagent-acp-cursor--enrich-tool-call (state update)
  "Enrich UPDATE from Cursor store.db when rawInput is empty."
  (if (fboundp 'emagent-cursor-enrich-tool-call-update)
      (emagent-cursor-enrich-tool-call-update (map-elt state :session-id) update)
    update))

(defun emagent-acp-cursor--defer-tool-call-p (_state update)
  "Return non-nil when Cursor sent a generic tool call without args yet."
  (and (map-elt update 'toolCallId)
       (not (emagent-acp--tool-call-meaningful-detail-p update))))

(defun emagent-acp-cursor--tool-resolve-active-p (state)
  "Return non-nil while Cursor store.db tool-call lookups are pending."
  (or (map-elt state :tool-resolve-worker)
      (map-elt state :tool-resolve-queue)))

(defun emagent-acp-cursor--reset-tool-resolve (state)
  "Clear Cursor tool-call resolve queue state in STATE."
  (map-put! state :tool-resolve-queue nil)
  (map-put! state :tool-resolve-worker nil)
  (clrhash (map-elt state :tool-resolve-attempts)))

(defun emagent-acp-cursor--enqueue-tool-resolve (state id &optional delay)
  "Queue ID for a serialized Cursor store.db lookup."
  (let ((queue (map-elt state :tool-resolve-queue)))
    (unless (member id queue)
      (map-put! state :tool-resolve-queue (append queue (list id)))))
  (unless (map-elt state :tool-resolve-worker)
    (emagent-acp-cursor--drain-tool-resolve-queue state delay)))

(defun emagent-acp-cursor--drain-tool-resolve-queue (state &optional delay)
  "Resolve one queued Cursor tool call via `run-at-time'."
  (unless (map-elt state :tool-resolve-worker)
    (if-let ((id (car (map-elt state :tool-resolve-queue))))
        (progn
          (map-put! state :tool-resolve-worker t)
          (run-at-time
           (or delay 0) nil
           (lambda ()
             (map-put! state :tool-resolve-worker nil)
             (map-put! state :tool-resolve-queue
                         (cdr (map-elt state :tool-resolve-queue)))
             (let ((retry-delay (emagent-acp-cursor--resolve-tool-from-store state id)))
               (if retry-delay
                   (emagent-acp-cursor--drain-tool-resolve-queue state retry-delay)
                 (progn
                   (emagent-acp-cursor--drain-tool-resolve-queue state 0)
                   (emagent-acp--maybe-complete-deferred-prompt state)))))))
      (progn
        (emagent-acp--drain-permission-queue state)
        (emagent-acp--maybe-complete-deferred-prompt state)))))

(defun emagent-acp-cursor--resolve-tool-from-store (state id)
  "Look up tool-call ID in Cursor store.db; return retry delay or nil when done."
  (when-let* ((pending-table (map-elt state :tool-call-pending))
              (merged (gethash id pending-table))
              (session-id (map-elt state :session-id))
              (fboundp 'emagent-cursor-enrich-tool-call-update))
    (let* ((enriched (emagent-cursor-enrich-tool-call-update session-id merged))
           (merged (if (equal enriched merged) merged
                     (emagent-acp--merged-tool-call-update state enriched)))
           (label (emagent-acp--tool-call-label merged))
           (status (map-elt merged 'status))
           (kind (map-elt merged 'kind))
           (attempts-table (map-elt state :tool-resolve-attempts))
           (attempts (gethash id attempts-table 0)))
      (cond
       ((emagent-acp--tool-call-meaningful-detail-p merged)
        (puthash id 0 attempts-table)
        (emagent-acp--emit-tool-call-display state id kind merged label status)
        (remhash id pending-table)
        nil)
       ((>= attempts emagent-acp-cursor--tool-resolve-max-attempts)
        (puthash id 0 attempts-table)
        (when (emagent-acp--tool-call-displayable-p state merged)
          (emagent-acp--emit-tool-call-display state id kind merged label status))
        (remhash id pending-table)
        nil)
       (t
        (puthash id (1+ attempts) attempts-table)
        (let ((queue (map-elt state :tool-resolve-queue)))
          (unless (member id queue)
            (map-put! state :tool-resolve-queue (append queue (list id)))))
        (* emagent-acp-cursor--tool-resolve-base-delay (expt 2 attempts)))))))

(defun emagent-acp-cursor--generic-title-p (title)
  "Return non-nil when TITLE is a generic Cursor ACP placeholder."
  (and (fboundp 'emagent-cursor--generic-acp-title-p)
       (funcall #'emagent-cursor--generic-acp-title-p title)))

(defun emagent-acp-cursor--normalize-slash-prompt (text)
  "Normalize slash commands for Cursor before sending a prompt."
  (if (fboundp 'emagent-cursor-normalize-slash-prompt)
      (emagent-cursor-normalize-slash-prompt text)
    text))

(defun emagent-acp-cursor--external-gate-reason (_state)
  'cursor-agent-cli)

(emagent-acp--register-provider
 'cursor
 :detect #'emagent-acp-cursor--detect-p
 :enrich-tool-call #'emagent-acp-cursor--enrich-tool-call
 :defer-tool-call-p #'emagent-acp-cursor--defer-tool-call-p
 :enqueue-tool-resolve #'emagent-acp-cursor--enqueue-tool-resolve
 :reset-tool-resolve #'emagent-acp-cursor--reset-tool-resolve
 :tool-resolve-active-p #'emagent-acp-cursor--tool-resolve-active-p
 :generic-title-p #'emagent-acp-cursor--generic-title-p
 :external-gate-reason #'emagent-acp-cursor--external-gate-reason
 :normalize-slash-prompt #'emagent-acp-cursor--normalize-slash-prompt)

(provide 'emagent-acp-cursor)
;;; emagent-acp-cursor.el ends here
