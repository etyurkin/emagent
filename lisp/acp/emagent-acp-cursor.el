;;; emagent-acp-cursor.el --- Cursor ACP provider adapter  -*- lexical-binding: t; -*-

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

;; Cursor-specific ACP behavior: store.db arg enrichment, deferred tool-call
;; display, and serialized resolve retries.

;;; Code:

(require 'cl-lib)
(require 'emagent-acp-state)
(require 'map)
(require 'emagent-acp-provider)
(require 'emagent-acp-tool-call)
(require 'emagent-acp-prompt)
(require 'emagent-acp-request)
(require 'emagent-cursor)

(defconst emagent-acp-cursor--tool-resolve-max-attempts 8
  "Maximum store.db lookups while waiting for Cursor tool-call args.")

(defconst emagent-acp-cursor--tool-resolve-base-delay 0.05
  "Initial idle delay between Cursor store.db resolve retries.")

(defconst emagent-acp-cursor--tool-resolve-yield 0.01
  "Delay before starting the next queued store.db lookup after one finishes.

Keeps successive sqlite + JSON parse bursts from stacking on zero-delay
timers during tool-heavy Cursor turns.")

(defun emagent-acp-cursor--detect-p (state)
  "Return non-nil when STATE's agent is Cursor's cursor-agent CLI."
  (when-let ((launch (emagent-acp--agent-launch-string state)))
    (string-match-p "cursor-agent" launch)))

(defun emagent-acp-cursor--enrich-tool-call (_state update)
  "Return UPDATE unchanged — never sync-read Cursor store.db here.

Cursor often omits tool args on the wire; looking them up in store.db via
`shell-command-to-string' on every tool-call notification froze Emacs.
Empty-arg updates are deferred and resolved asynchronously by
`emagent-acp-cursor--resolve-tool-from-store'.

Arguments: UPDATE."
  update)

(defun emagent-acp-cursor--defer-tool-call-p (_state update)
  "Return non-nil when Cursor sent a generic tool call without args yet.

Arguments: UPDATE."
  (and (map-elt update 'toolCallId)
       (not (emagent-acp--tool-call-meaningful-detail-p update))))

(defun emagent-acp-cursor--tool-resolve-active-p (state)
  "Return non-nil while Cursor store.db tool-call lookups are pending.

Arguments: STATE."
  (or (emagent-acp-state-tool-resolve-worker state)
      (emagent-acp-state-tool-resolve-queue state)))

(defun emagent-acp-cursor--reset-tool-resolve (state)
  "Clear Cursor tool-call resolve queue state in STATE."
  (setf (emagent-acp-state-tool-resolve-queue state) nil)
  (setf (emagent-acp-state-tool-resolve-worker state) nil)
  (clrhash (emagent-acp-state-tool-resolve-attempts state)))

(defun emagent-acp-cursor--enqueue-tool-resolve (state id &optional delay)
  "Queue ID for a serialized Cursor store.db lookup.

Arguments: STATE, DELAY."
  (let ((queue (emagent-acp-state-tool-resolve-queue state)))
    (unless (member id queue)
      (setf (emagent-acp-state-tool-resolve-queue state) (append queue (list id)))))
  (unless (emagent-acp-state-tool-resolve-worker state)
    (emagent-acp-cursor--drain-tool-resolve-queue state delay)))

(defun emagent-acp-cursor--drain-tool-resolve-queue (state &optional delay)
  "Resolve one queued Cursor tool call via `run-at-time'.

The resolve worker stays set until `emagent-acp-cursor--resolve-tool-from-store'
finishes (sync in tests, async process sentinel interactively), so store.db
lookups never overlap and never run on the ACP process filter.

Arguments: STATE, DELAY."
  (unless (emagent-acp-state-tool-resolve-worker state)
    (if-let ((id (car (emagent-acp-state-tool-resolve-queue state))))
        (progn
          (setf (emagent-acp-state-tool-resolve-worker state) t)
          (run-at-time
           (or delay 0) nil
           (lambda ()
             (setf (emagent-acp-state-tool-resolve-queue state)
                   (cdr (emagent-acp-state-tool-resolve-queue state)))
             ;; Worker stays t until resolve's finish callback clears it.
             (emagent-acp-cursor--resolve-tool-from-store state id))))
      (progn
        (emagent-acp--drain-permission-queue state)
        (emagent-acp--maybe-complete-deferred-prompt state)))))

(defun emagent-acp-cursor--apply-resolved-entry (state id entry)
  "Apply store ENTRY to pending tool-call ID in STATE; return retry delay or nil.

Arguments: STATE, ID, ENTRY."
  (when-let* ((pending-table (emagent-acp-state-tool-call-pending state))
              (merged (gethash id pending-table)))
    (let* ((enriched (if (and entry (fboundp 'emagent-cursor--apply-store-entry))
                         (emagent-cursor--apply-store-entry merged entry)
                       merged))
           (merged (if (equal enriched merged) merged
                     (emagent-acp--merged-tool-call-update state enriched)))
           (label (emagent-acp--tool-call-label merged))
           (status (map-elt merged 'status))
           (kind (map-elt merged 'kind))
           (attempts-table (emagent-acp-state-tool-resolve-attempts state))
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
        (let ((queue (emagent-acp-state-tool-resolve-queue state)))
          (unless (member id queue)
            (setf (emagent-acp-state-tool-resolve-queue state)
                  (append queue (list id)))))
        (* emagent-acp-cursor--tool-resolve-base-delay (expt 2 attempts)))))))

(defun emagent-acp-cursor--resolve-tool-from-store (state id)
  "Look up tool-call ID in Cursor store.db asynchronously.

Returns nil immediately after starting the lookup (or applying a sync result
in batch tests).  The resolve worker stays busy until the callback runs.

Arguments: STATE, ID."
  (cl-flet
      ((finish (entry)
         (let ((retry-delay
                (emagent-acp-cursor--apply-resolved-entry state id entry)))
           (setf (emagent-acp-state-tool-resolve-worker state) nil)
           (if retry-delay
               (emagent-acp-cursor--drain-tool-resolve-queue state retry-delay)
             (progn
               (emagent-acp-cursor--drain-tool-resolve-queue
                state emagent-acp-cursor--tool-resolve-yield)
               (emagent-acp--maybe-complete-deferred-prompt state))))))
    (let* ((pending-table (emagent-acp-state-tool-call-pending state))
           (merged (and pending-table (gethash id pending-table)))
           (session-id (emagent-acp-state-session-id state)))
      (cond
       ((not (and merged session-id))
        (finish nil)
        nil)
       ;; ERT and other noninteractive callers mock the sync store helper and
       ;; expect an immediate apply.
       ((or noninteractive
            (not (fboundp 'emagent-cursor-tool-call-from-store-async)))
        (finish (and (fboundp 'emagent-cursor-tool-call-from-store)
                     (emagent-cursor-tool-call-from-store session-id id)))
        nil)
       (t
        (emagent-cursor-tool-call-from-store-async
         session-id id (lambda (entry) (finish entry)))
        nil)))))

(defun emagent-acp-cursor--generic-title-p (title)
  "Return non-nil when TITLE is a generic Cursor ACP placeholder."
  (and (fboundp 'emagent-cursor--generic-acp-title-p)
       (funcall #'emagent-cursor--generic-acp-title-p title)))

(defun emagent-acp-cursor--normalize-slash-prompt (text)
  "Normalize slash commands for Cursor before sending a prompt.

Arguments: TEXT."
  (if (fboundp 'emagent-cursor-normalize-slash-prompt)
      (emagent-cursor-normalize-slash-prompt text)
    text))

(defun emagent-acp-cursor--external-gate-reason (_state)
  "Return the Cursor CLI trust-gate reason symbol."
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
 :normalize-slash-prompt #'emagent-acp-cursor--normalize-slash-prompt
 ;; Cursor does not expose context-window figures over ACP.
 :context-usage-unavailable-p (lambda (_state) t))

(provide 'emagent-acp-cursor)
;;; emagent-acp-cursor.el ends here
