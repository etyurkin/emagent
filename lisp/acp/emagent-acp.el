;;; emagent-acp.el --- ACP wire-up for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.8
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
;;
;; ACP facade: provider wiring, Cursor registration, and public helpers.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-custom)
(require 'emagent-acp-lifecycle)
(require 'emagent-acp-permit)
(require 'emagent-acp-prompt)
(require 'emagent-acp-protocol)
(require 'emagent-acp-provider)
(require 'emagent-acp-request)
(require 'emagent-acp-state)
(require 'emagent-acp-tool-call)
(require 'emagent-acp-usage)
(require 'emagent-chat)
(require 'emagent-session)
(require 'emagent-cursor)
(require 'emagent-log)
(require 'emagent-mcp)
(require 'emagent-prompts)

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

(eval-when-compile
  (require 'cl-lib))

;; Register grouped lisp/ subdirectories on load-path so that
;; cross-directory requires (emagent-log from lisp/core/ etc.)
;; work during byte-compilation by Elpaca or other build tools.
;; Uses `byte-compile-current-file' when set (Elpaca compile).
(eval-and-compile
  (when-let ((file (or load-file-name
                       (and (boundp 'byte-compile-current-file)
                            byte-compile-current-file)))
             (lisp (expand-file-name ".." (file-name-directory file))))
    (when (file-directory-p lisp)
      (dolist (dir (directory-files lisp nil "^[^.]"))
        (let ((path (expand-file-name dir lisp)))
          (when (file-directory-p path)
            (add-to-list 'load-path path)))))))

(defun emagent-acp-prefer-emacs-p ()
  "Return non-nil when emagent instructs the agent to prefer Emacs tools."
  emagent-acp-prefer-emacs)

(defun emagent-reset-permissions ()
  "Reset stored emagent permissions via a minibuffer menu.

Choices:
  project: all      — clears fingerprints and allowed tools for the current
                      project directory
  project: session  — clears fingerprints and auto-approve for the current
                      ACP session
  global: all       — clears all globally approved fingerprints."
  (interactive)
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Not in an emagent buffer"))
  (let* ((session-id (emagent-session-id))
         (project-dir (emagent-session-project-directory))
         (choices
          (delq nil
                (list
                 (when project-dir  "project: all")
                 (when session-id   "project: session")
                 "global: all")))
         (choice (completing-read "Reset permissions: " choices nil t)))
    (pcase choice
      ("project: all"
       (unless project-dir (user-error "No project directory for this buffer"))
       (emagent-permissions-reset-project project-dir)
       (message "emagent: cleared project permissions for %s" project-dir))
      ("project: session"
       (unless session-id (user-error "No active session for this buffer"))
       (emagent-permissions-reset-session session-id)
       (message "emagent: cleared session permissions for session %s" session-id))
      ("global: all"
       (emagent-permissions-reset-global)
       (message "emagent: cleared all global permissions")))))

(defun emagent-acp-current-model-id ()
  "Return the ACP session model id for the current buffer, or nil."
  (when-let ((state (emagent-acp--session)))
    (emagent-acp--current-model-id state nil)))

(defun emagent-acp-set-model-transient (model-id on-done)
  "Switch this buffer's ACP session model to MODEL-ID without persisting it.
The buffer model (`emagent-session-model') is left unchanged, so this is a
per-turn override.  ON-DONE is called once the switch resolves (success or
failure) so the caller can proceed to send the prompt."
  (let ((state (emagent-acp--session)))
    (if state
        (emagent-acp--config-option-set-model-id
         :state state
         :session-id (emagent-acp-state-session-id state)
         :model-id model-id
         :persist nil
         :on-success on-done
         :on-failure (lambda (&rest _) (when on-done (funcall on-done))))
      (when on-done (funcall on-done)))))

(defun emagent-set-model ()
  "Set the ACP model for the current emagent session."
  (interactive)
  (let* ((state (emagent-acp--session))
         (session-id (emagent-acp-state-session-id state))
         (choices (emagent-acp--model-choices state nil))
         (labels (mapcar #'car choices))
         (selection (emagent-acp--read-labeled-choice
                     "Set emagent model: "
                     labels))
         (model-id (cdr (assoc-string selection choices))))
    (unless session-id
      (user-error "No active session"))
    (unless choices
      (user-error "No models available"))
    (unless model-id
      (user-error "Unknown model: %s" selection))
    (when-let ((current (emagent-acp--current-model-id state nil)))
      (when (string= model-id current)
        (user-error "Model already %s"
                    (emagent-acp--model-display-name state nil model-id))))
    (emagent-acp--config-option-set-model-id
     :state state
     :session-id session-id
     :model-id model-id)))

(provide 'emagent-acp)
;;; emagent-acp.el ends here
