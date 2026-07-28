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
;; ACP facade: connect/lifecycle wiring and public helpers.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-lifecycle)
(require 'emagent-acp-permit)
(require 'emagent-acp-prompt)
(require 'emagent-acp-protocol)
(require 'emagent-acp-request)
(require 'emagent-acp-tool-call)
(require 'emagent-acp-usage)
(require 'emagent-chat)
(require 'emagent-chat-ui)
(require 'emagent-cursor)
(require 'emagent-log)
(require 'emagent-mcp)
(require 'emagent-prompts)
(require 'emagent-session)
(require 'emagent-tools)

(eval-when-compile
  (require 'cl-lib))

(defgroup emagent-claude nil
  "Claude (Anthropic Agent SDK) ACP provider configuration for emagent."
  :group 'emagent)

(defcustom emagent-claude-acp-command
  '("claude-agent-acp")
  "Command and parameters for the Claude ACP agent.

The npm package @agentclientprotocol/claude-agent-acp installs a
`claude-agent-acp' binary.  To run without a global install, set this to
`(\"npx\" \"-y\" \"@agentclientprotocol/claude-agent-acp\")'."
  :type '(repeat string)
  :group 'emagent-claude)

(defcustom emagent-claude-environment nil
  "Environment variables for the Claude ACP agent (strings \"KEY=value\").

The Claude Agent SDK reads credentials from the environment; set
ANTHROPIC_API_KEY here if you do not export it in your shell profile.

If tools stay blocked after Emacs approves ACP, adjust the SDK or
claude-agent-acp (approval/sandbox).
See `emagent-acp-external-tool-gate-hints'."
  :type '(repeat string)
  :group 'emagent-claude)

(defconst emagent-claude-install-hint
  "Install: npm install -g @agentclientprotocol/claude-agent-acp
Or set `emagent-claude-acp-command' to (\"npx\" \"-y\" \"@agentclientprotocol/claude-agent-acp\").
See https://github.com/agentclientprotocol/claude-agent-acp")

(defun emagent-claude-command ()
  "Return the Claude ACP command name."
  (car emagent-claude-acp-command))

(defun emagent-claude-command-params ()
  "Return extra parameters for the Claude ACP agent.
That is the `cdr' of `emagent-claude-acp-command'."
  (cdr emagent-claude-acp-command))

(defun emagent-claude-check-command ()
  "Signal a clear error when the Claude agent is missing."
  (unless (executable-find (emagent-claude-command))
    (error "Claude ACP agent not found on PATH (%s).\n%s"
           (emagent-claude-command)
           emagent-claude-install-hint)))

(cl-defun emagent-claude-make-client (&key context-buffer process-directory)
  "Create an ACP client for Claude using CONTEXT-BUFFER.
PROCESS-DIRECTORY is passed to `make-process' as the working directory
\(see `emagent-chat--session-directory' / #+EMAGENT_PROJECT)."
  (emagent-claude-check-command)
  (emagent-acp-make-client :context-buffer context-buffer
                   :process-directory process-directory
                   :command (emagent-claude-command)
                   :command-params (emagent-claude-command-params)
                   :environment-variables emagent-claude-environment))

(defun emagent-claude--project-hash (dir)
  "Return the ~/.claude/projects directory name for absolute path DIR.
Claude Code derives the name by replacing every '/' and '.' with '-'."
  (replace-regexp-in-string "[/.]" "-" (directory-file-name (expand-file-name dir))))

(defun emagent-claude-relocate-session (session-id old-dir new-dir)
  "Move Claude session files for SESSION-ID from OLD-DIR's hash to NEW-DIR's.
Claude Code stores sessions under ~/.claude/projects/<hashed-cwd>/<session-id>.
This moves those files so session/load succeeds after the project directory
changes.  Does nothing when the session is not found under OLD-DIR's hash."
  (let* ((projects-base (expand-file-name "~/.claude/projects"))
         (old-proj (expand-file-name (emagent-claude--project-hash old-dir)
                                     projects-base))
         (new-proj (expand-file-name (emagent-claude--project-hash new-dir)
                                     projects-base)))
    (when (and (file-directory-p projects-base)
               (file-directory-p old-proj))
      (make-directory new-proj t)
      (dolist (suffix '("" ".jsonl"))
        (let ((src (expand-file-name (concat session-id suffix) old-proj))
              (dst (expand-file-name (concat session-id suffix) new-proj)))
          (when (file-exists-p src)
            (rename-file src dst)
            (message "emagent: moved %s → %s" src dst)))))))

(defun emagent-acp--make-client (provider buffer)
  "Create an ACP client for PROVIDER using BUFFER as context."
  (let ((process-directory (and (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (emagent-chat--session-directory)))))
    (pcase provider
      ('cursor (emagent-cursor-make-client :context-buffer buffer
                                           :process-directory process-directory))
      ('claude (emagent-claude-make-client :context-buffer buffer
                                           :process-directory process-directory))
      (_ (user-error "Unknown emagent provider: %s" provider)))))

(defvar emagent-default-provider)

(cl-defun emagent-acp-ensure-connected (&key on-ready on-reveal)
  "Connect the current emagent buffer to its ACP provider if needed.

When the agent process died but buffer-local state remains, tear it down and
reconnect (resuming the saved session id when present).  Optional ON-READY runs
once the session is ready; ON-REVEAL runs when the chat buffer should be shown.
While a connection is already in flight, ON-READY is queued instead of tearing
the session down and starting over."
  (when on-ready (push on-ready emagent-acp--when-connected-queue))
  (cond
   ((emagent-acp--connected-p)
    (emagent-acp--run-when-connected-queue))
   ((emagent-acp--connecting-p)
    nil)
   (t
    (emagent-acp--teardown-stale-session)
    (let* ((provider (or emagent-chat-provider emagent-default-provider))
           (client (emagent-acp--make-client provider (current-buffer))))
      (emagent-acp-start :client client
                        :chat-buffer (current-buffer)
                        :on-ready #'emagent-acp--run-when-connected-queue
                        :on-reveal on-reveal
                        :callbacks
                        `((:cb-chunk          . ,#'emagent-chat-append-assistant)
                          (:cb-thought        . ,#'emagent-chat-append-thought)
                          (:cb-finish         . ,(lambda (&rest args)
                                                    (apply #'emagent-chat-finish-assistant args)
                                                    (emagent-acp--restore-turn-model)))
                          (:cb-fail           . ,(lambda (&rest args)
                                                    (apply #'emagent-chat-fail-assistant args)
                                                    (emagent-acp--turn-model-on-failure
                                                     (car args))))
                          (:cb-slash-commands . ,#'emagent-chat-set-slash-commands)
                          (:cb-tool-call      . ,#'emagent-chat-show-tool-call)
                          (:cb-permission     . ,#'emagent-chat-permission-prompt)
                          (:cb-status         . ,#'emagent-chat-set-status)))))))

(defun emagent-acp--send-prompt-safe (buffer user-text &optional compress)
  "Send USER-TEXT from BUFFER, logging and surfacing failures in the chat.
COMPRESS is forwarded to `emagent-acp-send-prompt'."
  (with-current-buffer buffer
    (condition-case err
        (emagent-acp-send-prompt user-text compress)
      (error
       (let ((msg (error-message-string err)))
         (when (fboundp 'emagent-chat--send-pending-end)
           (emagent-chat--send-pending-end))
         (emagent-log "emagent: send failed: %s" msg)
         (when (fboundp 'emagent-chat-fail-assistant)
           (emagent-chat-fail-assistant msg)))))))

(defun emagent-acp-send (user-text &optional compress)
  "Ensure connection and send USER-TEXT from the current buffer.

When a per-turn model override (`emagent-chat--turn-model', set by `/model') is
active and differs from the session model, switch to it transiently first, then
send; the buffer model is restored when the turn ends (see
`emagent-chat-finish-assistant' / `emagent-chat-fail-assistant').

COMPRESS is forwarded to `emagent-acp-send-prompt': set by
`emagent-chat--dispatch-compress' when USER-TEXT is already a compression
summary prompt rather than ordinary chat input."
  (let ((buf (current-buffer))
        (turn-model emagent-chat--turn-model)
        (token emagent-chat--send-token))
    (emagent-acp-ensure-connected
     :on-ready
     (lambda ()
       (with-current-buffer buf
         (when (emagent-chat--send-active-p token)
           (let* ((state emagent-acp--session)
                  (current (and state (emagent-acp-current-model-id)))
                  (target (and turn-model state
                                (emagent-acp--match-model-id turn-model state nil))))
             (if (and target current (not (string= target current)))
                 (progn
                   ;; Remember the real session model to restore to (only the
                   ;; first time, so a sticky post-failure override still points
                   ;; back at the original global model).
                   (unless emagent-chat--turn-model-base
                     (setq emagent-chat--turn-model-base current))
                   (when (fboundp 'emagent-acp--progress)
                     (emagent-acp--progress
                      state
                      (format "switching model to %s for this turn…"
                              (if (fboundp 'emagent-acp--model-display-name)
                                  (emagent-acp--model-display-name state nil target)
                                target))))
                   (emagent-acp-set-model-transient
                    target
                    (lambda ()
                      (when (emagent-chat--send-active-p token)
                        (emagent-acp--send-prompt-safe buf user-text compress)))))
               (emagent-acp--send-prompt-safe buf user-text compress)))))))))

(defun emagent-acp--wire-chat-buffer ()
  "Install buffer teardown for the current emagent chat buffer.

Adds `emagent-acp-shutdown-buffer' on `kill-buffer-hook'.  Send/attach/quit
commands call ACP entry points directly (lazy `require'), so no buffer-local
function slots are needed."
  (add-hook 'kill-buffer-hook #'emagent-acp-shutdown-buffer nil t))

(add-hook 'emagent-mode-hook #'emagent-acp--wire-chat-buffer)

(defun emagent-acp--restore-turn-model ()
  "Restore the session model overridden by `/model' and clear the override.
Called on a successful turn: switches back to the captured base model and clears
`emagent-chat--turn-model' so the next prompt uses the buffer model again."
  (when emagent-chat--turn-model
    (when emagent-chat--turn-model-base
      (emagent-acp-set-model-transient emagent-chat--turn-model-base #'ignore))
    (setq emagent-chat--turn-model nil
          emagent-chat--turn-model-base nil)))

(defun emagent-acp--turn-model-on-failure (&optional message)
  "After a failed `/model' turn, keep or restore the per-turn override.

MESSAGE is the failure text used to classify transient vs permanent errors.
Only transient network failures (after retries are exhausted) ask whether to
keep the override for a manual `retry'.  Permanent errors such as context
overflow restore the buffer model immediately."
  (when emagent-chat--turn-model
    (if (and message (fboundp 'emagent-acp--retriable-prompt-error-p)
         (emagent-acp--retriable-prompt-error-p message))
        (emagent-tools--buttons-prompt
         (format "Continue with %s for the next prompt?" emagent-chat--turn-model)
         '(("Yes, keep it" . keep) ("No, use the buffer model" . restore))
         (current-buffer)
         (lambda (choice)
           (when (eq choice 'restore)
             (emagent-acp--restore-turn-model))))
      (emagent-acp--restore-turn-model))))

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
