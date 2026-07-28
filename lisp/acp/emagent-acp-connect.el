;;; emagent-acp-connect.el --- ACP connect and send composition  -*- lexical-binding: t; -*-

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
;;
;; ACP connect/reconnect entrypoints and Claude provider helpers.
;;
;;; Code:

(require 'cl-lib)
(require 'emagent-acp)
(require 'emagent-acp-protocol)
(require 'emagent-acp-prompt)
(require 'emagent-chat)
(require 'emagent-chat-ui)
(require 'emagent-cursor)
(require 'emagent-log)
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

(provide 'emagent-acp-connect)
;;; emagent-acp-connect.el ends here
