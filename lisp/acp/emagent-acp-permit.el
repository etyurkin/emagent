;;; emagent-acp-permit.el --- Permission helpers and cursor tool-resolve  -*- lexical-binding: t; -*-

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
;; Permission prompts, allowlists, and the ACP permission queue.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'emagent-acp-protocol)
(require 'emagent-acp-tool-call)
(require 'emagent-acp-usage)
(require 'emagent-chat)
(require 'emagent-chat-ui)
(require 'emagent-cursor)
(require 'emagent-log)
(require 'emagent-policy)
(require 'emagent-session)
(require 'emagent-tools)
(require 'emagent-trust)

(defun emagent-acp--permission-question-line (emagent-acp-request)
  "Return the command or path to show on the permission ? line.

Arguments: EMAGENT-ACP-REQUEST."
  (let* ((tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (detail (and tool-call (emagent-acp--tool-call-detail-from-tool-call tool-call)))
         (title (emagent-acp--permission-prompt-title emagent-acp-request))
         (name (and title (replace-regexp-in-string "\\`Allow \\(.*\\)[?]\\'" "\\1" title))))
    (cond
     ((emagent-acp--human-tool-detail-p detail)
      ;; A shell detail is self-explanatory ("make test"), but an MCP
      ;; tool's raw-input fragment ("--oneline -10") is meaningless
      ;; without the tool's name — prepend it.
      (if (and name (string-match-p "\\`mcp__" name)
               (not (string-match-p (regexp-quote name) detail)))
          (format "%s %s" name detail)
        detail))
     (name name)
     (t "Permission request"))))

(defun emagent-acp--permission-prompt-title (emagent-acp-request)
  "Return the primary permission question line from EMAGENT-ACP-REQUEST."
  (let* ((tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (raw (or (map-nested-elt emagent-acp-request '(params title))
                  (map-nested-elt emagent-acp-request '(params toolCall title))
                  "Permission request"))
         (title (car (split-string raw "\n" t))))
    (if (and tool-call (emagent-acp--switch-mode-tool-p tool-call))
        (emagent-acp--switch-mode-display-title
         (if (map-elt tool-call 'title)
             tool-call
           (cons (cons 'title title) tool-call)))
      title)))

(defun emagent-acp--permission-prompt-text (emagent-acp-request)
  "Return user-facing permission prompt text for EMAGENT-ACP-REQUEST."
  (let* ((title (emagent-acp--permission-prompt-title emagent-acp-request))
         (tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (detail (emagent-acp--tool-call-detail-from-tool-call tool-call)))
    (if (and (emagent-acp--human-tool-detail-p detail)
             (not (string-match-p (regexp-quote detail) title)))
        (format "%s\n%s" title (emagent-acp--tool-call-truncate detail))
      title)))

(cl-defun emagent-acp--handle-one-permission (&key state emagent-acp-request on-complete)
  "Show permission dialog for EMAGENT-ACP-REQUEST in STATE's chat buffer.

For auto-deny/auto-approve: sends the ACP response synchronously and calls
ON-COMPLETE immediately.  For interactive prompts: inserts the dialog
non-blockingly and returns; ON-COMPLETE is called after the user responds.

`switch_mode' permissions (Claude ExitPlanMode, Cursor SwitchMode) never
auto-approve: the agent's optionIds are mode ids and must be returned
unchanged."
  (let* ((raw-tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (tool-call (and raw-tool-call
                         (emagent-acp--permission-tool-call state raw-tool-call)))
         (options (map-nested-elt emagent-acp-request '(params options)))
         (request-id (map-elt emagent-acp-request 'id))
         (question (emagent-acp--permission-question-line emagent-acp-request))
         (fingerprint (and tool-call (emagent-acp--permission-fingerprint tool-call)))
         (validation (and tool-call (emagent-acp--permission-validate tool-call)))
         (buf (emagent-acp--chat-buffer state))
         (allow-id (emagent-acp--permission-acp-allow-id options))
         (deny-id (emagent-acp--permission-acp-deny-id options))
         (switch-mode (emagent-acp--switch-mode-tool-p tool-call))
         (switch-choices (and switch-mode
                              (emagent-acp--switch-mode-choices options))))
    (when raw-tool-call
      (emagent-acp--ingest-tool-call-request state raw-tool-call))
    (let ((respond
           (lambda (choice)
             (when (and (not switch-mode)
                        (emagent-acp--permission-approved-choice-p choice))
               (emagent-acp--permission-apply-choice state fingerprint buf choice))
             (let* ((response
                     (cond
                      ((and (stringp choice) (not (string-empty-p choice)))
                       (emagent-acp-make-session-request-permission-response
                        :request-id request-id :option-id choice))
                      ((and (not switch-mode)
                            validation (eq (car validation) :deny))
                       (if deny-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id deny-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      ((and (not switch-mode)
                            (emagent-acp--permission-approved-choice-p choice))
                       (if allow-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id allow-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      ((and (not switch-mode) (eq choice :deny))
                       (if deny-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id deny-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      (t
                       (emagent-acp-make-session-request-permission-response
                        :request-id request-id :cancelled t))))
                    (outcome (map-nested-elt response '(:result outcome))))
               (emagent-log "permission response: question=%s outcome=%s choice=%s"
                            question (or outcome "?") choice)
               (emagent-acp-send-response :client (emagent-acp-state-client state) :response response))
             (when on-complete (funcall on-complete)))))
      (cond
       (switch-mode
        (unless (fboundp 'emagent-acp--prepare-interactive-context)
          (require 'emagent-acp-prompt))
        (emagent-acp--prepare-interactive-context state)
        (emagent-acp--clear-prompt-watchdog state)
        (let ((after-response
               (lambda (choice)
                 (when choice
                   (emagent-acp--show-permission-decision state tool-call choice))
                 (when (emagent-acp-state-busy state)
                   (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
                     (require 'emagent-acp-prompt))
                   (emagent-acp--schedule-prompt-watchdog state))
                 (emagent-acp--refresh-mode-line state)
                 (funcall respond (or choice :cancel))))
              (choices (or switch-choices
                           '(("Cancel" . :cancel))))
              (prompt (or (and tool-call
                               (emagent-acp--switch-mode-display-title tool-call))
                          question))
              (preamble (emagent-acp--switch-mode-preamble
                         (or raw-tool-call tool-call))))
          (emagent-tools--buttons-prompt
           prompt choices buf after-response preamble)))
       ((and validation (eq (car validation) :deny))
        (emagent-log "permission denied by emagent gate: %s — %s" question (cdr validation))
        (emagent-acp--show-permission-decision state tool-call :deny)
        (funcall respond :deny))
       ((emagent-acp--permission-gate-auto-approve-p state tool-call validation fingerprint buf)
        (let ((stored (emagent-acp--permission-stored-auto-choice state fingerprint buf)))
          (emagent-log "permission auto-approve: %s (fingerprint %s)" question (or fingerprint "none"))
          (emagent-acp--show-permission-decision state tool-call (or stored :allow))
          (funcall respond :allow-once)))
       (t
        (unless (fboundp 'emagent-acp--prepare-interactive-context)
          (require 'emagent-acp-prompt))
        (emagent-acp--prepare-interactive-context state)
        (emagent-acp--clear-prompt-watchdog state)
        (let ((after-response
               (lambda (choice)
                 (when choice
                   (emagent-acp--show-permission-decision state tool-call choice))
                 (when (emagent-acp-state-busy state)
                   (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
                     (require 'emagent-acp-prompt))
                   (emagent-acp--schedule-prompt-watchdog state))
                 (emagent-acp--refresh-mode-line state)
                 (funcall respond (or choice :cancel)))))
          (if (and buf (buffer-live-p buf)
                   (emagent-acp-state-cb-permission state)
                   (with-current-buffer buf (emagent-chat--open-response-p)))
              (with-current-buffer buf
                (funcall (emagent-acp-state-cb-permission state)
                         question emagent-acp--permission-emagent-choices
                         after-response tool-call))
            (emagent-tools--buttons-prompt
             question emagent-acp--permission-emagent-choices buf after-response))))))))

(defun emagent-acp--permission-interactive-p (state)
  "Return non-nil when ACP permission dialogue may need user input.

Arguments: STATE."
  (and (not (emagent-acp-state-session-auto-approve state))
       (not (eq emagent-acp-auto-approve-permissions t))))

(defun emagent-acp--cancel-permission-request (state request)
  "Reply `cancelled' to permission REQUEST so the waiting agent does not hang.
Used when a request is abandoned by an error, interrupt, or teardown rather
than by a user decision.

Arguments: STATE."
  (when-let ((request-id (map-elt request 'id)))
    (ignore-errors
      (emagent-acp-send-response
       :client (emagent-acp-state-client state)
       :response (emagent-acp-make-session-request-permission-response
                  :request-id request-id :cancelled t)))))

(defun emagent-acp--cancel-outstanding-permissions (state)
  "Reply `cancelled' to every queued permission request, then clear the queue.
Leaves `:permission-busy' untouched: an in-flight interactive prompt owns its
own response.  Call from interrupt/teardown so abandoned requests never leave
the agent blocked.

Arguments: STATE."
  (dolist (request (emagent-acp-state-permission-queue state))
    (emagent-acp--cancel-permission-request state request))
  (setf (emagent-acp-state-permission-queue state) nil))

(defun emagent-acp--schedule-permission-drain (state)
  "Run `emagent-acp--drain-permission-queue-now' outside the ACP process filter.

Arguments: STATE."
  (unless (or (emagent-acp-state-permission-drain-timer state)
              (emagent-acp-state-permission-busy state))
    (setf (emagent-acp-state-permission-drain-timer state)
              (run-at-time 0 nil
                           (lambda ()
                             (setf (emagent-acp-state-permission-drain-timer state) nil)
                             (emagent-acp--drain-permission-queue-now state))))))

(defun emagent-acp--drain-permission-queue-now (state)
  "Process queued permission requests without recursive auto-approve nesting.

For auto-deny/auto-approve: handle at most
`emagent-acp-permission-drain-batch-size' requests, then reschedule so a flood
of MCP permissions cannot peg the Emacs command loop.  For interactive
prompts: insert one dialog and return; the button callback schedules the
next drain.

Arguments: STATE."
  (if (and (emagent-acp-state-permission-queue state)
           (active-minibuffer-window))
      ;; Minibuffer is active — inserting a dialog would conflict.  Poll.
      (unless (or (emagent-acp-state-permission-drain-timer state)
                  (emagent-acp-state-permission-busy state))
        (setf (emagent-acp-state-permission-drain-timer state)
              (run-at-time 0.3 nil
                           (lambda ()
                             (setf (emagent-acp-state-permission-drain-timer state) nil)
                             (emagent-acp--drain-permission-queue-now state)))))
    (let ((batch 0)
          (limit (max 1 emagent-acp-permission-drain-batch-size))
          (interactive (emagent-acp--permission-interactive-p state)))
      (while (and (emagent-acp-state-permission-queue state)
                  (not (emagent-acp-state-permission-busy state))
                  (or interactive (< batch limit)))
        (setq batch (1+ batch))
        (let ((request (car (emagent-acp-state-permission-queue state))))
          (setf (emagent-acp-state-permission-queue state)
                (cdr (emagent-acp-state-permission-queue state)))
          (setf (emagent-acp-state-permission-busy state) t)
          (emagent-acp--refresh-mode-line state)
          (condition-case err
              (emagent-acp--handle-one-permission
               :state state
               :emagent-acp-request request
               :on-complete
               (lambda ()
                 ;; Auto-approve runs this before handle-one returns (busy
                 ;; clears; the while loop continues).  Interactive prompts
                 ;; run this later from the button callback.
                 (setf (emagent-acp-state-permission-busy state) nil)
                 (emagent-acp--refresh-mode-line state)
                 (condition-case cont-err
                     (progn
                       (unless (fboundp 'emagent-acp--maybe-complete-deferred-prompt)
                         (require 'emagent-acp-prompt))
                       (emagent-acp--maybe-complete-deferred-prompt state)
                       (when (and (emagent-acp-state-permission-queue state)
                                  (emagent-acp--permission-interactive-p state))
                         (emagent-acp--schedule-permission-drain state)))
                   ((error quit)
                    (emagent-log "permission on-complete error: %s"
                                 (error-message-string cont-err))))))
            ((error quit)
             ;; Request was popped but not answered: cancel so the agent is
             ;; not left blocked, then continue the iterative drain.
             (emagent-log "permission handler error: %s" (error-message-string err))
             (emagent-acp--cancel-permission-request state request)
             (setf (emagent-acp-state-permission-busy state) nil)
             (emagent-acp--refresh-mode-line state)
             (when (and (emagent-acp-state-permission-queue state)
                        (emagent-acp--permission-interactive-p state))
               (emagent-acp--schedule-permission-drain state))))))
      ;; Yield after an auto-approve batch so redisplay can run.
      (when (and (not interactive)
                 (emagent-acp-state-permission-queue state)
                 (not (emagent-acp-state-permission-busy state)))
        (emagent-acp--schedule-permission-drain state)))))

(defun emagent-acp--drain-permission-queue (state)
  "Process queued permission requests one at a time.

Interactive prompts are deferred to the next event cycle so
`recursive-edit' never runs inside the ACP process filter.

Arguments: STATE."
  (when (emagent-acp-state-permission-queue state)
    (if (emagent-acp--permission-interactive-p state)
        (emagent-acp--schedule-permission-drain state)
      (emagent-acp--drain-permission-queue-now state))))

(defun emagent-acp--hydrate-session-permissions (state session-id)
  "Load ~/.emagent session permissions for SESSION-ID into STATE."
  (when (and session-id (not (string-empty-p session-id)))
    (setf (emagent-acp-state-permission-auto-allow state)
              (copy-sequence (emagent-permissions-session-fingerprints session-id)))
    (when (emagent-permissions-session-auto-approve-p session-id)
      (setf (emagent-acp-state-session-auto-approve state) t))))

(defun emagent-acp--permission-option-deny-p (opt)
  "Return non-nil when OPT is a deny-type ACP permission option."
  (let ((kind (downcase (or (map-elt opt 'kind) "")))
        (id (downcase (or (map-elt opt 'optionId) "")))
        (name (downcase (or (map-elt opt 'name) ""))))
    (or (member kind '("deny" "deny_once" "deny-once" "reject"))
        (member id '("deny" "deny_once" "deny-once" "no" "reject"))
        (string-match-p "deny\\|reject" id)
        (string-match-p "\\`\\(?:deny\\|no\\|reject\\)" name))))

(defun emagent-acp--permission-option-always-id (options)
  "Return allow_always/allow-always optionId from OPTIONS, or nil."
  (when options
    (or (map-elt (seq-find (lambda (opt)
                             (let ((id (downcase (or (map-elt opt 'optionId) ""))))
                               (member id '("allow_always" "allow-always"))))
                   options)
                'optionId)
        (map-elt (seq-find (lambda (opt)
                             (let ((kind (downcase (or (map-elt opt 'kind) ""))))
                               (member kind '("allow_always" "allow-always"))))
                   options)
                'optionId))))

(defconst emagent-acp--permission-acp-allow-prefer
  '("allow_once" "allow-once" "run_once" "yes" "run" "allow")
  "ACP optionIds emagent may return to the agent (never allow_always).")

(defconst emagent-acp--permission-emagent-choices
  '(("Allow" . :allow-once)
    ("Allow for session" . :allow-session)
    ("Allow always" . :allow-always)
    ("Allow all (session)" . :allow-all)
    ("Deny" . :deny))
  "User-facing permission choices handled by emagent, not the external agent.")

(defun emagent-acp--permission-acp-allow-id (options)
  "Return a one-shot allow optionId from OPTIONS, or nil; never allow_always.

emagent always answers the agent one-shot and remembers durable grants on its
own side (~/.emagent), so a user's \"Allow once\" can never become a permanent
agent-side whitelist.  If the agent offers no one-shot allow option, return nil
so the request is cancelled (fail-closed) rather than escalated to allow_always."
  (or (map-elt (seq-find (lambda (opt)
                           (let ((id (downcase (or (map-elt opt 'optionId) ""))))
                             (and id (member id emagent-acp--permission-acp-allow-prefer))))
                 options)
              'optionId)
      (let ((fallback (map-elt (seq-find #'emagent-acp--permission-option-allow-p options)
                               'optionId)))
        (when (and fallback
                   (not (member (downcase fallback) '("allow_always" "allow-always"))))
          fallback))
      (progn
        (when (emagent-acp--permission-option-always-id options)
          (emagent-log "permission: agent offers only allow_always; refusing to escalate a one-shot grant, cancelling"))
        nil)))

(defun emagent-acp--permission-acp-deny-id (options)
  "Return a deny optionId from OPTIONS, or nil."
  (map-elt (seq-find #'emagent-acp--permission-option-deny-p options) 'optionId))

(defun emagent-acp--switch-mode-choices (options)
  "Return (NAME . OPTION-ID) pairs from switch_mode OPTIONS."
  (let (choices)
    (dolist (opt (append options nil))
      (when-let* ((id (map-elt opt 'optionId))
                  ((and (stringp id) (not (string-empty-p id))))
                  (name (or (map-elt opt 'name) id))
                  ((stringp name)))
        (push (cons name id) choices)))
    (nreverse choices)))

(defun emagent-acp--switch-mode-plan-text (tool-call)
  "Return plan text from switch_mode TOOL-CALL content blocks, or nil."
  (when-let ((blocks (append (map-elt tool-call 'content) nil)))
    (let (parts)
      (dolist (block blocks)
        (let* ((inner (or (map-elt block 'content) block))
               (text (or (map-elt inner 'text)
                         (map-elt block 'text)
                         (and (stringp inner) inner))))
          (when (and (stringp text) (not (string-empty-p (string-trim text))))
            (push (string-trim text) parts))))
      (when parts
        (string-join (nreverse parts) "\n\n")))))

(defun emagent-acp--switch-mode-preamble (tool-call)
  "Return an org quote preamble for switch_mode TOOL-CALL plan text."
  (when-let ((text (emagent-acp--switch-mode-plan-text tool-call)))
    (concat "#+begin_quote\n" text "\n#+end_quote\n")))

(defconst emagent-acp--subcommand-programs
  '("git" "npm" "npx" "pnpm" "yarn" "docker" "docker-compose" "kubectl"
    "cargo" "go" "pip" "pip3" "gh" "brew" "apt" "apt-get" "systemctl"
    "make" "gradle" "mvn" "terraform" "helm" "dotnet" "rustup")
  "Programs whose first sub-verb changes what the command does.
For these, an execute fingerprint includes the sub-verb so a grant for e.g.
`git status' does not also auto-approve `git push --force'.")

(defun emagent-acp--execute-subverb (args)
  "Return the sub-verb in ARGS (a command's arguments), or nil.

Skips flags and the value a single-dash short flag consumes, so a global option
with a value (`git -C DIR', `kubectl -n NS', `docker -H HOST') does not make its
value masquerade as the subcommand.  Best-effort: a `-X' short flag is assumed
to take the next word as its value; a `--long' flag is assumed self-contained."
  (let ((consume nil) result)
    (cl-loop for w in args do
             (cond
              ((string-prefix-p "--" w) (setq consume nil))
              ((string-prefix-p "-" w) (setq consume t))
              (consume (setq consume nil))
              (t (setq result w) (cl-return))))
    result))

(defun emagent-acp--leaf-fingerprint (leaf)
  "Return the execute-fingerprint token for one leaf shell command LEAF, or nil.

The token is the program name, plus the sub-verb for
`emagent-acp--subcommand-programs' (so `git status' and `git push' differ).
Comments and empty leaves yield nil.  The program is the first
whitespace-delimited word after dropping leading VAR=VALUE assignments; a plain
whitespace split (rather than a shell tokenizer) is used because patterns like
`grep \"a\\|b\"' confuse `split-string-shell-command'."
  (let* ((words (emagent-policy-match--strip-leading-assignments
                 (split-string (string-trim leaf) "[[:space:]]+" t)))
         (program (car words)))
    (cond
     ((null program) nil)
     ((string-prefix-p "#" program) nil)
     ((member program emagent-acp--subcommand-programs)
      (if-let ((verb (emagent-acp--execute-subverb (cdr words))))
          (format "%s:%s" program verb)
        program))
     (t program))))

(defun emagent-acp--execute-fingerprint (command)
  "Return the execute fingerprint for shell COMMAND.

Keyed on the sorted set of leaf program names within COMMAND (and the sub-verb
for `emagent-acp--subcommand-programs'), so a grant is scoped to the operations
involved rather than the exact argv.  Two commands that differ only in their
path/glob arguments — or in how a pipeline or `VAR=$(...)' assignment is
composed — share one fingerprint, so a single \"Allow for session\" covers
both.  A plain single command keeps its former `execute:PROGRAM[:VERB]' key.
Policy rules still block dangerous commands regardless of any grant."
  (let* ((leaves (or (emagent-policy-shell-commands command)
                     (list (string-trim command))))
         (parts (delete-dups
                 (delq nil (mapcar #'emagent-acp--leaf-fingerprint leaves)))))
    (if parts
        (concat "execute:" (mapconcat #'identity (sort parts #'string<) ","))
      (format "execute:%s"
              (car (split-string (string-trim command) "[[:space:]]+" t))))))

(defun emagent-acp--permission-fingerprint (tool-call)
  "Return a stable fingerprint string for auto-allowing similar TOOL-CALLs.

Execute commands are keyed on the program name (and sub-verb for tools like
git/npm/docker, see `emagent-acp--subcommand-programs').  Policy rules still
block dangerous commands regardless.

Arguments: TOOL-CALL."
  (when tool-call
    (let* ((kind    (downcase (or (emagent-acp--tool-call-infer-kind tool-call) "")))
           (command (emagent-acp--tool-call-command-text tool-call))
           (form    (emagent-acp--tool-call-eval-form tool-call))
           (path    (emagent-acp--tool-call-path tool-call))
           (title   (or (map-elt tool-call 'title) "")))
      (cond
       ((and (string= kind "execute") (stringp command) (not (string-empty-p command)))
        (emagent-acp--execute-fingerprint command))
       (form
        (format "eval:%s" (secure-hash 'sha1 form)))
       ((and (member kind '("read" "write")) path)
        (format "%s:%s" kind path))
       ((not (string-empty-p title))
        (format "%s:%s" (if (string-empty-p kind) "tool" kind) title))
       (command
        (emagent-acp--execute-fingerprint command))
       (t "unknown")))))

(defun emagent-acp--tool-call-execute-p (tool-call)
  "Return non-nil when TOOL-CALL is an execute (shell) request."
  (let ((kind (emagent-acp--tool-call-infer-kind tool-call)))
    (and kind (member kind '("execute")))))

(defun emagent-acp--permission-validate (tool-call)
  "Return nil when TOOL-CALL passes emagent validation.
Otherwise (:deny . REASON) or (:confirm . REASON)."
  (or       (when-let ((form (emagent-acp--tool-call-eval-form tool-call)))
        (emagent-policy-check-elisp form))
      (when-let* ((command (and (emagent-acp--tool-call-execute-p tool-call)
                                (emagent-acp--tool-call-command-text tool-call))))
        (emagent-policy-check-shell command))))

(defun emagent-acp--permission-auto-allowed-p (state fingerprint chat-buffer)
  "Return non-nil when FINGERPRINT is auto-approved for STATE or CHAT-BUFFER."
  (or (emagent-acp-state-session-auto-approve state)
      (and fingerprint
           (or (member fingerprint (emagent-acp-state-permission-auto-allow state))
               (member fingerprint (emagent-permissions-global-fingerprints))
               (member fingerprint
                       (emagent-permissions-session-fingerprints
                        (emagent-acp-state-session-id state)))
               (and chat-buffer (buffer-live-p chat-buffer)
                    (with-current-buffer chat-buffer
                      (or (member fingerprint (emagent-session-allowed-permissions))
                          (member fingerprint
                                  (emagent-permissions-project-fingerprints
                                   (emagent-session-project-directory))))))))))

(defun emagent-acp--permission-stored-auto-choice (state fingerprint chat-buffer)
  "Return the stored user CHOICE that auto-approves FINGERPRINT, or nil.

Arguments: STATE, CHAT-BUFFER."
  (cond
   ((emagent-acp-state-session-auto-approve state) :allow-all)
   ((and fingerprint (member fingerprint (emagent-permissions-global-fingerprints)))
    :allow-always)
   ((and fingerprint
         (member fingerprint (emagent-acp-state-permission-auto-allow state)))
    :allow-session)
   ((and fingerprint
         (member fingerprint
                 (emagent-permissions-session-fingerprints
                  (emagent-acp-state-session-id state))))
    :allow-session)
   ((and fingerprint chat-buffer (buffer-live-p chat-buffer)
         (with-current-buffer chat-buffer
           (or (member fingerprint (emagent-session-allowed-permissions))
               (member fingerprint
                       (emagent-permissions-project-fingerprints
                        (emagent-session-project-directory))))))
    :allow-session)
   (t nil)))

(defun emagent-acp--show-permission-decision (state tool-call choice)
  "Update the permission tool-call line for TOOL-CALL with CHOICE.

Records CHOICE in STATE's :tool-call-decisions table so later tool_call_update
renders of the same line keep the decision suffix instead of dropping it."
  (when (and tool-call choice)
    (when-let* ((id (map-elt tool-call 'toolCallId))
                (update (emagent-acp--tool-call-update-from-request tool-call))
                (merged (emagent-acp--merged-tool-call-update state update))
                (base (emagent-acp--tool-call-label merged))
                (label (emagent-acp--permission-decision-label base choice))
                (buf (emagent-acp--chat-buffer state)))
      (when-let ((decisions (emagent-acp-state-tool-call-decisions state)))
        (puthash id choice decisions))
      (when-let ((cb (emagent-acp-state-cb-tool-call state)))
        (let ((spec (emagent-acp--tool-call-block-spec merged)))
          (with-current-buffer buf
            (funcall cb id label (car spec) (cdr spec))))))))

(defun emagent-acp--permission-gate-auto-approve-p (state tool-call validation fingerprint chat-buffer)
  "Return non-nil when emagent should approve without prompting.

FINGERPRINT identifies the request.  A policy :deny is never auto-approved.
A policy :confirm is auto-approved only
under \"Allow all (session)\" — the explicit user opt-out of prompting.  A
stored fingerprint grant (or the t/safe auto-approve modes) removes the prompt
only for policy-clean requests: it must not silence a :confirm, so e.g. an
`execute:rm' grant made for `rm foo.log' cannot auto-run `rm -rf ~'.

The `safe' mode auto-approves only `read'/`write' tool kinds; `execute', `eval',
and unknown/MCP tools always prompt under `safe' (an eval or MCP call is never
\"safe\" merely because it is not a shell command).

Arguments: STATE, TOOL-CALL, VALIDATION, CHAT-BUFFER."
  (let ((deny (and validation (eq (car validation) :deny)))
        (confirm (and validation (eq (car validation) :confirm))))
    (and (not deny)
         (or (emagent-acp-state-session-auto-approve state)
             (and (not confirm)
                  (or (emagent-acp--permission-auto-allowed-p state fingerprint chat-buffer)
                      (eq emagent-acp-auto-approve-permissions t)
                      (and (eq emagent-acp-auto-approve-permissions 'safe)
                           (member (emagent-acp--tool-call-infer-kind tool-call)
                                   '("read" "write")))))))))

(defun emagent-acp--permission-apply-choice (state fingerprint _chat-buffer choice)
  "Record user CHOICE for FINGERPRINT in STATE."
  (pcase choice
    (:allow-all
     (setf (emagent-acp-state-session-auto-approve state) t)
     (when-let ((session-id (emagent-acp-state-session-id state)))
       (emagent-permissions-set-session-auto-approve session-id))
     (emagent-log "permission: allow all (session)"))
    (:allow-session
     (when fingerprint
       (setf (emagent-acp-state-permission-auto-allow state)
                 (append (emagent-acp-state-permission-auto-allow state)
                         (list fingerprint)))
       (when-let ((session-id (emagent-acp-state-session-id state)))
         (emagent-permissions-add-session-fingerprint session-id fingerprint))))
    (:allow-always
     (when fingerprint
       (emagent-permissions-add-global-fingerprint fingerprint)))
    (_ nil)))

(defun emagent-acp--permission-approved-choice-p (choice)
  "Return non-nil when user CHOICE approves the request."
  (memq choice '(:allow-once :allow-session :allow-always :allow-all)))

(defun emagent-acp--permission-option-allow-p (opt)
  "Return non-nil when OPT is an allow-type ACP permission option."
  (let ((kind (downcase (or (map-elt opt 'kind) "")))
        (id (downcase (or (map-elt opt 'optionId) "")))
        (name (downcase (or (map-elt opt 'name) ""))))
    (or (member kind '("allow" "allow_once" "allow_always" "allow-once" "allow-always"))
        (member id '("allow_once" "allow-once" "allow_always" "allow-always" "allow" "yes" "run" "run_once"))
        (string-match-p "allow" id)
        (string-match-p "\\`\\(?:allow\\|yes\\|run\\)" name))))

(defun emagent-acp--tool-call-shell-needs-confirm-p (tool-call)
  "Return non-nil when TOOL-CALL is execute and policy requires confirmation."
  (when-let ((command (and (emagent-acp--tool-call-execute-p tool-call)
                            (emagent-acp--tool-call-command-text tool-call))))
    (emagent-policy-shell-needs-confirm-p command)))

(defun emagent-acp--permission-tool-call (state tool-call)
  "Return TOOL-CALL merged with session inputs and provider enrichment.

Cursor tool-call notifications skip sync store.db lookups (they freeze
Emacs); permission prompts still enrich once here because the user is
already waiting on the dialog.

Arguments: STATE, TOOL-CALL."
  (when tool-call
    (let* ((update (or (emagent-acp--tool-call-update-from-request tool-call)
                       tool-call))
           (merged (emagent-acp--merged-tool-call-update state update))
           (enriched (emagent-acp--provider-enrich-tool-call state merged)))
      ;; Cursor's provider enrich is intentionally a no-op on the hot path.
      ;; A single sync store.db read is acceptable when showing a permission
      ;; dialog so the user sees the real command/path.
      (if (and (eq (emagent-acp--provider-symbol state) 'cursor)
               (fboundp 'emagent-cursor-enrich-tool-call-update)
               (emagent-acp-state-session-id state))
          (emagent-cursor-enrich-tool-call-update
           (emagent-acp-state-session-id state) enriched)
        enriched))))

(defun emagent-acp--tool-call-write-content-block (tool-call raw _detail path)
  "Return an Allow-edit org block for TOOL-CALL's write to PATH from RAW."
  (when (and path (not (string-empty-p path)))
    (let* ((data (emagent-acp--tool-call-normalize-data raw))
           (resolved (emagent-tools--root-directory path))
           (heading (format "Allow edit: %s" (file-name-nondirectory resolved))))
      (if-let ((diff (when data
                       (emagent-acp--tool-call-edit-diff-string
                        path data (map-elt tool-call 'toolCallId)))))
          (format "** %s\n#+BEGIN_SRC diff\n%s\n#+END_SRC" heading diff)
        (if-let ((proposed (emagent-acp--tool-call-proposed-content path data)))
            (let ((lang (or (file-name-extension resolved) "text")))
              (format "** %s\n#+BEGIN_SRC %s\n%s\n#+END_SRC"
                      heading lang
                      (substring proposed 0 (min (length proposed) 4000))))
          (format "** Allow edit\n= %s =" resolved))))))

(defun emagent-acp--tool-call-content-block (tool-call)
  "Return an org subsection string for the permission prompt, or nil.
For eval, shell, and edit tool calls, return a code block with the payload.
Edit prompts prefer a unified diff; patch edits fall back to a hunk preview.

Arguments: TOOL-CALL."
  (when tool-call
    (or (when-let ((form (emagent-acp--tool-call-eval-form tool-call)))
          (format "** Allow eval\n#+BEGIN_SRC elisp\n%s\n#+END_SRC" form))
        (let* ((kind (emagent-acp--tool-call-infer-kind tool-call))
               (raw (or (map-elt tool-call 'rawInput)
                        (map-elt tool-call 'arguments)))
               (command (emagent-acp--tool-call-command-text tool-call))
               (detail (emagent-acp--tool-call-detail-from-tool-call tool-call)))
          (when kind
            (cond
             ((member kind '("execute" ""))
              (when command
                (if-let ((heredoc (emagent-acp--tool-call-heredoc-script command)))
                    (format "** Allow execute\n#+BEGIN_SRC %s\n%s\n#+END_SRC"
                            (car heredoc) (cdr heredoc))
                  (format "** Allow execute\n#+BEGIN_SRC sh\n%s\n#+END_SRC" command))))
             ((emagent-acp--tool-call-write-kind-p kind)
              (or (when-let ((path (emagent-acp--tool-call-write-path
                                    tool-call raw detail)))
                    (emagent-acp--tool-call-write-content-block
                     tool-call raw detail path))
                  (when command
                    (format "** Allow edit\n#+BEGIN_SRC sh\n%s\n#+END_SRC"
                            command))))
             (t nil)))))))

(provide 'emagent-acp-permit)
;;; emagent-acp-permit.el ends here
