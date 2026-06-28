;;; emagent-claude.el --- Claude (Agent SDK) ACP provider for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'emagent-acp-protocol)

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
(see `emagent-chat--session-directory' / #+EMAGENT_PROJECT)."
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

(provide 'emagent-claude)

;;; emagent-claude.el ends here
