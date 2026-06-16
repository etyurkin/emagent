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
ANTHROPIC_API_KEY here if you do not export it in your shell profile."
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

(cl-defun emagent-claude-make-client (&key context-buffer)
  "Create an ACP client for Claude using CONTEXT-BUFFER."
  (emagent-claude-check-command)
  (emagent-acp-make-client :context-buffer context-buffer
                   :command (emagent-claude-command)
                   :command-params (emagent-claude-command-params)
                   :environment-variables emagent-claude-environment))

(provide 'emagent-claude)

;;; emagent-claude.el ends here
