;;; emagent-cursor.el --- Cursor provider config for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'emagent-acp-protocol)
(require 'emagent-mcp)

(defgroup emagent-cursor nil
  "Cursor provider configuration for emagent."
  :group 'emagent)

(defcustom emagent-cursor-acp-command
  '("cursor-agent" "acp")
  "Command and parameters for the Cursor ACP agent.

Uses Cursor's own ACP server (the registry `cursor' entry, `cursor-agent
acp'), which speaks ACP natively.  Cursor discovers the in-Emacs MCP server
from ~/.cursor/mcp.json (see `emagent-mcp-ensure-cursor-config')."
  :type '(repeat string)
  :group 'emagent-cursor)

(defcustom emagent-cursor-acp-extra-args
  '("--approve-mcps" "--force" "--sandbox" "disabled")
  "Extra arguments appended after \"acp\" when spawning cursor-agent.

Cursor documents these flags on the top-level agent command; passing them
here may still take effect depending on the CLI version.  They reduce
sandbox blocks on WebSearch, shell, and MCP tool calls in ACP sessions.
Set to nil to pass no extra flags."
  :type '(repeat string)
  :group 'emagent-cursor)

(defcustom emagent-cursor-environment nil
  "Environment variables for the Cursor ACP agent."
  :type '(repeat string)
  :group 'emagent-cursor)

(defconst emagent-cursor-install-hint
  "Install Cursor's CLI (provides `cursor-agent acp'): https://cursor.com/cli")

(defun emagent-cursor-command ()
  "Return the Cursor ACP command name."
  (car emagent-cursor-acp-command))

(defun emagent-cursor-command-params ()
  "Return the Cursor ACP command parameters."
  (append (cdr emagent-cursor-acp-command) emagent-cursor-acp-extra-args))

(defun emagent-cursor-check-command ()
  "Signal a clear error when the Cursor agent is missing."
  (unless (executable-find (emagent-cursor-command))
    (error "Cursor ACP agent %s not found on PATH.\n%s"
           (emagent-cursor-command) emagent-cursor-install-hint)))

(defun emagent-cursor--environment (context-buffer)
  "Return env vars for the Cursor agent, including the per-session MCP token.

Cursor discovers MCP servers from ~/.cursor/mcp.json, whose emagent url
interpolates ${env:EMAGENT_SESSION_TOKEN}.  Setting it per buffer routes each
invocation to its own in-Emacs MCP session."
  (let ((token (with-current-buffer context-buffer (emagent-mcp-buffer-token))))
    (append emagent-cursor-environment
            (list (format "EMAGENT_SESSION_TOKEN=%s" token)))))

(cl-defun emagent-cursor-make-client (&key context-buffer)
  "Create an ACP client for Cursor using CONTEXT-BUFFER."
  (emagent-cursor-check-command)
  (emagent-mcp-ensure-cursor-config)
  (emagent-acp-make-client :context-buffer context-buffer
                   :command (emagent-cursor-command)
                   :command-params (emagent-cursor-command-params)
                   :environment-variables (emagent-cursor--environment context-buffer)))

(provide 'emagent-cursor)

;;; emagent-cursor.el ends here
