;;; emagent-claude.el --- Claude ACP provider for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.3.1
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
;; Claude (Anthropic Agent SDK) ACP provider configuration and adapter hooks.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-protocol)
(require 'emagent-chat)

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

;;;; Subagent discovery

(defconst emagent-claude--builtin-agent-names
  '("claude" "general-purpose" "Explore" "Plan" "statusline-setup")
  "Claude Code's built-in Task-delegation subagent types.

Mirrors `BUILTIN_AGENT_NAMES' in the installed claude-agent-acp bridge
\(acp-agent.js).  Not exposed over ACP, so this list can drift from the
installed bridge/SDK version -- update it if Claude Code's built-in
roster changes.")

(defun emagent-claude--frontmatter-field (key limit)
  "Return frontmatter KEY's value up to LIMIT, or nil.
Strips one layer of matching double or single quotes."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^%s:[ \t]*\\(.+?\\)[ \t]*$" (regexp-quote key)) limit t)
      (let ((value (match-string 1)))
        (if (and (> (length value) 1)
                 (memq (aref value 0) '(?\" ?\'))
                 (eq (aref value 0) (aref value (1- (length value)))))
            (substring value 1 -1)
          value)))))

(defun emagent-claude--agent-frontmatter (file)
  "Return (NAME . DESCRIPTION) parsed from FILE's YAML frontmatter, or nil.
Reads only the `name' and `description' keys between a leading `---'
delimiter pair; no general YAML parsing, matching Claude Code's subagent
file format."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (goto-char (point-min))
    (when (looking-at-p "^---[ \t]*$")
      (forward-line 1)
      (let ((limit (save-excursion
                     (if (re-search-forward "^---[ \t]*$" nil t)
                         (match-beginning 0)
                       (point-max)))))
        (let ((name (emagent-claude--frontmatter-field "name" limit))
              (description (emagent-claude--frontmatter-field "description" limit)))
          (when name (cons name description)))))))

(defun emagent-claude--builtin-agent-plists ()
  "Return built-in Claude subagents as slash-command plists."
  (mapcar (lambda (name)
            (emagent-chat--slash-command-plist name "Built-in Claude Code subagent"))
          emagent-claude--builtin-agent-names))

(defun emagent-claude--custom-agent-plists (directory)
  "Return subagent plists discovered under DIRECTORY/agents/*.md."
  (let ((agents-dir (and directory (expand-file-name "agents" directory))))
    (when (and agents-dir (file-directory-p agents-dir))
      (sort
       (delq nil
             (mapcar
              (lambda (file)
                (when-let ((parsed (ignore-errors (emagent-claude--agent-frontmatter file))))
                  (emagent-chat--slash-command-plist (car parsed) (cdr parsed))))
              (directory-files agents-dir t "\\.md\\'")))
       (lambda (a b) (string< (map-elt a 'name) (map-elt b 'name)))))))

(defun emagent-claude-agents (&optional project-dir)
  "Return Claude subagents for delegation: built-ins, then user, then project.

Arguments: PROJECT-DIR.  Later layers override earlier ones by name, so a
custom agent (e.g. under PROJECT-DIR/.claude/agents) can replace a
same-named built-in."
  (emagent-chat--merge-slash-commands
   (emagent-chat--merge-slash-commands
    (emagent-claude--builtin-agent-plists)
    (or (emagent-claude--custom-agent-plists (expand-file-name "~/.claude")) '()))
   (or (emagent-claude--custom-agent-plists
        (and project-dir (expand-file-name ".claude" project-dir)))
       '())))

(defun emagent-acp-claude--detect-p (state)
  "Return non-nil when STATE's agent is claude-agent-acp."
  (when-let ((launch (emagent-acp--agent-launch-string state)))
    (string-match-p "claude-agent-acp" launch)))

(defun emagent-acp-claude--enrich-tool-call (_state update)
  "Normalize Claude ACP tool-call UPDATE before display merging.
claude-agent-acp echoes rawInput fields as the title on tool_call_update:
title = rawInput.command for Bash, title = rawInput.description for Agent.
Strip the redundant title so the stored display name (e.g. \"Terminal\",
\"Task\") is kept and the rawInput field becomes the visible detail."
  (let* ((raw (or (map-elt update 'rawInput) (map-elt update 'arguments)))
         (title (map-elt update 'title)))
    (if (and title (listp raw)
             (let ((cmd (alist-get 'command raw))
                   (desc (alist-get 'description raw)))
               (or (and cmd (string= title cmd))
                   (and desc (string= title desc)))))
        (cons (cons 'title nil) update)
      update)))

(defun emagent-acp-claude--external-gate-reason (_state)
  "Return the Claude SDK external tool-gate reason symbol."
  'claude-agent-sdk)

(emagent-acp--register-provider
 'claude
 :make-client #'emagent-claude-make-client
 :detect #'emagent-acp-claude--detect-p
 :enrich-tool-call #'emagent-acp-claude--enrich-tool-call
 :external-gate-reason #'emagent-acp-claude--external-gate-reason)

(provide 'emagent-claude)
;;; emagent-claude.el ends here
