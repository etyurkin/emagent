;;; emagent-acp-custom.el --- ACP customization for emagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Defcustoms, defconsts, and obsolete aliases for the ACP layer.
;; Purely declarative configuration with no runtime dependencies.

;;; Code:

;; Backward compatibility (aliases before their referents).
(define-obsolete-variable-alias 'emagent-acp-emacs-native 'emagent-acp-prefer-emacs "0.1.0")
(define-obsolete-variable-alias 'emagent-acp-emacs-only 'emagent-acp-prefer-emacs "0.1.0")

(defcustom emagent-acp-prefer-emacs t
  "Instruct the agent to prefer emagent and Emacs tools, with external fallback.

When non-nil (default), emagent tells the agent to reach for emagent MCP tools
and Emacs Lisp first, but still allows Claude Code built-in tools, plugin slash
commands, and forwarded MCP gateways when Emacs cannot do the job.  File search
uses pure Emacs grep rather than ripgrep.  Keep `emagent-acp-file-access'
enabled so ACP file read/write route through Emacs buffers."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-file-access t
  "Route ACP file read/write through Emacs file tools.

When non-nil (default), agent read_file and write_file calls run
`emagent-tools--read-file-content' and `emagent-tools--write-file-content'
instead of cursor-agent's own file tools.  That matches agent-shell and
avoids macOS \"access data from other apps\" prompts on normal project trees.

Emagent still refuses iCloud and ~/Library/Containers paths to avoid separate
iCloud Drive prompts.  Set to nil only if you prefer the agent's own file tools."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-auto-approve-permissions nil
  "Automatically approve ACP session permission prompts at the emagent gate.

When nil (default), emagent prompts in the chat buffer unless the user has
already allowed the request fingerprint for this ACP session
(`emagent-permissions-directory'/sessions), globally (\"Allow always\"), per
project directory (projects/), or in a legacy buffer header
(#+EMAGENT_ALLOWED_PERMISSIONS).

When `safe', read and write tools are auto-approved without prompting.
Execute (shell) commands are inspected for destructive operations — rm,
dd, formatting, and similar — and only prompted then.  Harmless
commands like `mvn compile` or `ls` pass through without prompting.

When t, all permission prompts that pass emagent validation are
auto-approved without user interaction.

Emagent always replies to the agent with a one-shot allow optionId
(never allow_always), so every tool call still arrives at the emagent
gate for validation even after the user chooses \"Allow always\"."
  :type '(choice
          (const :tag "Prompt for all permissions" nil)
          (const :tag "Auto-approve safe tools, prompt only for destructive shell commands" safe)
          (const :tag "Auto-approve all permissions" t))
  :group 'emagent)

(defcustom emagent-acp-confirm-fs-writes nil
  "When non-nil, require diff + Allow before each ACP fs/write_text_file.

When nil (default), Emacs writes immediately after path checks, so file edits
are not blocked by a second in-editor prompt.  ACP `session/request_permission'
is unchanged; see `emagent-acp-auto-approve-permissions'."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-external-tool-gate-hints t
  "When non-nil, detect extra agent-side tool permission layers and log hints.

Emacs only answers ACP `session/request_permission' (see
`emagent-acp-auto-approve-permissions').  Claude Agent SDK, Cursor, and
similar stacks may still block tools afterward.  When this is non-nil,
emagent records hints after `initialize' (from the agent command line and
optional capability metadata) and when refusal-shaped text appears in
streamed chunks or tool-call labels, then logs to `emagent-log-buffer-name'."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-stream-to-buffer nil
  "When non-nil, stream agent chunks into the chat buffer while a prompt is busy.

Disabled by default because interleaved ACP notifications can finalize before
the full reply arrives.  Thinking and answers are rendered once the prompt
completes."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-thought-progress 'buffer
  "How to surface agent reasoning from `agent_thought_chunk' updates.

- nil — silent in `emagent-log-buffer-name' (Thinking still appears on finish)
- buffer — stream and show Thinking in the chat buffer (default)
- minimal — truncated one-line log entries in `emagent-log-buffer-name'
- trail — log entries keeping the sentence tail visible
- both — Thinking block in the chat buffer and minimal log lines"
  :type '(choice (const :tag "Silent" nil)
                 (const :tag "Chat buffer" buffer)
                 (const :tag "Log per sentence" minimal)
                 (const :tag "Log sentence tail" trail)
                 (const :tag "Chat buffer and log" both))
  :group 'emagent)

(defcustom emagent-acp-render-delay 0.05
  "Seconds to wait after the last prompt chunk before rendering the response."
  :type 'number
  :group 'emagent)

(defcustom emagent-acp-message-drain-batch-size 1
  "ACP wire messages handled per timer event before yielding to Emacs.

1 keeps the UI responsive during heavy agent output.  Raise only if you
prefer throughput and accept longer stretches without timer service."
  :type 'integer
  :group 'emagent)

(defcustom emagent-log-agent-stderr nil
  "When non-nil, log filtered cursor-agent stderr to `emagent-log-buffer-name'."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-watchdog-timeout 300
  "Seconds of inactivity before the prompt watchdog fires.

The watchdog resets on each tool-call notification, so this measures idle
time since the last tool call, not total prompt duration.  Increase if your
agent regularly makes long chains of tool calls."
  :type 'integer
  :group 'emagent)

(defcustom emagent-acp-trace nil
  "Log ACP wire events to `emagent-log-buffer-name'.

Shows outgoing methods, each `session/update' type with payload size, and
when `session/prompt' completes.  Also enables `emagent-acp-logging-enabled' for
the full wire log in the ACP logs buffer (`emagent-acp-logs-buffer')."
  :type 'boolean
  :group 'emagent)

(defconst emagent-acp-auto-model-id "auto"
  "Model id used by cursor-agent-acp for automatic model selection.

Claude and other agents that do not advertise this id fall back to the
session's current model instead.")

(provide 'emagent-acp-custom)
;;; emagent-acp-custom.el ends here
