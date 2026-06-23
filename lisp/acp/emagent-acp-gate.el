;;; emagent-acp-gate.el --- External tool gate detection for emagent  -*- lexical-binding: t; -*-

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

;; Detecting and reporting when the agent binary is a non-emagent gateway
;; (Claude Agent SDK, Cursor CLI) that may enforce its own tool permissions
;; beyond ACP session/request_permission.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)

(defun emagent-acp--agent-launch-string (state)
  "Return the agent argv as a single shell-like string, or nil."
  (when-let ((client (map-elt state :client))
             (cmd (map-elt client :command)))
    (string-trim
     (mapconcat #'identity
                (delq nil (cons cmd (append (map-elt client :command-params) nil)))
                " "))))

(defun emagent-acp--external-refusal-text-p (text)
  "Return non-nil when TEXT looks like an out-of-band tool refusal message."
  (let ((s (downcase text)))
    (and (not (string-empty-p s))
         (or (string-search "user refused permission" s)
             (string-search "refused permission to run tool" s)
             (string-search "permission to run tool was denied" s)
             (string-search "tool use was denied" s)))))

(defun emagent-acp--external-tool-gate-add (state reason)
  "Record REASON (a symbol) in STATE's external-tool-gate hint list."
  (unless (memq reason (map-elt state :external-tool-gate-reasons))
    (map-put! state :external-tool-gate-reasons
              (cons reason (map-elt state :external-tool-gate-reasons)))))

(defun emagent-acp--infer-external-tool-gate-from-agent (state)
  "Infer likely SDK-side tool gates from the agent executable (see defcustom)."
  (when emagent-acp-external-tool-gate-hints
    (when-let ((launch (emagent-acp--agent-launch-string state)))
      (cond
       ((string-match-p "claude-agent-acp" launch)
        (emagent-acp--external-tool-gate-add state 'claude-agent-sdk))
       ((string-match-p "cursor-agent" launch)
        (emagent-acp--external-tool-gate-add state 'cursor-agent-cli))))))

(defun emagent-acp--format-external-tool-gate-proactive-hint (reasons)
  "Return a log line for SDK/capability hints in REASONS, or nil."
  (let (parts)
    (when (memq 'claude-agent-sdk reasons)
      (push (concat "claude-agent-acp (Claude Agent SDK) may still enforce its own "
                    "tool approvals; emagent only answers ACP session/request_permission.")
            parts))
    (when (memq 'cursor-agent-cli reasons)
      (push (concat "cursor-agent may enforce separate CLI tool approvals; "
                    "emagent only answers ACP session/request_permission.")
            parts))
    (when (memq 'agent-capability-metadata reasons)
      (push (concat "The agent's initialize response included permission-related "
                    "capability metadata; check the agent/SDK for an extra approval layer.")
            parts))
    (when (and emagent-acp-auto-approve-permissions
               (or (memq 'claude-agent-sdk reasons)
                   (memq 'cursor-agent-cli reasons)
                   (memq 'agent-capability-metadata reasons)))
      (push (concat "With `emagent-acp-auto-approve-permissions' non-nil, Emacs auto-approves "
                    "ACP permission requests; that does not satisfy separate agent gates.")
            parts))
    (when parts
      (mapconcat #'identity (nreverse parts) "  "))))

(defun emagent-acp--maybe-log-external-tool-gate-proactive (state)
  "Log a one-time proactive hint after `initialize' when we inferred SDK gates."
  (when emagent-acp-external-tool-gate-hints
    (unless (map-elt state :external-tool-gate-proactive-logged)
      (when-let ((reasons (map-elt state :external-tool-gate-reasons))
                 (msg (emagent-acp--format-external-tool-gate-proactive-hint reasons)))
        (map-put! state :external-tool-gate-proactive-logged t)
        (emagent-log "emagent: external tool permission hint — %s" msg)))))

(defun emagent-acp--infer-external-tool-gate-from-initialize-response (state response)
  "If RESPONSE `agentCapabilities' mention permission-like keys, record a hint."
  (when emagent-acp-external-tool-gate-hints
    (when-let ((caps (map-elt response 'agentCapabilities)))
      (when (listp caps)
        (dolist (pair caps)
          (when (and (consp pair)
                     (symbolp (car pair))
                     (let ((case-fold-search t))
                       (string-match-p "permission\\|approval\\|policy"
                                       (symbol-name (car pair))))
                     (cdr pair))
            (emagent-acp--external-tool-gate-add state 'agent-capability-metadata)))))))

(defun emagent-acp--detect-external-refusal-in-text (state text)
  "If TEXT looks like a tool refusal, record it and maybe log once."
  (when (and emagent-acp-external-tool-gate-hints
             (emagent-acp--external-refusal-text-p text))
    (emagent-acp--external-tool-gate-add state 'observed-refusal-in-stream)
    (unless (map-elt state :external-tool-refusal-logged)
      (map-put! state :external-tool-refusal-logged t)
      (emagent-log (concat "emagent: agent output looks like a tool was refused "
                           "outside Emacs (ACP approval alone is not enough); "
                           "check the agent/SDK permission or sandbox settings.")))))

(provide 'emagent-acp-gate)
;;; emagent-acp-gate.el ends here
