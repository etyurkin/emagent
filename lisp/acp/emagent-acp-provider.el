;;; emagent-acp-provider.el --- ACP provider adapter registry  -*- lexical-binding: t; -*-

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
;; Provider helpers, connect/session gates, and Claude ACP wiring.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-log)

(defvar emagent-acp--provider-specs (make-hash-table :test 'eq)
  "Hash table mapping provider symbol to adapter property list.")

(cl-defun emagent-acp--register-provider (symbol &key detect enrich-tool-call
                                                 defer-tool-call-p
                                                 enqueue-tool-resolve
                                                 reset-tool-resolve
                                                 tool-resolve-active-p
                                                 generic-title-p
                                                 external-gate-reason
                                                 normalize-slash-prompt
                                                 context-usage-unavailable-p)
  "Register provider SYMBOL with adapter hooks.

Arguments: DETECT, ENRICH-TOOL-CALL, DEFER-TOOL-CALL-P,
ENQUEUE-TOOL-RESOLVE, RESET-TOOL-RESOLVE, TOOL-RESOLVE-ACTIVE-P,
GENERIC-TITLE-P, EXTERNAL-GATE-REASON, NORMALIZE-SLASH-PROMPT,
CONTEXT-USAGE-UNAVAILABLE-P."
  (puthash symbol
           (list :detect detect
                 :enrich-tool-call enrich-tool-call
                 :defer-tool-call-p defer-tool-call-p
                 :enqueue-tool-resolve enqueue-tool-resolve
                 :reset-tool-resolve reset-tool-resolve
                 :tool-resolve-active-p tool-resolve-active-p
                 :generic-title-p generic-title-p
                 :external-gate-reason external-gate-reason
                 :normalize-slash-prompt normalize-slash-prompt
                 :context-usage-unavailable-p context-usage-unavailable-p)
           emagent-acp--provider-specs))

(defun emagent-acp--provider-context-usage-unavailable-p (state)
  "Return non-nil when STATE's provider cannot report context-window usage."
  (and (emagent-acp--provider-hook state :context-usage-unavailable-p state) t))

(defun emagent-acp--provider-spec (state)
  "Return the adapter property list for STATE's provider, or nil."
  (when state
    (gethash (emagent-acp--provider-symbol state) emagent-acp--provider-specs)))

(defun emagent-acp--provider-symbol (state)
  "Return the provider symbol for STATE (`cursor' or `claude')."
  (or (emagent-acp-state-provider state)
      (cl-loop for sym being the hash-keys of emagent-acp--provider-specs
               for detect = (plist-get (gethash sym emagent-acp--provider-specs) :detect)
               when (and detect (funcall detect state))
               return sym
               finally return 'cursor)))

(defun emagent-acp--provider-hook (state prop &rest args)
  "Call provider hook PROP for STATE with ARGS, or nil when unset."
  (when-let ((fn (plist-get (emagent-acp--provider-spec state) prop)))
    (apply fn args)))

(defun emagent-acp--provider-enrich-tool-call (state update)
  "Return UPDATE enriched by the active provider adapter.

Arguments: STATE."
  (or (emagent-acp--provider-hook state :enrich-tool-call state update)
      update))

(defun emagent-acp--provider-defer-tool-call-p (state update)
  "Return non-nil when UPDATE tool-call display should wait for enrichment.

Arguments: STATE."
  (and (emagent-acp--provider-hook state :defer-tool-call-p state update)))

(defun emagent-acp--provider-enqueue-tool-resolve (state id &optional delay)
  "Queue tool-call ID for provider-specific arg resolution.

Arguments: STATE, DELAY."
  (emagent-acp--provider-hook state :enqueue-tool-resolve state id delay))

(defun emagent-acp--provider-reset-tool-resolve (state)
  "Clear provider-specific pending tool-call resolution state for STATE."
  (emagent-acp--provider-hook state :reset-tool-resolve state))

(defun emagent-acp--provider-tool-resolve-active-p (state)
  "Return non-nil while provider tool-call resolution is in flight.

Arguments: STATE."
  (and (emagent-acp--provider-hook state :tool-resolve-active-p state)))

(defun emagent-acp--provider-generic-title-p (state title)
  "Return non-nil when TITLE is too generic to show without provider detail.

Arguments: STATE."
  (and (emagent-acp--provider-hook state :generic-title-p title)))

(defun emagent-acp--provider-external-gate-reason (state)
  "Return a symbol naming this provider's external tool gate, or nil.

Arguments: STATE."
  (emagent-acp--provider-hook state :external-gate-reason state))

(defun emagent-acp--provider-normalize-slash-prompt (state text)
  "Return TEXT normalized for the active provider, or TEXT when unset.

Arguments: STATE."
  (or (emagent-acp--provider-hook state :normalize-slash-prompt text) text))

(defun emagent-acp--agent-launch-string (state)
  "Return the agent argv as a single shell-like string, or nil.

Arguments: STATE."
  (when-let ((client (emagent-acp-state-client state))
             (cmd (map-elt client :command)))
    (string-trim
     (mapconcat #'identity
                (delq nil (cons cmd (append (map-elt client :command-params) nil)))
                " "))))

(defun emagent-acp--external-refusal-text-p (text)
  "Return non-nil when TEXT resembles an out-of-band tool refusal message."
  (let ((s (downcase text)))
    (and (not (string-empty-p s))
         (or (string-search "user refused permission" s)
             (string-search "refused permission to run tool" s)
             (string-search "permission to run tool was denied" s)
             (string-search "tool use was denied" s)))))

(defun emagent-acp--external-tool-gate-add (state reason)
  "Record REASON (a symbol) in STATE's external-tool-gate hint list."
  (unless (memq reason (emagent-acp-state-external-tool-gate-reasons state))
    (setf (emagent-acp-state-external-tool-gate-reasons state)
              (cons reason (emagent-acp-state-external-tool-gate-reasons state)))))

(defun emagent-acp--infer-external-tool-gate-from-agent (state)
  "Infer likely SDK-side tool gates from the agent executable (see defcustom).

Arguments: STATE."
  (when emagent-acp-external-tool-gate-hints
    (when-let ((reason (emagent-acp--provider-external-gate-reason state)))
      (emagent-acp--external-tool-gate-add state reason))))

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
  "Log a one-time proactive hint after `initialize' when we inferred SDK gates.

Arguments: STATE."
  (when emagent-acp-external-tool-gate-hints
    (unless (emagent-acp-state-external-tool-gate-logged state)
      (when-let ((reasons (emagent-acp-state-external-tool-gate-reasons state))
                 (msg (emagent-acp--format-external-tool-gate-proactive-hint reasons)))
        (setf (emagent-acp-state-external-tool-gate-logged state) t)
        (emagent-log "emagent: external tool permission hint — %s" msg)))))

(defun emagent-acp--infer-external-tool-gate-from-initialize-response (state response)
  "If RESPONSE `agentCapabilities' mention permission-like keys, record a hint.

Arguments: STATE."
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
  "If TEXT resembles a tool refusal, record it and maybe log once.

Arguments: STATE."
  (when (and emagent-acp-external-tool-gate-hints
             (emagent-acp--external-refusal-text-p text))
    (emagent-acp--external-tool-gate-add state 'observed-refusal-in-stream)
    (unless (emagent-acp-state-external-tool-refusal-logged state)
      (setf (emagent-acp-state-external-tool-refusal-logged state) t)
      (emagent-log (concat "emagent: agent output looks like a tool was refused "
                           "outside Emacs (ACP approval alone is not enough); "
                           "check the agent/SDK permission or sandbox settings.")))))

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
  
  "Internal helper."'claude-agent-sdk)

(emagent-acp--register-provider
 'claude
 :detect #'emagent-acp-claude--detect-p
 :enrich-tool-call #'emagent-acp-claude--enrich-tool-call
 :external-gate-reason #'emagent-acp-claude--external-gate-reason)

(provide 'emagent-acp-provider)
;;; emagent-acp-provider.el ends here
