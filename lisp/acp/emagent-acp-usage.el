;;; emagent-acp-usage.el --- Session state queries and usage tracking  -*- lexical-binding: t; -*-

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
;; Provider registry, usage/progress, and model selection.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-protocol)
(require 'emagent-log)
(require 'emagent-session)

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

(defun emagent-acp-busy-p ()
  "Return non-nil when the current buffer's ACP session is processing a prompt."
  (and emagent-acp--session (emagent-acp-state-busy emagent-acp--session)))

(defun emagent-acp-turn-in-flight-p ()
  "Return non-nil while the session is busy or finishing a prompt.

Used to defer expensive org font-lock until the turn settles."
  (and emagent-acp--session
       (or (emagent-acp-state-busy emagent-acp--session)
           (emagent-acp-state-prompt-finishing emagent-acp--session))))

(defun emagent-acp-waiting-permission-p ()
  "Return non-nil while permission requests are queued or being answered."
  (and emagent-acp--session
       (emagent-acp--permission-pending-p emagent-acp--session)))

(defun emagent-acp-ready-p ()
  "Return non-nil when the current buffer's ACP session is connected and idle."
  (and emagent-acp--session (emagent-acp-state-ready emagent-acp--session)))

(defun emagent-acp-current-tool ()
  "Return the name of the tool currently running, or nil."
  (and emagent-acp--session (emagent-acp-state-current-tool emagent-acp--session)))

(defun emagent-acp-current-tool-kind ()
  "Return the kind of the running tool (\"read\", \"write\", \"execute\"), or nil."
  (and emagent-acp--session (emagent-acp-state-current-tool-kind emagent-acp--session)))

(defun emagent-acp-agent-rss ()
  "Return the agent process RSS in MB, or nil."
  (and emagent-acp--session (emagent-acp-state-agent-rss emagent-acp--session)))

(defun emagent-acp-context-usage ()
  "Return (USED . SIZE) context token counts for the current session, or nil."
  (when-let* ((state emagent-acp--session)
              (usage (emagent-acp-state-usage state))
              (used (map-elt usage :context-used))
              (size (map-elt usage :context-size)))
    (cons used size)))

(defun emagent-acp-context-usage-unavailable-p ()
  "Return non-nil when a connected session cannot report context usage.
Cursor does not expose context-window figures over ACP, so emagent has no data
to compute a percentage and the mode line shows `ctx:n/a' instead."
  (and emagent-acp--session
       (or (emagent-acp-state-busy emagent-acp--session)
           (emagent-acp-state-ready emagent-acp--session))
       (emagent-acp--provider-context-usage-unavailable-p emagent-acp--session)))

(defun emagent-acp-external-tool-gate-reasons ()
  "Return external tool-gate reason symbols for the current session, or nil.
See `emagent-acp-external-tool-gate-hints'."
  (and emagent-acp--session
       (emagent-acp-state-external-tool-gate-reasons emagent-acp--session)))

(defun emagent-acp--chat-buffer (state)
  "Return STATE's chat buffer if it is live, else nil.

A killed buffer must not be returned: timers and callbacks still hold STATE
after the user kills the chat buffer, and `with-current-buffer' on a dead
buffer signals \"Selecting deleted buffer\"."
  (let ((buf (emagent-acp-state-chat-buffer state)))
    (when (buffer-live-p buf)
      buf)))

(defun emagent-acp--session-cwd (state)
  
  "Internal helper for STATE."
  (if-let ((buf (emagent-acp--chat-buffer state)))
      (with-current-buffer buf (emagent-chat--session-directory))
    (user-error "Emagent chat buffer is no longer available")))

(defun emagent-acp--persist-session-id (state session-id)
  
  "Internal helper for STATE and SESSION-ID."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (let ((was-modified (buffer-modified-p)))
        (unwind-protect
            (emagent-session-set-id session-id)
          (set-buffer-modified-p was-modified))))))

(defun emagent-acp--saved-session-id (state)
  
  "Internal helper for STATE."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (emagent-session-id))))

(defun emagent-acp--saved-model-id (state)
  
  "Internal helper for STATE."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (emagent-session-model))))

(defun emagent-acp--persist-model-id (state model-id)
  
  "Internal helper for STATE and MODEL-ID."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (with-current-buffer buf
      (let ((was-modified (buffer-modified-p)))
        (unwind-protect
            (emagent-session-set-model model-id)
          (set-buffer-modified-p was-modified)))))
  ;; The status push from --refresh-mode-line re-renders the model label.
  (emagent-acp--refresh-mode-line state))

(defun emagent-acp--current-model-id (state models)
  "Return the current model id for STATE.

Prefer the session config-option current value, then MODELS'
`currentModelId', then the chat buffer's saved model."
  (or (map-elt
       (or (seq-find (lambda (option)
                       (equal "model" (map-elt option :category)))
                     (emagent-acp-state-config-options state))
           (seq-find (lambda (option)
                       (string= (map-elt option :id) "model"))
                     (emagent-acp-state-config-options state)))
       :current-value)
      (and models (map-elt models 'currentModelId))
      (emagent-acp--saved-model-id state)))

(defun emagent-acp--maybe-recover-stall (state)
  "Unstick a session that finished on the wire but left the buffer open.

Lazy-loads prompt/permission-queue so this leaf need not require them
\(those modules require this file\).

Arguments: STATE."
  (when (and state
             (emagent-acp-state-ready state)
             (not (emagent-acp-state-busy state)))
    (unless (fboundp 'emagent-acp--maybe-complete-deferred-prompt)
      (require 'emagent-acp))
    (emagent-acp--maybe-complete-deferred-prompt state)
    (when (emagent-acp-state-permission-queue state)
      (unless (fboundp 'emagent-acp--drain-permission-queue)
        (require 'emagent-acp-permit))
      (emagent-acp--drain-permission-queue state))))

(defun emagent-acp--status-snapshot (state)
  "Return a mode-line status plist computed from STATE.

Built entirely from STATE so it does not depend on the current buffer; the UI
renders from this snapshot instead of pulling session state back out of the ACP
layer (see `emagent-chat-set-status')."
  (let ((usage (emagent-acp-state-usage state)))
    (list :busy (and (emagent-acp-state-busy state) t)
          :waiting-permission (and (emagent-acp--permission-pending-p state) t)
          :ready (and (emagent-acp-state-ready state) t)
          :prompt-finishing (and (emagent-acp-state-prompt-finishing state) t)
          :tool (emagent-acp-state-current-tool state)
          :tool-kind (emagent-acp-state-current-tool-kind state)
          :rss (emagent-acp-state-agent-rss state)
          :model-id (and (emagent-acp-state-ready state)
                         (emagent-acp--current-model-id state nil))
          :ctx-usage (when-let ((used (and usage (map-elt usage :context-used)))
                                (size (map-elt usage :context-size)))
                       (cons used size))
          :ctx-unavailable (and (or (emagent-acp-state-busy state)
                                    (emagent-acp-state-ready state))
                                (emagent-acp--provider-context-usage-unavailable-p
                                 state))
          :mode-id (emagent-acp-state-session-mode-id state))))

(defun emagent-acp--refresh-mode-line (state)
  
  "Internal helper for STATE."
  (emagent-acp--maybe-recover-stall state)
  (when-let ((buffer (emagent-acp--chat-buffer state))
             (cb (emagent-acp-state-cb-status state)))
    (let ((snapshot (emagent-acp--status-snapshot state)))
      (with-current-buffer buffer
        (funcall cb snapshot)))))

(defun emagent-acp--agent-rss-mb (state)
  "Return the agent process RSS in MB via `process-attributes', or nil.

Arguments: STATE."
  (when-let* ((client (emagent-acp-state-client state))
              (proc (and client (map-elt client :process)))
              ((processp proc))
              (pid (process-id proc))
              ((> pid 0))
              (attrs (ignore-errors (process-attributes pid)))
              (rss-kb (alist-get 'rss attrs)))
    (round (/ (float rss-kb) 1024))))

(defun emagent-acp--start-rss-timer (state)
  "Start a repeating timer that refreshes :agent-rss in STATE every 15 s."
  (when-let ((old (emagent-acp-state-agent-rss-timer state)))
    (cancel-timer old))
  (setf (emagent-acp-state-agent-rss-timer state)
        (run-with-timer
         5 15
         (lambda ()
           (if (buffer-live-p (emagent-acp-state-chat-buffer state))
               (let ((mb (emagent-acp--agent-rss-mb state)))
                 (setf (emagent-acp-state-agent-rss state) mb)
                 (emagent-acp--refresh-mode-line state))
             (emagent-acp--stop-rss-timer state))))))

(defun emagent-acp--stop-rss-timer (state)
  "Cancel the RSS polling timer for STATE."
  (when-let ((timer (and state (emagent-acp-state-agent-rss-timer state))))
    (cancel-timer timer)
    (setf (emagent-acp-state-agent-rss-timer state) nil)))

(defun emagent-acp--usage-state (state)
  
  "Internal helper for STATE."
  (or (emagent-acp-state-usage state)
      (let ((usage (make-hash-table :test 'eq)))
        (puthash :context-used nil usage)
        (puthash :context-size nil usage)
        (puthash :total-tokens 0 usage)
        (setf (emagent-acp-state-usage state) usage)
        usage)))

(defun emagent-acp--usage-context-used (data)
  "Return cumulative context-window fill from DATA, or nil.
Per-turn input/prompt token counts are not context fill."
  (or (map-elt data 'contextUsed)
      (map-elt data 'used)
      (map-elt data 'contextWindowUsed)
      (map-elt data 'tokensUsed)))

(defun emagent-acp--usage-context-size (data)
  "Return context window size/limit from DATA, or nil."
  (or (map-elt data 'contextSize)
      (map-elt data 'contextLimit)
      (map-elt data 'contextWindow)
      (map-elt data 'size)
      (map-elt data 'maxTokens)))

(defun emagent-acp--save-usage-from-response (state emagent-acp-usage)
  "Update STATE usage from a prompt response usage field.

Arguments: EMAGENT-ACP-USAGE."
  (let ((usage (emagent-acp--usage-state state)))
    (when-let ((total (map-elt emagent-acp-usage 'totalTokens)))
      (map-put! usage :total-tokens total))
    (when-let ((used (emagent-acp--usage-context-used emagent-acp-usage)))
      (map-put! usage :context-used used))
    (when-let ((size (emagent-acp--usage-context-size emagent-acp-usage)))
      (map-put! usage :context-size size))
    (setf (emagent-acp-state-usage state) usage)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp--update-usage-from-notification (state emagent-acp-update)
  "Update STATE usage from a session/update usage_update payload.

Arguments: EMAGENT-ACP-UPDATE."
  (let ((usage (emagent-acp--usage-state state)))
    (when-let ((used (emagent-acp--usage-context-used emagent-acp-update)))
      (map-put! usage :context-used used))
    (when-let ((size (emagent-acp--usage-context-size emagent-acp-update)))
      (map-put! usage :context-size size))
    (setf (emagent-acp-state-usage state) usage)
    (emagent-acp--refresh-mode-line state)))

(defun emagent-acp--notify-user (_state message)
  "Append MESSAGE to `emagent-log-buffer-name'."
  (emagent-log "%s" message))

(defun emagent-acp--trace (format-string &rest args)
  "Append a trace line when `emagent-acp-trace' is non-nil.

Arguments: FORMAT-STRING, ARGS."
  (when emagent-acp-trace
    (apply #'emagent-log (cons (concat "acp: " format-string) args))))

(defun emagent-acp--progress (state message)
  "Show init stage MESSAGE in the minibuffer and refresh the mode line.

Arguments: STATE."
  (emagent-acp--notify-user state (format "emagent: %s" message))
  (emagent-acp--refresh-mode-line state))

(cl-defun emagent-acp--send-request (&key state request on-success on-failure)

  "Internal helper for STATE and REQUEST and ON-SUCCESS and ON-FAILURE."
  (let ((method (map-elt request :method)))
    (emagent-acp--trace "send %s" method)
    (emagent-acp-send-request
     :client (emagent-acp-state-client state)
     :request request
     :buffer (emagent-acp--chat-buffer state)
     :on-success
     (lambda (response)
       (emagent-acp--trace "recv %s ok" method)
       (when on-success (funcall on-success response)))
     :on-failure
     (lambda (error raw)
       (emagent-acp--trace "recv %s error: %s"
                           method
                           (or (map-elt error 'message) (format "%s" error)))
       (when on-failure (funcall on-failure error raw))))))

(defvar emagent-model-history nil
  "Minibuffer history for agent/model choices.")

(defun emagent-acp--auto-model-candidate (state models)
  "Return the agent's auto/default model id when advertised.

Arguments: STATE, MODELS."
  (or (and (emagent-acp--model-available-p "default[]" state models) "default[]")
      (and (emagent-acp--model-available-p emagent-acp-auto-model-id state models)
           emagent-acp-auto-model-id)))

(defun emagent-acp--normalize-config-option (emagent-acp-option)
  "Internal helper for EMAGENT-ACP-OPTION."
  `((:id . ,(map-elt emagent-acp-option 'id))
    (:name . ,(map-elt emagent-acp-option 'name))
    (:description . ,(map-elt emagent-acp-option 'description))
    (:category . ,(map-elt emagent-acp-option 'category))
    (:type . ,(map-elt emagent-acp-option 'type))
    (:current-value . ,(map-elt emagent-acp-option 'currentValue))
    (:options . ,(mapcar (lambda (emagent-acp-value)
                           `((:value . ,(map-elt emagent-acp-value 'value))
                             (:name . ,(map-elt emagent-acp-value 'name))
                             (:description . ,(map-elt emagent-acp-value 'description))))
                         (append (map-elt emagent-acp-option 'options) nil)))))

(defun emagent-acp--normalize-config-options (emagent-acp-config-options)
  
  "Internal helper for EMAGENT-ACP-CONFIG-OPTIONS."
  (mapcar #'emagent-acp--normalize-config-option
          (append emagent-acp-config-options nil)))

(defun emagent-acp--save-config-options (state emagent-acp-config-options)
  
  "Internal helper for STATE and EMAGENT-ACP-CONFIG-OPTIONS."
  (when emagent-acp-config-options
    (setf (emagent-acp-state-config-options state)
              (emagent-acp--normalize-config-options emagent-acp-config-options))))

(defun emagent-acp--config-options (state)
  
  "Internal helper for STATE."
  (emagent-acp-state-config-options state))

(defun emagent-acp--config-option-by-category (state category)
  
  "Internal helper for STATE and CATEGORY."
  (seq-find (lambda (option)
              (equal category (map-elt option :category)))
            (emagent-acp--config-options state)))

(defun emagent-acp--model-config-option (state)
  
  "Internal helper for STATE."
  (or (emagent-acp--config-option-by-category state "model")
      (seq-find (lambda (option)
                  (string= (map-elt option :id) "model"))
                (emagent-acp--config-options state))))

(defun emagent-acp--config-option-value-name (option value)
  
  "Internal helper for OPTION and VALUE."
  (or (map-elt (seq-find (lambda (candidate)
                           (equal value (map-elt candidate :value)))
                         (map-elt option :options))
               :name)
      value))

(defun emagent-acp--config-option-set-value (state config-id value)
  
  "Internal helper for STATE and CONFIG-ID and VALUE."
  (dolist (option (emagent-acp--config-options state))
    (when (equal config-id (map-elt option :id))
      (setf (map-elt option :current-value) value))))

(defun emagent-acp--read-labeled-choice (prompt labels &optional _default-label)
  "Read one of LABELS with `completing-read'.

Uses a completion table so `value' accepts only an exact label — the
highlighted Vertico candidate is returned, not a partial filter string.
LABELS may include text properties (e.g. faces) for display.

Arguments: PROMPT."
  (let* ((labels (copy-sequence labels))
         (plain-labels (mapcar #'substring-no-properties labels))
         (collection (lambda (input _predicate action)
                       (pcase action
                         ('value
                          (let ((input (substring-no-properties input)))
                            (when (member input plain-labels) (list input))))
                         (_ (all-completions input labels)))))
         (selection (substring-no-properties
                     (completing-read prompt collection nil t nil
                                      'emagent-model-history))))
    (unless (member selection plain-labels)
      (user-error "Invalid choice: %s" selection))
    selection))

(defun emagent-acp--model-entry-id (entry)
  
  "Internal helper for ENTRY."
  (or (map-elt entry 'modelId) (map-elt entry 'model-id) (map-elt entry 'value)))

(defun emagent-acp--model-entry-name (entry)
  
  "Internal helper for ENTRY."
  (or (map-elt entry 'name) (emagent-acp--model-entry-id entry)))

(defun emagent-acp--model-entries-from-response (response)
  "Return a list of ((:model-id . ID) (:name . NAME)) from a session/new RESPONSE."
  (when response
    (let* ((models (emagent-acp--models-from-response response))
           (entries (append (emagent-acp--available-model-entries models) nil)))
      (delq nil
            (mapcar (lambda (entry)
                      (let ((id (or (map-elt entry :model-id)
                                    (map-elt entry :value)
                                    (emagent-acp--model-entry-id entry)))
                            (name (or (map-elt entry :name)
                                      (emagent-acp--model-entry-name entry))))
                        (when id
                          `((:model-id . ,id) (:name . ,name)))))
                    entries)))))

(defun emagent-acp--models-from-response (response)
  "Extract model list from session/new RESPONSE.
Prefer `configOptions' (canonical ids for set-config-option)
over legacy `models'."
  (or (when-let* ((options (map-elt response 'configOptions))
                  (model-opt
                   (seq-find (lambda (option)
                               (or (string= (map-elt option 'category) "model")
                                   (string= (map-elt option 'id) "model")))
                             options)))
        (list (cons 'availableModels (map-elt model-opt 'options))
              (cons 'currentModelId (map-elt model-opt 'currentValue))))
      (map-elt response 'models)))

(defun emagent-acp--available-model-entries (models)
  
  "Internal helper for MODELS."
  (or (map-elt models 'availableModels) (map-elt models 'options) nil))

(defun emagent-acp--get-available-models (state models)
  
  "Internal helper for STATE and MODELS."
  (if-let ((model-option (emagent-acp--model-config-option state)))
      (mapcar (lambda (value)
                `((:model-id . ,(map-elt value :value))
                  (:name . ,(map-elt value :name))
                  (:description . ,(map-elt value :description))))
              (map-elt model-option :options))
    (emagent-acp--available-model-entries models)))

(defun emagent-acp--model-display-name (state models model-id)
  
  "Internal helper for STATE and MODELS and MODEL-ID."
  (or (map-elt (seq-find (lambda (model)
                           (string= (or (map-elt model :model-id)
                                        (emagent-acp--model-entry-id model))
                                    model-id))
                         (emagent-acp--get-available-models state models))
               :name)
      (emagent-acp--model-entry-name
       (seq-find (lambda (model)
                   (string= (emagent-acp--model-entry-id model) model-id))
                 (emagent-acp--get-available-models state models)))
      model-id))

(defun emagent-acp--model-choices (state models)
  
  "Internal helper for STATE and MODELS."
  (mapcar (lambda (entry)
            (let ((id (or (map-elt entry :model-id)
                          (emagent-acp--model-entry-id entry)))
                  (name (or (map-elt entry :name)
                            (emagent-acp--model-entry-name entry))))
              (cons (emagent-model-choice-label-display id name) id)))
          (emagent-acp--get-available-models state models)))

(defun emagent-acp--model-available-p (model-id state models)
  
  "Internal helper for MODEL-ID and STATE and MODELS."
  (and model-id (not (string-empty-p model-id))
       (seq-find (lambda (entry)
                   (string= model-id
                            (or (map-elt entry :model-id)
                                (emagent-acp--model-entry-id entry))))
                 (emagent-acp--get-available-models state models))))

(defun emagent-acp--match-model-id (model-id state models)
  "Return canonical MODEL-ID for set-config-option, matching by id or name.

Arguments: STATE, MODELS."
  (when (and model-id (not (string-empty-p model-id)))
    (let ((model-id (emagent-model-canonical-id model-id)))
      (or (and (emagent-acp--model-available-p model-id state models) model-id)
          (cl-loop for entry across (vconcat (emagent-acp--get-available-models state models))
                   for id = (or (map-elt entry :model-id)
                                (emagent-acp--model-entry-id entry))
                   for name = (or (map-elt entry :name)
                                  (emagent-acp--model-entry-name entry))
                   when (or (string= id model-id)
                            (string= name model-id)
                            (string= (downcase name) (downcase model-id))
                            (string= (emagent-model-normalize-id id)
                                     (emagent-model-normalize-id model-id)))
                   return id)
          model-id))))

(defun emagent-acp--resolve-model-id (state models saved-model-id)
  "Return a model id for session connect without prompting.

Prefers the buffer's saved model (from startup selection), then \"auto\"
when advertised, then the agent's current model.

Arguments: STATE, MODELS, SAVED-MODEL-ID."
  (let* ((available (emagent-acp--get-available-models state models))
         (current (and models (map-elt models 'currentModelId))))
    (cond
     ((and saved-model-id (not (string-empty-p saved-model-id)))
      (emagent-acp--match-model-id saved-model-id state models))
     ((emagent-acp--auto-model-candidate state models))
     ((and current (not (string-empty-p current))) current)
     ((= (length available) 1)
      (or (map-elt (car available) :model-id)
          (emagent-acp--model-entry-id (car available))))
     (t nil))))

(cl-defun emagent-acp--config-option-set-model-id (&key state session-id model-id
                                                        on-success on-failure
                                                        (persist t))
  "Switch the ACP session model to MODEL-ID.
When PERSIST is non-nil (the default) also record MODEL-ID as the buffer model;
pass nil for a transient per-turn switch that must not change the buffer model.

Arguments: STATE, SESSION-ID, ON-SUCCESS, ON-FAILURE."
  (if-let ((model-option (emagent-acp--model-config-option state)))
      (emagent-acp--send-request
       :state state
       :request (emagent-acp-make-session-set-config-option-request
                 :session-id session-id
                 :config-id (map-elt model-option :id)
                 :value model-id)
       :on-success (lambda (response)
                     (if (map-elt response 'configOptions)
                         (emagent-acp--save-config-options state
                                                           (map-elt response 'configOptions))
                       (emagent-acp--config-option-set-value state
                                                             (map-elt model-option :id)
                                                             model-id))
                     (when persist (emagent-acp--persist-model-id state model-id))
                     (unless persist (emagent-acp--refresh-mode-line state))
                     (emagent-acp--progress
                      state
                      (format "model %s"
                              (emagent-acp--config-option-value-name model-option model-id)))
                     (when on-success (funcall on-success)))
       :on-failure (lambda (error _raw)
                     (emagent-acp--notify-user
                      state
                      (format "emagent: model %s not applied: %s"
                              model-id
                              (or (map-elt error 'message) (format "%s" error))))
                     (when on-failure (funcall on-failure))))
    (emagent-acp--send-request
     :state state
     :request (emagent-acp-make-session-set-model-request
               :session-id session-id
               :model-id model-id)
     :on-success (lambda (_response)
                   (when persist (emagent-acp--persist-model-id state model-id))
                   (unless persist (emagent-acp--refresh-mode-line state))
                   (emagent-acp--notify-user
                    state
                    (format "emagent: model %s" model-id))
                   (when on-success (funcall on-success)))
     :on-failure (lambda (error _raw)
                   (emagent-acp--notify-user
                    state
                    (format "emagent: model %s not applied: %s"
                            model-id
                            (or (map-elt error 'message) (format "%s" error))))
                   (when on-failure (funcall on-failure))))))

(defun emagent-acp--save-session-modes (state response)
  "Store available modes and current mode id from session RESPONSE in STATE."
  (when-let ((modes (or (map-elt response 'modes) (map-elt response :modes))))
    (when-let ((available (or (map-elt modes 'availableModes)
                              (map-elt modes :availableModes))))
      (setf (emagent-acp-state-available-modes state) (append available nil)))
    (when-let ((mode-id (or (map-elt modes 'currentModeId)
                            (map-elt modes :currentModeId)
                            (map-elt modes 'modeId)
                            (map-elt modes :modeId))))
      (when (and (stringp mode-id) (not (string-empty-p mode-id)))
        (setf (emagent-acp-state-session-mode-id state) mode-id)))))

(defun emagent-acp--finish-configure-model (state session-id on-ready resumed)
  
  "Internal helper for STATE and SESSION-ID and ON-READY and RESUMED."
  (unless (fboundp 'emagent-acp--session-ready)
    (require 'emagent-acp))
  (emagent-acp--session-ready
   :state state
   :session-id session-id
   :on-ready on-ready
   :resumed resumed))

(cl-defun emagent-acp--configure-model (&key state session-id response on-ready resumed)
  
  "Internal helper for STATE and SESSION-ID and RESPONSE and ON-READY and RESUMED."
  (emagent-acp--progress state "selecting model…")
  (emagent-acp--save-config-options state (map-elt response 'configOptions))
  (emagent-acp--save-session-modes state response)
  (let* ((models (emagent-acp--models-from-response response))
         (current (emagent-acp--current-model-id state models))
         (choice (emagent-acp--resolve-model-id state models
                                                (emagent-acp--saved-model-id state))))
    (cond
     ((and choice session-id (not (string-empty-p choice))
           current (string= choice current))
      (emagent-acp--progress
       state
       (format "model %s"
               (emagent-acp--model-display-name state models choice)))
      (emagent-acp--persist-model-id state choice)
      (emagent-acp--finish-configure-model state session-id on-ready resumed))
     ((and choice session-id (not (string-empty-p choice)))
      (emagent-acp--progress
       state
       (format "setting model to %s…"
               (emagent-acp--model-display-name state models choice)))
      (emagent-acp--config-option-set-model-id
       :state state
       :session-id session-id
       :model-id choice
       :on-success (lambda ()
                     (emagent-acp--finish-configure-model state session-id on-ready resumed))
       :on-failure (lambda ()
                     (emagent-acp--finish-configure-model state session-id on-ready resumed))))
     (t
      (when current
        (emagent-acp--progress
         state
         (format "model %s"
                 (emagent-acp--model-display-name state models current)))
        (emagent-acp--persist-model-id state current))
      (emagent-acp--finish-configure-model state session-id on-ready resumed)))))

(provide 'emagent-acp-usage)
;;; emagent-acp-usage.el ends here
