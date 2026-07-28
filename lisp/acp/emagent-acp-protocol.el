;;; emagent-acp-protocol.el --- ACP protocol layer for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.8
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
;; ACP protocol, session state/customs, usage, and model selection.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'json)
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
        (require 'emagent-acp))
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
\(`emagent-permissions-directory'/sessions), globally (\"Allow always\"), per
project directory (projects/), or in a legacy buffer header
\(#+EMAGENT_ALLOWED_PERMISSIONS).

When `safe', read and write tools are auto-approved without prompting.
Execute (shell) commands are inspected for destructive operations — rm,
dd, formatting, and similar — and only prompted then.  Harmless
commands like `mvn compile` or `ls` pass through without prompting.

When t, all permission prompts that pass emagent validation are
auto-approved without user interaction.

Emagent always replies to the agent with a one-shot allow optionId
\(never allow_always), so every tool call still arrives at the emagent
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

(defcustom emagent-acp-honor-schedule-wakeup t
  "When non-nil, honor the agent's `ScheduleWakeup' tool calls.

Agents pace long-running loops by calling ScheduleWakeup with a delay and
a prompt, ending their turn and expecting the client to re-invoke them
after the delay.  When enabled, emagent arms a timer when the turn
completes and sends the wakeup prompt as a new user turn; a manual prompt
sent in the meantime cancels the pending wakeup.  When nil, ScheduleWakeup
calls render but nothing fires."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-auto-accept-plans nil
  "When to auto-accept Cursor `cursor/create_plan' without prompting.

nil — always show the plan with Accept/Reject (default; matches Cursor IDE)
t — always accept
permissions — accept when the session is Allow-all or
`emagent-acp-auto-approve-permissions' is t

Batch/noninteractive sessions always auto-accept so ERT does not hang."
  :type '(choice (const :tag "Always prompt" nil)
                 (const :tag "Always accept" t)
                 (const :tag "Follow permission auto-approve" permissions))
  :group 'emagent)

(defcustom emagent-acp-auto-build-plans t
  "When non-nil, accept of `cursor/create_plan' queues a Build turn.

Cursor ends the planning turn after create_plan; the IDE's Build button
is a separate follow-up.  Emagent mirrors that by sending an execute
prompt (and switching to agent mode) once the current turn finishes."
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

(defcustom emagent-acp-message-drain-yield 0.01
  "Seconds to wait before draining the next ACP message batch.

After each batch (`emagent-acp-message-drain-batch-size'), if more wire
messages remain, the drain reschedules with this delay so redisplay and
other timers can run during heavy Cursor/Claude output.  The first
enqueued message still starts a drain immediately (delay 0)."
  :type 'number
  :group 'emagent)

(defcustom emagent-acp-permission-drain-batch-size 16
  "Auto-approve permission requests handled per drain turn before yielding.

A flood of ACP `session/request_permission' messages (common with MCP tools)
used to run a tight `while' on the Emacs command loop.  Process at most this
many auto-approved requests, then reschedule so the UI can breathe.
Interactive permission prompts still insert one dialog and return."
  :type 'integer
  :group 'emagent)

(defcustom emagent-log-agent-stderr nil
  "When non-nil, log filtered cursor-agent stderr to `emagent-log-buffer-name'."
  :type 'boolean
  :group 'emagent)

(defcustom emagent-acp-watchdog-timeout 300
  "Seconds of inactivity before the prompt watchdog fires.

The watchdog resets on each tool-call notification, so this measures idle
time since the last tool call, not total prompt duration.  When ACP work is
still outstanding (pending request, permission prompt, tool-resolve), the
watchdog extends instead of closing the Response.  Increase if your agent
regularly makes long chains of tool calls."
  :type 'integer
  :group 'emagent)

(defcustom emagent-acp-prompt-retry-attempts 3
  "How many times to try a prompt before showing a network error to the user.

A prompt request that fails with a transient network error (see
`emagent-acp--retriable-prompt-error-p', e.g. Cursor's
\"RetriableError: [unavailable] getaddrinfo ENOTFOUND api2.cursor.sh\")
is retried automatically with exponential backoff up to this many total
attempts.  Only after the last attempt fails is the error surfaced in the
chat buffer.  Set to 1 to disable retries."
  :type 'integer
  :group 'emagent)

(defcustom emagent-acp-prompt-retry-base-delay 1.5
  "Base seconds for exponential backoff between retriable prompt retries.

The delay before retry N (1-based) is BASE * 2^(N-1), so with the default
1.5 the waits are roughly 1.5s, then 3s, then 6s.  See
`emagent-acp-prompt-retry-attempts'."
  :type 'number
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

(defvar-local emagent-acp--session nil
  "ACP session state for the current emagent buffer.")

(defvar-local emagent-acp--when-connected-queue nil
  "Callbacks waiting for `emagent-acp--connected-p' in this buffer.")

(defun emagent-acp--make-usage ()
  "Return a fresh usage hash table."
  (let ((u (make-hash-table :test 'eq)))
    (puthash :context-used nil u)
    (puthash :context-size nil u)
    (puthash :total-tokens 0 u)
    u))

;; Defined before any code that reads or `setf's its slots: the accessors' gv
;; setter expanders must be registered at compile time, else a `setf' on a slot
;; earlier in the file falls back to a nonexistent `(setf ...)' function.
(cl-defstruct (emagent-acp-state
               (:constructor emagent-acp--state-create)
               (:copier nil))
  "Mutable per-buffer ACP session state.

Replaces the former untyped hash table so field access is checked
at byte-compile time.  Slots that are themselves maps (the usage
and tool-call/tool-resolve tables, keyed by id) stay hash tables."
  ;; Connection
  client chat-buffer on-reveal provider mcp-http initialized
  ;; Session
  session-id config-options (usage (emagent-acp--make-usage))
  session-auto-approve permission-auto-allow
  external-tool-gate-reasons external-tool-gate-logged
  external-tool-refusal-logged
  agent-rss agent-rss-timer
  ;; Turn
  ready busy
  (assistant-text "") (thought-text "") (thought-buffer "")
  prompt-finalized prompt-finishing (prompt-generation 0)
  prompt-retry-gen
  finish-token finish-timer prompt-watchdog prompt-watchdog-timer
  extra-context compress-pending quiet-prompt replaying-history
  continue-attempts deferred-complete-response
  current-tool current-tool-kind tool-call-since-last-chunk
  (tool-call-titles (make-hash-table :test 'equal))
  (tool-call-inputs (make-hash-table :test 'equal))
  (tool-call-labels (make-hash-table :test 'equal))
  (tool-call-decisions (make-hash-table :test 'equal))
  (tool-call-pending (make-hash-table :test 'equal))
  tool-resolve-queue tool-resolve-worker
  (tool-resolve-attempts (make-hash-table :test 'equal))
  ;; Permission gate
  permission-queue permission-busy permission-drain-timer
  ;; Agent-scheduled wakeup (ScheduleWakeup tool)
  wakeup-request wakeup-timer
  ;; Callbacks (wired by the app; see emagent.el)
  cb-chunk cb-thought cb-finish cb-fail cb-slash-commands
  cb-tool-call cb-permission cb-status
  ;; After cursor/create_plan accept: follow-up Build turn
  plan-build-prompt plan-build-timer
  ;; Session mode (ACP modes / current_mode_update); kept last for hot-reload.
  session-mode-id available-modes)

(defun emagent-acp--strip-pino-colors (string)
  "Remove literal pino color tokens like [32m from STRING."
  (replace-regexp-in-string "\\[[0-9]+m" "" string))

(defun emagent-acp--agent-log-line-p (line)
  "Return non-nil when LINE is cursor-agent info/warn stderr."
  (let ((line (string-trim (emagent-acp--strip-pino-colors line))))
    (or (string-empty-p line)
        (string-match-p "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] .*\\[ *\\(info\\|warn\\) *\\]:" line))))

(defun emagent-acp--stderr-notify-p (emagent-acp-error)
  "Return non-nil when ACP-ERROR should be shown to the user.

Arguments: EMAGENT-ACP-ERROR."
  (let ((message (string-trim (or (map-elt emagent-acp-error 'message) (format "%s" emagent-acp-error)))))
    (cond
     ((string-empty-p message) nil)
     ((string-match-p "\\`\\(?:finished\\|Process\\|acp-client(\\)" message) nil)
     ((string-match-p "\\[32minfo\\|\\[33mwarn" message) nil)
     ((string-match-p "RetriableError" message) nil)
     ((string-match-p "ApiError\\|failed with status\\|\\[31merror" message) t)
     (t
      (let ((lines (split-string message "\n" t)))
        (not (and lines (seq-every-p #'emagent-acp--agent-log-line-p lines))))))))

(defun emagent-acp--log-agent-stderr (message)
  
  "Internal helper for MESSAGE."
  (when emagent-log-agent-stderr
    (emagent-log "agent: %s" (string-trim message))))

(defun emagent-acp--session ()
  
  "Internal helper."
  (or emagent-acp--session
      (error "No active emagent session for this buffer")))

(defun emagent-acp--turn-phase (state)
  "Return the lifecycle phase of STATE's current turn.

One of:
  `idle'        no turn in flight;
  `streaming'   a prompt is in flight (`:busy'), receiving output and possibly
                paused on a permission prompt;
  `finalizing'  streaming ended, the response is being rendered;
  `done'        the response has been fully rendered.

This derives the phase from the turn flags so callers share one
vocabulary for the turn state machine.  The flags remain the
underlying representation for now."
  (cond
   ((emagent-acp-state-busy state) 'streaming)
   ((emagent-acp-state-prompt-finishing state)
    (if (emagent-acp-state-prompt-finalized state) 'done 'finalizing))
   (t 'idle)))

(defun emagent-acp--connecting-p ()
  "Return non-nil when an ACP session is starting but not yet ready."
  (and emagent-acp--session
       (not (emagent-acp-state-ready emagent-acp--session))
       ;; From first `emagent-acp-start' until `session-ready': keep any
       ;; additional `ensure-connected' calls from tearing the attempt down.
       ;; After init, require a live client so a dead half-session can reconnect.
       (or (not (emagent-acp-state-initialized emagent-acp--session))
           (and (emagent-acp-state-client emagent-acp--session)
                (emagent-acp--client-started-p
                 (emagent-acp-state-client emagent-acp--session))))))

(defun emagent-acp--run-when-connected-queue ()
  "Run and clear `emagent-acp--when-connected-queue'."
  (while emagent-acp--when-connected-queue
    (let ((fn (pop emagent-acp--when-connected-queue)))
      (condition-case err
          (funcall fn)
        (error
         (emagent-log "connect callback failed: %s"
                      (error-message-string err)))))))

(defun emagent-acp--clear-when-connected-queue ()
  "Drop queued `emagent-acp-ensure-connected' callbacks without running them."
  (setq emagent-acp--when-connected-queue nil))

(defun emagent-acp--connected-p ()
  "Return non-nil when the current buffer has a live, ready ACP session."
  (and emagent-acp--session
       (emagent-acp-state-ready emagent-acp--session)
       (let ((client (emagent-acp-state-client emagent-acp--session)))
         (and client (emagent-acp--client-started-p client)))))

(defun emagent-acp--permission-pending-p (state)
  "Return non-nil when STATE has unanswered permission requests."
  (or (emagent-acp-state-permission-busy state)
      (emagent-acp-state-permission-queue state)))

(defun emagent-acp--cancel-wakeup (state)
  "Cancel a pending or armed agent wakeup (ScheduleWakeup) for STATE."
  (when-let ((timer (emagent-acp-state-wakeup-timer state)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (emagent-acp-state-wakeup-timer state) nil)
  (setf (emagent-acp-state-wakeup-request state) nil))

(defun emagent-acp--cancel-plan-build (state)
  "Cancel a pending post-create_plan Build turn for STATE.

Clears deferred user-stub suppression and inserts a stub when the chat
response is already closed so the buffer is not left without a prompt."
  (when-let ((timer (emagent-acp-state-plan-build-timer state)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (emagent-acp-state-plan-build-timer state) nil)
  (setf (emagent-acp-state-plan-build-prompt state) nil)
  (when (fboundp 'emagent-acp--chat-buffer)
    (when-let ((buf (emagent-acp--chat-buffer state)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (boundp 'emagent-chat--defer-user-stub)
                     emagent-chat--defer-user-stub)
            (setq emagent-chat--defer-user-stub nil)
            (when (and (fboundp 'emagent-chat--open-response-p)
                       (not (emagent-chat--open-response-p))
                       (fboundp 'emagent-chat--insert-user-heading-stub))
              (emagent-chat--insert-user-heading-stub))))))))

(defun emagent-acp--cancel-state-timers (state)
  "Cancel every timer stored in STATE and clear its slot.
Prevents a reconnect or shutdown from leaving repeating/pending timers
\(RSS poll, watchdog, finish, permission drain, wakeup, plan-build)
pointed at dead state."
  (dolist (timer (list (emagent-acp-state-agent-rss-timer state)
                       (emagent-acp-state-prompt-watchdog-timer state)
                       (emagent-acp-state-finish-timer state)
                       (emagent-acp-state-permission-drain-timer state)
                       (emagent-acp-state-wakeup-timer state)
                       (emagent-acp-state-plan-build-timer state)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (emagent-acp-state-agent-rss-timer state) nil
        (emagent-acp-state-prompt-watchdog-timer state) nil
        (emagent-acp-state-finish-timer state) nil
        (emagent-acp-state-permission-drain-timer state) nil
        (emagent-acp-state-wakeup-timer state) nil
        (emagent-acp-state-wakeup-request state) nil
        (emagent-acp-state-plan-build-timer state) nil
        (emagent-acp-state-plan-build-prompt state) nil))

(defun emagent-acp--teardown-stale-session ()
  "Shut down a dead or incomplete ACP session without clearing persisted ids."
  (when-let* ((state emagent-acp--session))
    (emagent-acp--cancel-state-timers state)
    (when-let ((client (emagent-acp-state-client state)))
      (ignore-errors (emagent-acp-shutdown :client client))))
  (setq emagent-acp--session nil))

(cl-defun emagent-acp--make-state (&key client chat-buffer on-reveal)
  "Return a fresh `emagent-acp-state' for a session.

Arguments: CLIENT, CHAT-BUFFER, ON-REVEAL."
  (emagent-acp--state-create :client client
                             :chat-buffer chat-buffer
                             :on-reveal on-reveal))

(defun emagent-acp--set-callback (state key value)
  "Set the :cb-* callback slot KEY on STATE to VALUE.
Bridges the keyword-keyed callback alist wired by the app to typed slots."
  (pcase key
    (:cb-chunk          (setf (emagent-acp-state-cb-chunk state) value))
    (:cb-thought        (setf (emagent-acp-state-cb-thought state) value))
    (:cb-finish         (setf (emagent-acp-state-cb-finish state) value))
    (:cb-fail           (setf (emagent-acp-state-cb-fail state) value))
    (:cb-slash-commands (setf (emagent-acp-state-cb-slash-commands state) value))
    (:cb-tool-call      (setf (emagent-acp-state-cb-tool-call state) value))
    (:cb-permission     (setf (emagent-acp-state-cb-permission state) value))
    (:cb-status         (setf (emagent-acp-state-cb-status state) value))
    (_ (emagent-log "unknown callback key %S" key))))

(defconst emagent-acp--agent-error-signature-re
  (concat "RetriableError\\|getaddrinfo\\|ENOTFOUND\\|EAI_AGAIN"
          "\\|ECONNRESET\\|ECONNREFUSED\\|ConnectionRefused"
          "\\|ETIMEDOUT\\|EPIPE"
          "\\|\\[unavailable\\]\\|socket hang up\\|WritableIterable is closed")
  "Machine-generated markers of a transient error emitted as agent output.
Deliberately stricter than `emagent-acp--retriable-prompt-error-p': it must
not match prose such as \"network error\" or \"timeout\" that can legitimately
appear inside a real answer.")

(defun emagent-acp--turn-did-no-work-p (state)
  "Return non-nil when STATE's turn did no real work.
No tool invocations and little text means replaying the prompt is safe."
  (let ((text (string-trim (or (emagent-acp-state-assistant-text state) "")))
        (titles (emagent-acp-state-tool-call-titles state)))
    (and (or (null titles) (zerop (hash-table-count titles)))
         (< (length text) 400))))

(defun emagent-acp--agent-error-only-response-p (state)
  "Return non-nil when STATE's finished turn is only a transient agent error.

Some agents (e.g. cursor-agent-acp) accept the prompt, then hit a transient
network failure and emit the error as the whole turn's output instead of
failing the request.  Such a turn carries no real content and no tool calls
\(`emagent-acp--turn-did-no-work-p'), so it is safe for emagent to re-issue the
prompt with backoff rather than surface the error.  Matching uses
`emagent-acp--agent-error-signature-re', which only recognises
machine-generated error markers."
  (let ((text (string-trim (or (emagent-acp-state-assistant-text state) ""))))
    (and (not (emagent-acp-state-compress-pending state))
         (not (emagent-acp-state-quiet-prompt state))
         (emagent-acp--turn-did-no-work-p state)
         (not (string-empty-p text))
         (string-match-p emagent-acp--agent-error-signature-re text))))

(defun emagent-acp--turn-hit-transient-error-p (state)
  "Return non-nil when STATE's finished turn ended on a transient error marker.

Unlike `emagent-acp--agent-error-only-response-p' this does not require the
turn to be empty: it is true even when tool calls ran or real content was
produced.  Such a turn must NOT be replayed (that would repeat side effects
like commits or pushes); instead emagent resumes it by sending \"continue\",
mirroring what a user does by hand."
  (let ((text (or (emagent-acp-state-assistant-text state) "")))
    (and (not (emagent-acp-state-compress-pending state))
         (not (emagent-acp-state-quiet-prompt state))
         (string-match-p emagent-acp--agent-error-signature-re text))))

(defgroup emagent-acp nil
  "ACP (Agent Client Protocol) implementation."
  :group 'tools
  :prefix "emagent-acp-")

(defcustom emagent-acp-logging-enabled nil
  "When non-nil, log ACP wire traffic to the client log buffer."
  :type 'boolean
  :group 'emagent-acp)

(cl-defun emagent-acp-logs-buffer (&key client)
  "Return (creating if needed) the log buffer for CLIENT."
  (let ((name (format "*acp-(%s)-%s log*"
                      (map-elt client :command)
                      (map-elt client :instance-count))))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (buffer-disable-undo)
          (current-buffer)))))

(defun emagent-acp--log (client label format-string &rest args)
  "Log to CLIENT's log buffer when `emagent-acp-logging-enabled' is set.

Arguments: LABEL, FORMAT-STRING, ARGS."
  (when emagent-acp-logging-enabled
    (with-current-buffer (emagent-acp-logs-buffer :client client)
      (goto-char (point-max))
      (if label
          (insert (format "%s >\n\n%s\n\n" label (apply #'format format-string args)))
        (insert (format "%s\n\n" (apply #'format format-string args)))))))

(defconst emagent-acp--jsonrpc-version "2.0")

(defun emagent-acp--parse-json (json)
  "Parse JSON string into an alist."
  (json-parse-string json :object-type 'alist :null-object nil :false-object nil))

(defun emagent-acp--serialize-json (object)
  "Serialize OBJECT to a JSON string with trailing newline."
  (concat (json-serialize object) "\n"))

(cl-defun emagent-acp-make-error (&key code message data)
  "Create a JSON-RPC error object with CODE and MESSAGE.

Arguments: DATA."
  (unless code (error ":code is required"))
  (unless message (error ":message is required"))
  (let ((err `((code . ,code) (message . ,message))))
    (when data (nconc err `((data . ,data))))
    err))

(defun emagent-acp--make-internal-error (message)
  "Create a synthetic internal error (JSON-RPC code -32603) with MESSAGE."
  (emagent-acp-make-error :code -32603 :message message))

(defun emagent-acp--parse-stderr-api-error (raw-output)
  "Parse RAW-OUTPUT from stderr; return a structured error alist or nil."
  (when (string-match
         "Attempt [0-9]+ failed with status [0-9]+\\. Retrying.*ApiError: \\({.*}\\)"
         raw-output)
    (let ((json (match-string 1 raw-output)))
      (condition-case nil
          (let-alist (emagent-acp--parse-json json)
            (condition-case nil
                (map-elt (emagent-acp--parse-json .error.message) 'error)
              (error nil)))
        (error nil)))))

(cl-defun emagent-acp--make-message (&key json object)
  "Wrap JSON string and parsed OBJECT into a message alist."
  (list (cons :object object) (cons :json json)))

(eval-when-compile
  (require 'cl-lib))

(defvar emagent-acp-instance-count 0
  "Monotonic counter used to name ACP client processes and log buffers.")

(defun emagent-acp--increment-instance-count ()
  "Return an incremented `emagent-acp-instance-count', wrapping at fixnum max."
  (if (= emagent-acp-instance-count most-positive-fixnum)
      (setq emagent-acp-instance-count 0)
    (setq emagent-acp-instance-count (1+ emagent-acp-instance-count))))

(cl-defun emagent-acp-make-client (&key context-buffer process-directory command command-params
                                environment-variables
                                request-sender notification-sender
                                request-resolver response-sender
                                outgoing-request-decorator)
  "Create an ACP client hash table.

CONTEXT-BUFFER is set as `current-buffer' for all callbacks.
PROCESS-DIRECTORY, when non-nil, is the absolute directory passed to
`make-process' as `:directory' so the agent binary starts in the emagent
project root (see #+EMAGENT_PROJECT).  When nil, the context buffer's
`default-directory' is used at start time.
COMMAND is the agent binary; COMMAND-PARAMS is a list of argument strings.
ENVIRONMENT-VARIABLES is a list of \"VAR=value\" strings.
REQUEST-SENDER, NOTIFICATION-SENDER, REQUEST-RESOLVER, RESPONSE-SENDER
override the default wire implementations.
OUTGOING-REQUEST-DECORATOR is an optional (lambda (request) ...) that may
modify each outgoing JSON-RPC request before it is sent."
  (unless command
    (error ":command is required"))
  (let ((client (make-hash-table :test 'eq)))
    (puthash :context-buffer context-buffer client)
    (when process-directory
      (puthash :process-directory process-directory client))
    (puthash :instance-count (emagent-acp--increment-instance-count) client)
    (puthash :process nil client)
    (puthash :command command client)
    (puthash :command-params command-params client)
    (puthash :environment-variables environment-variables client)
    (puthash :pending-requests () client)
    (puthash :request-id 0 client)
    (puthash :notification-handlers () client)
    (puthash :request-handlers () client)
    (puthash :error-handlers () client)
    (puthash :request-sender (or request-sender #'emagent-acp--request-sender) client)
    (puthash :notification-sender (or notification-sender #'emagent-acp--notification-sender) client)
    (puthash :request-resolver (or request-resolver #'emagent-acp--request-resolver) client)
    (puthash :response-sender (or response-sender #'emagent-acp--response-sender) client)
    (puthash :outgoing-request-decorator outgoing-request-decorator client)
    client))

(defun emagent-acp--client-started-p (client)
  "Return non-nil when the CLIENT process is live."
  (and (map-elt client :process)
       (process-live-p (map-elt client :process))))

(defconst emagent-acp--history-replay-update-re
  (concat "\"sessionUpdate\"[[:space:]]*:[[:space:]]*\""
          "\\(?:agent_message_chunk\\|agent_thought_chunk\\|"
          "tool_call\\|tool_call_update\\)\"")
  "Match compact/spaced ACP history `sessionUpdate' types on one wire line.")

(defun emagent-acp--history-replay-wire-line-p (json)
  "Return non-nil when JSON is a history-replay `session/update' wire line.

Used during `session/load' to drop transcript replay chunks before they enter
the message queue.  The org chat buffer already holds the conversation; parsing
thousands of chunks at `emagent-acp-message-drain-batch-size' 1 makes resume
appear hung."
  (and (stringp json)
       (string-match-p emagent-acp--history-replay-update-re json)))

(defun emagent-acp--set-suppress-history-updates (client suppress)
  "Set CLIENT `:suppress-history-updates' to SUPPRESS (non-nil to drop replay)."
  (map-put! client :suppress-history-updates (and suppress t)))

(cl-defun emagent-acp--start-client (&key client)
  "Start the CLIENT process with a cooperative, timer-driven message queue.

Wire lines are queued from the process filter; JSON parsing and handler
dispatch run via `run-with-timer' in bounded batches so Emacs timers and
redisplay are not starved during heavy agent output."
  (unless client (error ":client is required"))
  (unless (map-elt client :command) (error ":command is required"))
  (when (emagent-acp--client-started-p client)
    (error "Client already started"))
  (let* ((ctx (map-elt client :context-buffer))
         (dir (or (map-elt client :process-directory)
                  (and (buffer-live-p ctx)
                       (with-current-buffer ctx
                         (expand-file-name default-directory)))
                  (expand-file-name default-directory))))
    (unless (file-directory-p dir)
      (error "ACP client directory is not a directory: %s" dir))
    (unless (executable-find (map-elt client :command) (file-remote-p dir))
      (error "\"%s\" not found; please install it" (map-elt client :command)))
    (let* ((coding-system-for-read  'utf-8-unix)
           (coding-system-for-write 'utf-8-unix)
           (pending-input "")
           (message-queue nil)
           (message-queue-tail nil)
           (message-queue-busy nil)
           (drain-pending nil)
           (process-environment (append (map-elt client :environment-variables)
                                        process-environment))
           (stderr-buffer (get-buffer-create
                           (format "acp-client-stderr(%s)-%s"
                                   (map-elt client :command)
                                   (map-elt client :instance-count)))))
      (with-current-buffer stderr-buffer
        (add-hook 'after-change-functions
                  (lambda (beg end _len)
                    (let ((raw (buffer-substring-no-properties beg end)))
                      (emagent-acp--log client "STDERR" "%s" (string-trim raw))
                      (when-let ((err (or (emagent-acp--parse-stderr-api-error raw)
                                          (and (not (string-empty-p (string-trim raw)))
                                               (emagent-acp--make-internal-error raw)))))
                        (emagent-acp--log client "API-ERROR" "%s" (string-trim raw))
                        (dolist (h (map-elt client :error-handlers))
                          (funcall h err)))))
                  nil t))
      (cl-labels
          ((route-parsed (json)
             (when-let* ((obj (condition-case nil
                                  (emagent-acp--parse-json json)
                                (error
                                 (emagent-acp--log client "JSON PARSE ERROR"
                                           "Invalid JSON: %s" json)
                                 nil))))
               (route (emagent-acp--make-message :json json :object obj))))
           (route (incoming)
             (let ((print-circle t) (print-level 25) (print-length 200))
               (emagent-acp--route-incoming-message
                :message incoming :client client
                :on-notification
                (lambda (notif)
                  (dolist (h (map-elt client :notification-handlers))
                    (condition-case-unless-debug err
                        (funcall h notif)
                      (error (emagent-acp--log client "NOTIFICATION HANDLER ERROR"
                                       "Failed: %S" err)))))
                :on-request
                (lambda (req)
                  (dolist (h (map-elt client :request-handlers))
                    (condition-case-unless-debug err
                        (funcall h req)
                      (error (emagent-acp--log client "REQUEST HANDLER ERROR"
                                       "Failed: %S" err))))))))
           (drain ()
             (setq drain-pending nil)
             (unless message-queue-busy
               (setq message-queue-busy t)
               ;; Pop each message BEFORE routing it and isolate the routing in
               ;; condition-case, so a throwing/quitting handler can neither
               ;; re-poison the queue head nor abort the whole batch.  The
               ;; reschedule lives in the unwind-protect cleanup so it survives
               ;; a non-local exit that escapes the loop entirely.
               (unwind-protect
                   (let ((batch 0)
                         (limit (max 1 (if (map-elt client :suppress-history-updates)
                                        256
                                      emagent-acp-message-drain-batch-size))))
                     (while (and message-queue (< batch limit))
                       (setq batch (1+ batch))
                       (let ((item (car message-queue)))
                         (setq message-queue (cdr message-queue))
                         (unless message-queue
                           (setq message-queue-tail nil))
                         (condition-case-unless-debug err
                             (route-parsed item)
                           ((error quit)
                            (emagent-acp--log client "DRAIN ITEM ERROR"
                                              "Dropped message: %S" err))))))
                 (setq message-queue-busy nil)
                 (when (and message-queue (not drain-pending))
                   (setq drain-pending t)
                   (run-with-timer
                    (if (map-elt client :suppress-history-updates)
                        0
                      (max 0 emagent-acp-message-drain-yield))
                    nil
                    (lambda () (drain)))))))
           (enqueue (json-line)
             (let ((cell (list json-line)))
               (if message-queue-tail
                   (setcdr message-queue-tail cell)
                 (setq message-queue cell))
               (setq message-queue-tail cell)
               (unless drain-pending
                 (setq drain-pending t)
                 (run-with-timer 0 nil (lambda () (drain)))))))
        (let ((proc
               (let ((default-directory dir))
                 (make-process
                  :name (format "acp-client(%s)-%s"
                                (map-elt client :command)
                                (map-elt client :instance-count))
                  :command (cons (map-elt client :command)
                                 (map-elt client :command-params))
                  :stderr stderr-buffer
                  :connection-type 'pipe
                  :noquery t
                  :file-handler (file-remote-p dir)
                  :filter
                  (lambda (_proc input)
                    (emagent-acp--log client "INCOMING TEXT" "%s" input)
                    (setq pending-input (concat pending-input input))
                    (let ((start 0) pos)
                      (while (setq pos (string-search "\n" pending-input start))
                        (let ((json (substring pending-input start pos)))
                          (if (and (map-elt client :suppress-history-updates)
                                   (emagent-acp--history-replay-wire-line-p json))
                              (emagent-acp--log
                               client "INCOMING LINE"
                               "(suppressed history) %s"
                               (truncate-string-to-width json 120 nil nil t))
                            (emagent-acp--log client "INCOMING LINE" "%s" json)
                            (enqueue json)))
                        (setq start (1+ pos)))
                      (setq pending-input (substring pending-input start))))
                  :sentinel
                  (lambda (process event)
                    (when (buffer-live-p stderr-buffer)
                      (kill-buffer stderr-buffer))
                    (when (memq (process-status process) '(exit signal))
                      (emagent-acp--fail-pending-requests :client client :event event)))))))
          (map-put! client :process proc))))))

(cl-defun emagent-acp-shutdown (&key client)
  "Shut down ACP CLIENT and release resources."
  (unless client (error ":client is required"))
  (when (and (map-elt client :process)
             (process-live-p (map-elt client :process)))
    (delete-process (map-elt client :process)))
  (when (buffer-live-p (emagent-acp-logs-buffer :client client))
    (kill-buffer (emagent-acp-logs-buffer :client client))))

(cl-defun emagent-acp-subscribe-to-notifications (&key client on-notification buffer)
  "Subscribe to incoming CLIENT notifications via ON-NOTIFICATION callback.

Arguments: BUFFER."
  (unless client (error ":client is required"))
  (unless on-notification (error ":on-notification is required"))
  (let ((handlers (map-elt client :notification-handlers)))
    (push (lambda (notification)
            (with-temp-buffer
              (with-current-buffer (or (when (buffer-live-p buffer) buffer)
                                       (when (buffer-live-p (map-elt client :context-buffer))
                                         (map-elt client :context-buffer))
                                       (current-buffer))
                (funcall on-notification notification))))
          handlers)
    (map-put! client :notification-handlers handlers)))

(cl-defun emagent-acp-subscribe-to-requests (&key client on-request buffer)
  "Subscribe to incoming CLIENT requests via ON-REQUEST callback.

Arguments: BUFFER."
  (unless client (error ":client is required"))
  (unless on-request (error ":on-request is required"))
  (let ((handlers (map-elt client :request-handlers)))
    (push (lambda (request)
            (with-temp-buffer
              (with-current-buffer (or (when (buffer-live-p buffer) buffer)
                                       (when (buffer-live-p (map-elt client :context-buffer))
                                         (map-elt client :context-buffer))
                                       (current-buffer))
                (funcall on-request request))))
          handlers)
    (map-put! client :request-handlers handlers)))

(cl-defun emagent-acp-subscribe-to-errors (&key client on-error buffer)
  "Subscribe to agent process errors via ON-ERROR callback.

Arguments: CLIENT, BUFFER."
  (unless client (error ":client is required"))
  (unless on-error (error ":on-error is required"))
  (let ((handlers (map-elt client :error-handlers)))
    (push (lambda (err)
            (with-temp-buffer
              (with-current-buffer (or (when (buffer-live-p buffer) buffer)
                                       (when (buffer-live-p (map-elt client :context-buffer))
                                         (map-elt client :context-buffer))
                                       (current-buffer))
                (funcall on-error err))))
          handlers)
    (map-put! client :error-handlers handlers)))

(cl-defun emagent-acp-send-request (&key client request buffer on-success on-failure sync)
  "Send REQUEST from CLIENT.

ON-SUCCESS is (lambda (response)), ON-FAILURE is (lambda (error)).
BUFFER overrides the context buffer for callbacks.
When SYNC is non-nil, block until the response arrives."
  (unless client (error ":client is required"))
  (unless request (error ":request is required"))
  (unless (emagent-acp--client-started-p client)
    (emagent-acp--start-client :client client))
  (funcall (map-elt client :request-sender)
           :client client :request request :buffer buffer
           :on-success on-success :on-failure on-failure :sync sync))

(cl-defun emagent-acp-send-response (&key client response)
  "Send a request RESPONSE from CLIENT."
  (unless client (error ":client is required"))
  (unless response (error ":response is required"))
  (funcall (map-elt client :response-sender) :client client :response response))

(cl-defun emagent-acp-send-notification (&key client notification sync)
  "Send NOTIFICATION from CLIENT.

Arguments: SYNC."
  (unless client (error ":client is required"))
  (unless notification (error ":notification is required"))
  (unless (emagent-acp--client-started-p client)
    (emagent-acp--start-client :client client))
  (funcall (map-elt client :notification-sender)
           :client client :notification notification :sync sync))

(eval-when-compile
  (require 'cl-lib))

(cl-defun emagent-acp--request-sender (&key client request buffer on-success on-failure sync)
  "Default implementation of the ACP request sender.

Arguments: CLIENT, REQUEST, BUFFER, ON-SUCCESS, ON-FAILURE, SYNC."
  (unless (emagent-acp--client-started-p client)
    (emagent-acp--start-client :client client))
  (when-let ((decorator (map-elt client :outgoing-request-decorator)))
    (if-let ((decorated (funcall decorator request)))
        (setq request decorated)
      (emagent-acp--log client "DECORATOR ERROR"
                "Decorator returned nil for \"%s\", sending original"
                (map-elt request :method))))
  (let* ((method (map-elt request :method))
         (params (map-elt request :params))
         (proc (map-elt client :process))
         (request-id (1+ (map-elt client :request-id)))
         (wire-request `((jsonrpc . ,emagent-acp--jsonrpc-version)
                         (method  . ,method)
                         (id      . ,request-id)
                         ,@(when params `((params . ,params)))))
         (result nil)
         (done nil))
    (map-put! client :request-id request-id)
    (map-put! client :pending-requests
              (cons (cons request-id `((:request  . ,wire-request)
                                       (:buffer   . ,buffer)
                                       (:on-success . ,on-success)
                                       (:on-failure . ,on-failure)))
                    (map-elt client :pending-requests)))
    (when sync
      (map-put! (map-nested-elt client `(:pending-requests ,request-id)) :on-success
                (lambda (data) (setq result data done t)))
      (map-put! (map-nested-elt client `(:pending-requests ,request-id)) :on-failure
                (lambda (data) (setq result data done 'error))))
    (emagent-acp--log client "OUTGOING OBJECT" "%s" wire-request)
    (let ((json (emagent-acp--serialize-json wire-request)))
      (emagent-acp--log client "OUTGOING TEXT" "%s" json)
      (process-send-string proc json))
    (when sync
      (while (not done)
        (accept-process-output proc 0.01))
      (if (eq done 'error)
          (error "ACP request failed: %s" result)
        result))))

(cl-defun emagent-acp--response-sender (&key client response)
  "Default implementation of the ACP response sender.

Arguments: CLIENT, RESPONSE."
  (let* ((request-id  (map-elt response :request-id))
         (result-data (map-elt response :result))
         (error-data  (map-elt response :error))
         (proc (map-elt client :process))
         (wire (if error-data
                   `((jsonrpc . ,emagent-acp--jsonrpc-version)
                     (id      . ,request-id)
                     (error   . ,error-data))
                 `((jsonrpc . ,emagent-acp--jsonrpc-version)
                   (id      . ,request-id)
                   (result  . ,result-data)))))
    (let ((json (emagent-acp--serialize-json wire)))
      (emagent-acp--log client "OUTGOING RESPONSE" "%s" json)
      (process-send-string proc json))))

(cl-defun emagent-acp--notification-sender (&key client notification &allow-other-keys)
  "Default implementation of the ACP notification sender.

Arguments: CLIENT, NOTIFICATION."
  (unless (emagent-acp--client-started-p client)
    (emagent-acp--start-client :client client))
  (let* ((method (map-elt notification :method))
         (params (map-elt notification :params))
         (proc (map-elt client :process))
         (wire `((jsonrpc . ,emagent-acp--jsonrpc-version)
                 (method  . ,method)
                 ,@(when params `((params . ,params))))))
    (emagent-acp--log client "OUTGOING NOTIFICATION" "%s" wire)
    (let ((json (emagent-acp--serialize-json wire)))
      (process-send-string proc json))))

(cl-defun emagent-acp--request-resolver (&key client id)
  "Resolve pending request by ID in CLIENT."
  (map-nested-elt client `(:pending-requests ,id)))

(cl-defun emagent-acp--route-incoming-message (&key client message on-notification on-request)
  "Route a CLIENT MESSAGE to the appropriate handler.

ON-NOTIFICATION receives notification objects; ON-REQUEST receives incoming
request objects (when the agent initiates a request to emagent)."
  (unless message        (error ":message is required"))
  (unless on-notification (error ":on-notification is required"))
  (unless on-request      (error ":on-request is required"))
  ;; A syntactically valid but non-object JSON line (e.g. `42`, `[]`) parses to
  ;; a non-alist; `let-alist' would signal on it.  Bind it to nil so routing
  ;; treats it as an ignorable message instead of letting the signal unwind the
  ;; drain and wedge the queue.
  (let* ((obj (map-elt message :object))
         (obj (and (listp obj) obj)))
    (unless obj
      (emagent-acp--log client nil "↳ Non-object message ignored: %s"
                        (map-elt message :object)))
    (let-alist obj
    (or
     ;; Non-object payload already logged above; nothing to route.
     (unless obj t)
     ;; Successful response to our outgoing request
     (when-let ((resp (and .id
                           (map-contains-key (map-elt message :object) 'result)
                           (funcall (map-elt client :request-resolver)
                                    :client client :id .id))))
       (emagent-acp--log client nil "↳ Routing as response (result)")
       (map-put! client :pending-requests
                 (map-delete (map-elt client :pending-requests) .id))
       (if (map-elt resp :on-success)
           (condition-case-unless-debug err
               (with-temp-buffer
                 (with-current-buffer (or (map-elt resp :buffer)
                                          (map-elt client :context-buffer)
                                          (current-buffer))
                   (funcall (map-elt resp :on-success) .result)))
             ((error quit)
              (emagent-acp--log client "RESPONSE CALLBACK ERROR"
                                "on-success failed: %S" err)))
         (emagent-acp--log client nil "Unhandled result: %s" message))
       t)

     ;; Error response to our outgoing request
     (when-let ((resp (and .error .id
                           (funcall (map-elt client :request-resolver)
                                    :client client :id .id))))
       (emagent-acp--log client nil "↳ Routing as response (error)")
       (map-put! client :pending-requests
                 (map-delete (map-elt client :pending-requests) .id))
       (if (map-elt resp :on-failure)
           (condition-case-unless-debug err
               (emagent-acp--call-request-failure
                :client client :incoming-response resp
                :error-data .error :message message)
             ((error quit)
              (emagent-acp--log client "RESPONSE CALLBACK ERROR"
                                "on-failure failed: %S" err)))
         (emagent-acp--log client nil "Unhandled error: %s" message))
       t)

     ;; Incoming request from agent (e.g. fs/read_text_file)
     (when (and .method .id)
       (emagent-acp--log client nil "↳ Routing as incoming request")
       (when on-request (funcall on-request (map-elt message :object)))
       t)

     ;; Notification (no id)
     (when (not .id)
       (emagent-acp--log client nil "↳ Routing as notification")
       (when on-notification (funcall on-notification (map-elt message :object)))
       t)

     ;; Unrecognized
     (emagent-acp--log client nil "↳ Unrecognized message: %s" (map-elt message :object))))))

(cl-defun emagent-acp--call-request-failure (&key client incoming-response error-data message)
  "Invoke the failure callback of INCOMING-RESPONSE with ERROR-DATA.

Arguments: CLIENT, MESSAGE."
  (with-temp-buffer
    (with-current-buffer (or (map-elt incoming-response :buffer)
                             (map-elt client :context-buffer)
                             (current-buffer))
      (let ((callback (map-elt incoming-response :on-failure)))
        (if (>= (cdr (func-arity callback)) 2)
            (funcall callback error-data message)
          (funcall callback error-data))))))

(cl-defun emagent-acp--fail-pending-requests (&key client event)
  "Fail all pending requests in CLIENT with a process-ended error.

Arguments: EVENT."
  (let* ((pending (map-elt client :pending-requests))
         (trimmed (string-trim event))
         (msg "Agent process ended before completing request")
         (err (emagent-acp--make-internal-error
               (if (string-empty-p trimmed) msg
                 (format "%s: %s" msg trimmed)))))
    (map-put! client :pending-requests nil)
    (dolist (entry pending)
      (when-let ((resp (cdr entry))
                 ((map-elt resp :on-failure)))
        (condition-case-unless-debug e
            (emagent-acp--call-request-failure
             :client client :incoming-response resp :error-data err
             :message (emagent-acp--make-message
                       :object `((jsonrpc . ,emagent-acp--jsonrpc-version)
                                 (id      . ,(car entry))
                                 (error   . ,err))
                       :json nil))
          (error (emagent-acp--log client "REQUEST FAILURE CALLBACK ERROR"
                           "Failed: %S" e)))))))

(eval-when-compile
  (require 'cl-lib))

(cl-defun emagent-acp-make-initialize-request (&key protocol-version client-info
                                            read-text-file-capability
                                            write-text-file-capability)
  "Build an \"initialize\" request.

PROTOCOL-VERSION is required.  CLIENT-INFO is an optional alist with
`name', `title', and `version' keys.
READ-TEXT-FILE-CAPABILITY and WRITE-TEXT-FILE-CAPABILITY are booleans."
  (unless protocol-version (error ":protocol-version is required"))
  `((:method . "initialize")
    (:params . (,@(when client-info `((clientInfo . ,client-info)))
                (protocolVersion . ,protocol-version)
                (clientCapabilities
                 . ((fs . ((readTextFile  . ,(if read-text-file-capability  t :false))
                           (writeTextFile . ,(if write-text-file-capability t :false))))))))))

(cl-defun emagent-acp-make-authenticate-request (&key method-id method)
  "Build an \"authenticate\" request.

Arguments: METHOD-ID, METHOD."
  (unless method-id (error ":method-id is required"))
  `((:method . "authenticate")
    (:params . ,(append `((methodId . ,method-id))
                        (when method `((authMethod . ,method)))))))

(cl-defun emagent-acp-make-session-new-request (&key cwd mcp-servers meta)
  "Build a \"session/new\" request.

CWD is required.  MCP-SERVERS is a list of MCP server configs.
META is an optional alist; a `systemPrompt' key is supported."
  (unless cwd (error ":cwd is required"))
  `((:method . "session/new")
    (:params . ((cwd       . ,(directory-file-name (expand-file-name cwd)))
                (mcpServers . ,(or mcp-servers []))
                ,@(when meta `((_meta . ,meta)))))))

(cl-defun emagent-acp-make-session-prompt-request (&key session-id prompt images)
  "Build a \"session/prompt\" request.

SESSION-ID and PROMPT are required.  PROMPT may be a string or a vector
of content blocks (e.g. already-structured [{type:text text:...}]).

IMAGES is an optional list of plists, each with `media-type' and `data'
keys (base64-encoded bytes), which are appended as image content blocks:

  ((media-type . \"image/png\") (data . \"<base64>\"))

This allows sending multimodal prompts to vision-capable agents."
  (unless session-id (error ":session-id is required"))
  (unless prompt     (error ":prompt is required"))
  (let* ((text-blocks (if (vectorp prompt)
                          prompt
                        (vector `((type . "text") (text . ,prompt)))))
         (image-blocks
          (apply #'vector
                 (mapcar (lambda (img)
                           `((type     . "image")
                             (data     . ,(map-elt img 'data))
                             (mimeType . ,(map-elt img 'media-type))))
                         (or images '()))))
         (all-blocks (vconcat text-blocks image-blocks)))
    `((:method . "session/prompt")
      (:params . ((sessionId . ,session-id)
                  (prompt    . ,all-blocks))))))

(cl-defun emagent-acp-make-session-load-request (&key session-id cwd mcp-servers meta)
  "Build a \"session/load\" request.

SESSION-ID and CWD are required.  MCP-SERVERS is an optional list.
META is an optional alist injected as the `_meta' field (e.g. for
system-prompt injection on load)."
  (unless session-id (error ":session-id is required"))
  (unless cwd        (error ":cwd is required"))
  `((:method . "session/load")
    (:params . ((sessionId  . ,session-id)
                (cwd        . ,(directory-file-name (expand-file-name cwd)))
                (mcpServers . ,(or mcp-servers []))
                ,@(when meta `((_meta . ,meta)))))))

(cl-defun emagent-acp-make-session-resume-request (&key session-id cwd mcp-servers)
  "Build a \"session/resume\" request.

Arguments: SESSION-ID, CWD, MCP-SERVERS."
  (unless session-id (error ":session-id is required"))
  (unless cwd        (error ":cwd is required"))
  `((:method . "session/resume")
    (:params . ((sessionId  . ,session-id)
                (cwd        . ,(directory-file-name (expand-file-name cwd)))
                (mcpServers . ,(or mcp-servers []))))))

(cl-defun emagent-acp-make-session-fork-request (&key session-id cwd mcp-servers)
  "Build a \"session/fork\" request.

Arguments: SESSION-ID, CWD, MCP-SERVERS."
  (unless session-id (error ":session-id is required"))
  (unless cwd        (error ":cwd is required"))
  `((:method . "session/fork")
    (:params . ((sessionId  . ,session-id)
                (cwd        . ,(directory-file-name (expand-file-name cwd)))
                (mcpServers . ,(or mcp-servers []))))))

(cl-defun emagent-acp-make-session-list-request (&key cwd)
  "Build a \"session/list\" request.

Arguments: CWD."
  (unless cwd (error ":cwd is required"))
  `((:method . "session/list")
    (:params . ((cwd . ,(directory-file-name (expand-file-name cwd)))))))

(cl-defun emagent-acp-make-session-delete-request (&key session-id)
  "Build a \"session/delete\" request.

Arguments: SESSION-ID."
  (unless session-id (error ":session-id is required"))
  `((:method . "session/delete")
    (:params . ((sessionId . ,session-id)))))

(cl-defun emagent-acp-make-session-set-model-request (&key session-id model-id)
  "Build a \"session/set_model\" request (Claude Code ACP extension).

Arguments: SESSION-ID, MODEL-ID."
  (unless session-id (error ":session-id is required"))
  (unless model-id   (error ":model-id is required"))
  `((:method . "session/set_model")
    (:params . ((sessionId . ,session-id)
                (modelId   . ,model-id)))))

(cl-defun emagent-acp-make-session-set-mode-request (&key session-id mode-id)
  "Build a \"session/set_mode\" request.

Arguments: SESSION-ID, MODE-ID."
  (unless session-id (error ":session-id is required"))
  (unless mode-id    (error ":mode-id is required"))
  `((:method . "session/set_mode")
    (:params . ((sessionId . ,session-id)
                (modeId    . ,mode-id)))))

(cl-defun emagent-acp-make-session-set-config-option-request (&key session-id config-id value)
  "Build a \"session/set_config_option\" request.

Arguments: SESSION-ID, CONFIG-ID, VALUE."
  (unless session-id (error ":session-id is required"))
  (unless config-id  (error ":config-id is required"))
  (unless value      (error ":value is required"))
  `((:method . "session/set_config_option")
    (:params . ((sessionId . ,session-id)
                (configId  . ,config-id)
                (value     . ,value)))))

(cl-defun emagent-acp-make-session-cancel-notification (&key session-id reason)
  "Build a \"session/cancel\" notification.

Arguments: SESSION-ID, REASON."
  (unless session-id (error ":session-id is required"))
  `((:method . "session/cancel")
    (:params . ((sessionId . ,session-id)
                ,@(when reason `((reason . ,reason)))))))

(cl-defun emagent-acp-make-session-request-permission-response (&key request-id option-id cancelled)
  "Build a \"session/request_permission\" response.

Provide either OPTION-ID (selected option) or CANCELLED (non-nil).

Arguments: REQUEST-ID."
  (unless request-id (error ":request-id is required"))
  (when (and option-id cancelled)
    (error "Provide :option-id or :cancelled, not both"))
  (unless (or option-id cancelled)
    (error "Must specify :option-id or :cancelled"))
  `((:request-id . ,request-id)
    (:result . ((outcome . ,(if cancelled
                                '((outcome . "cancelled"))
                              `((outcome  . "selected")
                                (optionId . ,option-id))))))))

(cl-defun emagent-acp-make-cursor-create-plan-response (&key request-id outcome reason plan-uri)
  "Build a Cursor cursor/create_plan response.

OUTCOME is a string: accepted, rejected, or cancelled.  REASON is used
when OUTCOME is rejected.  PLAN-URI is optional when accepting.

Arguments: REQUEST-ID."
  (unless request-id (error ":request-id is required"))
  (unless (member outcome '("accepted" "rejected" "cancelled"))
    (error "Invalid :outcome %S" outcome))
  (let ((inner (pcase outcome
                 ("accepted"
                  (append '((outcome . "accepted"))
                          (when plan-uri `((planUri . ,plan-uri)))))
                 ("rejected"
                  (append '((outcome . "rejected"))
                          (when reason `((reason . ,reason)))))
                 (_ '((outcome . "cancelled"))))))
    `((:request-id . ,request-id)
      (:result . ((outcome . ,inner))))))

(cl-defun emagent-acp-make-fs-read-text-file-response (&key request-id content error)
  "Build a \"fs/read_text_file\" response with CONTENT or ERROR.

Arguments: REQUEST-ID."
  (unless request-id (error ":request-id is required"))
  (cond
   ((and content error) (error "Provide :content or :error, not both"))
   (error   `((:request-id . ,request-id) (:error  . ,error)))
   (content `((:request-id . ,request-id) (:result . ((content . ,content)))))
   (t       (error "Must provide :content or :error"))))

(cl-defun emagent-acp-make-fs-write-text-file-response (&key request-id error)
  "Build a \"fs/write_text_file\" response.

Arguments: REQUEST-ID, ERROR."
  (unless request-id (error ":request-id is required"))
  (if error
      `((:request-id . ,request-id) (:error  . ,error))
    `((:request-id . ,request-id) (:result . nil))))

(provide 'emagent-acp-protocol)
;;; emagent-acp-protocol.el ends here
