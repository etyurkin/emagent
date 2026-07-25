;;; emagent-acp-tool-call.el --- ACP tool-call display facade  -*- lexical-binding: t; -*-

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

;; Session mutation and display for ACP tool calls.  Parsing, edit/diff,
;; and shell/CLI block specs live in leaf modules required below.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'emagent-acp-state)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-provider)
(require 'emagent-acp-tool-block)


(defun emagent-acp--tool-call-emagent-tool-p (update)
  "Return non-nil when UPDATE names a tool from emagent's own MCP server.

Such tools run inside Emacs; once one's permission is granted (it is
running or finished) without a recorded ACP permission decision, its line
is tagged (Allow: Emacs) instead of the inferred (Allow: Agent) used for
agent-native tools.  A pending call stays untagged — it may still be
awaiting a permission prompt.

Detection relies on the emagent MCP namespace (e.g. `mcp_emagent_read_file'):
an explicit `emagent-tool' flag set by provider enrichment, or the word
`emagent' surviving in the title.  Bare tool names are deliberately not matched
because generic names like `grep' collide with agent-native tools."
  (or (map-elt update 'emagent-tool)
      (let ((title (downcase (string-trim (or (map-elt update 'title) "")))))
        (and (not (string-empty-p title))
             (string-match-p "\\bemagent\\b" title)
             t))))

(defun emagent-acp--switch-mode-tool-p (tool-call)
  "Return non-nil when TOOL-CALL is an ACP mode-switch permission/tool."
  (when tool-call
    (let ((kind (downcase (or (map-elt tool-call 'kind) "")))
          (title (string-trim (or (map-elt tool-call 'title) ""))))
      (or (string= kind "switch_mode")
          (string-match-p "\\`ExitPlanMode\\'" title)
          (string-match-p "\\`Ready to code[?]\\'" title)))))

(defun emagent-acp--switch-mode-target-id (tool-call)
  "Return target mode id from TOOL-CALL rawInput, or nil."
  (when-let* ((raw (or (map-elt tool-call 'rawInput)
                       (map-elt tool-call 'arguments)))
              (data (emagent-acp--tool-call-normalize-data raw)))
    (let ((target (or (emagent-acp--tool-call-data-get data 'targetModeId)
                      (emagent-acp--tool-call-data-get data 'target_mode_id))))
      (when (and (stringp target) (not (string-empty-p (string-trim target))))
        (string-trim target)))))

(defun emagent-acp--switch-mode-display-title (tool-call)
  "Return a user-facing title for switch_mode TOOL-CALL.

Never leaves bare `unknown' (Cursor titles SwitchMode that way when
targetModeId is missing)."
  (let* ((title (string-trim (or (map-elt tool-call 'title) "")))
         (raw (or (map-elt tool-call 'rawInput) (map-elt tool-call 'arguments)))
         (data (and raw (emagent-acp--tool-call-normalize-data raw)))
         (target (emagent-acp--switch-mode-target-id tool-call))
         (explanation (and data (emagent-acp--tool-call-data-get data 'explanation)))
         (explanation (and (stringp explanation)
                           (let ((e (string-trim explanation)))
                             (unless (string-empty-p e) e))))
         (bad (or (string-empty-p title)
                  (string-match-p "\\`unknown\\'" title)
                  (string-match-p ":\\s-*unknown\\s-*\\'" title))))
    (cond
     ((and (not bad) (not (string-empty-p title))) title)
     (target (format "Switch to %s" target))
     (explanation
      (format "Switch mode: %s"
              (truncate-string-to-width explanation 60 nil nil "...")))
     (t "Switch mode"))))


(defun emagent-acp--ingest-tool-call-request (state tool-call)
  "Merge TOOL-CALL from session/request_permission and refresh display.

Arguments: STATE."
  (when-let ((update (emagent-acp--tool-call-update-from-request tool-call)))
    (emagent-acp--on-tool-call state update)))

(defun emagent-acp--emit-tool-call-display (state id kind merged label status)
  "Push TOOL-CALL LABEL to the chat buffer and update session UI.

Arguments: STATE, ID, KIND, MERGED, STATUS."
  (let* ((labels (emagent-acp-state-tool-call-labels state))
         (prev (and id labels (gethash id labels)))
         (decision (and id (when-let ((d (emagent-acp-state-tool-call-decisions state)))
                             (gethash id d))))
         (completed (member status '("completed" "failed")))
         ;; A running or finished call already had its permission granted;
         ;; a pending call may still be awaiting a permission prompt.
         (granted (or completed (equal status "in_progress")))
         (display (cond
                   ((or (null label) (string-empty-p label)) label)
                   (decision (emagent-acp--permission-decision-label label decision))
                   ((and granted (emagent-acp--tool-call-emagent-tool-p merged))
                    (format "%s (Allow: Emacs)" label))
                   ;; Tool runs without ACP permission: the agent's own
                   ;; allow-list permitted it directly — infer the decision.
                   (granted (format "%s (Allow: Agent)" label))
                   (t label)))
         (label-changed (and display (not (string-empty-p display))
                             (or (null prev) (not (string= prev display))))))
    (when label
      (emagent-acp--detect-external-refusal-in-text state label))
    (when label-changed
      (when id (puthash id display labels))
      (unless completed
        (emagent-acp--notify-user state (format "emagent: tool %s" label)))
      (when-let ((buf (emagent-acp--chat-buffer state))
                 (cb (emagent-acp-state-cb-tool-call state)))
        (let ((spec (emagent-acp--tool-call-block-spec merged)))
          (with-current-buffer buf
            (funcall cb id display (car spec) (cdr spec))))))
    (if completed
        (progn
          (setf (emagent-acp-state-current-tool state) nil)
          (setf (emagent-acp-state-current-tool-kind state) nil))
      (when label-changed
        (setf (emagent-acp-state-current-tool state) label)
        (when kind (setf (emagent-acp-state-current-tool-kind state) kind))
        (emagent-acp--schedule-prompt-watchdog state)))
    (when (or label-changed completed)
      (emagent-acp--refresh-mode-line state))))


(defun emagent-acp--permission-choice-label (choice)
  "Return a short display label for permission CHOICE, or nil."
  (pcase choice
    (:allow-once "Once")
    (:allow-session "Session")
    (:allow-always "Always")
    (:allow-all "All")
    (:deny "Denied")
    (_ nil)))

(defun emagent-acp--permission-decision-label (base-label choice)
  "Return BASE-LABEL with permission CHOICE appended in parentheses when known.

A scoped approval (`:allow-session' etc.) renders as `(Allow: Session)'; a
generic approval (`:allow', used for policy/auto-trust) renders as `(Allow)';
`:deny' renders as `(Denied)'.  A string CHOICE (switch_mode optionId) is
shown as-is."
  (pcase choice
    ('nil base-label)
    (:deny (format "%s (Denied)" base-label))
    ((pred stringp) (format "%s (%s)" base-label choice))
    (_ (if-let ((suffix (emagent-acp--permission-choice-label choice)))
           (format "%s (Allow: %s)" base-label suffix)
         (format "%s (Allow)" base-label)))))


(defun emagent-acp--tool-call-displayable-p (state update)
  "Return non-nil when UPDATE should appear in the Thinking block.

Arguments: STATE."
  (let* ((title (string-trim (or (map-elt update 'title) "")))
         (detail (emagent-acp--tool-call-detail update)))
    (cond
     ;; Show when detail is meaningful — redundancy check belongs only in
     ;; label-building/block-spec, not in the visibility decision.
     ((and detail (emagent-acp--tool-call-meaningful-detail-p update)) t)
     ((and (not (string-empty-p title))
           (not (emagent-acp--tool-call-generic-title-p state title))
           (or (null detail) (string-empty-p detail)))
      t)
     (t nil))))

(defun emagent-acp--tool-call-label (update)
  "Return a display label for ACP tool-call UPDATE."
  (let* ((title (string-trim (or (map-elt update 'title) "tool")))
         (title (if (string-match-p "\\`MCP:? *tool\\'" title) "MCP" title))
         (switch-mode (emagent-acp--switch-mode-tool-p update))
         (title (if switch-mode
                    (emagent-acp--switch-mode-display-title update)
                  title))
         (detail (and (not switch-mode)
                      (emagent-acp--tool-call-detail update))))
    (cond
     ((and detail (not (string-empty-p detail))
           (not (string-match-p (regexp-quote detail) title))
           (not (emagent-acp--tool-call-redundant-detail-p title detail)))
      (format "%s: %s" title (emagent-acp--tool-call-truncate detail)))
     ;; Detail is redundant (basename already in title) or equals title: when
     ;; it is an absolute path, the title carries a user-friendly relative path
     ;; — prefer the title so the operation name and relative path stay visible.
     ((and detail (not (string-empty-p detail))
           (string-match-p "\\`/" (string-trim detail)))
      title)
     ((and detail (not (string-empty-p detail))) detail)
     (t title))))

(defun emagent-acp--merged-tool-call-update (state update)
  "Return UPDATE merged with stored title/rawInput for STATE."
  (let* ((id (map-elt update 'toolCallId))
         (titles (emagent-acp-state-tool-call-titles state))
         (inputs (emagent-acp-state-tool-call-inputs state))
         (stored-title (and id titles (gethash id titles)))
         (stored-input (and id inputs (gethash id inputs)))
         (title (or (map-elt update 'title) stored-title))
         (raw-input (or (map-elt update 'rawInput)
                        (map-elt update 'arguments)
                        stored-input))
         (merged update))
    (when (and id title)
      (puthash id title titles))
    (when (and id raw-input)
      (puthash id raw-input inputs))
    (when title
      (setq merged (emagent-acp--update-put merged 'title title)))
    (when (and raw-input (not (emagent-acp--tool-call-raw-input-empty-p raw-input)))
      (setq merged (emagent-acp--update-put merged 'rawInput raw-input)))
    (when (and id (map-elt merged 'rawInput))
      (puthash id (map-elt merged 'rawInput) inputs))
    merged))

(defun emagent-acp--wakeup-tool-p (title)
  "Return non-nil when TITLE names the ScheduleWakeup harness tool."
  (and (stringp title)
       (string-match-p "\\(?:\\`\\|__\\)ScheduleWakeup\\'" (string-trim title))))

(defun emagent-acp--capture-schedule-wakeup (state update)
  "Record a ScheduleWakeup request from tool-call UPDATE in STATE.

The agent ends its turn after calling ScheduleWakeup and expects the
client to send the wakeup prompt after the delay.  Only recorded here;
the timer is armed when the turn completes (`emagent-acp--arm-wakeup'),
and a `stop' call cancels a pending wakeup immediately."
  (when (and emagent-acp-honor-schedule-wakeup
             (emagent-acp--wakeup-tool-p (map-elt update 'title)))
    (when-let* ((raw (map-elt update 'rawInput))
                (data (emagent-acp--tool-call-normalize-data raw)))
      (let ((stop (emagent-acp--tool-call-data-get data 'stop))
            (delay (emagent-acp--tool-call-data-get data 'delaySeconds))
            (prompt (emagent-acp--tool-call-data-get data 'prompt))
            (reason (emagent-acp--tool-call-data-get data 'reason)))
        (when (stringp delay)
          (setq delay (string-to-number delay)))
        (cond
         ((and stop (not (memq stop '(:false :json-false))))
          (emagent-acp--cancel-wakeup state)
          (emagent-log "wakeup: loop stopped by agent"))
         ((and (numberp delay) (> delay 0))
          (setf (emagent-acp-state-wakeup-request state)
                (list :delay (max 10 (min (round delay) 3600))
                      :prompt (and (stringp prompt)
                                   (not (string-empty-p prompt))
                                   prompt)
                      :reason (and (stringp reason)
                                   (not (string-empty-p reason))
                                   reason)))))))))

(defun emagent-acp--on-tool-call (state update)
  "Display or refresh a tool-call line from ACP UPDATE.

Arguments: STATE."
  (unless (or (emagent-acp-state-replaying-history state)
              (emagent-acp-state-quiet-prompt state))
    (let* ((update (emagent-acp--provider-enrich-tool-call state update))
           (id (map-elt update 'toolCallId))
           (status (map-elt update 'status))
           (kind (map-elt update 'kind))
           (merged (emagent-acp--merged-tool-call-update state update))
           (label (emagent-acp--tool-call-label merged))
           (pending-table (emagent-acp-state-tool-call-pending state))
           (defer (emagent-acp--provider-defer-tool-call-p state merged))
           (show (and label (not (string-empty-p label)) (not defer)
                        (emagent-acp--tool-call-displayable-p state merged))))
      (emagent-acp--capture-schedule-wakeup state merged)
      (when defer
        (puthash id merged pending-table)
        (emagent-acp--provider-enqueue-tool-resolve state id))
      (when show
        (emagent-acp--emit-tool-call-display state id kind merged label status)
        (when id (remhash id pending-table)))
      (when (emagent-acp-state-permission-queue state)
        (unless (fboundp 'emagent-acp--drain-permission-queue)
          (require 'emagent-acp-permission-queue))
        (emagent-acp--drain-permission-queue state)))))

(provide 'emagent-acp-tool-call)
;;; emagent-acp-tool-call.el ends here
