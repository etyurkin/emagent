;;; emagent-acp-state.el --- ACP session state for emagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ACP session state hash table, stderr filtering helpers, RSS monitoring
;; timers, and the state constructor.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-protocol)

(declare-function emagent-acp--refresh-mode-line "emagent-acp")

(defvar-local emagent-acp--session nil
  "ACP session state for the current emagent buffer.")

(defun emagent-acp--strip-pino-colors (string)
  "Remove literal pino color tokens like [32m from STRING."
  (replace-regexp-in-string "\\[[0-9]+m" "" string))

(defun emagent-acp--agent-log-line-p (line)
  "Return non-nil when LINE is cursor-agent info/warn stderr."
  (let ((line (string-trim (emagent-acp--strip-pino-colors line))))
    (or (string-empty-p line)
        (string-match-p "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] .*\\[ *\\(info\\|warn\\) *\\]:" line))))

(defun emagent-acp--stderr-notify-p (emagent-acp-error)
  "Return non-nil when ACP-ERROR should be shown to the user."
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
  (when emagent-log-agent-stderr
    (emagent-log "agent: %s" (string-trim message))))

(defun emagent-acp--session ()
  (or emagent-acp--session
      (error "No active emagent session for this buffer")))

(defun emagent-acp--agent-rss-mb (state)
  "Return the agent process RSS in MB via `process-attributes', or nil."
  (when-let* ((client (map-elt state :client))
              (proc (and client (map-elt client :process)))
              ((processp proc))
              (pid (process-id proc))
              ((> pid 0))
              (attrs (ignore-errors (process-attributes pid)))
              (rss-kb (alist-get 'rss attrs)))
    (round (/ (float rss-kb) 1024))))

(defun emagent-acp--start-rss-timer (state)
  "Start a repeating timer that refreshes :agent-rss in STATE every 15 s."
  (when-let ((old (map-elt state :agent-rss-timer)))
    (cancel-timer old))
  (map-put! state :agent-rss-timer
            (run-with-timer
             5 15
             (lambda ()
               (if (buffer-live-p (map-elt state :chat-buffer))
                   (let ((mb (emagent-acp--agent-rss-mb state)))
                     (map-put! state :agent-rss mb)
                     (emagent-acp--refresh-mode-line state))
                 (emagent-acp--stop-rss-timer state))))))

(defun emagent-acp--stop-rss-timer (state)
  "Cancel the RSS polling timer for STATE."
  (when-let ((timer (and state (map-elt state :agent-rss-timer))))
    (cancel-timer timer)
    (map-put! state :agent-rss-timer nil)))

(defun emagent-acp--turn-phase (state)
  "Return the lifecycle phase of STATE's current turn.

One of:
  `idle'        no turn in flight;
  `streaming'   a prompt is in flight (`:busy'), receiving output and possibly
                paused on a permission prompt;
  `finalizing'  streaming ended, the response is being rendered;
  `done'        the response has been fully rendered.

This derives the phase from the turn flags so callers share one vocabulary for
the turn state machine.  The flags remain the underlying representation for now."
  (cond
   ((map-elt state :busy) 'streaming)
   ((map-elt state :prompt-finishing)
    (if (map-elt state :prompt-finalized) 'done 'finalizing))
   (t 'idle)))

(defun emagent-acp--connected-p ()
  "Return non-nil when the current buffer has a live, ready ACP session."
  (and emagent-acp--session
       (map-elt emagent-acp--session :ready)
       (let ((client (map-elt emagent-acp--session :client)))
         (and client (emagent-acp--client-started-p client)))))

(defun emagent-acp--cancel-state-timers (state)
  "Cancel every timer stored in STATE and clear its slot.
Prevents a reconnect or shutdown from leaving repeating/pending timers
(RSS poll, watchdog, finish, permission drain) pointed at dead state."
  (dolist (key '(:agent-rss-timer :prompt-watchdog-timer
                 :finish-timer :permission-drain-timer))
    (when-let ((timer (map-elt state key)))
      (when (timerp timer) (cancel-timer timer))
      (map-put! state key nil))))

(defun emagent-acp--teardown-stale-session ()
  "Shut down a dead or incomplete ACP session without clearing persisted ids."
  (when-let* ((state emagent-acp--session))
    (emagent-acp--cancel-state-timers state)
    (when-let ((client (map-elt state :client)))
      (ignore-errors (emagent-acp-shutdown :client client))))
  (setq emagent-acp--session nil))

(cl-defun emagent-acp--make-state (&key client chat-buffer on-reveal)
  "Return a mutable session state table for Emacs 30 `map-put!'.

Plain alists cannot grow via `map-put!' on Emacs 30; hash tables can."
  (let ((state (make-hash-table :test 'eq))
        (usage (make-hash-table :test 'eq)))
    (puthash :context-used nil usage)
    (puthash :context-size nil usage)
    (puthash :total-tokens 0 usage)
    (puthash :client client state)
    (puthash :chat-buffer chat-buffer state)
    (puthash :session-id nil state)
    (puthash :config-options nil state)
    (puthash :usage usage state)
    (puthash :initialized nil state)
    (puthash :mcp-http nil state)
    (puthash :permission-queue nil state)
    (puthash :permission-busy nil state)
    (puthash :permission-drain-timer nil state)
    (puthash :deferred-complete-response nil state)
    (puthash :session-auto-approve nil state)
    (puthash :permission-auto-allow nil state)
    (puthash :external-tool-gate-reasons nil state)
    (puthash :external-tool-gate-proactive-logged nil state)
    (puthash :external-tool-refusal-logged nil state)
    (puthash :ready nil state)
    (puthash :busy nil state)
    (puthash :assistant-text "" state)
    (puthash :thought-text "" state)
    (puthash :thought-buffer "" state)
    (puthash :prompt-finalized nil state)
    (puthash :prompt-finishing nil state)
    (puthash :prompt-generation 0 state)
    (puthash :finish-token nil state)
    (puthash :finish-timer nil state)
    (puthash :prompt-watchdog nil state)
    (puthash :extra-context nil state)
    (puthash :compress-pending nil state)
    (puthash :replaying-history nil state)
    (puthash :current-tool nil state)
    (puthash :current-tool-kind nil state)
    (puthash :tool-call-titles (make-hash-table :test 'equal) state)
    (puthash :tool-call-inputs (make-hash-table :test 'equal) state)
    (puthash :tool-call-labels (make-hash-table :test 'equal) state)
    (puthash :tool-call-decisions (make-hash-table :test 'equal) state)
    (puthash :tool-call-pending (make-hash-table :test 'equal) state)
    (puthash :provider nil state)
    (puthash :tool-resolve-queue nil state)
    (puthash :tool-resolve-worker nil state)
    (puthash :tool-resolve-attempts (make-hash-table :test 'equal) state)
    (puthash :cb-chunk nil state)
    (puthash :cb-thought nil state)
    (puthash :cb-finish nil state)
    (puthash :cb-fail nil state)
    (puthash :cb-slash-commands nil state)
    (puthash :cb-tool-call nil state)
    (puthash :cb-permission nil state)
    (puthash :agent-rss nil state)
    (puthash :agent-rss-timer nil state)
    (puthash :on-reveal on-reveal state)
    state))

(provide 'emagent-acp-state)
;;; emagent-acp-state.el ends here
