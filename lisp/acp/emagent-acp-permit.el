;;; emagent-acp-permit.el --- Permission helpers and cursor tool-resolve  -*- lexical-binding: t; -*-

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

;; Permission option helpers, dangerous command detection, cursor store.db
;; tool-resolve queue, and permission drain scheduling for the ACP layer.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-tool-call)
(require 'emagent-tools)
(require 'emagent-policy)

(declare-function emagent-chat-allowed-permissions "emagent-chat")
(declare-function emagent-chat-add-allowed-permission "emagent-chat")

(declare-function emagent-acp--send-request "emagent-acp")
(declare-function emagent-acp--emit-tool-call-display "emagent-acp")
(declare-function emagent-chat--open-response-p "emagent-chat")
(declare-function emagent-cursor-enrich-tool-call-update "emagent-cursor")

(defun emagent-acp--permission-option-deny-p (opt)
  "Return non-nil when OPT is a deny-type ACP permission option."
  (let ((kind (downcase (or (map-elt opt 'kind) "")))
        (id (downcase (or (map-elt opt 'optionId) "")))
        (name (downcase (or (map-elt opt 'name) ""))))
    (or (member kind '("deny" "deny_once" "deny-once" "reject"))
        (member id '("deny" "deny_once" "deny-once" "no" "reject"))
        (string-match-p "deny\\|reject" id)
        (string-match-p "\\`\\(?:deny\\|no\\|reject\\)" name))))

(defun emagent-acp--permission-option-always-id (options)
  "Return allow_always/allow-always optionId from OPTIONS, or nil."
  (when options
    (or (map-elt (seq-find (lambda (opt)
                             (let ((id (downcase (or (map-elt opt 'optionId) ""))))
                               (member id '("allow_always" "allow-always"))))
                   options)
                'optionId)
        (map-elt (seq-find (lambda (opt)
                             (let ((kind (downcase (or (map-elt opt 'kind) ""))))
                               (member kind '("allow_always" "allow-always"))))
                   options)
                'optionId))))

(defconst emagent-acp--permission-acp-allow-prefer
  '("allow_once" "allow-once" "run_once" "yes" "run" "allow")
  "ACP optionIds emagent may return to the agent (never allow_always).")

(defconst emagent-acp--permission-emagent-choices
  '(("Allow" . :allow-once)
    ("Allow for session" . :allow-session)
    ("Allow always" . :allow-always)
    ("Allow all (session)" . :allow-all)
    ("Deny" . :deny))
  "User-facing permission choices handled by emagent, not the external agent.")

(defun emagent-acp--permission-acp-allow-id (options)
  "Return a one-shot allow optionId from OPTIONS; never allow_always.
When no one-shot option is available, falls back to allow_always as
last resort so the permission can still be granted."
  (or (map-elt (seq-find (lambda (opt)
                           (let ((id (downcase (or (map-elt opt 'optionId) ""))))
                             (and id (member id emagent-acp--permission-acp-allow-prefer))))
                 options)
              'optionId)
      (let ((fallback (map-elt (seq-find #'emagent-acp--permission-option-allow-p options)
                               'optionId)))
        (when (and fallback
                   (not (member (downcase fallback) '("allow_always" "allow-always"))))
          fallback))
      ;; Last resort: use allow_always when no one-shot option exists.
      (let ((always (emagent-acp--permission-option-always-id options)))
        (when always
          (emagent-log "permission: no one-shot allow option found, falling back to allow_always")
          always))))

(defun emagent-acp--permission-acp-deny-id (options)
  "Return a deny optionId from OPTIONS, or nil."
  (map-elt (seq-find #'emagent-acp--permission-option-deny-p options) 'optionId))

(defun emagent-acp--tool-call-eval-form (tool-call)
  "Return an eval form string from permission TOOL-CALL, or nil."
  (when tool-call
    (let ((raw (or (map-elt tool-call 'arguments)
                   (map-elt tool-call 'rawInput))))
      (when-let ((data (emagent-acp--tool-call-normalize-data raw)))
        (or (emagent-acp--tool-call-data-get data 'form)
            (emagent-acp--tool-call-data-get data 'code))))))

(defun emagent-acp--tool-call-path (tool-call)
  "Return a file path from permission TOOL-CALL, or nil."
  (when tool-call
    (let ((raw (or (map-elt tool-call 'arguments)
                   (map-elt tool-call 'rawInput))))
      (when-let ((data (emagent-acp--tool-call-normalize-data raw)))
        (or (emagent-acp--tool-call-data-get data 'path)
            (emagent-acp--tool-call-data-get data 'file_path)
            (emagent-acp--tool-call-data-get data 'file))))))

(defun emagent-acp--permission-fingerprint (tool-call)
  "Return a stable fingerprint string for auto-allowing similar TOOL-CALLs."
  (when tool-call
    (let* ((kind (downcase (or (map-elt tool-call 'kind) "")))
           (title (or (map-elt tool-call 'title) ""))
           (command (emagent-acp--tool-call-command-text tool-call))
           (form (emagent-acp--tool-call-eval-form tool-call))
           (path (emagent-acp--tool-call-path tool-call)))
      (cond
       ((and (string= kind "execute") (stringp command) (not (string-empty-p command)))
        (format "execute:%s" (car (split-string command "[[:space:]]+" t))))
       (form
        ;; Key on the form text itself: distinct elisp forms must not share a
        ;; fingerprint, or approving one would auto-allow unrelated evals.
        (format "eval:%s" (secure-hash 'sha1 form)))
       ((and (member kind '("read" "write")) path)
        (format "%s:%s" kind path))
       ((not (string-empty-p title))
        (format "%s:%s" (if (string-empty-p kind) "tool" kind) title))
       (command (format "execute:%s" command))
       (t "unknown")))))

(defun emagent-acp--tool-call-execute-p (tool-call)
  "Return non-nil when TOOL-CALL is an execute (shell) request."
  (let ((kind (and (listp tool-call) (map-elt tool-call 'kind))))
    (and (stringp kind) (equal (downcase kind) "execute"))))

(defun emagent-acp--permission-validate (tool-call)
  "Return nil when TOOL-CALL passes emagent checks.
Otherwise (:deny . REASON) or (:confirm . REASON)."
  (or       (when-let ((form (emagent-acp--tool-call-eval-form tool-call)))
        (emagent-policy-check-elisp form))
      (when-let* ((command (and (emagent-acp--tool-call-execute-p tool-call)
                                (emagent-acp--tool-call-command-text tool-call))))
        (emagent-policy-check-shell command))))

(defun emagent-acp--permission-auto-allowed-p (state fingerprint chat-buffer)
  "Return non-nil when FINGERPRINT is auto-approved for STATE or CHAT-BUFFER."
  (or (map-elt state :session-auto-approve)
      (and fingerprint
           (or (member fingerprint (or (map-elt state :permission-auto-allow) nil))
               (and chat-buffer (buffer-live-p chat-buffer)
                    (with-current-buffer chat-buffer
                      (member fingerprint (emagent-chat-allowed-permissions))))))))

(defun emagent-acp--permission-gate-auto-approve-p (state tool-call validation fingerprint chat-buffer)
  "Return non-nil when emagent should approve without prompting."
  (and (not (and validation (eq (car validation) :deny)))
       (or (emagent-acp--permission-auto-allowed-p state fingerprint chat-buffer)
           (and (eq emagent-acp-auto-approve-permissions t)
                (not (and validation (eq (car validation) :confirm))))
           (and (eq emagent-acp-auto-approve-permissions 'safe)
                (not (emagent-acp--tool-call-shell-needs-confirm-p tool-call))
                (not (and validation (eq (car validation) :confirm)))))))

(defun emagent-acp--permission-apply-choice (state fingerprint chat-buffer choice)
  "Record user CHOICE for FINGERPRINT in STATE and CHAT-BUFFER."
  (pcase choice
    (:allow-all
     (map-put! state :session-auto-approve t)
     (emagent-log "permission: allow all (session)"))
    (:allow-session
     (when fingerprint
       (map-put! state :permission-auto-allow
                 (append (or (map-elt state :permission-auto-allow) nil)
                         (list fingerprint)))))
    (:allow-always
     (when fingerprint
       (map-put! state :permission-auto-allow
                 (append (or (map-elt state :permission-auto-allow) nil)
                         (list fingerprint)))
       (when (and chat-buffer (buffer-live-p chat-buffer))
         (with-current-buffer chat-buffer
           (emagent-chat-add-allowed-permission fingerprint)))))
    (_ nil)))

(defun emagent-acp--permission-approved-choice-p (choice)
  "Return non-nil when user CHOICE approves the request."
  (memq choice '(:allow-once :allow-session :allow-always :allow-all)))

(defun emagent-acp--permission-option-allow-p (opt)
  "Return non-nil when OPT is an allow-type ACP permission option."
  (let ((kind (downcase (or (map-elt opt 'kind) "")))
        (id (downcase (or (map-elt opt 'optionId) "")))
        (name (downcase (or (map-elt opt 'name) ""))))
    (or (member kind '("allow" "allow_once" "allow_always" "allow-once" "allow-always"))
        (member id '("allow_once" "allow-once" "allow_always" "allow-always" "allow" "yes" "run" "run_once"))
        (string-match-p "allow" id)
        (string-match-p "\\`\\(?:allow\\|yes\\|run\\)" name))))

(defconst emagent-acp--tool-call-detail-limit 120
  "Maximum detail length shown in Executing tool-call lines.")

(defun emagent-acp--update-put (update key value)
  "Return UPDATE alist with KEY bound to VALUE, replacing any prior binding."
  (cons (cons key value) (assoc-delete-all key update)))

(defun emagent-acp--cursor-agent-p (state)
  "Return non-nil when STATE's agent is Cursor's cursor-agent CLI."
  (when-let ((launch (emagent-acp--agent-launch-string state)))
    (string-match-p "cursor-agent" launch)))

(defconst emagent-acp--cursor-tool-resolve-max-attempts 8
  "Maximum store.db lookups while waiting for Cursor tool-call args.")

(defconst emagent-acp--cursor-tool-resolve-base-delay 0.05
  "Initial idle delay between Cursor store.db resolve retries.")

(defun emagent-acp--cursor-tool-resolve-active-p (state)
  "Return non-nil while Cursor store.db tool-call lookups are pending."
  (and (emagent-acp--cursor-agent-p state)
       (or (map-elt state :cursor-tool-resolve-worker)
           (map-elt state :cursor-tool-resolve-queue))))

(defun emagent-acp--tool-call-shell-needs-confirm-p (tool-call)
  "Return non-nil when TOOL-CALL is execute and policy requires confirmation."
  (when-let ((command (and (emagent-acp--tool-call-execute-p tool-call)
                            (emagent-acp--tool-call-command-text tool-call))))
    (emagent-policy-shell-needs-confirm-p command)))

(defun emagent-acp--tool-call-command-text (tool-call)
  "Extract the command string from a tool-call ACP object."
  (or (when-let* ((raw (or (map-elt tool-call 'rawInput)
                           (map-elt tool-call 'arguments)))
                  (data (emagent-acp--tool-call-normalize-data raw)))
        (or (emagent-acp--tool-call-data-get data 'command)
            (emagent-acp--tool-call-data-get data 'text)
            (emagent-acp--tool-call-data-get data 'cmd)))
      (map-elt tool-call 'subtitle)
      (map-elt tool-call 'title)))

(defun emagent-acp--tool-call-content-block (tool-call)
  "Return an org subsection string for the permission prompt, or nil.
For shell commands, returns a code block with the command.
For edits, returns the file path and content.
For reads, returns nil (the question line is enough)."
  (when (and (listp tool-call) (map-elt tool-call 'kind))
    (let* ((kind (downcase (or (map-elt tool-call 'kind) "")))
           (raw (or (map-elt tool-call 'rawInput)
                    (map-elt tool-call 'arguments)))
           (command (emagent-acp--tool-call-command-text tool-call))
           (detail (emagent-acp--tool-call-detail-from-tool-call tool-call))
           (path (and (member kind '("read" "write"))
                      (or (emagent-acp--tool-call-data-get
                           (emagent-acp--tool-call-normalize-data raw)
                           'path)
                          (emagent-acp--tool-call-data-get
                           (emagent-acp--tool-call-normalize-data raw)
                           'file_path)
                          detail))))
      (cond
       ((member kind '("execute" ""))
        (when command
          (format "** Allow execute\n#+BEGIN_SRC sh\n%s\n#+END_SRC" command)))
       ((equal kind "write")
        (if path
            (let ((content (emagent-acp--tool-call-data-get
                           (emagent-acp--tool-call-normalize-data raw)
                           'content))
                  (lang (if (string-match "\\.\\([a-z]+\\)\\'" path)
                            (match-string 1 path)
                          "text"))
                  (heading (if detail (format "Allow edit: %s" detail)
                             "Allow edit")))
              (if (and content (not (string-empty-p content)))
                  (format "** %s\n#+BEGIN_SRC %s\n%s\n#+END_SRC"
                          heading lang (substring content 0 (min (length content) 1000)))
                (format "** Allow edit\n= %s =" path)))
          (when command
            (format "** Allow edit\n#+BEGIN_SRC sh\n%s\n#+END_SRC" command))))
       (t nil)))))

(defun emagent-acp--permission-interactive-p (state)
  "Return non-nil when ACP permission prompts may need user input."
  (and (not (map-elt state :session-auto-approve))
       (not (eq emagent-acp-auto-approve-permissions t))))


(defun emagent-acp--schedule-permission-drain (state)
  "Run `emagent-acp--drain-permission-queue-now' outside the ACP process filter."
  (unless (or (map-elt state :permission-drain-timer)
              (map-elt state :permission-busy))
    (map-put! state :permission-drain-timer
              (run-at-time 0 nil
                           (lambda ()
                             (map-put! state :permission-drain-timer nil)
                             (emagent-acp--drain-permission-queue-now state))))))

(defun emagent-acp--enqueue-cursor-tool-resolve (state id &optional delay)
  "Queue ID for a serialized Cursor store.db lookup."
  (let ((queue (map-elt state :cursor-tool-resolve-queue)))
    (unless (member id queue)
      (map-put! state :cursor-tool-resolve-queue (append queue (list id)))))
  (unless (map-elt state :cursor-tool-resolve-worker)
    (emagent-acp--drain-cursor-tool-resolve-queue state delay)))

(defun emagent-acp--drain-cursor-tool-resolve-queue (state &optional delay)
  "Resolve one queued Cursor tool call via `run-at-time'."
  (unless (map-elt state :cursor-tool-resolve-worker)
    (if-let ((id (car (map-elt state :cursor-tool-resolve-queue))))
        (progn
          (map-put! state :cursor-tool-resolve-worker t)
          (run-at-time
           (or delay 0) nil
           (lambda ()
             (map-put! state :cursor-tool-resolve-worker nil)
             (map-put! state :cursor-tool-resolve-queue
                         (cdr (map-elt state :cursor-tool-resolve-queue)))
             (let ((retry-delay (emagent-acp--resolve-cursor-tool-from-store state id)))
               (if retry-delay
                   (emagent-acp--drain-cursor-tool-resolve-queue state retry-delay)
                 (progn
                   (emagent-acp--drain-cursor-tool-resolve-queue state 0)
                   (emagent-acp--maybe-complete-deferred-prompt state)))))))
      (progn
        (emagent-acp--drain-permission-queue state)
        (emagent-acp--maybe-complete-deferred-prompt state)))))

(defun emagent-acp--resolve-cursor-tool-from-store (state id)
  "Look up tool-call ID in Cursor store.db; return retry delay or nil when done."
  (when-let* ((pending-table (map-elt state :tool-call-pending))
              (merged (gethash id pending-table))
              (session-id (map-elt state :session-id))
              (fboundp 'emagent-cursor-enrich-tool-call-update))
    (let* ((enriched (emagent-cursor-enrich-tool-call-update session-id merged))
           (merged (if (equal enriched merged) merged
                     (emagent-acp--merged-tool-call-update state enriched)))
           (label (emagent-acp--tool-call-label merged))
           (status (map-elt merged 'status))
           (kind (map-elt merged 'kind))
           (attempts-table (map-elt state :cursor-tool-resolve-attempts))
           (attempts (gethash id attempts-table 0)))
      (cond
       ((emagent-acp--tool-call-meaningful-detail-p merged)
        (puthash id 0 attempts-table)
        (emagent-acp--emit-tool-call-display state id kind merged label status)
        (remhash id pending-table)
        nil)
       ((>= attempts emagent-acp--cursor-tool-resolve-max-attempts)
        (puthash id 0 attempts-table)
        (when (emagent-acp--tool-call-displayable-p merged)
          (emagent-acp--emit-tool-call-display state id kind merged label status))
        (remhash id pending-table)
        nil)
       (t
        (puthash id (1+ attempts) attempts-table)
        (let ((queue (map-elt state :cursor-tool-resolve-queue)))
          (unless (member id queue)
            (map-put! state :cursor-tool-resolve-queue (append queue (list id)))))
        (* emagent-acp--cursor-tool-resolve-base-delay (expt 2 attempts)))))))

(provide 'emagent-acp-permit)
;;; emagent-acp-permit.el ends here
