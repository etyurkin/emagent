;;; emagent-acp-permit.el --- Permission helpers and cursor tool-resolve  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

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
(require 'emagent-acp-provider)
(require 'emagent-tools)
(require 'emagent-policy)
(require 'emagent-permissions)
(require 'emagent-session)

(declare-function emagent-chat-show-tool-call "emagent-chat")
(declare-function emagent-acp--chat-buffer "emagent-acp-usage")

(defun emagent-acp--hydrate-session-permissions (state session-id)
  "Load ~/.emagent session permissions for SESSION-ID into STATE."
  (when (and session-id (not (string-empty-p session-id)))
    (map-put! state :permission-auto-allow
              (copy-sequence (emagent-permissions-session-fingerprints session-id)))
    (when (emagent-permissions-session-auto-approve-p session-id)
      (map-put! state :session-auto-approve t))))

(declare-function emagent-acp--send-request "emagent-acp")
(declare-function emagent-acp--emit-tool-call-display "emagent-acp")
(declare-function emagent-chat--open-response-p "emagent-chat")

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
  "Return a one-shot allow optionId from OPTIONS, or nil; never allow_always.

emagent always answers the agent one-shot and remembers durable grants on its
own side (~/.emagent), so a user's \"Allow once\" can never become a permanent
agent-side whitelist.  If the agent offers no one-shot allow option, return nil
so the request is cancelled (fail-closed) rather than escalated to allow_always."
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
      (progn
        (when (emagent-acp--permission-option-always-id options)
          (emagent-log "permission: agent offers only allow_always; refusing to escalate a one-shot grant, cancelling"))
        nil)))

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

(defconst emagent-acp--tool-call-path-keys
  '(path file_path filePath target_file relativeWorkspacePath file filename)
  "JSON keys that carry a file path in ACP tool-call rawInput.")

(defconst emagent-acp--tool-call-edit-old-keys
  '(old_string oldText old_text oldString before search)
  "JSON keys for text replaced by patch-style edits.")

(defconst emagent-acp--tool-call-edit-new-keys
  '(new_string newText new_text newString after replace content text)
  "JSON keys for replacement text in patch-style edits.")

(defun emagent-acp--tool-call-data-path (data)
  "Return the first file path string from tool-call DATA, or nil."
  (when data
    (cl-loop for key in emagent-acp--tool-call-path-keys
             for val = (emagent-acp--tool-call-data-get data key)
             when (and (stringp val) (not (string-empty-p val)))
             return val)))

(defun emagent-acp--tool-call-locations-path (tool-call)
  "Return a file path from TOOL-CALL locations, or nil."
  (when tool-call
    (emagent-acp--tool-call-locations-detail (map-elt tool-call 'locations))))

(defun emagent-acp--tool-call-write-kind-p (kind)
  "Return non-nil when KIND is a file write/edit tool call."
  (and kind (member kind '("write" "edit"))))

(defun emagent-acp--tool-call-path (tool-call)
  "Return a file path from permission TOOL-CALL, or nil."
  (when tool-call
    (or (emagent-acp--tool-call-locations-path tool-call)
        (let ((raw (or (map-elt tool-call 'arguments)
                       (map-elt tool-call 'rawInput))))
          (when-let ((data (emagent-acp--tool-call-normalize-data raw)))
            (emagent-acp--tool-call-data-path data))))))

(defun emagent-acp--permission-fingerprint (tool-call)
  "Return a stable fingerprint string for auto-allowing similar TOOL-CALLs.

Execute commands are keyed on the executable name only (first word), so
approving `mvn test A' covers `mvn test B' in the same session.  Policy
rules still block dangerous commands (rm, mv, etc.) regardless."
  (when tool-call
    (let* ((kind    (downcase (or (emagent-acp--tool-call-infer-kind tool-call) "")))
           (command (emagent-acp--tool-call-command-text tool-call))
           (form    (emagent-acp--tool-call-eval-form tool-call))
           (path    (emagent-acp--tool-call-path tool-call))
           (title   (or (map-elt tool-call 'title) "")))
      (cond
       ((and (string= kind "execute") (stringp command) (not (string-empty-p command)))
        (format "execute:%s" (car (split-string command "[[:space:]]+" t))))
       (form
        (format "eval:%s" (secure-hash 'sha1 form)))
       ((and (member kind '("read" "write")) path)
        (format "%s:%s" kind path))
       ((not (string-empty-p title))
        (format "%s:%s" (if (string-empty-p kind) "tool" kind) title))
       (command
        (format "execute:%s" (car (split-string command "[[:space:]]+" t))))
       (t "unknown")))))

(defun emagent-acp--tool-call-infer-kind (tool-call)
  "Guess tool-call kind when ACP omits it (common with Cursor permissions)."
  (when tool-call
    (let* ((explicit (map-elt tool-call 'kind))
           (title (downcase (or (map-elt tool-call 'title) "")))
           (raw (or (map-elt tool-call 'rawInput) (map-elt tool-call 'arguments)))
           (data (when raw (emagent-acp--tool-call-normalize-data raw)))
           (content (when data (emagent-acp--tool-call-data-get data 'content)))
           (edits (when data (emagent-acp--tool-call-data-get data 'edits)))
           (command (emagent-acp--tool-call-command-text tool-call)))
      (cond
       ((and explicit (not (string-empty-p explicit)))
        (downcase explicit))
       ((or (and content (not (string-empty-p content))) edits) "write")
       ((emagent-acp--tool-call-edit-field data 'new_string 'newText 'new_text
                                           'newString 'after 'replace 'content 'text)
        "write")
       ((string-match-p "\\(?:edit\\|write\\|apply\\|replace\\|patch\\)" title) "write")
       ((string-match-p "\\`read" title) "read")
       ((emagent-acp--tool-call-eval-form tool-call) "eval")
       (command "execute")
       (t nil)))))

(defun emagent-acp--tool-call-execute-p (tool-call)
  "Return non-nil when TOOL-CALL is an execute (shell) request."
  (let ((kind (emagent-acp--tool-call-infer-kind tool-call)))
    (and kind (member kind '("execute")))))

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
               (member fingerprint (emagent-permissions-global-fingerprints))
               (member fingerprint
                       (emagent-permissions-session-fingerprints
                        (map-elt state :session-id)))
               (and chat-buffer (buffer-live-p chat-buffer)
                    (with-current-buffer chat-buffer
                      (or (member fingerprint (emagent-session-allowed-permissions))
                          (member fingerprint
                                  (emagent-permissions-project-fingerprints
                                   (emagent-session-project-directory))))))))))

(defun emagent-acp--permission-choice-label (choice)
  "Return a short display label for permission CHOICE, or nil."
  (pcase choice
    (:allow-once "Once")
    (:allow-session "Session")
    (:allow-always "Always")
    (:allow-all "All")
    (:deny "Denied")
    (_ nil)))

(defun emagent-acp--permission-stored-auto-choice (state fingerprint chat-buffer)
  "Return the stored user CHOICE that auto-approves FINGERPRINT, or nil."
  (cond
   ((map-elt state :session-auto-approve) :allow-all)
   ((and fingerprint (member fingerprint (emagent-permissions-global-fingerprints)))
    :allow-always)
   ((and fingerprint
         (member fingerprint (or (map-elt state :permission-auto-allow) nil)))
    :allow-session)
   ((and fingerprint
         (member fingerprint
                 (emagent-permissions-session-fingerprints
                  (map-elt state :session-id))))
    :allow-session)
   ((and fingerprint chat-buffer (buffer-live-p chat-buffer)
         (with-current-buffer chat-buffer
           (or (member fingerprint (emagent-session-allowed-permissions))
               (member fingerprint
                       (emagent-permissions-project-fingerprints
                        (emagent-session-project-directory))))))
    :allow-session)
   (t nil)))

(defun emagent-acp--permission-decision-label (base-label choice)
  "Return BASE-LABEL with permission CHOICE appended in parentheses when known.

A scoped approval (`:allow-session' etc.) renders as `(Allow: Session)'; a
generic approval (`:allow', used for policy/auto-trust) renders as `(Allow)';
`:deny' renders as `(Denied)'."
  (pcase choice
    ('nil base-label)
    (:deny (format "%s (Denied)" base-label))
    (_ (if-let ((suffix (emagent-acp--permission-choice-label choice)))
           (format "%s (Allow: %s)" base-label suffix)
         (format "%s (Allow)" base-label)))))

(defun emagent-acp--show-permission-decision (state tool-call choice)
  "Update the permission tool-call line for TOOL-CALL with CHOICE.

Records CHOICE in STATE's :tool-call-decisions table so later tool_call_update
renders of the same line keep the decision suffix instead of dropping it."
  (when (and tool-call choice)
    (when-let* ((id (map-elt tool-call 'toolCallId))
                (update (emagent-acp--tool-call-update-from-request tool-call))
                (merged (emagent-acp--merged-tool-call-update state update))
                (base (emagent-acp--tool-call-label merged))
                (label (emagent-acp--permission-decision-label base choice))
                (buf (emagent-acp--chat-buffer state)))
      (when-let ((decisions (map-elt state :tool-call-decisions)))
        (puthash id choice decisions))
      (let ((spec (emagent-acp--tool-call-block-spec merged)))
        (with-current-buffer buf
          (emagent-chat-show-tool-call id label (car spec) (cdr spec)))))))

(defun emagent-acp--permission-gate-auto-approve-p (state tool-call validation fingerprint chat-buffer)
  "Return non-nil when emagent should approve without prompting.

A policy :deny is never auto-approved.  A policy :confirm is auto-approved only
under \"Allow all (session)\" — the explicit user opt-out of prompting.  A
stored fingerprint grant (or the t/safe auto-approve modes) removes the prompt
only for policy-clean requests: it must not silence a :confirm, so e.g. an
`execute:rm' grant made for `rm foo.log' cannot auto-run `rm -rf ~'.

The `safe' mode auto-approves only `read'/`write' tool kinds; `execute', `eval',
and unknown/MCP tools always prompt under `safe' (an eval or MCP call is never
\"safe\" merely because it is not a shell command)."
  (let ((deny (and validation (eq (car validation) :deny)))
        (confirm (and validation (eq (car validation) :confirm))))
    (and (not deny)
         (or (map-elt state :session-auto-approve)
             (and (not confirm)
                  (or (emagent-acp--permission-auto-allowed-p state fingerprint chat-buffer)
                      (eq emagent-acp-auto-approve-permissions t)
                      (and (eq emagent-acp-auto-approve-permissions 'safe)
                           (member (emagent-acp--tool-call-infer-kind tool-call)
                                   '("read" "write")))))))))

(defun emagent-acp--permission-apply-choice (state fingerprint _chat-buffer choice)
  "Record user CHOICE for FINGERPRINT in STATE."
  (pcase choice
    (:allow-all
     (map-put! state :session-auto-approve t)
     (when-let ((session-id (map-elt state :session-id)))
       (emagent-permissions-set-session-auto-approve session-id))
     (emagent-log "permission: allow all (session)"))
    (:allow-session
     (when fingerprint
       (map-put! state :permission-auto-allow
                 (append (or (map-elt state :permission-auto-allow) nil)
                         (list fingerprint)))
       (when-let ((session-id (map-elt state :session-id)))
         (emagent-permissions-add-session-fingerprint session-id fingerprint))))
    (:allow-always
     (when fingerprint
       (emagent-permissions-add-global-fingerprint fingerprint)))
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

(defun emagent-acp--permission-tool-call (state tool-call)
  "Return TOOL-CALL merged with session inputs and provider enrichment."
  (when tool-call
    (let* ((update (or (emagent-acp--tool-call-update-from-request tool-call)
                       tool-call))
           (merged (emagent-acp--merged-tool-call-update state update)))
      (emagent-acp--provider-enrich-tool-call state merged))))

(defun emagent-acp--tool-call-edit-field (item &rest keys)
  (when item
    (cl-loop for key in keys
             for val = (emagent-acp--tool-call-data-get item key)
             when (and (stringp val) (not (string-empty-p val)))
             return val)))

(defun emagent-acp--tool-call-edit-items (data)
  (when data
    (when-let ((edits (emagent-acp--tool-call-data-get data 'edits)))
      (cond
       ((vectorp edits) (append edits nil))
       ((listp edits) edits)
       (t nil)))))

(defun emagent-acp--tool-call-edit-path (item)
  (emagent-acp--tool-call-edit-field
   item 'path 'file_path 'target_file 'relativeWorkspacePath 'file 'filename))

(defun emagent-acp--tool-call-write-path (tool-call raw detail)
  (or (emagent-acp--tool-call-path tool-call)
      (when-let ((data (and raw (emagent-acp--tool-call-normalize-data raw))))
        (or (emagent-acp--tool-call-data-path data)
            (when-let ((items (emagent-acp--tool-call-edit-items data)))
              (emagent-acp--tool-call-edit-path (car items)))))
      detail))

(defun emagent-acp--tool-call-apply-edit (text old new)
  (cond
   ((and (stringp old) (stringp new))
    (if (string-empty-p old)
        (concat (or text "") new)
      (replace-regexp-in-string (regexp-quote old) new (or text "") t t)))
   ((stringp new) new)
   (t text)))

(defun emagent-acp--tool-call-proposed-content (path data)
  (when (and path data)
    (let ((content (emagent-acp--tool-call-data-get data 'content))
          (items (emagent-acp--tool-call-edit-items data)))
      (cond
       ((and (stringp content) (not (string-empty-p content))) content)
       (items
        (let ((current (condition-case nil
                          (emagent-tools--read-file-content path)
                        (error ""))))
          (cl-loop for item in items
                   with text = current
                   for old = (emagent-acp--tool-call-edit-field
                              item 'old_string 'oldText 'old_text 'oldString
                              'before 'search)
                   for new = (emagent-acp--tool-call-edit-field
                              item 'new_string 'newText 'new_text 'newString
                              'after 'replace 'content 'text)
                   when (stringp new)
                   do (setq text (emagent-acp--tool-call-apply-edit text old new))
                   finally return (if (string-empty-p text) nil text))))
       ((emagent-acp--tool-call-edit-field
         data 'new_string 'newText 'new_text 'newString 'after 'replace 'content 'text)
        (let* ((current (condition-case nil
                           (emagent-tools--read-file-content path)
                         (error "")))
               (old (emagent-acp--tool-call-edit-field
                     data 'old_string 'oldText 'old_text 'oldString 'before 'search))
               (new (emagent-acp--tool-call-edit-field
                     data 'new_string 'newText 'new_text 'newString
                     'after 'replace 'content 'text)))
          (emagent-acp--tool-call-apply-edit current old new)))
       (t nil)))))

(defun emagent-acp--tool-call-edit-patch-string (path old new)
  (when (and (stringp new) (not (string-empty-p new)))
    (let* ((name (file-name-nondirectory path))
           (old-lines (when (and old (not (string-empty-p old)))
                        (split-string old "\n" t)))
           (new-lines (split-string new "\n" t)))
      (concat "--- " name " (current)\n+++ " name " (proposed)\n"
              (if old-lines
                  (concat "@@ edit @@\n"
                          (mapconcat (lambda (line) (concat "-" line))
                                     old-lines "\n")
                          "\n"
                          (mapconcat (lambda (line) (concat "+" line))
                                     new-lines "\n"))
                (mapconcat (lambda (line) (concat "+" line)) new-lines "\n"))))))

(defun emagent-acp--tool-call-edit-diff-string (path data)
  (when-let* ((proposed (emagent-acp--tool-call-proposed-content path data))
              (resolved (emagent-tools--root-directory path)))
    (or (emagent-tools--write-diff-string resolved proposed)
        (when-let* ((items (emagent-acp--tool-call-edit-items data))
                    (item (car items))
                    (new (emagent-acp--tool-call-edit-field
                          item 'new_string 'newText 'new_text 'newString
                          'after 'replace 'content 'text)))
          (let ((old (emagent-acp--tool-call-edit-field
                      item 'old_string 'oldText 'old_text 'oldString
                      'before 'search)))
            (emagent-acp--tool-call-edit-patch-string resolved old new)))
        (when-let ((new (emagent-acp--tool-call-edit-field
                         data 'new_string 'newText 'new_text 'newString
                         'after 'replace 'content 'text)))
          (let ((old (emagent-acp--tool-call-edit-field
                      data 'old_string 'oldText 'old_text 'oldString
                      'before 'search)))
            (emagent-acp--tool-call-edit-patch-string resolved old new))))))

(defun emagent-acp--tool-call-write-content-block (_tool-call raw _detail path)
  (when (and path (not (string-empty-p path)))
    (let* ((data (emagent-acp--tool-call-normalize-data raw))
           (resolved (emagent-tools--root-directory path))
           (heading (format "Allow edit: %s" (file-name-nondirectory resolved))))
      (if-let ((diff (when data (emagent-acp--tool-call-edit-diff-string path data))))
          (format "** %s\n#+BEGIN_SRC diff\n%s\n#+END_SRC" heading diff)
        (if-let ((proposed (emagent-acp--tool-call-proposed-content path data)))
            (let ((lang (or (file-name-extension resolved) "text")))
              (format "** %s\n#+BEGIN_SRC %s\n%s\n#+END_SRC"
                      heading lang
                      (substring proposed 0 (min (length proposed) 4000))))
          (format "** Allow edit\n= %s =" resolved))))))

(defun emagent-acp--tool-call-edit-block-spec (update)
  "Return (\"diff\" . DIFF) when UPDATE is a write/edit whose change can be
reconstructed as a diff, else nil.  Lets an auto-allowed edit render the
same diff a permission prompt would, instead of a bare arrow line."
  (when-let* ((kind (emagent-acp--tool-call-infer-kind update))
              ((emagent-acp--tool-call-write-kind-p kind))
              (raw (or (map-elt update 'rawInput) (map-elt update 'arguments)))
              (path (emagent-acp--tool-call-write-path
                     update raw (emagent-acp--tool-call-detail update)))
              (data (emagent-acp--tool-call-normalize-data raw))
              (diff (emagent-acp--tool-call-edit-diff-string path data)))
    (cons "diff" diff)))

(defun emagent-acp--tool-call-content-block (tool-call)
  "Return an org subsection string for the permission prompt, or nil.
For eval, shell, and edit tool calls, return a code block with the payload.
Edit prompts prefer a unified diff; patch edits fall back to a hunk preview."
  (when tool-call
    (or (when-let ((form (emagent-acp--tool-call-eval-form tool-call)))
          (format "** Allow eval\n#+BEGIN_SRC elisp\n%s\n#+END_SRC" form))
        (let* ((kind (emagent-acp--tool-call-infer-kind tool-call))
               (raw (or (map-elt tool-call 'rawInput)
                        (map-elt tool-call 'arguments)))
               (command (emagent-acp--tool-call-command-text tool-call))
               (detail (emagent-acp--tool-call-detail-from-tool-call tool-call)))
          (when kind
            (cond
             ((member kind '("execute" ""))
              (when command
                (if-let ((heredoc (emagent-acp--tool-call-heredoc-script command)))
                    (format "** Allow execute\n#+BEGIN_SRC %s\n%s\n#+END_SRC"
                            (car heredoc) (cdr heredoc))
                  (format "** Allow execute\n#+BEGIN_SRC sh\n%s\n#+END_SRC" command))))
             ((emagent-acp--tool-call-write-kind-p kind)
              (or (when-let ((path (emagent-acp--tool-call-write-path
                                    tool-call raw detail)))
                    (emagent-acp--tool-call-write-content-block
                     tool-call raw detail path))
                  (when command
                    (format "** Allow edit\n#+BEGIN_SRC sh\n%s\n#+END_SRC"
                            command))))
             (t nil)))))))

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

(provide 'emagent-acp-permit)
;;; emagent-acp-permit.el ends here
