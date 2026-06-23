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

(declare-function emagent-acp--send-request "emagent-acp")
(declare-function emagent-acp--emit-tool-call-display "emagent-acp")
(declare-function emagent-chat--open-response-p "emagent-chat")
(declare-function emagent-cursor-enrich-tool-call-update "emagent-cursor")

(defun emagent-acp--permission-option-allow-p (opt)
  "Return non-nil when OPT is an allow-type ACP permission option."
  (let ((kind (downcase (or (map-elt opt 'kind) "")))
        (id (downcase (or (map-elt opt 'optionId) "")))
        (name (downcase (or (map-elt opt 'name) ""))))
    (or (member kind '("allow" "allow_once" "allow_always" "allow-once" "allow-always"))
        (member id '("allow_once" "allow-once" "allow_always" "allow-always" "allow" "yes" "run" "run_once"))
        (string-match-p "allow" id)
        (string-match-p "\\`\\(?:allow\\|yes\\|run\\)" name))))

(defun emagent-acp--permission-option-id (options)
  "Return an allow-type option id from OPTIONS.
Tries preferred optionIds, then `kind' = allow, then name/id matching allow.
Never returns a deny option.  OPTIONS may be a list or vector."
  (let ((prefer '("allow_once" "allow-once" "allow_always" "allow-always" "allow" "yes" "run" "run_once")))
    (or (map-elt (seq-find (lambda (opt)
                             (let ((id (map-elt opt 'optionId)))
                               (and id (member id prefer))))
                           options)
                 'optionId)
        (map-elt (seq-find #'emagent-acp--permission-option-allow-p options)
                 'optionId))))

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

(defconst emagent-acp--dangerous-command-regexps
  (mapconcat #'identity
             '("\\brm[[:space:]]+-[rf]+"
               "\\brm[[:space:]]+--recursive"
               "\\brm[[:space:]]+--force"
               "\\bdd[[:space:]]+"
               "\\bmkfs\\."
               "\\bmke2fs\\b"
               "\\bformat[[:space:]]+"
               "\\bshutdown\\b"
               "\\breboot\\b"
               "\\binit[[:space:]]+0\\b"
               "\\bsudo[[:space:]]+rm"
               "curl[[:space:]]+.*|.*sh\\b"
               "\\btrash\\b"
               "kill[[:space:]]+-9"
               ">*/dev/[sh]d[a-z]"
               "chmod[[:space:]]+-R[[:space:]]*777")
             "\\|")
  "Regexp matching shell commands that are destructive enough to prompt about.
Auto-approve in `safe' mode skips these and prompts for confirmation.")

(defun emagent-acp--tool-call-command-text (tool-call)
  "Extract the command string from a tool-call ACP object."
  (or (let ((raw (or (map-elt tool-call 'rawInput)
                     (map-elt tool-call 'arguments))))
        (when (stringp raw)
          (condition-case nil
              (let ((parsed (json-parse-string raw
                                               :object-type 'alist
                                               :array-type 'list
                                               :null-object nil
                                               :false-object nil)))
                (or (map-elt parsed 'command)
                    (map-elt parsed 'text)
                    (map-elt parsed 'cmd)))
            (error nil))))
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

(defun emagent-acp--tool-call-dangerous-p (tool-call)
  "Return non-nil when TOOL-CALL is a shell command with destructive intent.
Read and write tools are always safe.

For execute tools, analyses the command text against known dangerous
patterns: recursive deletes, disk writes, formatting, and similar.
Harmless commands like `mvn compile', `git status', or `ls' pass
through without prompting."
  (let ((kind (and (listp tool-call) (map-elt tool-call 'kind))))
    (when (and (stringp kind) (equal (downcase kind) "execute"))
      (let ((command (emagent-acp--tool-call-command-text tool-call)))
        (and (stringp command)
             (string-match-p emagent-acp--dangerous-command-regexps command))))))

(defun emagent-acp--permission-interactive-p (state)
  "Return non-nil when ACP permission prompts may need user input.
For `safe' auto-approve mode this returns t because the per-tool
decision is made in `emagent-acp--handle-one-permission'."
  (and (not (eq emagent-acp-auto-approve-permissions t))
       (not (map-elt state :session-auto-approve))))


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
