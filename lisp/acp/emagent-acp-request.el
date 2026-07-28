;;; emagent-acp-request.el --- ACP permission request dispatch  -*- lexical-binding: t; -*-

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
;; Incoming ACP client requests, file helpers, and Cursor extensions.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'emagent-acp-protocol)
(require 'emagent-acp-permit)
(require 'emagent-acp-prompt)
(require 'emagent-acp-usage)
(require 'emagent-chat-ui)
(require 'emagent-log)
(require 'emagent-policy)
(require 'emagent-session)
(require 'emagent-tools)

(defun emagent-guard--path-verdict (path)
  "Return an authorization verdict for file PATH.
Resolves and confines PATH via `emagent-tools--root-directory', turning its
boundary/protected-tree signal into a `:deny' verdict."
  (condition-case err
      (cons :allow (emagent-tools--root-directory path))
    (error (cons :deny (error-message-string err)))))

(defun emagent-guard-check (op payload)
  "Return the authorization verdict for OP applied to PAYLOAD.

OP is one of:
  `read' `write' `delete' — PAYLOAD is a file path; a `:allow' verdict carries
                            the resolved canonical path.
  `shell'                 — PAYLOAD is a command string.
  `eval'                  — PAYLOAD is an elisp form string.

See the commentary for the verdict shape."
  (pcase op
    ((or 'read 'write 'delete) (emagent-guard--path-verdict payload))
    ('shell (or (emagent-policy-check-shell payload) '(:allow . t)))
    ('eval  (or (emagent-policy-check-elisp payload) '(:allow . t)))
    (_ (cons :deny (format "unknown guarded operation: %S" op)))))

(defun emagent-guard-allow-p (verdict)
  "Return non-nil when VERDICT authorizes the effect."
  (eq (car-safe verdict) :allow))

(defun emagent-guard-deny-p (verdict)
  "Return non-nil when VERDICT refuses the effect outright."
  (eq (car-safe verdict) :deny))

(defun emagent-guard-resolved (verdict)
  "Return the resolved value of an allowing VERDICT, or nil."
  (and (eq (car-safe verdict) :allow) (cdr verdict)))

(defun emagent-guard-reason (verdict)
  "Return the human-readable reason string of VERDICT, or nil."
  (and (memq (car-safe verdict) '(:deny :confirm)) (cdr verdict)))

(defvar emagent-tools--root-boundary)

(defvar emagent-tools--project-directory)

(defun emagent-acp--fs-session-root (state)
  "Return the project root ACP fs/* operations must stay within, or nil.

Mirrors the boundary the MCP dispatcher binds for its tools; without it the
fs/* handlers would resolve agent-supplied paths with no project confinement.

Arguments: STATE."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (ignore-errors (emagent-session-project-directory))))))

(defun emagent-acp--fs-unavailable-response (method)
  
  "Internal helper for METHOD."
  (emagent-acp-make-error
   :code -32601
   :message (format "%s disabled; use the external agent's project file tools"
                    method)))

(defun emagent-acp--fs-send (client make request-id &rest response-args)
  "Send the fs response built by MAKE for REQUEST-ID over CLIENT.
MAKE is a `*-text-file-response' constructor; RESPONSE-ARGS are its remaining
keyword arguments (`:content' or `:error')."
  (emagent-acp-send-response
   :client client
   :response (apply make :request-id request-id response-args)))

(defun emagent-acp--fs-send-error (client make request-id code message)
  "Send an fs error response with CODE and MESSAGE (see `emagent-acp--fs-send').

Arguments: CLIENT, MAKE, REQUEST-ID."
  (emagent-acp--fs-send client make request-id
                        :error (emagent-acp-make-error :code code :message message)))

(cl-defun emagent-acp--on-fs-read (&key state emagent-acp-request)
  
  "Internal helper for STATE and EMAGENT-ACP-REQUEST."
  (let ((client (emagent-acp-state-client state))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path)))
        (make #'emagent-acp-make-fs-read-text-file-response))
    (if (not emagent-acp-file-access)
        (emagent-acp--fs-send client make request-id
                              :error (emagent-acp--fs-unavailable-response
                                      "fs/read_text_file"))
      ;; Confine to the session root; fall back to the ambient project directory
      ;; rather than nil (no confinement) when the chat buffer is gone, so a
      ;; missing session root cannot open up unconfined filesystem access.
      (let* ((emagent-tools--root-boundary
              (or (emagent-acp--fs-session-root state)
                  emagent-tools--project-directory))
             (emagent-tools--project-directory
              (or emagent-tools--root-boundary emagent-tools--project-directory))
             (verdict (emagent-guard-check 'read path)))
        (if (not (emagent-guard-allow-p verdict))
            (emagent-acp--fs-send-error client make request-id -32603
                                        (emagent-guard-reason verdict))
          (condition-case err
              (let* ((canonical (emagent-guard-resolved verdict))
                     (line (or (map-nested-elt emagent-acp-request '(params line)) 1))
                     (limit (map-nested-elt emagent-acp-request '(params limit)))
                     (content (emagent-tools--read-file-content canonical line limit)))
                (emagent-acp--fs-send client make request-id :content content))
            (file-missing
             (emagent-acp--fs-send-error client make request-id -32002
                                         "Resource not found"))
            (error
             (emagent-acp--fs-send-error client make request-id -32603
                                         (error-message-string err)))))))))

(cl-defun emagent-acp--on-fs-write (&key state emagent-acp-request)
  
  "Internal helper for STATE and EMAGENT-ACP-REQUEST."
  (let ((client (emagent-acp-state-client state))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path)))
        (content (or (map-nested-elt emagent-acp-request '(params content)) ""))
        (make #'emagent-acp-make-fs-write-text-file-response))
    (if (not emagent-acp-file-access)
        (emagent-acp--fs-send client make request-id
                              :error (emagent-acp--fs-unavailable-response
                                      "fs/write_text_file"))
      ;; Confine to the session root; fall back to the ambient project directory
      ;; rather than nil (no confinement) when the chat buffer is gone, so a
      ;; missing session root cannot open up unconfined filesystem access.
      (let* ((emagent-tools--root-boundary
              (or (emagent-acp--fs-session-root state)
                  emagent-tools--project-directory))
             (emagent-tools--project-directory
              (or emagent-tools--root-boundary emagent-tools--project-directory))
             (verdict (emagent-guard-check 'write path)))
        (if (not (emagent-guard-allow-p verdict))
            (emagent-acp--fs-send-error client make request-id -32603
                                        (emagent-guard-reason verdict))
          (let ((resolved (emagent-guard-resolved verdict)))
            (when emagent-acp-confirm-fs-writes
              (emagent-acp--prepare-interactive-context state))
            (condition-case err
                (if (and emagent-acp-confirm-fs-writes
                         (not (emagent-tools--confirm-write
                               'emagent-tool-write-file resolved content
                               (emagent-acp--chat-buffer state))))
                    (emagent-acp--fs-send-error client make request-id -32603
                                                "Write denied by user")
                  (let ((written (emagent-tools--write-file-content resolved content)))
                    (emagent-acp--notify-user
                     state (format "emagent: wrote %s (C-/ to undo in that buffer)"
                                   written))
                    (emagent-acp--fs-send client make request-id)))
              (error
               (emagent-acp--fs-send-error client make request-id -32603
                                           (error-message-string err))))))))))

(cl-defun emagent-acp--on-permission (&key state emagent-acp-request)
  
  "Internal helper for STATE and EMAGENT-ACP-REQUEST."
  (setf (emagent-acp-state-permission-queue state)
            (append (emagent-acp-state-permission-queue state) (list emagent-acp-request)))
  (emagent-acp--drain-permission-queue state))

(cl-defun emagent-acp--on-request (&key state emagent-acp-request)
  "Dispatch an inbound ACP request for STATE.

Handles fs/*, session/request_permission, and Cursor extension methods
cursor/create_plan and cursor/ask_question (blocking).

Arguments: EMAGENT-ACP-REQUEST."
  (pcase (map-elt emagent-acp-request 'method)
    ("fs/read_text_file"
     (emagent-acp--on-fs-read :state state :emagent-acp-request emagent-acp-request))
    ("fs/write_text_file"
     (emagent-acp--on-fs-write :state state :emagent-acp-request emagent-acp-request))
    ("session/request_permission"
     (emagent-acp--on-permission :state state :emagent-acp-request emagent-acp-request))
    ("cursor/create_plan"
     (emagent-acp--on-create-plan :state state :emagent-acp-request emagent-acp-request))
    ("cursor/ask_question"
     (emagent-acp--on-ask-question :state state :emagent-acp-request emagent-acp-request))
    (_
     (emagent-acp-send-response
      :client (emagent-acp-state-client state)
      :response `((:request-id . ,(map-elt emagent-acp-request 'id))
                  (:error . ,(emagent-acp-make-error
                              :code -32601
                              :message (format "Unsupported method: %s"
                                               (map-elt emagent-acp-request 'method)))))))))

(defun emagent-acp--cursor-ext-params (request)
  "Return params alist from Cursor extension REQUEST."
  (or (map-elt request 'params) (map-elt request :params)))

(defun emagent-acp--cursor-auto-accept-plan-p (state)
  "Return non-nil when STATE should accept `cursor/create_plan' without prompting.

Noninteractive sessions always auto-accept.  Interactively, honor
`emagent-acp-auto-accept-plans' (default nil = prompt)."
  (or noninteractive
      (pcase emagent-acp-auto-accept-plans
        ('t t)
        ('nil nil)
        (_
         (or (emagent-acp-state-session-auto-approve state)
             (eq emagent-acp-auto-approve-permissions t))))))

(defun emagent-acp--format-create-plan-text (params)
  "Return display text for a `cursor/create_plan' PARAMS alist."
  (let* ((name (map-elt params 'name))
         (overview (map-elt params 'overview))
         (plan (map-elt params 'plan))
         (todos (map-elt params 'todos))
         (parts nil))
    (when (and (stringp name) (not (string-empty-p (string-trim name))))
      (push (format "Plan: %s" (string-trim name)) parts))
    (when (and (stringp overview) (not (string-empty-p (string-trim overview))))
      (push (string-trim overview) parts))
    (when (and (stringp plan) (not (string-empty-p (string-trim plan))))
      (push (string-trim plan) parts))
    (when todos
      (let ((lines
             (delq nil
                   (mapcar
                    (lambda (todo)
                      (let ((content (map-elt todo 'content))
                            (status (or (map-elt todo 'status) "pending")))
                        (when (and (stringp content)
                                   (not (string-empty-p (string-trim content))))
                          (format "- [%s] %s" status (string-trim content)))))
                    (append todos nil)))))
        (when lines
          (push (concat "Todos:\n" (string-join lines "\n")) parts))))
    (string-join (nreverse parts) "\n\n")))

(defun emagent-acp--create-plan-preamble (text)
  "Return an org quote block wrapping plan TEXT for approval."
  (concat "#+begin_quote\n"
          (string-trim text)
          "\n#+end_quote\n"))

(defun emagent-acp--insert-create-plan-thought (state text)
  "Append plan TEXT into STATE's chat Thinking section when possible."
  (when-let* ((buf (emagent-acp--chat-buffer state))
              (trimmed (string-trim (or text ""))))
    (unless (string-empty-p trimmed)
      (with-current-buffer buf
        (when (and (fboundp 'emagent-chat-append-thought)
                   (fboundp 'emagent-chat--flush-thought-pending)
                   (or (not (fboundp 'emagent-chat--open-response-p))
                       (emagent-chat--open-response-p)))
          (emagent-chat-append-thought (concat "\n\n" trimmed "\n"))
          (emagent-chat--flush-thought-pending t))))))

(defun emagent-acp--persist-create-plan (state params)
  "Write create_plan PARAMS under ~/.cursor/plans/; return file:// URI.

Arguments: STATE."
  (let* ((dir (expand-file-name "~/.cursor/plans"))
         (raw-name (or (map-elt params 'name) "Plan"))
         (slug (replace-regexp-in-string
                "[^a-zA-Z0-9-_ ]" "" (format "%s" raw-name)))
         (slug (string-trim (substring slug 0 (min 50 (length slug)))))
         (slug (if (string-empty-p slug) "Plan" slug))
         (sid (or (emagent-acp-state-session-id state) "session"))
         (suffix (substring sid 0 (min 8 (length sid))))
         (file (expand-file-name
                (format "%s-%s.plan.md" slug suffix) dir))
         (plan (or (map-elt params 'plan) ""))
         (overview (map-elt params 'overview))
         (parts (list (format "<!-- %s -->" sid)))
         (body nil))
    (when (and (stringp overview)
               (not (string-empty-p (string-trim overview))))
      (setq parts (append parts (list (string-trim overview)))))
    (when (and (stringp plan) (not (string-empty-p plan)))
      (setq parts (append parts (list plan))))
    (setq body (concat (string-join parts "\n\n") "\n"))
    (make-directory dir t)
    (with-temp-file file (insert body))
    (concat "file://" (expand-file-name file))))

(defun emagent-acp--plan-build-prompt (plan-uri params)
  "Return the follow-up Build prompt for PLAN-URI and PARAMS."
  (let ((name (or (map-elt params 'name) "the approved plan")))
    (format
     (concat "Build the approved plan %S (%s). Execute its todos now; "
             "do not stop after planning - implement and verify.")
     name plan-uri)))

(defun emagent-acp--queue-plan-build (state plan-uri params)
  "Queue a Build follow-up on STATE after create_plan accept.

PLAN-URI and PARAMS feed the execute prompt text.  Defers the chat
user-heading stub until Build starts so Accept does not leave an empty
`* user>' between the plan dialog and agent work."
  (when emagent-acp-auto-build-plans
    (setf (emagent-acp-state-plan-build-prompt state)
          (emagent-acp--plan-build-prompt plan-uri params))
    (when-let ((buf (emagent-acp--chat-buffer state)))
      (with-current-buffer buf
        (setq emagent-chat--defer-user-stub t)))
    (emagent-log "cursor/create_plan: queued Build turn")))

(defun emagent-acp--send-create-plan-outcome (state request-id outcome
                                                     &optional reason
                                                     plan-uri)
  "Reply to `cursor/create_plan' REQUEST-ID for STATE with OUTCOME.
OUTCOME is a string: accepted, rejected, or cancelled.  REASON is
optional.  PLAN-URI is sent when accepting.

Arguments: STATE, REQUEST-ID."
  (emagent-log "cursor/create_plan response: %s%s"
               outcome
               (if reason (format " (%s)" reason) ""))
  (emagent-acp-send-response
   :client (emagent-acp-state-client state)
   :response (emagent-acp-make-cursor-create-plan-response
              :request-id request-id
              :outcome outcome
              :reason reason
              :plan-uri plan-uri)))

(defun emagent-acp--accept-create-plan (state request-id params)
  "Accept create_plan for STATE: persist plan, queue Build, reply.

Arguments: REQUEST-ID, PARAMS."
  (let ((plan-uri (emagent-acp--persist-create-plan state params)))
    (emagent-acp--queue-plan-build state plan-uri params)
    (emagent-acp--send-create-plan-outcome
     state request-id "accepted" nil plan-uri)))

(cl-defun emagent-acp--on-create-plan (&key state emagent-acp-request)
  "Handle blocking Cursor `cursor/create_plan' for STATE.

Shows the plan for approval (default) or auto-accepts when configured.
Accept persists a plan file, returns planUri, and queues a Build
follow-up turn (see `emagent-acp-auto-build-plans').

Arguments: EMAGENT-ACP-REQUEST."
  (let* ((request-id (map-elt emagent-acp-request 'id))
         (params (emagent-acp--cursor-ext-params emagent-acp-request))
         (text (emagent-acp--format-create-plan-text params))
         (buf (emagent-acp--chat-buffer state)))
    (emagent-log "cursor/create_plan: name=%s plan-chars=%d"
                 (or (map-elt params 'name) "?")
                 (length (or (map-elt params 'plan) "")))
    (emagent-acp--insert-create-plan-thought state text)
    (cond
     ((emagent-acp--cursor-auto-accept-plan-p state)
      (emagent-acp--accept-create-plan state request-id params))
     (t
      (unless (fboundp 'emagent-acp--prepare-interactive-context)
        (require 'emagent-acp-prompt))
      (emagent-acp--prepare-interactive-context state)
      (emagent-acp--clear-prompt-watchdog state)
      (emagent-tools--buttons-prompt
       (if emagent-acp-auto-build-plans
           "Accept and build this plan?"
         "Accept this plan?")
       (if emagent-acp-auto-build-plans
           '(("Accept & Build" . :accept) ("Reject" . :reject))
         '(("Accept" . :accept) ("Reject" . :reject)))
       buf
       (lambda (choice)
         (when (emagent-acp-state-busy state)
           (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
             (require 'emagent-acp-prompt))
           (emagent-acp--schedule-prompt-watchdog state))
         (emagent-acp--refresh-mode-line state)
         (pcase choice
           (:accept
            (emagent-acp--accept-create-plan state request-id params))
           (:reject
            (emagent-acp--cancel-plan-build state)
            (emagent-acp--send-create-plan-outcome
             state request-id "rejected" "User rejected the plan"))
           (_
            (emagent-acp--cancel-plan-build state)
            (emagent-acp--send-create-plan-outcome
             state request-id "cancelled"))))
       (emagent-acp--create-plan-preamble text))))))

(defun emagent-acp--ask-question-default-answers (params)
  "Return default answered outcome payload for ask_question PARAMS."
  (let ((questions (append (map-elt params 'questions) nil))
        answers)
    (dolist (q questions)
      (let* ((qid (map-elt q 'id))
             (options (append (map-elt q 'options) nil))
             (first (car options))
             (oid (and first (map-elt first 'id))))
        (when (and qid oid)
          (push `((questionId . ,qid)
                  (selectedOptionIds . ,(vector oid)))
                answers))))
    `((outcome . "answered")
      (answers . ,(apply #'vector (nreverse answers))))))

(defun emagent-acp--send-ask-question-result (state request-id result)
  "Send ask_question RESULT for REQUEST-ID on STATE."
  (emagent-log "cursor/ask_question response: %s"
               (or (map-elt result 'outcome) "?"))
  (emagent-acp-send-response
   :client (emagent-acp-state-client state)
   :response `((:request-id . ,request-id)
               (:result . ((outcome . ,result))))))

(cl-defun emagent-acp--on-ask-question (&key state emagent-acp-request)
  "Handle blocking Cursor `cursor/ask_question' for STATE.

Arguments: EMAGENT-ACP-REQUEST."
  (let* ((request-id (map-elt emagent-acp-request 'id))
         (params (emagent-acp--cursor-ext-params emagent-acp-request))
         (questions (append (map-elt params 'questions) nil))
         (title (or (map-elt params 'title) "Question"))
         (buf (emagent-acp--chat-buffer state)))
    (emagent-log "cursor/ask_question: title=%s questions=%d"
                 title (length questions))
    (cond
     ((or noninteractive (null questions))
      (emagent-acp--send-ask-question-result
       state request-id
       (if questions
           (emagent-acp--ask-question-default-answers params)
         '((outcome . "skipped") (reason . "No questions")))))
     (t
      ;; Interactive: answer questions sequentially, then respond once.
      (unless (fboundp 'emagent-acp--prepare-interactive-context)
        (require 'emagent-acp-prompt))
      (emagent-acp--prepare-interactive-context state)
      (emagent-acp--clear-prompt-watchdog state)
      (let* ((remaining questions)
             (answers nil)
             (finish
              (lambda (result)
                (when (emagent-acp-state-busy state)
                  (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
                    (require 'emagent-acp-prompt))
                  (emagent-acp--schedule-prompt-watchdog state))
                (emagent-acp--refresh-mode-line state)
                (emagent-acp--send-ask-question-result state request-id result)))
             (ask-next nil))
        (setq ask-next
              (lambda ()
                (if (null remaining)
                    (funcall finish
                             `((outcome . "answered")
                               (answers . ,(apply #'vector (nreverse answers)))))
                  (let* ((q (car remaining))
                         (qid (map-elt q 'id))
                         (prompt (or (map-elt q 'prompt) title))
                         (options (append (map-elt q 'options) nil))
                         (choices
                          (mapcar (lambda (opt)
                                    (cons (or (map-elt opt 'label)
                                              (map-elt opt 'id)
                                              "?")
                                          (map-elt opt 'id)))
                                  options)))
                    (setq remaining (cdr remaining))
                    (if (null choices)
                        (funcall ask-next)
                      (emagent-tools--buttons-prompt
                       prompt
                       (append choices '(("Skip" . :skip)))
                       buf
                       (lambda (choice)
                         (cond
                          ((eq choice :skip)
                           (funcall finish
                                    '((outcome . "skipped")
                                      (reason . "User skipped"))))
                          (t
                           (when (and qid choice)
                             (push `((questionId . ,qid)
                                     (selectedOptionIds . ,(vector choice)))
                                   answers))
                           (funcall ask-next))))))))))
        (funcall ask-next))))))

(provide 'emagent-acp-request)
;;; emagent-acp-request.el ends here
