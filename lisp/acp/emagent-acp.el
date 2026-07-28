;;; emagent-acp.el --- ACP wire-up for emagent -*- lexical-binding: t; -*-

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
;; ACP facade: connect, prompt/lifecycle, and request handling.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'emagent-acp-permit)
(require 'emagent-acp-protocol)
(require 'emagent-acp-usage)
(require 'emagent-chat)
(require 'emagent-chat-ui)
(require 'emagent-cursor)
(require 'emagent-log)
(require 'emagent-mcp)
(require 'emagent-policy)
(require 'emagent-session)
(require 'emagent-struct)
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
        (require 'emagent-acp))
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
             (require 'emagent-acp))
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
        (require 'emagent-acp))
      (emagent-acp--prepare-interactive-context state)
      (emagent-acp--clear-prompt-watchdog state)
      (let* ((remaining questions)
             (answers nil)
             (finish
              (lambda (result)
                (when (emagent-acp-state-busy state)
                  (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
                    (require 'emagent-acp))
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

(defun emagent-acp-attach-context (text)
  "Attach TEXT to the next prompt in the current buffer."
  (let ((state (emagent-acp--session)))
    (setf (emagent-acp-state-extra-context state)
              (append (emagent-acp-state-extra-context state) (list text)))))

(defun emagent-acp--image-media-type (ext)
  "Return the MIME type string for image extension EXT, or nil if not an image."
  (pcase (downcase (or ext ""))
    ("png"  "image/png")
    ("jpg"  "image/jpeg")
    ("jpeg" "image/jpeg")
    ("gif"  "image/gif")
    ("webp" "image/webp")
    (_      nil)))

(defun emagent-acp--extract-image-links (text)
  "Extract [[file:...]] image links from TEXT.

Scans for org file links whose paths end in PNG/JPEG/GIF/WebP, reads and
base64-encodes each file, and removes the link from the text.  Non-image
links and unreadable paths are left in place.

Returns (CLEANED-TEXT . IMAGES) where IMAGES is a list of
 ((media-type . TYPE) (data . BASE64)) plists."
  (let ((link-re "\\[\\[file:\\([^]\n]+\\)\\]\\(?:\\[[^]]*\\]\\)?\\]")
        images parts (pos 0))
    (while (string-match link-re text pos)
      (let* ((link-beg (match-beginning 0))
             (link-end (match-end 0))
             (path (match-string 1 text))
             (expanded (expand-file-name path))
             (media-type (emagent-acp--image-media-type
                          (file-name-extension expanded))))
        (push (substring text pos link-beg) parts)
        (if (and media-type (file-readable-p expanded))
            (let ((data (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally expanded)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string))))
              (push `((media-type . ,media-type) (data . ,data)) images))
          (push (substring text link-beg link-end) parts))
        (setq pos link-end)))
    (push (substring text pos) parts)
    (cons (string-trim (apply #'concat (nreverse parts)))
          (nreverse images))))

(defconst emagent-acp--materialize-prompt-text
  (concat "Acknowledge that this compacted session is ready. "
          "Reply with exactly: ready. Do not use tools.")
  "Quiet prompt text that forces the agent to persist a new session.

Cursor ACP creates only meta.json until the first session/prompt; without
this turn, compact then restart fails session/load.")

(defun emagent-acp--materialize-session (state)
  "Send a quiet prompt so STATE's new session is durable across restarts.

Called after /compact creates a fresh session/new.  The reply is not
rendered into the chat buffer."
  (when-let ((session-id (emagent-acp-state-session-id state)))
    (when (and (emagent-acp-state-ready state)
               (not (emagent-acp-state-busy state)))
      (emagent-log "materializing compacted session…")
      (emagent-acp--progress state "materializing compacted session…")
      (setf (emagent-acp-state-quiet-prompt state) t)
      (emagent-acp--turn-begin state)
      (emagent-acp--dispatch-prompt-request
       :state state
       :session-id session-id
       :blocks `[((type . "text")
                  (text . ,emagent-acp--materialize-prompt-text))]
       :images nil
       :gen (emagent-acp-state-prompt-generation state)
       :attempt 1))))

(defun emagent-acp--schedule-prompt-retry (state session-id blocks images gen attempt reason)
  "Re-dispatch the in-flight prompt after exponential backoff.

REASON is a short human-readable phrase describing why the retry fires; it is
shown to the user together with the attempt count.  The GEN guard prevents a
stale retry from firing after the prompt was superseded or interrupted.

Arguments: STATE, SESSION-ID, BLOCKS, IMAGES."
  (let* ((delay (emagent-acp--prompt-retry-delay attempt))
         (next (1+ attempt)))
    (setf (emagent-acp-state-prompt-retry-gen state) gen)
    (emagent-acp--notify-user
     state
     (format "emagent: %s; retrying prompt (%d/%d) in %.1fs"
             reason next emagent-acp-prompt-retry-attempts delay))
    (emagent-acp--schedule-prompt-watchdog state)
    (run-with-timer
     delay nil
     (lambda ()
       (setf (emagent-acp-state-prompt-retry-gen state) nil)
       (if (and (eq (emagent-acp-state-prompt-generation state) gen)
                (emagent-acp-state-busy state))
           (emagent-acp--dispatch-prompt-request
            :state state :session-id session-id
            :blocks blocks :images images
            :gen gen :attempt next)
         (emagent-log "emagent: prompt retry skipped (busy=%s gen=%s/%s)"
                      (if (emagent-acp-state-busy state) "yes" "no")
                      (emagent-acp-state-prompt-generation state)
                      gen))))))

(defun emagent-acp--log-transient-error (state &optional message)
  "Log MESSAGE and STATE's partial assistant output to `emagent-log-buffer-name'.

Used when a transient error ends an in-flight turn: the details are recorded in
the log instead of the chat buffer, and the turn is then resumed with
\"continue\" (see `emagent-acp--schedule-continue')."
  (when (and message (not (string-empty-p message)))
    (emagent-log "transient error: %s" message))
  (let ((text (string-trim (or (emagent-acp-state-assistant-text state) ""))))
    (unless (string-empty-p text)
      (emagent-log "partial output before auto-continue:\n%s" text))))

(defun emagent-acp--schedule-continue (state session-id images gen reason)
  "Resume an errored in-flight turn by re-dispatching a \"continue\" prompt.

Unlike `emagent-acp--schedule-prompt-retry' (which replays the ORIGINAL prompt
and is only safe when the turn did no work), this sends a fresh \"continue\"
turn so tool side effects such as commits or pushes are never repeated.  The
open response block is kept, so the continued output renders into it; the
transient error itself is only logged (see `emagent-acp--log-transient-error'),
never rendered into the chat buffer.  REASON is logged with the attempt count;
the `:continue-attempts' counter bounds the number of resumes and the GEN guard
cancels a stale resume after an interrupt or new prompt.

Arguments: STATE, SESSION-ID, IMAGES."
  (let* ((attempt (1+ (or (emagent-acp-state-continue-attempts state) 0)))
         (delay (emagent-acp--prompt-retry-delay attempt)))
    (setf (emagent-acp-state-continue-attempts state) attempt)
    (emagent-acp--notify-user
     state
     (format "emagent: %s; auto-continuing (%d/%d) in %.1fs"
             reason attempt emagent-acp-prompt-retry-attempts delay))
    (emagent-acp--schedule-prompt-watchdog state)
    (run-with-timer
     delay nil
     (lambda ()
       (when (and (eq (emagent-acp-state-prompt-generation state) gen)
                  (emagent-acp-state-busy state))
         (emagent-acp--dispatch-prompt-request
          :state state :session-id session-id
          :blocks [((type . "text") (text . "continue"))]
          :images images
          :gen gen :attempt 1))))))

(cl-defun emagent-acp--dispatch-prompt-request (&key state session-id blocks images gen attempt)
  "Send the session/prompt request, recovering from transient network failures.

ATTEMPT is the 1-based try count.  Recovery depends on how the failure arrives
and whether the turn already did work:

- Pure transient failure with no tool calls or content
  (`emagent-acp--agent-error-only-response-p' /
  `emagent-acp--turn-did-no-work-p') is replayed with exponential backoff up to
  `emagent-acp-prompt-retry-attempts' via `emagent-acp--schedule-prompt-retry'.

- A turn that already ran tool calls or produced content but ended on a
  transient error (`emagent-acp--turn-hit-transient-error-p') is resumed by
  auto-sending \"continue\" via `emagent-acp--schedule-continue', so side
  effects such as commits or pushes are never repeated.  The error is logged to
  `emagent-log-buffer-name' rather than rendered into the chat buffer.

GEN guards against a stale retry firing after the prompt was superseded or
interrupted.

Arguments: STATE, SESSION-ID, BLOCKS, IMAGES."
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-prompt-request
             :session-id session-id :prompt blocks :images images)
   :on-success
   (lambda (response)
     (when (eq (emagent-acp-state-prompt-generation state) gen)
       (cond
        ((and (emagent-acp-state-busy state)
              (< attempt emagent-acp-prompt-retry-attempts)
              (emagent-acp--agent-error-only-response-p state))
         (let ((message (string-trim (or (emagent-acp-state-assistant-text state) ""))))
           (setf (emagent-acp-state-assistant-text state) "")
           (setf (emagent-acp-state-thought-text state) "")
           (emagent-acp--clear-thought-buffer state)
           (emagent-acp--cancel-prompt-render state)
           (emagent-acp--schedule-prompt-retry
            state session-id blocks images gen attempt
            (format "agent returned a transient error (%s)" message))))
        ((and (emagent-acp-state-busy state)
              (< (or (emagent-acp-state-continue-attempts state) 0)
                 emagent-acp-prompt-retry-attempts)
              (emagent-acp--turn-hit-transient-error-p state))
         (emagent-acp--log-transient-error state)
         (setf (emagent-acp-state-assistant-text state) "")
         (setf (emagent-acp-state-thought-text state) "")
         (emagent-acp--clear-thought-buffer state)
         (emagent-acp--cancel-prompt-render state)
         (emagent-acp--schedule-continue
          state session-id images gen "agent turn ended on a transient error"))
        (t
         (emagent-acp--complete-prompt state response)))))
   :on-failure
   (lambda (error _raw)
     (when (eq (emagent-acp-state-prompt-generation state) gen)
       (let ((message (or (map-elt error 'message) (format "%s" error))))
         (cond
          ((and (emagent-acp-state-busy state)
                (< attempt emagent-acp-prompt-retry-attempts)
                (emagent-acp--retriable-prompt-error-p message)
                (emagent-acp--turn-did-no-work-p state))
           (emagent-acp--schedule-prompt-retry
            state session-id blocks images gen attempt
            (format "prompt failed (%s)" message)))
          ((and (emagent-acp-state-busy state)
                (emagent-acp--retriable-prompt-error-p message)
                (< (or (emagent-acp-state-continue-attempts state) 0)
                   emagent-acp-prompt-retry-attempts))
           (emagent-acp--log-transient-error state message)
           (setf (emagent-acp-state-assistant-text state) "")
           (setf (emagent-acp-state-thought-text state) "")
           (emagent-acp--clear-thought-buffer state)
           (emagent-acp--cancel-prompt-render state)
           (emagent-acp--schedule-continue
            state session-id images gen (format "prompt interrupted (%s)" message)))
          (t
           (emagent-acp--abort-prompt state (format "prompt failed: %s" message))
           (emagent-acp--notify-user
            state (format "emagent: prompt failed: %s" message)))))))))

(defun emagent-acp--reset-permission-gate (state)
  "Cancel STATE's pending permission drain and clear the permission gate.
Replies `cancelled' to any outstanding requests so the agent does not hang.
Shared by the two turn-boundary owners (`--turn-begin' and finalize)."
  (when-let ((timer (emagent-acp-state-permission-drain-timer state)))
    (cancel-timer timer)
    (setf (emagent-acp-state-permission-drain-timer state) nil))
  (emagent-acp--cancel-outstanding-permissions state)
  (setf (emagent-acp-state-permission-busy state) nil)
  (setf (emagent-acp-state-deferred-complete-response state) nil))

(defun emagent-acp--turn-begin (state)
  "Enter the streaming phase of a new turn for STATE.

Mints a fresh turn generation (so a late response from a previous turn fails
the GEN guard instead of finalizing this one) and resets all turn-scoped state:
resume budget, streamed text, finalize flags, the tool-call display tables, the
provider tool-resolve queue, and any outstanding permission requests.  This is
the single entry point for turn start; the terminal paths (`--complete-prompt',
`--abort-prompt', `--finalize-in-flight-prompt') own turn end."
  (setf (emagent-acp-state-busy state) t)
  (setf (emagent-acp-state-prompt-generation state) (1+ (or (emagent-acp-state-prompt-generation state) 0)))
  (setf (emagent-acp-state-continue-attempts state) 0)
  (setf (emagent-acp-state-assistant-text state) "")
  (setf (emagent-acp-state-thought-text state) "")
  (setf (emagent-acp-state-prompt-finalized state) nil)
  (setf (emagent-acp-state-prompt-finishing state) nil)
  (clrhash (emagent-acp-state-tool-call-titles state))
  (clrhash (emagent-acp-state-tool-call-inputs state))
  (clrhash (emagent-acp-state-tool-call-labels state))
  (clrhash (emagent-acp-state-tool-call-decisions state))
  (clrhash (emagent-acp-state-tool-call-pending state))
  ;; A new turn supersedes any agent-scheduled wakeup: a stale request must
  ;; not arm after an unrelated prompt, and a pending timer must not fire
  ;; into the middle of this turn's conversation.
  (emagent-acp--cancel-wakeup state)
  (emagent-acp--cancel-plan-build state)
  (emagent-acp--provider-reset-tool-resolve state)
  (emagent-acp--reset-permission-gate state)
  (emagent-acp--cancel-prompt-render state)
  (emagent-acp--clear-thought-buffer state)
  (emagent-acp--schedule-prompt-watchdog state)
  (unless (emagent-acp-state-quiet-prompt state)
    (when (fboundp 'emagent-chat--send-pending-end)
      (when-let ((buf (emagent-acp--chat-buffer state)))
        (with-current-buffer buf
          (emagent-chat--send-pending-end))))
    (when (fboundp 'emagent-chat--promote-transient-to-thinking)
      (when-let ((buf (emagent-acp--chat-buffer state)))
        (with-current-buffer buf
          (emagent-chat--promote-transient-to-thinking)))))
  ;; Push the now-busy status; the mode line starts the spinner from it.
  (emagent-acp--refresh-mode-line state))

(cl-defun emagent-acp-send-prompt (user-text &optional compress)
  "Send USER-TEXT to the current buffer's ACP session.

When COMPRESS is non-nil, USER-TEXT is already a compression summary prompt
assembled by `emagent-chat--dispatch-compress': context injection is skipped
and the turn is marked so `emagent-acp--render-prompt-response' resets the
session with the summary once it finishes.  MCP and /compress detection live
in the chat send path (`emagent-chat-send'); by the time a prompt reaches
here it is always the final text to dispatch."
  (let* ((state (emagent-acp--session))
         (session-id (emagent-acp-state-session-id state)))
    (unless (emagent-acp-state-ready state)
      (user-error "Emagent is still connecting"))
    (when (emagent-acp-state-busy state)
      (user-error "Emagent is busy"))
    (setq user-text (emagent-acp--provider-normalize-slash-prompt state user-text))
    (when compress
      (setf (emagent-acp-state-compress-pending state) t))
    (let* ((slash-command-p (and (not compress) (emagent-chat--bare-slash-command-p user-text)))
           (extra (emagent-acp-state-extra-context state))
           (full-prompt (if (or slash-command-p compress)
                            user-text
                          (emagent-context-build-prompt user-text extra)))
           (extracted (emagent-acp--extract-image-links
                       (substring-no-properties full-prompt)))
           (clean-text (car extracted))
           (images (cdr extracted))
           (blocks `[((type . "text") (text . ,clean-text))]))
      (setf (emagent-acp-state-extra-context state) nil)
      (cond
       (compress (emagent-log "compressing conversation"))
       (slash-command-p (emagent-log "send slash command: %s" user-text)))
      (emagent-log "dispatch prompt (%d chars)" (length clean-text))
      (emagent-acp--turn-begin state)
      (emagent-acp--dispatch-prompt-request
       :state state :session-id session-id
       :blocks blocks :images images
       :gen (emagent-acp-state-prompt-generation state) :attempt 1))))

(cl-defun emagent-acp--finalize-in-flight-prompt (&optional stop-notice)
  "Finalize the in-flight prompt and cancel it on the agent side.

When STOP-NOTICE is non-nil, append it to any partial assistant text
before closing the response block.  Returns non-nil when a prompt was
finalized."
  (let ((state emagent-acp--session))
    (unless (and state
                 (or (emagent-acp-state-busy state)
                     (emagent-acp-state-prompt-finishing state)))
      (cl-return-from emagent-acp--finalize-in-flight-prompt nil))
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    ;; Interrupt/stop must not leave a ScheduleWakeup to arm later.
    (emagent-acp--cancel-wakeup state)
    (emagent-acp--cancel-plan-build state)
    (emagent-acp--flush-thought-buffer state)
    (when (and stop-notice (not (string-empty-p stop-notice)))
      (let* ((text (or (emagent-acp-state-assistant-text state) ""))
             (full (if (string-empty-p text)
                       stop-notice
                     (concat text "\n\n" stop-notice))))
        (setf (emagent-acp-state-assistant-text state) full)))
    (setf (emagent-acp-state-prompt-generation state) (1+ (or (emagent-acp-state-prompt-generation state) 0)))
    (when-let ((client (emagent-acp-state-client state))
               (session-id (emagent-acp-state-session-id state)))
      (ignore-errors
        (emagent-acp-send-notification
         :client client
         :notification (emagent-acp-make-session-cancel-notification
                        :session-id session-id))))
    (emagent-acp--reset-permission-gate state)
    (setf (emagent-acp-state-busy state) nil)
    (setf (emagent-acp-state-prompt-finishing state) t)
    (setf (emagent-acp-state-prompt-finalized state) nil)
    (emagent-acp--render-prompt-response state)
    (emagent-acp--refresh-mode-line state)
    t))

(defun emagent-acp-interrupt ()
  "Interrupt the in-flight prompt and close the response block cleanly.

Appends a user-visible stop notice to whatever the agent has produced so far,
then finalizes the response as if it completed normally.  The pending ACP
request continues in the background but its result is ignored."
  (interactive)
  (if (emagent-acp--finalize-in-flight-prompt
       "/Stopped — awaiting new instructions./")
      (message "emagent: interrupted")
    (user-error "No active emagent prompt to interrupt")))

(defun emagent-acp-shutdown-buffer ()
  "Shut down the ACP session for the current buffer."
  (emagent-chat-clear-slash-commands)
  (when emagent-mcp--token
    (emagent-mcp-deregister-session emagent-mcp--token))
  (when-let ((state emagent-acp--session))
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    (emagent-acp--cancel-state-timers state)
    (when-let ((client (emagent-acp-state-client state)))
      (emagent-acp-shutdown :client client))
    (setq emagent-acp--session nil)))

(defvar emagent-chat--finish-close)

(defun emagent-acp--clear-prompt-watchdog (state)
  "Cancel any pending prompt stall watchdog for STATE."
  (when-let ((timer (emagent-acp-state-prompt-watchdog-timer state)))
    (cancel-timer timer))
  (setf (emagent-acp-state-prompt-watchdog state) nil)
  (setf (emagent-acp-state-prompt-watchdog-timer state) nil))

(defun emagent-acp--schedule-prompt-watchdog (state)
  "Abort a prompt that stays busy without ACP progress.

Cancel any existing watchdog first: this is re-invoked on every displayed tool
call and permission answer, and without the cancel each call would leak a live
timer (token-guarded no-ops that still pin STATE for the whole timeout).

When ACP work is still outstanding (pending RPC, permission prompt, or
tool-resolve), extend the watchdog instead of finalizing: otherwise the UI
closes the Response while the agent keeps working and logging."
  (when-let ((old (emagent-acp-state-prompt-watchdog-timer state)))
    (cancel-timer old))
  (let* ((token (cl-gensym "emagent-prompt-watchdog"))
         (timer (run-with-timer
                 emagent-acp-watchdog-timeout nil
                 (lambda ()
                   (when (and (eq (emagent-acp-state-prompt-watchdog state) token)
                              (emagent-acp-state-busy state))
                     (let* ((client (emagent-acp-state-client state))
                            (pending (and client (map-elt client :pending-requests)))
                            (waiting
                             (or pending
                                 (emagent-acp--permission-pending-p state)
                                 (and (fboundp 'emagent-acp--provider-tool-resolve-active-p)
                                      (emagent-acp--provider-tool-resolve-active-p state)))))
                       (emagent-log "emagent: prompt stalled (no ACP completion in %ds)"
                                    emagent-acp-watchdog-timeout)
                       (when pending
                         (emagent-log "emagent: pending ACP request count: %d"
                                      (length pending)))
                       (cond
                        (waiting
                         (emagent-log "emagent: prompt still waiting on agent work; extending watchdog")
                         (emagent-acp--schedule-prompt-watchdog state))
                        ((and (emagent-acp-state-assistant-text state)
                              (not (string-empty-p
                                    (emagent-acp-state-assistant-text state))))
                         (emagent-log "emagent: prompt stalled; finalizing partial response")
                         (emagent-acp--complete-prompt state nil))
                        (t
                         (emagent-acp--abort-prompt
                          state
                          "prompt stalled — reconnect with M-x emagent-mode or kill and reopen the buffer")))))))))
    (setf (emagent-acp-state-prompt-watchdog state) token)
    (setf (emagent-acp-state-prompt-watchdog-timer state) timer)))

(defun emagent-acp--stream-to-buffer-p (state)
  "Return non-nil when agent chunks may update the chat buffer live.

Arguments: STATE.

Chunks may stream while the prompt is busy or while a finish render is
still settling (`prompt-finishing'), so late agent text is not stranded
after an early stub.  Once `prompt-finalized' is set, streaming stops."
  (and emagent-acp-stream-to-buffer
       (not (emagent-acp-state-compress-pending state))
       (not (emagent-acp-state-quiet-prompt state))
       (not (emagent-acp-state-prompt-finalized state))
       (or (emagent-acp-state-busy state)
           (emagent-acp-state-prompt-finishing state))))

(defun emagent-acp--stream-thought-to-buffer-p (state)
  "Return non-nil when reasoning may stream into the chat buffer live.

Arguments: STATE."
  (and (memq emagent-acp-thought-progress '(buffer both))
       (not (emagent-acp-state-compress-pending state))
       (not (emagent-acp-state-quiet-prompt state))
       (not (emagent-acp-state-prompt-finalized state))
       (or (emagent-acp-state-busy state)
           (emagent-acp-state-prompt-finishing state))))

(defun emagent-acp--cancel-prompt-render (state)
  "Cancel a pending debounced render for STATE."
  (when-let ((timer (emagent-acp-state-finish-timer state)))
    (cancel-timer timer))
  (setf (emagent-acp-state-finish-timer state) nil)
  (setf (emagent-acp-state-finish-token state) nil))

(defun emagent-acp--schedule-prompt-render (state)
  "Debounced render of the accumulated prompt into the chat buffer.

Arguments: STATE."
  (let ((token (cl-gensym "emagent-finish")))
    (emagent-acp--cancel-prompt-render state)
    (setf (emagent-acp-state-finish-token state) token)
    (setf (emagent-acp-state-finish-timer state)
              (run-with-timer
               emagent-acp-render-delay nil
               (lambda ()
                 (when (and (eq (emagent-acp-state-finish-token state) token)
                            (emagent-acp-state-prompt-finishing state))
                   (setf (emagent-acp-state-finish-timer state) nil)
                   (emagent-acp--render-prompt-response state)))))))

(defun emagent-acp--render-prompt-response (state)
  "Render accumulated prompt text into the chat buffer for STATE.

For a normal finish, rewrite the open response without closing it, then
close only when assistant/thought text is still the snapshot that was
rendered.  Late chunks that arrive during the debounce or the finish
callback update state and reschedule; an early stub must not land before
the final text is stable."
  (when (emagent-acp-state-prompt-finishing state)
    (when-let ((buffer (emagent-acp--chat-buffer state)))
      (cond
       ((emagent-acp-state-quiet-prompt state)
        (setf (emagent-acp-state-quiet-prompt state) nil)
        (setf (emagent-acp-state-assistant-text state) "")
        (setf (emagent-acp-state-thought-text state) "")
        (emagent-acp--clear-thought-buffer state)
        (emagent-acp--cancel-prompt-render state)
        (setf (emagent-acp-state-prompt-finishing state) nil)
        (setf (emagent-acp-state-prompt-finalized state) t)
        (emagent-log "compacted session materialized")
        (emagent-acp--progress state "connected")
        (emagent-acp--refresh-mode-line state))
       ((emagent-acp-state-compress-pending state)
        (let ((summary (string-trim (or (emagent-acp-state-assistant-text state) ""))))
          (setf (emagent-acp-state-compress-pending state) nil)
          (if (string-empty-p summary)
              (progn
                (emagent-log "compression aborted: empty summary")
                (with-current-buffer buffer
                  (when-let ((cb (emagent-acp-state-cb-fail state)))
                    (funcall cb "Compression produced no summary; conversation left intact"))))
            (with-current-buffer buffer
              (when-let ((cb (emagent-acp-state-cb-finish state)))
                (funcall cb
                         (format "*Context compacted.* Agent session reset; the summary below is its only memory of the prior conversation.\n\n%s"
                                 summary))))
            (emagent-log "compressed session (%d chars)" (length summary))
            (unless (fboundp 'emagent-acp--new-session)
              (require 'emagent-acp))
            (emagent-acp--new-session
             :state state
             :compressed-context summary
             :on-ready
             (lambda ()
               (emagent-acp--materialize-session state))))
          (setf (emagent-acp-state-prompt-finishing state) nil)
          (setf (emagent-acp-state-prompt-finalized state) t)
          (with-current-buffer buffer
            (emagent-chat--flush-deferred-font-lock))
          (emagent-acp--refresh-mode-line state)))
       (t
        (let ((token (emagent-acp-state-finish-token state))
              (assistant (emagent-acp-state-assistant-text state))
              (thought (emagent-acp-state-thought-text state))
              (failed nil))
          (condition-case err
              (with-current-buffer buffer
                (when-let ((cb (emagent-acp-state-cb-finish state)))
                  (let ((emagent-chat--finish-close nil))
                    (funcall cb assistant thought))))
            (error
             (setq failed t)
             (emagent-log "emagent: finish failed: %s" (error-message-string err))
             (with-current-buffer buffer
               (when-let ((cb (emagent-acp-state-cb-fail state)))
                 (funcall cb (format "response finalize failed: %s"
                                     (error-message-string err)))))))
          (cond
           (failed
            (setf (emagent-acp-state-prompt-finishing state) nil)
            (setf (emagent-acp-state-prompt-finalized state) t)
            (with-current-buffer buffer
              (emagent-chat--flush-deferred-font-lock))
            (emagent-acp--refresh-mode-line state))
           ((and (emagent-acp-state-prompt-finishing state)
                 (eq (emagent-acp-state-finish-token state) token)
                 (eq (emagent-acp-state-assistant-text state) assistant)
                 (eq (emagent-acp-state-thought-text state) thought))
            ;; Finalize before close so a reentrant chunk cannot stream or
            ;; schedule another render against a half-closed response.
            (setf (emagent-acp-state-prompt-finalized state) t)
            (setf (emagent-acp-state-prompt-finishing state) nil)
            (emagent-acp--cancel-prompt-render state)
            (with-current-buffer buffer
              (emagent-chat--close-finished-response))
            (emagent-acp--refresh-mode-line state))
           ((and (emagent-acp-state-prompt-finishing state)
                 (eq (emagent-acp-state-finish-token state) token))
            ;; Text changed during finish but no newer timer was scheduled.
            (emagent-acp--schedule-prompt-render state)))))))))

(defun emagent-acp--maybe-complete-deferred-prompt (state)
  "Run a deferred `emagent-acp--complete-prompt' when permissions are clear.

Arguments: STATE."
  (when-let ((response (emagent-acp-state-deferred-complete-response state)))
    (unless (emagent-acp--permission-pending-p state)
      (setf (emagent-acp-state-deferred-complete-response state) nil)
      (emagent-acp--complete-prompt state response))))

(defun emagent-acp--complete-prompt (state response)
  "Finalize the in-flight prompt for STATE using RESPONSE and close chat."
  (cond
   ((emagent-acp-state-prompt-finalized state)
    (when (emagent-acp-state-busy state)
      (setf (emagent-acp-state-busy state) nil)
      (emagent-acp--refresh-mode-line state)))
   ((not (emagent-acp-state-busy state))
    nil)
   ((emagent-acp--permission-pending-p state)
    (setf (emagent-acp-state-deferred-complete-response state) response))
   (t
    (setf (emagent-acp-state-prompt-retry-gen state) nil)
    (setf (emagent-acp-state-prompt-finishing state) t)
    (setf (emagent-acp-state-busy state) nil)
    (setf (emagent-acp-state-current-tool state) nil)
    (setf (emagent-acp-state-current-tool-kind state) nil)
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--trace "prompt done (%d chars, %d thought)"
                        (length (or (emagent-acp-state-assistant-text state) ""))
                        (length (or (emagent-acp-state-thought-text state) "")))
    (emagent-acp--flush-thought-buffer state)
    (when (and response (map-elt response 'usage))
      (emagent-acp--save-usage-from-response state (map-elt response 'usage)))
    (emagent-acp--refresh-mode-line state)
    (emagent-acp--schedule-prompt-render state)
    (emagent-acp--arm-wakeup state)
    (emagent-acp--arm-plan-build state))))

(defun emagent-acp--arm-wakeup (state)
  "Start the ScheduleWakeup timer for STATE after this turn completes.
Called when the turn completes: the agent has ended its reply and now
waits to be re-invoked.  The wakeup prompt is sent as a regular user
turn so the transcript records what re-started the agent."
  (when-let ((request (and emagent-acp-honor-schedule-wakeup
                           (emagent-acp-state-wakeup-request state))))
    (emagent-acp--cancel-wakeup state)
    (let ((delay (plist-get request :delay))
          (text (or (plist-get request :prompt)
                    (if-let ((reason (plist-get request :reason)))
                        (format "Wake up: %s" reason)
                      "Wake up: continue the scheduled task."))))
      (emagent-acp--notify-user
       state (format "emagent: wakeup armed in %ds%s" delay
                     (if-let ((reason (plist-get request :reason)))
                         (format " — %s" reason)
                       "")))
      (setf (emagent-acp-state-wakeup-timer state)
            (run-with-timer delay nil #'emagent-acp--fire-wakeup state text)))))

(defun emagent-acp--fire-wakeup (state text)
  "Send TEXT as a new user turn for STATE's chat buffer.
Skips silently when the buffer is gone or a prompt is already running
\(a manual turn superseded the loop)."
  (setf (emagent-acp-state-wakeup-timer state) nil)
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (cond
       ((emagent-acp-state-busy state)
        (emagent-log "wakeup: skipped — a prompt is already running"))
       ((not (fboundp 'emagent-chat--insert-user-heading-with-text))
        (emagent-log "wakeup: skipped — chat send unavailable"))
       (t
        (emagent-log "wakeup: %s" (emagent-log-truncate-line text 80))
        (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
          (emagent-chat--begin-response response-pos))
        (emagent-chat--ensure-follow-window buffer)
        ;; emagent-acp-send drops the turn unless a send token is armed
        ;; (manual C-c C-c calls send-pending-begin; Build/wakeup must too).
        (emagent-chat--send-pending-begin)
        (unless (fboundp 'emagent-acp-send)
          (require 'emagent-acp))
        (emagent-acp-send text))))))

(defun emagent-acp--set-session-mode (state mode-id)
  "Best-effort `session/set_mode' to MODE-ID for STATE."
  (when-let ((session-id (emagent-acp-state-session-id state)))
    (unless (fboundp 'emagent-acp--send-request)
      (require 'emagent-acp-usage))
    (emagent-acp--send-request
     :state state
     :request (emagent-acp-make-session-set-mode-request
               :session-id session-id
               :mode-id mode-id)
     :on-success
     (lambda (_response)
       (setf (emagent-acp-state-session-mode-id state) mode-id)
       (emagent-acp--refresh-mode-line state))
     :on-failure
     (lambda (err _raw)
       (emagent-log "session/set_mode %s failed: %s"
                    mode-id
                    (or (map-elt err 'message) err))))))

(defun emagent-acp--ensure-agent-mode (state)
  "Best-effort `session/set_mode' to agent for STATE before Build."
  (emagent-acp--set-session-mode state "agent"))

(defun emagent-acp--fire-plan-build (state text)
  "Send TEXT as the Build follow-up for STATE without a user heading.

Build instructions are agent-internal: open Thinking/Response for the
work, but do not invent a synthetic `* user>' line in the transcript."
  (setf (emagent-acp-state-plan-build-timer state) nil)
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (cond
       ((emagent-acp-state-busy state)
        (emagent-log "plan-build: skipped — a prompt is already running"))
       (t
        (emagent-log "plan-build: %s" (emagent-log-truncate-line text 80))
        ;; Build owns the next turn; allow a normal stub after it finishes.
        (setq emagent-chat--defer-user-stub nil)
        (emagent-chat--begin-response (emagent-chat--user-zone-start))
        (emagent-chat--ensure-follow-window buffer)
        (emagent-chat--send-pending-begin)
        (unless (fboundp 'emagent-acp-send)
          (require 'emagent-acp))
        (emagent-acp-send text))))))

(defun emagent-acp--arm-plan-build (state)
  "Arm a Build turn when create_plan queued one on STATE."
  (when-let ((text (emagent-acp-state-plan-build-prompt state)))
    (setf (emagent-acp-state-plan-build-prompt state) nil)
    (when-let ((timer (emagent-acp-state-plan-build-timer state)))
      (when (timerp timer) (cancel-timer timer))
      (setf (emagent-acp-state-plan-build-timer state) nil))
    (emagent-log "cursor/create_plan: arming Build turn")
    (emagent-acp--ensure-agent-mode state)
    (setf (emagent-acp-state-plan-build-timer state)
          (run-with-timer 0.35 nil
                          #'emagent-acp--fire-plan-build state text))))

(defun emagent-acp--log-thought-line (mode text)
  "Log one thought TEXT line according to MODE."
  (let ((line (string-trim text)))
    (unless (string-empty-p line)
      (pcase mode
        ('minimal
         (emagent-log "… %s" (emagent-log-truncate-line line 80)))
        ('trail
         (emagent-log "… %s" (emagent-log-truncate-line line 72 t)))
        (_ nil)))))

(defun emagent-acp--clear-thought-buffer (state)
  
  "Internal helper for STATE."
  (setf (emagent-acp-state-thought-buffer state) ""))

(defun emagent-acp--flush-thought-buffer (state)
  "Log any trailing thought text for STATE and clear the buffer."
  (when-let ((mode emagent-acp-thought-progress))
    (when-let ((tail (string-trim (or (emagent-acp-state-thought-buffer state) ""))))
      (unless (string-empty-p tail)
        (emagent-acp--log-thought-line mode tail)))
    (emagent-acp--clear-thought-buffer state)))

(defun emagent-acp--thought-chunk (state text)
  "Accumulate thought TEXT for display and optional logging.

Arguments: STATE."
  (unless (string-empty-p text)
    (emagent-acp--detect-external-refusal-in-text state text)
    (setf (emagent-acp-state-thought-text state)
              (concat (or (emagent-acp-state-thought-text state) "") text))
    (when-let ((mode emagent-acp-thought-progress))
      (when (emagent-acp-state-prompt-finishing state)
        (emagent-acp--schedule-prompt-render state))
      (when (memq mode '(buffer both))
        (when-let ((buf (and (emagent-acp--stream-thought-to-buffer-p state)
                             (emagent-acp--chat-buffer state))))
          (with-current-buffer buf
            (when-let ((cb (emagent-acp-state-cb-thought state)))
              (funcall cb text)))))
      (when (memq mode '(minimal trail both))
        (let ((pending (concat (or (emagent-acp-state-thought-buffer state) "") text)))
          (while (string-match "\\`\\(.+?[.!?]\\)\\(?:[[:space:]]\\|\\'\\)" pending)
            (let ((end (match-end 0)))
              (emagent-acp--log-thought-line
               (if (eq mode 'both) 'minimal mode)
               (substring pending 0 end))
              (setq pending (substring pending end))))
          (setf (emagent-acp-state-thought-buffer state) pending))))))

(defun emagent-acp--run-reveal (reveal &optional now)
  
  "Internal helper for REVEAL and NOW."
  (when reveal
    (if now
        (funcall reveal)
      (run-with-idle-timer 0 nil reveal))))

(defun emagent-acp--reveal-buffer (state &optional now)
  "Run the buffer reveal callback for STATE, if any.

When NOW is non-nil, show the buffer immediately for interactive prompts."
  (when-let ((reveal (emagent-acp-state-on-reveal state)))
    (setf (emagent-acp-state-on-reveal state) nil)
    (emagent-acp--run-reveal reveal now)))

(defun emagent-acp--prepare-interactive-context (state)
  "Focus the chat buffer's window before a user prompt, without rearranging.

Selects the chat window only when it is already visible in the selected
frame, so permission shortcuts (y/n/…) work when the user is looking at
the session.  Never pops the buffer into a window or touches other
frames: with several sessions across frames, stealing a window would
flip an unrelated frame to this session's project.  Background prompts
are surfaced by `emagent-chat--notify-inactive-update' instead.

Arguments: STATE."
  (emagent-acp--reveal-buffer state t)
  (when-let* ((buffer (emagent-acp--chat-buffer state))
              (window (get-buffer-window buffer)))
    (select-window window)))

(defun emagent-acp--fail-connect (state message)
  "Show MESSAGE, reveal the chat buffer, and stop connecting.

Arguments: STATE."
  (setf (emagent-acp-state-ready state) nil)
  (emagent-acp--notify-user state message)
  (emagent-acp--reveal-buffer state))

(defun emagent-acp--quota-error-p (message)
  "Return non-nil when MESSAGE is a session/rate/usage quota error."
  (and (stringp message)
       (string-match-p
        (concat "session limit\\|rate limit\\|usage limit\\|spend limit"
                "\\|You've hit your\\|hit your limit\\|out of credits"
                "\\|quota exceeded\\|quota limit")
        message)))

(defun emagent-acp--fatal-agent-error-p (message)
  "Return non-nil when MESSAGE should abort the in-flight prompt.

RetriableError and other transient network failures are excluded: those are
retried by `emagent-acp--schedule-prompt-retry' and must not be double-handled
via stderr subscription (which would clear `:busy' before the retry fires).

Session/rate quota errors are fatal so they surface in the chat buffer even
when they arrive only on agent stderr."
  (and (stringp message)
       (not (string-match-p "RetriableError" message))
       (not (emagent-acp--retriable-prompt-error-p message))
       (or (emagent-acp--quota-error-p message)
           (string-match-p
            "timed out\\|timeout\\|failed with status\\|ApiError\\|API Error\\|\\[31merror"
            message))))

(defun emagent-acp--prompt-retry-pending-p (state)
  "Return non-nil when STATE is waiting to replay a failed prompt."
  (and state
       (emagent-acp-state-prompt-retry-gen state)
       (eq (emagent-acp-state-prompt-retry-gen state)
           (emagent-acp-state-prompt-generation state))
       (emagent-acp-state-busy state)))

(defun emagent-acp--retriable-prompt-error-p (message)
  "Return non-nil when a failed prompt MESSAGE is a transient network error.

Covers Cursor's own RetriableError wrapper and the common DNS/connection
failures underneath it (getaddrinfo ENOTFOUND api2.cursor.sh, connection
resets, timeouts).  These usually recover on a second attempt, so emagent
retries them before surfacing the error (`emagent-acp-prompt-retry-attempts')."
  (and (stringp message)
       (string-match-p
        (concat "RetriableError\\|getaddrinfo\\|ENOTFOUND\\|EAI_AGAIN"
                "\\|ECONNRESET\\|ECONNREFUSED\\|ConnectionRefused"
                "\\|ETIMEDOUT\\|EPIPE"
                "\\|\\[unavailable\\]\\|socket hang up\\|network error"
                "\\|Unable to connect to API")
        message)))

(defun emagent-acp--prompt-retry-delay (attempt)
  "Return backoff seconds to wait before the next retry after ATTEMPT (1-based)."
  (* emagent-acp-prompt-retry-base-delay (expt 2 (max 0 (1- attempt)))))

(defun emagent-acp--abort-prompt (state message)
  "Abort the in-flight prompt for STATE and show MESSAGE.

Quota/session-limit errors are always shown in the chat buffer, even when the
watchdog already finalized a partial Response (busy cleared) while the agent
was still working."
  (setf (emagent-acp-state-prompt-retry-gen state) nil)
  (let ((quiet (emagent-acp-state-quiet-prompt state))
        (in-flight (or (emagent-acp-state-busy state)
                       (emagent-acp-state-prompt-finishing state)))
        (force (and (not (emagent-acp-state-quiet-prompt state))
                    (emagent-acp--quota-error-p message))))
    (when (or in-flight force)
      ;; Do not arm a ScheduleWakeup captured during a failed/aborted turn.
      (emagent-acp--cancel-wakeup state)
      (emagent-acp--cancel-plan-build state)
      (when in-flight
        (emagent-acp--clear-prompt-watchdog state)
        (emagent-acp--cancel-prompt-render state)
        (setf (emagent-acp-state-busy state) nil)
        (setf (emagent-acp-state-prompt-finishing state) nil)
        (setf (emagent-acp-state-prompt-finalized state) nil)
        (setf (emagent-acp-state-assistant-text state) "")
        (setf (emagent-acp-state-compress-pending state) nil)
        (setf (emagent-acp-state-quiet-prompt state) nil)
        (emagent-acp--flush-thought-buffer state))
      (emagent-acp--trace "prompt aborted: %s" message)
      (cond
       (quiet
        (emagent-log "compacted session materialize failed: %s" message))
       (t
        (when-let ((buffer (emagent-acp--chat-buffer state)))
          (with-current-buffer buffer
            (when-let ((cb (emagent-acp-state-cb-fail state)))
              (funcall cb message))))))
      (emagent-acp--refresh-mode-line state))))

(defun emagent-acp--system-prompt ()
  "Return the system prompt for new ACP sessions."
  (concat emagent-acp-system-prompt
          (emagent-mcp-gateway-system-prompt)
          (when emagent-acp-prefer-emacs
            (emagent-prompts--prefer-emacs-prompt))
          (when emagent-acp-prefer-emacs
            (emagent-prompts--structural-policy))))

(defun emagent-acp--session-system-prompt (&optional compressed-context)
  "Return the system prompt for session/new, optionally with COMPRESSED-CONTEXT."
  (let ((summary (string-trim (or compressed-context ""))))
    (if (string-empty-p summary)
        (emagent-acp--system-prompt)
      (concat (emagent-acp--system-prompt)
              (format "\n\n[Compressed prior conversation context]\n%s"
                      summary)))))

(defun emagent-acp--trace-update (update-type emagent-acp-notification)
  "Log UPDATE-TYPE and a short payload summary when tracing.

Arguments: EMAGENT-ACP-NOTIFICATION."
  (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) ""))
        (title (map-nested-elt emagent-acp-notification '(params update title))))
    (pcase update-type
      ((or "agent_message_chunk" "agent_thought_chunk")
       (emagent-acp--trace "recv %s +%d" update-type (length text)))
      ((or "tool_call" "tool_call_update")
       (let* ((update (map-nested-elt emagent-acp-notification '(params update)))
              (raw (or (map-elt update 'rawInput) (map-elt update 'arguments)))
              (subtitle (map-elt update 'subtitle))
              (locations (map-elt update 'locations))
              (id (map-elt update 'toolCallId))
              (raw-summary
               (cond
                ((or (null raw) (equal raw :null) (equal raw "")) nil)
                ((hash-table-p raw)
                 (format "keys(%s)"
                         (string-join (hash-table-keys raw) ",")))
                ((listp raw)
                 (format "keys(%s)"
                         (string-join (mapcar (lambda (p) (format "%s" (car p))) raw) ",")))
                ((stringp raw)
                 (format "str(%d)" (length raw)))
                (t "?")))
              (detail (or raw-summary
                          (when subtitle (format "sub=%s" (truncate-string-to-width subtitle 40 nil nil "…")))
                          (when locations (format "locs=%d" (length (append locations nil))))
                          "no-detail")))
         (emagent-acp--trace "recv %s %s [%s] %s"
                             update-type
                             (or title id "?")
                             (or (map-elt update 'status) "")
                             detail)))
      (_
       (emagent-acp--trace "recv %s" (or update-type "session/update"))))))

(cl-defun emagent-acp--on-notification (&key state emagent-acp-notification)
  
  "Internal helper for STATE and EMAGENT-ACP-NOTIFICATION."
  (when (equal (map-elt emagent-acp-notification 'method) "session/update")
    (let ((update-type (map-nested-elt emagent-acp-notification '(params update sessionUpdate))))
      (emagent-acp--trace-update update-type emagent-acp-notification)
      (pcase update-type
        ("agent_message_chunk"
         (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) "")))
           (unless (emagent-acp-state-replaying-history state)
             (when (and (not (string-empty-p text))
                        (emagent-acp-state-tool-call-since-last-chunk state)
                        (not (string-empty-p (or (emagent-acp-state-assistant-text state) ""))))
               (setq text (concat "\n\n" text)))
             (setf (emagent-acp-state-tool-call-since-last-chunk state) nil)
             (emagent-acp--detect-external-refusal-in-text state text)
             (setf (emagent-acp-state-assistant-text state) (concat (emagent-acp-state-assistant-text state) text))
             (when (emagent-acp-state-prompt-finishing state)
               (emagent-acp--schedule-prompt-render state))
             (when-let ((buf (and (emagent-acp--stream-to-buffer-p state)
                                 (emagent-acp--chat-buffer state))))
               (with-current-buffer buf
                 (when-let ((cb (emagent-acp-state-cb-chunk state)))
                   (funcall cb text)))))))
        ("agent_thought_chunk"
         (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) "")))
           (emagent-acp--thought-chunk state text)))
        ("tool_call"
         (setf (emagent-acp-state-tool-call-since-last-chunk state) t)
         (emagent-acp--on-tool-call state (map-nested-elt emagent-acp-notification '(params update))))
        ("tool_call_update"
         (emagent-acp--on-tool-call state (map-nested-elt emagent-acp-notification '(params update))))
        ("config_option_update"
         (emagent-acp--save-config-options
          state
          (map-nested-elt emagent-acp-notification '(params update configOptions)))
         (when-let ((model-id (emagent-acp--current-model-id state nil)))
           (emagent-acp--persist-model-id state model-id)))
        ("usage_update"
         (emagent-acp--update-usage-from-notification
          state
          (map-nested-elt emagent-acp-notification '(params update))))
        ("available_commands_update"
         (let ((commands (map-nested-elt emagent-acp-notification
                                         '(params update availableCommands))))
           (when-let* ((buffer (emagent-acp--chat-buffer state))
                       (cb (emagent-acp-state-cb-slash-commands state)))
             (with-current-buffer buffer
               (funcall cb commands)))))
        ("current_mode_update"
         (let* ((update (map-nested-elt emagent-acp-notification
                                        '(params update)))
                (mode-id (or (map-elt update 'currentModeId)
                             (map-elt update 'modeId)
                             (map-elt update :currentModeId)
                             (map-elt update :modeId))))
           (when (and (stringp mode-id) (not (string-empty-p mode-id)))
             (setf (emagent-acp-state-session-mode-id state) mode-id)
             (emagent-acp--refresh-mode-line state))))
        (_ nil)))))

(cl-defun emagent-acp--subscribe (&key state)
  "Subscribe STATE's client to ACP errors, notifications, and requests."
  (let ((buffer (emagent-acp--chat-buffer state)))
    (emagent-acp-subscribe-to-errors
     :client (emagent-acp-state-client state)
     :buffer buffer
     :on-error
     (lambda (emagent-acp-error)
       (let ((message (or (map-elt emagent-acp-error 'message)
                          (format "%s" emagent-acp-error))))
         (emagent-acp--log-agent-stderr message)
         (when (and (emagent-acp--fatal-agent-error-p message)
                    (not (emagent-acp--prompt-retry-pending-p state))
                    (or (emagent-acp-state-busy state)
                        (emagent-acp-state-prompt-finishing state)
                        (emagent-acp--quota-error-p message)))
           (emagent-acp--abort-prompt state message))
         (when (emagent-acp--stderr-notify-p emagent-acp-error)
           (emagent-acp--notify-user state (format "emagent error: %s" message))))))
    (emagent-acp-subscribe-to-notifications
     :client (emagent-acp-state-client state)
     :buffer buffer
     :on-notification
     (lambda (notification)
       (emagent-acp--on-notification :state state
                                     :emagent-acp-notification notification)))
    (emagent-acp-subscribe-to-requests
     :client (emagent-acp-state-client state)
     :buffer buffer
     :on-request
     (lambda (request)
       (emagent-acp--on-request :state state :emagent-acp-request request)))))

(cl-defun emagent-acp--authenticate (&key state method-id on-ready)
  "Send an authenticate request with METHOD-ID, then connect the session.

Called when `initialize' returns authMethods (e.g. cursor_login).
The authenticate call completes the credential handshake so the agent
grants full plan access (including Auto model) to this ACP session.

Arguments: STATE, ON-READY."
  (emagent-acp--progress state (format "authenticating (%s)…" method-id))
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-authenticate-request :method-id method-id)
   :on-success (lambda (_response)
                 (emagent-acp--connect-session :state state :on-ready on-ready))
   :on-failure (lambda (error _raw)
                 (emagent-log "authenticate %s failed: %s — proceeding anyway"
                              method-id
                              (or (map-elt error 'message) (format "%s" error)))
                 (emagent-acp--connect-session :state state :on-ready on-ready))))

(cl-defun emagent-acp--initialize (&key state on-ready)
  
  "Internal helper for STATE and ON-READY."
  (emagent-acp--progress state "initializing ACP…")
  (emagent-acp--send-request
   :state state
   :request (if emagent-acp-file-access
                (emagent-acp-make-initialize-request
                 :protocol-version 1
                 :client-info `((name . "emagent")
                                (title . "Emacs Emagent")
                                (version . "1.0.2"))
                 :read-text-file-capability t
                 :write-text-file-capability t)
              (emagent-acp-make-initialize-request
               :protocol-version 1
               :client-info `((name . "emagent")
                              (title . "Emacs Emagent")
                              (version . "1.0.2"))))
   :on-success (lambda (response)
                 (setf (emagent-acp-state-initialized state) t)
                 (setf (emagent-acp-state-mcp-http state) (emagent-acp--mcp-http-capable-p response))
                 (emagent-acp--infer-external-tool-gate-from-agent state)
                 (emagent-acp--infer-external-tool-gate-from-initialize-response state response)
                 (emagent-acp--maybe-log-external-tool-gate-proactive state)
                 (let ((auth-methods (append (map-elt response 'authMethods) nil)))
                   (if-let ((method-id (map-elt (seq-find
                                                 (lambda (m) (map-elt m 'id))
                                                 auth-methods)
                                                'id)))
                       (emagent-acp--authenticate
                        :state state :method-id method-id :on-ready on-ready)
                     (emagent-acp--connect-session :state state :on-ready on-ready))))
   :on-failure (lambda (error _raw)
                 (emagent-acp--fail-connect
                  state
                  (format "emagent: initialize failed: %s"
                          (or (map-elt error 'message) (format "%s" error)))))))

(defun emagent-acp--mcp-http-capable-p (initialize-response)
  "Return non-nil when INITIALIZE-RESPONSE advertises http MCP support."
  (let ((value (map-nested-elt initialize-response
                               '(agentCapabilities mcpCapabilities http))))
    (and value (not (eq value :false)) (not (eq value :json-false)))))

(cl-defun emagent-acp--session-ready (&key state session-id on-ready resumed)
  
  "Internal helper for STATE and SESSION-ID and ON-READY and RESUMED."
  (setf (emagent-acp-state-session-id state) session-id)
  (setf (emagent-acp-state-ready state) t)
  (emagent-acp--persist-session-id state session-id)
  (emagent-acp--hydrate-session-permissions state session-id)
  (emagent-tools-set-project-directory (emagent-acp--session-cwd state))
  (emagent-acp--progress state (if resumed "resumed" "connected"))
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (pcase emagent-chat-provider
        ('cursor (emagent-chat-seed-cursor-slash-commands))
        ('claude
         (when (null emagent-chat-slash-commands)
           (emagent-log "loading slash commands from agent…"))))))
  (emagent-acp--start-rss-timer state)
  (emagent-acp--reveal-buffer state)
  (when on-ready (funcall on-ready)))

(cl-defun emagent-acp--new-session (&key state on-ready compressed-context)
  
  "Internal helper for STATE and ON-READY and COMPRESSED-CONTEXT."
  (emagent-acp--progress state "creating session…")
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-new-request
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (emagent-acp-state-mcp-http state)
                                                       (emagent-acp--chat-buffer state))
             :meta `((systemPrompt . ((append . ,(emagent-acp--session-system-prompt
                                                  compressed-context))))))
   :on-success (lambda (response)
                 (unless (fboundp 'emagent-acp--configure-model)
                   (require 'emagent-acp-usage))
                 (emagent-acp--configure-model
                  :state state
                  :session-id (map-elt response 'sessionId)
                  :response response
                  :on-ready on-ready))
   :on-failure (lambda (error _raw)
                 (emagent-acp--fail-connect
                  state
                  (format "emagent: session/new failed: %s"
                          (or (map-elt error 'message) (format "%s" error)))))))

(cl-defun emagent-acp--load-session (&key state session-id on-ready)
  "Resume SESSION-ID for STATE, falling back to session/new on failure."
  (emagent-acp--progress state "resuming session…")
  (setf (emagent-acp-state-replaying-history state) t)
  (emagent-acp--set-suppress-history-updates
   (emagent-acp-state-client state) t)
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-load-request
             :session-id session-id
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (emagent-acp-state-mcp-http state)
                                                       (emagent-acp--chat-buffer state))
             :meta `((systemPrompt . ((append . ,(emagent-acp--system-prompt))))))
   :on-success (lambda (response)
                 (emagent-acp--set-suppress-history-updates
                  (emagent-acp-state-client state) nil)
                 (setf (emagent-acp-state-replaying-history state) nil)
                 (unless (fboundp 'emagent-acp--configure-model)
                   (require 'emagent-acp-usage))
                 (emagent-acp--configure-model
                  :state state
                  :session-id session-id
                  :response response
                  :on-ready on-ready
                  :resumed t))
   :on-failure (lambda (error _raw)
                 (emagent-acp--set-suppress-history-updates
                  (emagent-acp-state-client state) nil)
                 (setf (emagent-acp-state-replaying-history state) nil)
                 (emagent-log "session/load failed for %s: %s"
                              session-id
                              (or (map-elt error 'message) (format "%s" error)))
                 (emagent-acp--progress state "resume failed, creating session…")
                 (when-let ((buf (emagent-acp--chat-buffer state)))
                   (with-current-buffer buf
                     (let ((was-modified (buffer-modified-p)))
                       (unwind-protect
                           (emagent-session-clear-id)
                         (set-buffer-modified-p was-modified)))))
                 (emagent-acp--new-session :state state :on-ready on-ready))))

(cl-defun emagent-acp--connect-session (&key state on-ready)
  
  "Internal helper for STATE and ON-READY."
  (emagent-acp--progress state "connecting session…")
  (let ((saved (emagent-acp--saved-session-id state)))
    (if (and saved (not (string-empty-p saved)))
        (emagent-acp--load-session :state state :session-id saved :on-ready on-ready)
      (emagent-acp--new-session :state state :on-ready on-ready))))

(cl-defun emagent-acp-start (&key client chat-buffer on-ready on-reveal callbacks)
  "Start an emagent ACP session in CHAT-BUFFER.

ON-REVEAL is called once when the chat buffer should be shown.
CALLBACKS is an alist of rendering callbacks keyed by:
  :cb-chunk, :cb-thought, :cb-finish, :cb-fail, :cb-slash-commands.

Arguments: CLIENT, ON-READY."
  (when (and emagent-acp-prefer-emacs (not emagent-acp-file-access))
    (emagent-log "prefer-Emacs mode works best with `emagent-acp-file-access'"))
  (when emagent-acp-trace
    (setq emagent-acp-logging-enabled t))
  (with-current-buffer chat-buffer
    (emagent-chat-clear-slash-commands)
    ;; Cursor built-ins are local; keep them available while the agent
    ;; connects so TAB completion does not go empty mid-reconnect.
    (emagent-chat-seed-cursor-slash-commands)
    (setq emagent-acp--session (emagent-acp--make-state :client client
                                                        :chat-buffer chat-buffer
                                                        :on-reveal on-reveal))
    (setf (emagent-acp-state-provider emagent-acp--session) (or emagent-chat-provider 'cursor))
    (dolist (cb callbacks)
      (emagent-acp--set-callback emagent-acp--session (car cb) (cdr cb)))
    (emagent-mcp-register-session :token (emagent-mcp-buffer-token)
                                  :cwd (emagent-chat--session-directory)
                                  :buffer chat-buffer
                                  :prefer-emacs emagent-acp-prefer-emacs
                                  :acp t)
    (emagent-acp--progress emagent-acp--session "starting agent…")
    (emagent-acp--subscribe :state emagent-acp--session)
    (emagent-acp--initialize :state emagent-acp--session :on-ready on-ready)
    emagent-acp--session))

(eval-when-compile
  (require 'cl-lib))

(defgroup emagent-claude nil
  "Claude (Anthropic Agent SDK) ACP provider configuration for emagent."
  :group 'emagent)

(defcustom emagent-claude-acp-command
  '("claude-agent-acp")
  "Command and parameters for the Claude ACP agent.

The npm package @agentclientprotocol/claude-agent-acp installs a
`claude-agent-acp' binary.  To run without a global install, set this to
`(\"npx\" \"-y\" \"@agentclientprotocol/claude-agent-acp\")'."
  :type '(repeat string)
  :group 'emagent-claude)

(defcustom emagent-claude-environment nil
  "Environment variables for the Claude ACP agent (strings \"KEY=value\").

The Claude Agent SDK reads credentials from the environment; set
ANTHROPIC_API_KEY here if you do not export it in your shell profile.

If tools stay blocked after Emacs approves ACP, adjust the SDK or
claude-agent-acp (approval/sandbox).
See `emagent-acp-external-tool-gate-hints'."
  :type '(repeat string)
  :group 'emagent-claude)

(defconst emagent-claude-install-hint
  "Install: npm install -g @agentclientprotocol/claude-agent-acp
Or set `emagent-claude-acp-command' to (\"npx\" \"-y\" \"@agentclientprotocol/claude-agent-acp\").
See https://github.com/agentclientprotocol/claude-agent-acp")

(defun emagent-claude-command ()
  "Return the Claude ACP command name."
  (car emagent-claude-acp-command))

(defun emagent-claude-command-params ()
  "Return extra parameters for the Claude ACP agent.
That is the `cdr' of `emagent-claude-acp-command'."
  (cdr emagent-claude-acp-command))

(defun emagent-claude-check-command ()
  "Signal a clear error when the Claude agent is missing."
  (unless (executable-find (emagent-claude-command))
    (error "Claude ACP agent not found on PATH (%s).\n%s"
           (emagent-claude-command)
           emagent-claude-install-hint)))

(cl-defun emagent-claude-make-client (&key context-buffer process-directory)
  "Create an ACP client for Claude using CONTEXT-BUFFER.
PROCESS-DIRECTORY is passed to `make-process' as the working directory
\(see `emagent-chat--session-directory' / #+EMAGENT_PROJECT)."
  (emagent-claude-check-command)
  (emagent-acp-make-client :context-buffer context-buffer
                   :process-directory process-directory
                   :command (emagent-claude-command)
                   :command-params (emagent-claude-command-params)
                   :environment-variables emagent-claude-environment))

(defun emagent-claude--project-hash (dir)
  "Return the ~/.claude/projects directory name for absolute path DIR.
Claude Code derives the name by replacing every '/' and '.' with '-'."
  (replace-regexp-in-string "[/.]" "-" (directory-file-name (expand-file-name dir))))

(defun emagent-claude-relocate-session (session-id old-dir new-dir)
  "Move Claude session files for SESSION-ID from OLD-DIR's hash to NEW-DIR's.
Claude Code stores sessions under ~/.claude/projects/<hashed-cwd>/<session-id>.
This moves those files so session/load succeeds after the project directory
changes.  Does nothing when the session is not found under OLD-DIR's hash."
  (let* ((projects-base (expand-file-name "~/.claude/projects"))
         (old-proj (expand-file-name (emagent-claude--project-hash old-dir)
                                     projects-base))
         (new-proj (expand-file-name (emagent-claude--project-hash new-dir)
                                     projects-base)))
    (when (and (file-directory-p projects-base)
               (file-directory-p old-proj))
      (make-directory new-proj t)
      (dolist (suffix '("" ".jsonl"))
        (let ((src (expand-file-name (concat session-id suffix) old-proj))
              (dst (expand-file-name (concat session-id suffix) new-proj)))
          (when (file-exists-p src)
            (rename-file src dst)
            (message "emagent: moved %s → %s" src dst)))))))

(defun emagent-acp--make-client (provider buffer)
  "Create an ACP client for PROVIDER using BUFFER as context."
  (let ((process-directory (and (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (emagent-chat--session-directory)))))
    (pcase provider
      ('cursor (emagent-cursor-make-client :context-buffer buffer
                                           :process-directory process-directory))
      ('claude (emagent-claude-make-client :context-buffer buffer
                                           :process-directory process-directory))
      (_ (user-error "Unknown emagent provider: %s" provider)))))

(defvar emagent-default-provider)

(cl-defun emagent-acp-ensure-connected (&key on-ready on-reveal)
  "Connect the current emagent buffer to its ACP provider if needed.

When the agent process died but buffer-local state remains, tear it down and
reconnect (resuming the saved session id when present).  Optional ON-READY runs
once the session is ready; ON-REVEAL runs when the chat buffer should be shown.
While a connection is already in flight, ON-READY is queued instead of tearing
the session down and starting over."
  (when on-ready (push on-ready emagent-acp--when-connected-queue))
  (cond
   ((emagent-acp--connected-p)
    (emagent-acp--run-when-connected-queue))
   ((emagent-acp--connecting-p)
    nil)
   (t
    (emagent-acp--teardown-stale-session)
    (let* ((provider (or emagent-chat-provider emagent-default-provider))
           (client (emagent-acp--make-client provider (current-buffer))))
      (emagent-acp-start :client client
                        :chat-buffer (current-buffer)
                        :on-ready #'emagent-acp--run-when-connected-queue
                        :on-reveal on-reveal
                        :callbacks
                        `((:cb-chunk          . ,#'emagent-chat-append-assistant)
                          (:cb-thought        . ,#'emagent-chat-append-thought)
                          (:cb-finish         . ,(lambda (&rest args)
                                                    (apply #'emagent-chat-finish-assistant args)
                                                    (emagent-acp--restore-turn-model)))
                          (:cb-fail           . ,(lambda (&rest args)
                                                    (apply #'emagent-chat-fail-assistant args)
                                                    (emagent-acp--turn-model-on-failure
                                                     (car args))))
                          (:cb-slash-commands . ,#'emagent-chat-set-slash-commands)
                          (:cb-tool-call      . ,#'emagent-chat-show-tool-call)
                          (:cb-permission     . ,#'emagent-chat-permission-prompt)
                          (:cb-status         . ,#'emagent-chat-set-status)))))))

(defun emagent-acp--send-prompt-safe (buffer user-text &optional compress)
  "Send USER-TEXT from BUFFER, logging and surfacing failures in the chat.
COMPRESS is forwarded to `emagent-acp-send-prompt'."
  (with-current-buffer buffer
    (condition-case err
        (emagent-acp-send-prompt user-text compress)
      (error
       (let ((msg (error-message-string err)))
         (when (fboundp 'emagent-chat--send-pending-end)
           (emagent-chat--send-pending-end))
         (emagent-log "emagent: send failed: %s" msg)
         (when (fboundp 'emagent-chat-fail-assistant)
           (emagent-chat-fail-assistant msg)))))))

(defun emagent-acp-send (user-text &optional compress)
  "Ensure connection and send USER-TEXT from the current buffer.

When a per-turn model override (`emagent-chat--turn-model', set by `/model') is
active and differs from the session model, switch to it transiently first, then
send; the buffer model is restored when the turn ends (see
`emagent-chat-finish-assistant' / `emagent-chat-fail-assistant').

COMPRESS is forwarded to `emagent-acp-send-prompt': set by
`emagent-chat--dispatch-compress' when USER-TEXT is already a compression
summary prompt rather than ordinary chat input."
  (let ((buf (current-buffer))
        (turn-model emagent-chat--turn-model)
        (token emagent-chat--send-token))
    (emagent-acp-ensure-connected
     :on-ready
     (lambda ()
       (with-current-buffer buf
         (when (emagent-chat--send-active-p token)
           (let* ((state emagent-acp--session)
                  (current (and state (emagent-acp-current-model-id)))
                  (target (and turn-model state
                                (emagent-acp--match-model-id turn-model state nil))))
             (if (and target current (not (string= target current)))
                 (progn
                   ;; Remember the real session model to restore to (only the
                   ;; first time, so a sticky post-failure override still points
                   ;; back at the original global model).
                   (unless emagent-chat--turn-model-base
                     (setq emagent-chat--turn-model-base current))
                   (when (fboundp 'emagent-acp--progress)
                     (emagent-acp--progress
                      state
                      (format "switching model to %s for this turn…"
                              (if (fboundp 'emagent-acp--model-display-name)
                                  (emagent-acp--model-display-name state nil target)
                                target))))
                   (emagent-acp-set-model-transient
                    target
                    (lambda ()
                      (when (emagent-chat--send-active-p token)
                        (emagent-acp--send-prompt-safe buf user-text compress)))))
               (emagent-acp--send-prompt-safe buf user-text compress)))))))))

(defun emagent-acp--wire-chat-buffer ()
  "Install buffer teardown for the current emagent chat buffer.

Adds `emagent-acp-shutdown-buffer' on `kill-buffer-hook'.  Send/attach/quit
commands call ACP entry points directly (lazy `require'), so no buffer-local
function slots are needed."
  (add-hook 'kill-buffer-hook #'emagent-acp-shutdown-buffer nil t))

(add-hook 'emagent-mode-hook #'emagent-acp--wire-chat-buffer)

(defun emagent-acp--restore-turn-model ()
  "Restore the session model overridden by `/model' and clear the override.
Called on a successful turn: switches back to the captured base model and clears
`emagent-chat--turn-model' so the next prompt uses the buffer model again."
  (when emagent-chat--turn-model
    (when emagent-chat--turn-model-base
      (emagent-acp-set-model-transient emagent-chat--turn-model-base #'ignore))
    (setq emagent-chat--turn-model nil
          emagent-chat--turn-model-base nil)))

(defun emagent-acp--turn-model-on-failure (&optional message)
  "After a failed `/model' turn, keep or restore the per-turn override.

MESSAGE is the failure text used to classify transient vs permanent errors.
Only transient network failures (after retries are exhausted) ask whether to
keep the override for a manual `retry'.  Permanent errors such as context
overflow restore the buffer model immediately."
  (when emagent-chat--turn-model
    (if (and message (fboundp 'emagent-acp--retriable-prompt-error-p)
         (emagent-acp--retriable-prompt-error-p message))
        (emagent-tools--buttons-prompt
         (format "Continue with %s for the next prompt?" emagent-chat--turn-model)
         '(("Yes, keep it" . keep) ("No, use the buffer model" . restore))
         (current-buffer)
         (lambda (choice)
           (when (eq choice 'restore)
             (emagent-acp--restore-turn-model))))
      (emagent-acp--restore-turn-model))))

(defconst emagent-acp-cursor--tool-resolve-max-attempts 8
  "Maximum store.db lookups while waiting for Cursor tool-call args.")

(defconst emagent-acp-cursor--tool-resolve-base-delay 0.05
  "Initial idle delay between Cursor store.db resolve retries.")

(defconst emagent-acp-cursor--tool-resolve-yield 0.01
  "Delay before starting the next queued store.db lookup after one finishes.

Keeps successive sqlite + JSON parse bursts from stacking on zero-delay
timers during tool-heavy Cursor turns.")

(defun emagent-acp-cursor--detect-p (state)
  "Return non-nil when STATE's agent is Cursor's cursor-agent CLI."
  (when-let ((launch (emagent-acp--agent-launch-string state)))
    (string-match-p "cursor-agent" launch)))

(defun emagent-acp-cursor--enrich-tool-call (_state update)
  "Return UPDATE unchanged — never sync-read Cursor store.db here.

Cursor often omits tool args on the wire; looking them up in store.db via
`shell-command-to-string' on every tool-call notification froze Emacs.
Empty-arg updates are deferred and resolved asynchronously by
`emagent-acp-cursor--resolve-tool-from-store'.

Arguments: UPDATE."
  update)

(defun emagent-acp-cursor--defer-tool-call-p (_state update)
  "Return non-nil when Cursor sent a generic tool call without args yet.

Arguments: UPDATE."
  (and (map-elt update 'toolCallId)
       (not (emagent-acp--tool-call-meaningful-detail-p update))))

(defun emagent-acp-cursor--tool-resolve-active-p (state)
  "Return non-nil while Cursor store.db tool-call lookups are pending.

Arguments: STATE."
  (or (emagent-acp-state-tool-resolve-worker state)
      (emagent-acp-state-tool-resolve-queue state)))

(defun emagent-acp-cursor--reset-tool-resolve (state)
  "Clear Cursor tool-call resolve queue state in STATE."
  (setf (emagent-acp-state-tool-resolve-queue state) nil)
  (setf (emagent-acp-state-tool-resolve-worker state) nil)
  (clrhash (emagent-acp-state-tool-resolve-attempts state)))

(defun emagent-acp-cursor--enqueue-tool-resolve (state id &optional delay)
  "Queue ID for a serialized Cursor store.db lookup.

Arguments: STATE, DELAY."
  (let ((queue (emagent-acp-state-tool-resolve-queue state)))
    (unless (member id queue)
      (setf (emagent-acp-state-tool-resolve-queue state) (append queue (list id)))))
  (unless (emagent-acp-state-tool-resolve-worker state)
    (emagent-acp-cursor--drain-tool-resolve-queue state delay)))

(defun emagent-acp-cursor--drain-tool-resolve-queue (state &optional delay)
  "Resolve one queued Cursor tool call via `run-at-time'.

The resolve worker stays set until `emagent-acp-cursor--resolve-tool-from-store'
finishes (sync in tests, async process sentinel interactively), so store.db
lookups never overlap and never run on the ACP process filter.

Arguments: STATE, DELAY."
  (unless (emagent-acp-state-tool-resolve-worker state)
    (if-let ((id (car (emagent-acp-state-tool-resolve-queue state))))
        (progn
          (setf (emagent-acp-state-tool-resolve-worker state) t)
          (run-at-time
           (or delay 0) nil
           (lambda ()
             (setf (emagent-acp-state-tool-resolve-queue state)
                   (cdr (emagent-acp-state-tool-resolve-queue state)))
             ;; Worker stays t until resolve's finish callback clears it.
             (emagent-acp-cursor--resolve-tool-from-store state id))))
      (progn
        (emagent-acp--drain-permission-queue state)
        (emagent-acp--maybe-complete-deferred-prompt state)))))

(defun emagent-acp-cursor--apply-resolved-entry (state id entry)
  "Apply store ENTRY to pending tool-call ID in STATE; return retry delay or nil.

Arguments: STATE, ID, ENTRY."
  (when-let* ((pending-table (emagent-acp-state-tool-call-pending state))
              (merged (gethash id pending-table)))
    (let* ((enriched (if (and entry (fboundp 'emagent-cursor--apply-store-entry))
                         (emagent-cursor--apply-store-entry merged entry)
                       merged))
           (merged (if (equal enriched merged) merged
                     (emagent-acp--merged-tool-call-update state enriched)))
           (label (emagent-acp--tool-call-label merged))
           (status (map-elt merged 'status))
           (kind (map-elt merged 'kind))
           (attempts-table (emagent-acp-state-tool-resolve-attempts state))
           (attempts (gethash id attempts-table 0)))
      (cond
       ((emagent-acp--tool-call-meaningful-detail-p merged)
        (puthash id 0 attempts-table)
        (emagent-acp--emit-tool-call-display state id kind merged label status)
        (remhash id pending-table)
        nil)
       ((>= attempts emagent-acp-cursor--tool-resolve-max-attempts)
        (puthash id 0 attempts-table)
        (when (emagent-acp--tool-call-displayable-p state merged)
          (emagent-acp--emit-tool-call-display state id kind merged label status))
        (remhash id pending-table)
        nil)
       (t
        (puthash id (1+ attempts) attempts-table)
        (let ((queue (emagent-acp-state-tool-resolve-queue state)))
          (unless (member id queue)
            (setf (emagent-acp-state-tool-resolve-queue state)
                  (append queue (list id)))))
        (* emagent-acp-cursor--tool-resolve-base-delay (expt 2 attempts)))))))

(defun emagent-acp-cursor--resolve-tool-from-store (state id)
  "Look up tool-call ID in Cursor store.db asynchronously.

Returns nil immediately after starting the lookup (or applying a sync result
in batch tests).  The resolve worker stays busy until the callback runs.

Arguments: STATE, ID."
  (cl-flet
      ((finish (entry)
         (let ((retry-delay
                (emagent-acp-cursor--apply-resolved-entry state id entry)))
           (setf (emagent-acp-state-tool-resolve-worker state) nil)
           (if retry-delay
               (emagent-acp-cursor--drain-tool-resolve-queue state retry-delay)
             (progn
               (emagent-acp-cursor--drain-tool-resolve-queue
                state emagent-acp-cursor--tool-resolve-yield)
               (emagent-acp--maybe-complete-deferred-prompt state))))))
    (let* ((pending-table (emagent-acp-state-tool-call-pending state))
           (merged (and pending-table (gethash id pending-table)))
           (session-id (emagent-acp-state-session-id state)))
      (cond
       ((not (and merged session-id))
        (finish nil)
        nil)
       ;; ERT and other noninteractive callers mock the sync store helper and
       ;; expect an immediate apply.
       ((or noninteractive
            (not (fboundp 'emagent-cursor-tool-call-from-store-async)))
        (finish (and (fboundp 'emagent-cursor-tool-call-from-store)
                     (emagent-cursor-tool-call-from-store session-id id)))
        nil)
       (t
        (emagent-cursor-tool-call-from-store-async
         session-id id (lambda (entry) (finish entry)))
        nil)))))

(defun emagent-acp-cursor--generic-title-p (title)
  "Return non-nil when TITLE is a generic Cursor ACP placeholder."
  (and (fboundp 'emagent-cursor--generic-acp-title-p)
       (funcall #'emagent-cursor--generic-acp-title-p title)))

(defun emagent-acp-cursor--normalize-slash-prompt (text)
  "Normalize slash commands for Cursor before sending a prompt.

Arguments: TEXT."
  (if (fboundp 'emagent-cursor-normalize-slash-prompt)
      (emagent-cursor-normalize-slash-prompt text)
    text))

(defun emagent-acp-cursor--external-gate-reason (_state)
  "Return the Cursor CLI trust-gate reason symbol."
  'cursor-agent-cli)

(emagent-acp--register-provider
 'cursor
 :detect #'emagent-acp-cursor--detect-p
 :enrich-tool-call #'emagent-acp-cursor--enrich-tool-call
 :defer-tool-call-p #'emagent-acp-cursor--defer-tool-call-p
 :enqueue-tool-resolve #'emagent-acp-cursor--enqueue-tool-resolve
 :reset-tool-resolve #'emagent-acp-cursor--reset-tool-resolve
 :tool-resolve-active-p #'emagent-acp-cursor--tool-resolve-active-p
 :generic-title-p #'emagent-acp-cursor--generic-title-p
 :external-gate-reason #'emagent-acp-cursor--external-gate-reason
 :normalize-slash-prompt #'emagent-acp-cursor--normalize-slash-prompt
 ;; Cursor does not expose context-window figures over ACP.
 :context-usage-unavailable-p (lambda (_state) t))

(eval-when-compile
  (require 'cl-lib))

;; Register grouped lisp/ subdirectories on load-path so that
;; cross-directory requires (emagent-log from lisp/core/ etc.)
;; work during byte-compilation by Elpaca or other build tools.
;; Uses `byte-compile-current-file' when set (Elpaca compile).
(eval-and-compile
  (when-let ((file (or load-file-name
                       (and (boundp 'byte-compile-current-file)
                            byte-compile-current-file)))
             (lisp (expand-file-name ".." (file-name-directory file))))
    (when (file-directory-p lisp)
      (dolist (dir (directory-files lisp nil "^[^.]"))
        (let ((path (expand-file-name dir lisp)))
          (when (file-directory-p path)
            (add-to-list 'load-path path)))))))

(defun emagent-acp-prefer-emacs-p ()
  "Return non-nil when emagent instructs the agent to prefer Emacs tools."
  emagent-acp-prefer-emacs)

(defun emagent-reset-permissions ()
  "Reset stored emagent permissions via a minibuffer menu.

Choices:
  project: all      — clears fingerprints and allowed tools for the current
                      project directory
  project: session  — clears fingerprints and auto-approve for the current
                      ACP session
  global: all       — clears all globally approved fingerprints."
  (interactive)
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Not in an emagent buffer"))
  (let* ((session-id (emagent-session-id))
         (project-dir (emagent-session-project-directory))
         (choices
          (delq nil
                (list
                 (when project-dir  "project: all")
                 (when session-id   "project: session")
                 "global: all")))
         (choice (completing-read "Reset permissions: " choices nil t)))
    (pcase choice
      ("project: all"
       (unless project-dir (user-error "No project directory for this buffer"))
       (emagent-permissions-reset-project project-dir)
       (message "emagent: cleared project permissions for %s" project-dir))
      ("project: session"
       (unless session-id (user-error "No active session for this buffer"))
       (emagent-permissions-reset-session session-id)
       (message "emagent: cleared session permissions for session %s" session-id))
      ("global: all"
       (emagent-permissions-reset-global)
       (message "emagent: cleared all global permissions")))))

(defun emagent-acp-current-model-id ()
  "Return the ACP session model id for the current buffer, or nil."
  (when-let ((state (emagent-acp--session)))
    (emagent-acp--current-model-id state nil)))

(defun emagent-acp-set-model-transient (model-id on-done)
  "Switch this buffer's ACP session model to MODEL-ID without persisting it.
The buffer model (`emagent-session-model') is left unchanged, so this is a
per-turn override.  ON-DONE is called once the switch resolves (success or
failure) so the caller can proceed to send the prompt."
  (let ((state (emagent-acp--session)))
    (if state
        (emagent-acp--config-option-set-model-id
         :state state
         :session-id (emagent-acp-state-session-id state)
         :model-id model-id
         :persist nil
         :on-success on-done
         :on-failure (lambda (&rest _) (when on-done (funcall on-done))))
      (when on-done (funcall on-done)))))

(defun emagent-set-model ()
  "Set the ACP model for the current emagent session."
  (interactive)
  (let* ((state (emagent-acp--session))
         (session-id (emagent-acp-state-session-id state))
         (choices (emagent-acp--model-choices state nil))
         (labels (mapcar #'car choices))
         (selection (emagent-acp--read-labeled-choice
                     "Set emagent model: "
                     labels))
         (model-id (cdr (assoc-string selection choices))))
    (unless session-id
      (user-error "No active session"))
    (unless choices
      (user-error "No models available"))
    (unless model-id
      (user-error "Unknown model: %s" selection))
    (when-let ((current (emagent-acp--current-model-id state nil)))
      (when (string= model-id current)
        (user-error "Model already %s"
                    (emagent-acp--model-display-name state nil model-id))))
    (emagent-acp--config-option-set-model-id
     :state state
     :session-id session-id
     :model-id model-id)))

(provide 'emagent-acp)
;;; emagent-acp.el ends here
