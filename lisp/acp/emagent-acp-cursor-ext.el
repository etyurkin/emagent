;;; emagent-acp-cursor-ext.el --- Cursor ACP extension request handlers  -*- lexical-binding: t; -*-

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

;; Handlers for Cursor ACP extension methods.  `cursor/create_plan' and
;; `cursor/ask_question' are blocking: the agent waits until the client
;; returns an outcome.  Ignoring them (Method Not Found) leaves the prompt
;; hanging with Thinking cut off at Create Plan.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-protocol)
(require 'emagent-chat-ui)
(require 'emagent-acp-usage)

(defun emagent-acp--cursor-ext-params (request)
  "Return params alist from Cursor extension REQUEST."
  (or (map-elt request 'params) (map-elt request :params)))

(defun emagent-acp--cursor-auto-accept-plan-p (state)
  "Return non-nil when STATE should accept `cursor/create_plan' without prompting."
  (pcase emagent-acp-auto-accept-plans
    ('t t)
    ('nil nil)
    (_
     (or (emagent-acp-state-session-auto-approve state)
         (eq emagent-acp-auto-approve-permissions t)
         noninteractive))))

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

(defun emagent-acp--send-create-plan-outcome (state request-id outcome &optional reason)
  "Reply to `cursor/create_plan' REQUEST-ID for STATE with OUTCOME.
OUTCOME is a string: accepted, rejected, or cancelled.  REASON is optional."
  (emagent-log "cursor/create_plan response: %s%s"
               outcome
               (if reason (format " (%s)" reason) ""))
  (emagent-acp-send-response
   :client (emagent-acp-state-client state)
   :response (emagent-acp-make-cursor-create-plan-response
              :request-id request-id
              :outcome outcome
              :reason reason)))

(cl-defun emagent-acp--on-create-plan (&key state emagent-acp-request)
  "Handle blocking Cursor `cursor/create_plan' for STATE.

Inserts the plan into Thinking, then accepts automatically or prompts with
Accept/Reject buttons.  The agent blocks until this returns an outcome.

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
      (emagent-acp--send-create-plan-outcome state request-id "accepted"))
     (t
      (unless (fboundp 'emagent-acp--prepare-interactive-context)
        (require 'emagent-acp-prompt))
      (emagent-acp--prepare-interactive-context state)
      (emagent-acp--clear-prompt-watchdog state)
      (emagent-tools--buttons-prompt
       "Accept this plan?"
       '(("Accept" . :accept) ("Reject" . :reject))
       buf
       (lambda (choice)
         (when (emagent-acp-state-busy state)
           (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
             (require 'emagent-acp-prompt))
           (emagent-acp--schedule-prompt-watchdog state))
         (emagent-acp--refresh-mode-line state)
         (pcase choice
           (:accept
            (emagent-acp--send-create-plan-outcome state request-id "accepted"))
           (:reject
            (emagent-acp--send-create-plan-outcome
             state request-id "rejected" "User rejected the plan"))
           (_
            (emagent-acp--send-create-plan-outcome
             state request-id "cancelled")))))))))

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

(provide 'emagent-acp-cursor-ext)
;;; emagent-acp-cursor-ext.el ends here
