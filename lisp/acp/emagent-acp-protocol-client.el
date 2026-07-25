;;; emagent-acp-protocol-client.el --- ACP client lifecycle for emagent  -*- lexical-binding: t; -*-

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

;; Client construction, process start/shutdown, subscriptions, and send API.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-custom)
(require 'emagent-acp-protocol-log)
(require 'emagent-acp-protocol-json)

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

(provide 'emagent-acp-protocol-client)

(require 'emagent-acp-protocol-wire)

;;; emagent-acp-protocol-client.el ends here
