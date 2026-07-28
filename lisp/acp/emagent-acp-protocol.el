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
;; Self-contained implementation of the Agent Communication Protocol (ACP)
;; for emagent.  Symbols use the emagent-acp- prefix so this file can
;; coexist with xenodium/acp.el in the same Emacs session.
;;
;; Covers wire logging, JSON helpers, client lifecycle, stdio framing /
;; routing, and outbound request builders.
;;
;; See https://agentclientprotocol.com for the ACP specification.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'json)
(require 'emagent-acp-custom)

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
