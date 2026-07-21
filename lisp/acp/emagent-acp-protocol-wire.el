;;; emagent-acp-protocol-wire.el --- ACP wire senders and routing for emagent  -*- lexical-binding: t; -*-

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

;; Default request/response/notification senders and incoming message routing.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'map)
(require 'emagent-acp-protocol-log)
(require 'emagent-acp-protocol-json)
(require 'emagent-acp-protocol-client)

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

(provide 'emagent-acp-protocol-wire)

;;; emagent-acp-protocol-wire.el ends here
