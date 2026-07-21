;;; emagent-mcp-server-http.el --- MCP HTTP/JSON-RPC listener  -*- lexical-binding: t; -*-

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

;; JSON-RPC helpers, HTTP request parsing, process filter, and drain.

;;; Code:

(require 'emagent-log)
(require 'emagent-mcp-registry)

(defun emagent-mcp--json-encode (object)
  "Serialize OBJECT to a JSON string."
  (json-serialize object :null-object :null :false-object :false))

(defun emagent-mcp--rpc-result (id result)
  "Return a JSON-RPC success response string for ID with RESULT."
  (emagent-mcp--json-encode `((jsonrpc . "2.0") (id . ,id) (result . ,result))))

(defun emagent-mcp--rpc-error (id code message)
  "Return a JSON-RPC error response string for ID with CODE and MESSAGE.
A nil ID is serialized as JSON null (not an empty object)."
  (emagent-mcp--json-encode
   `((jsonrpc . "2.0") (id . ,(or id :null))
     (error . ((code . ,code) (message . ,message))))))

(defun emagent-mcp--tool-content (text is-error)
  "Return a tools/call result alist wrapping TEXT, flagged IS-ERROR."
  `((content . ,(vector `((type . "text") (text . ,(or text "")))))
    (isError . ,(if is-error t :false))))

(defun emagent-mcp--initialize-result (params)
  "Return the initialize result, echoing PARAMS protocolVersion when present."
  (let ((version (and (hash-table-p params)
                      (gethash "protocolVersion" params))))
    `((protocolVersion . ,(or version emagent-mcp-protocol-version))
      (capabilities . ((tools . ((listChanged . :false)))))
      (serverInfo . ((name . ,emagent-mcp-server-name)
                     (version . "1.0.2"))))))

;;;; HTTP layer

(defun emagent-mcp--path-token (path)
  "Extract the session token from request PATH like /mcp/TOKEN."
  (when (and path (string-match "/mcp/\\([^/?#]+\\)" path))
    (match-string 1 path)))

(defun emagent-mcp--parse-headers (lines)
  "Parse HTTP header LINES into a lowercased-key alist."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
                    (cons (downcase (match-string 1 line))
                          (match-string 2 line))))
                lines)))

(defun emagent-mcp--reason-phrase (status)
  "Return the HTTP reason phrase for STATUS."
  (pcase status
    (200 "OK")
    (202 "Accepted")
    (204 "No Content")
    (400 "Bad Request")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (_ "OK")))

(defun emagent-mcp--respond (proc status headers body)
  "Send an HTTP response on PROC with STATUS, HEADERS alist, and BODY string."
  (when (process-live-p proc)
    (let* ((body-bytes (encode-coding-string (or body "") 'utf-8))
           (head (concat
                  (format "HTTP/1.1 %d %s\r\n" status (emagent-mcp--reason-phrase status))
                  (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h)))
                             headers "")
                  (format "Content-Length: %d\r\n" (length body-bytes))
                  "Connection: keep-alive\r\n"
                  "\r\n")))
      (process-send-string proc (encode-coding-string head 'utf-8))
      (when (> (length body-bytes) 0)
        (process-send-string proc body-bytes)))))

(defun emagent-mcp--respond-json (proc json-string)
  "Send JSON-STRING as an application/json HTTP 200 response on PROC."
  (emagent-mcp--respond proc 200 '(("Content-Type" . "application/json")) json-string))

(defun emagent-mcp--defer-tools-call (proc id params token)
  "Handle a tools/call for PROC out of the process filter, then respond.

A tool call may prompt the user for confirmation, and Emacs cannot reliably
read keyboard input from a process filter (keystrokes are dropped).  Running
the call from an idle timer moves the prompt into the command loop where input
works, and keeps Emacs responsive while the user decides.

The HTTP response is sent via a RESPOND callback that may be called either
from within the idle timer (synchronous tools) or from a process sentinel
after the subprocess exits (async tools such as run_shell_command), so Emacs
stays fully responsive during long-running shell commands.

Arguments: ID, PARAMS, TOKEN."
  (run-with-idle-timer
   0 nil
   (lambda ()
     (let* ((name (and (hash-table-p params) (gethash "name" params)))
            (args (or (and (hash-table-p params) (gethash "arguments" params))
                      (make-hash-table :test 'equal)))
            (session (and token (gethash token emagent-mcp--sessions)))
            (respond (lambda (result is-error)
                       (emagent-mcp--respond-json
                        proc
                        (emagent-mcp--rpc-result
                         id (emagent-mcp--tool-content result is-error))))))
       (cond
        ((null token)
         (funcall respond "No emagent session token in request path" t))
        ((null session)
         (funcall respond "Unknown or expired emagent session" t))
        (t
         (emagent-mcp--run-tool-async name args session respond)))))))

(defun emagent-mcp--dispatch (proc token message)
  "Dispatch a parsed JSON-RPC MESSAGE (hash-table) from PROC with TOKEN."
  (let ((id (gethash "id" message))
        (method (gethash "method" message))
        (params (gethash "params" message)))
    ;; Fail closed: a throwing synchronous handler must still produce a
    ;; response, otherwise the client blocks until its own timeout.  (The
    ;; deferred tools/call path has its own error handling in the idle timer.)
    (condition-case err
        (pcase method
          ("initialize"
           (emagent-mcp--respond-json
            proc (emagent-mcp--rpc-result id (emagent-mcp--initialize-result params))))
          ("notifications/initialized"
           (emagent-mcp--respond proc 202 nil ""))
          ("ping"
           (emagent-mcp--respond-json
            proc (emagent-mcp--rpc-result id (make-hash-table :test 'equal))))
          ("tools/list"
           (emagent-mcp--respond-json
            proc (emagent-mcp--rpc-result id (emagent-mcp--tools-list-payload))))
          ("tools/call"
           (emagent-mcp--defer-tools-call proc id params token))
          ((guard (null id))
           ;; Any other notification: acknowledge without a body.
           (emagent-mcp--respond proc 202 nil ""))
          (_
           (emagent-mcp--respond-json
            proc (emagent-mcp--rpc-error id -32601 (format "Method not found: %s" method)))))
      (error
       (emagent-log "mcp dispatch error (method %s): %s"
                    method (error-message-string err))
       (if id
           (emagent-mcp--respond-json
            proc (emagent-mcp--rpc-error
                  id -32603 (format "Internal error: %s" (error-message-string err))))
         (emagent-mcp--respond proc 202 nil ""))))))

(defun emagent-mcp--handle-request (proc request-line _headers body)
  "Handle one parsed HTTP request on PROC.

Arguments: REQUEST-LINE, BODY."
  (let* ((parts (split-string request-line " "))
         (http-method (nth 0 parts))
         (path (nth 1 parts))
         (token (emagent-mcp--path-token path)))
    (pcase http-method
      ("OPTIONS" (emagent-mcp--respond proc 204 nil ""))
      ("POST"
       (let ((message (condition-case nil
                          (json-parse-string (decode-coding-string body 'utf-8)
                                             :object-type 'hash-table
                                             :array-type 'list
                                             :null-object :null
                                             :false-object :false)
                        (error nil))))
         (cond
          ((null message)
           (emagent-mcp--respond-json proc (emagent-mcp--rpc-error :null -32700 "Parse error")))
          ((hash-table-p message)
           (emagent-mcp--dispatch proc token message))
          (t
           ;; JSON-RPC batch (list of messages); handle each in order.
           (dolist (item message)
             (when (hash-table-p item)
               (emagent-mcp--dispatch proc token item)))))))
      (_ (emagent-mcp--respond proc 405 nil "")))))

(defcustom emagent-mcp-drain-yield 0.01
  "Seconds to wait before draining the next buffered MCP HTTP request.

The process filter only accumulates bytes; parsing and dispatching a
complete HTTP request runs from a `run-with-timer' tick so a burst of
pipelined tool calls cannot starve Emacs redisplay and other timers.
Handling one request per tick mirrors `emagent-acp-message-drain-yield'
for the ACP wire.  The first buffered request is still drained
immediately (delay 0)."
  :type 'number
  :group 'emagent)

(defun emagent-mcp--drain-one (proc)
  "Parse and handle one complete HTTP request buffered on PROC.
Return non-nil when a request was handled."
  (let* ((buffer (or (process-get proc 'emagent-mcp-data) ""))
         (sep (string-search "\r\n\r\n" buffer)))
    (when sep
      (let* ((head (substring buffer 0 sep))
             (rest (substring buffer (+ sep 4)))
             (lines (split-string head "\r\n"))
             (request-line (car lines))
             (headers (emagent-mcp--parse-headers (cdr lines)))
             (content-length (string-to-number
                              (or (cdr (assoc "content-length" headers)) "0"))))
        (when (>= (length rest) content-length)
          (let ((req-body (substring rest 0 content-length)))
            (process-put proc 'emagent-mcp-data (substring rest content-length))
            (emagent-mcp--handle-request proc request-line headers req-body)
            t))))))

(defun emagent-mcp--schedule-drain (proc delay)
  "Schedule a drain tick for PROC after DELAY seconds.
A no-op when PROC already has a drain tick pending."
  (unless (process-get proc 'emagent-mcp-drain-timer)
    (process-put proc 'emagent-mcp-drain-timer
                 (run-with-timer (max 0 delay) nil #'emagent-mcp--drain proc))))

(defun emagent-mcp--cancel-drain (proc)
  "Cancel PROC's pending drain timer, if any."
  (when-let ((timer (process-get proc 'emagent-mcp-drain-timer)))
    (cancel-timer timer)
    (process-put proc 'emagent-mcp-drain-timer nil)))

(defun emagent-mcp--drain (proc)
  "Handle one buffered HTTP request on PROC, then yield before the next.

Runs from a timer instead of the process filter so a burst of pipelined
requests cannot monopolize the command loop; mirrors the ACP wire's
timer-yield drain with a batch size of one HTTP request per tick."
  (process-put proc 'emagent-mcp-drain-timer nil)
  (when (and (process-live-p proc)
             (emagent-mcp--drain-one proc)
             (process-live-p proc)
             (string-search "\r\n\r\n" (or (process-get proc 'emagent-mcp-data) "")))
    (emagent-mcp--schedule-drain proc emagent-mcp-drain-yield)))

(defun emagent-mcp--filter (proc data)
  "Process filter: accumulate DATA on PROC and schedule a drain tick.

Parsing and dispatch happen later from `emagent-mcp--drain', not here, so
the filter itself never blocks on JSON-RPC handling."
  (process-put proc 'emagent-mcp-data
               (concat (or (process-get proc 'emagent-mcp-data) "") data))
  (emagent-mcp--schedule-drain proc 0))

(defun emagent-mcp--sentinel (proc _event)
  "Clean up PROC connection state when it closes."
  (unless (process-live-p proc)
    (emagent-mcp--cancel-drain proc)
    (process-put proc 'emagent-mcp-data nil)))

(provide 'emagent-mcp-server-http)
;;; emagent-mcp-server-http.el ends here
