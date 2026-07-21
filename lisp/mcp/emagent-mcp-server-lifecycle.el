;;; emagent-mcp-server-lifecycle.el --- MCP server lifecycle  -*- lexical-binding: t; -*-

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

;; Start/stop the MCP listener and register/deregister sessions.

;;; Code:

(require 'cl-lib)
(require 'emagent-mcp-server-http)

;;;; Lifecycle

(defun emagent-mcp-ensure-server ()
  "Start the MCP server if needed and return its port."
  (unless (process-live-p emagent-mcp--server)
    (let ((proc (make-network-process
                 :name "emagent-mcp"
                 :server t
                 :host "127.0.0.1"
                 :service (if (and emagent-mcp-port (> emagent-mcp-port 0))
                              emagent-mcp-port
                            t)
                 :family 'ipv4
                 :coding 'binary
                 :filter #'emagent-mcp--filter
                 :sentinel #'emagent-mcp--sentinel)))
      (setq emagent-mcp--server proc
            emagent-mcp--port (process-contact proc :service))))
  emagent-mcp--port)

(defun emagent-mcp-maybe-shutdown ()
  "Stop the MCP server when no emagent sessions remain registered."
  (when (and emagent-mcp--server
             (zerop (hash-table-count emagent-mcp--sessions)))
    (ignore-errors (delete-process emagent-mcp--server))
    (setq emagent-mcp--server nil
          emagent-mcp--port nil)))

(cl-defun emagent-mcp-register-session (&key token cwd buffer prefer-emacs acp)
  "Register session TOKEN with project CWD, owning BUFFER, and EMACS-ONLY flag.

When ACP is non-nil, the session is driven by an emagent ACP chat; MCP tool
confirmation is handled via ACP `session/request_permission' instead.

Starts the server if needed and returns the port.

Arguments: PREFER-EMACS."
  (emagent-mcp-ensure-server)
  (puthash token
           (list :root (and cwd (expand-file-name cwd))
                 :cwd cwd
                 :buffer buffer
                 :prefer-emacs prefer-emacs
                 :acp acp)
           emagent-mcp--sessions)
  emagent-mcp--port)

(defun emagent-mcp--acp-session-p (session)
  "Return non-nil when SESSION is owned by an emagent ACP chat buffer."
  (and session (plist-get session :acp)))

(defun emagent-mcp-deregister-session (token)
  "Deregister session TOKEN and stop the server if it was the last one."
  (when token
    (remhash token emagent-mcp--sessions))
  (emagent-mcp-maybe-shutdown))

(defun emagent-mcp-session-url (token)
  "Return the MCP endpoint URL for session TOKEN (and begin the server)."
  (format "http://127.0.0.1:%d/mcp/%s" (emagent-mcp-ensure-server) token))

(provide 'emagent-mcp-server-lifecycle)
;;; emagent-mcp-server-lifecycle.el ends here
