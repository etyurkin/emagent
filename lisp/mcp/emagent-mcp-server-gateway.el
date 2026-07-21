;;; emagent-mcp-server-gateway.el --- MCP gateway config forwarding  -*- lexical-binding: t; -*-

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

;; Extra mcpServers config, ACP session servers, and gateway prompt.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-mcp-server-cursor)

;;;; External MCP server forwarding

(defcustom emagent-acp-extra-mcp-config-file "~/.claude.json"
  "JSON file whose top-level `mcpServers' block is forwarded to ACP agents.

Emagent reads the `mcpServers' object from this file and advertises those
servers, alongside the in-Emacs emagent server, to agents that support http MCP
over ACP (e.g. Claude).  This reuses existing Claude MCP server entries without
re-declaring them for emagent.

Only agents wired through ACP `mcpServers' are affected; Cursor discovers MCP
servers from its own ~/.cursor/mcp.json and ignores this option.
Set to nil to forward only the emagent server."
  :type '(choice (const :tag "None" nil) (file :tag "JSON config"))
  :group 'emagent)

(defun emagent-mcp--kv-array (object)
  "Convert OBJECT (alist of KEY . VALUE) to an ACP [{name,value}] vector."
  (vconcat
   (mapcar (lambda (pair)
             `((name . ,(let ((k (car pair)))
                          (if (symbolp k) (symbol-name k) k)))
               (value . ,(cdr pair))))
           object)))

(defun emagent-mcp--convert-gateway-entry (name cfg)
  "Convert config-file MCP entry NAME/CFG to an ACP mcpServer alist, or nil."
  (let ((type (or (map-elt cfg 'type)
                  (and (map-elt cfg 'url) "http"))))
    (pcase type
      ((or "http" "sse")
       (when (map-elt cfg 'url)
         `((type . ,type)
           (name . ,name)
           (url . ,(map-elt cfg 'url))
           (headers . ,(emagent-mcp--kv-array (map-elt cfg 'headers))))))
      (_
       (when (map-elt cfg 'command)
         `((type . "stdio")
           (name . ,name)
           (command . ,(map-elt cfg 'command))
           (args . ,(vconcat (map-elt cfg 'args)))
           (env . ,(emagent-mcp--kv-array (map-elt cfg 'env)))))))))

(defun emagent-mcp-config-file-servers ()
  "Return ACP mcpServer specs from `emagent-acp-extra-mcp-config-file', or nil."
  (when-let* ((file emagent-acp-extra-mcp-config-file)
              (path (expand-file-name file))
              ((file-readable-p path)))
    (condition-case err
        (let* ((data (with-temp-buffer
                       (insert-file-contents path)
                       (json-parse-buffer :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object :false)))
               (servers (map-elt data 'mcpServers)))
          (delq nil
                (mapcar (lambda (pair)
                          (let ((name (symbol-name (car pair))))
                            (unless (equal name emagent-mcp-server-name)
                              (emagent-mcp--convert-gateway-entry name (cdr pair)))))
                        servers)))
      (error
       (require 'emagent-log)
       (emagent-log "could not read MCP servers from %s: %s"
                    path (error-message-string err))
       nil))))

(defun emagent-mcp-session-servers (mcp-http chat-buffer)
  "Return the mcpServers vector to advertise, or nil.

MCP-HTTP is non-nil when the agent advertised http MCP capability.
CHAT-BUFFER is the emagent chat buffer (for the per-buffer token)."
  (when mcp-http
    (with-current-buffer chat-buffer
      (let* ((url (emagent-mcp-session-url (emagent-mcp-buffer-token)))
             (emagent-server `((type . "http")
                               (name . ,emagent-mcp-server-name)
                               (url . ,url)
                               (headers . [])))
             (extra (emagent-mcp-config-file-servers)))
        (vconcat (list emagent-server) extra)))))

(defun emagent-mcp-gateway-system-prompt ()
  "Return MCP guidance when external servers are configured, or nil."
  (when (or (emagent-mcp-config-file-servers)
            (emagent-mcp--cursor-extra-servers-p))
    (bound-and-true-p emagent-acp-system-prompt-gateway)))

(provide 'emagent-mcp-server-gateway)
;;; emagent-mcp-server-gateway.el ends here
