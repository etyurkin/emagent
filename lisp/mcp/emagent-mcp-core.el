;;; emagent-mcp-core.el --- Shared MCP identity and session state  -*- lexical-binding: t; -*-

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

;; Shared MCP identity: defgroup, port/config customs, server process state,
;; session table, and per-buffer tokens.  Leaf module with no MCP requires.

;;; Code:

(defgroup emagent-mcp nil
  "In-Emacs MCP server for emagent."
  :group 'emagent)

(defcustom emagent-mcp-port 8771
  "TCP port for the in-Emacs MCP server on 127.0.0.1.

A fixed port keeps the agent configuration (e.g. ~/.cursor/mcp.json) stable
across Emacs restarts.  Set it to 0 to let the OS assign an ephemeral port
instead; emagent then writes whatever port it gets into the agent config."
  :type 'integer
  :group 'emagent-mcp)

(defcustom emagent-mcp-cursor-config-file
  (expand-file-name "~/.cursor/mcp.json")
  "Path to the global cursor-agent MCP config emagent manages for Cursor."
  :type 'file
  :group 'emagent-mcp)

(defconst emagent-mcp-server-name "emagent"
  "Name advertised for the emagent MCP server.")

(defconst emagent-mcp-protocol-version "2025-06-18"
  "MCP protocol version emagent speaks when a client omits one.")

(defvar emagent-mcp--server nil
  "The singleton MCP server network process, or nil.")

(defvar emagent-mcp--port nil
  "Actual port the MCP server is listening on, or nil.")

(defvar emagent-mcp--sessions (make-hash-table :test 'equal)
  "Map session token to plist (:root :cwd :buffer :prefer-emacs).")

(defvar-local emagent-mcp--token nil
  "Per-buffer MCP session token.")

;;;; Tokens and per-buffer identity

(defun emagent-mcp-make-token ()
  "Return a fresh opaque session token."
  (let ((seed (format "%s-%s-%s-%s"
                      (random most-positive-fixnum)
                      (emacs-pid)
                      (float-time)
                      (recent-keys))))
    (substring (md5 seed) 0 24)))

(defun emagent-mcp-buffer-token ()
  "Return this buffer's MCP session token, creating one if needed."
  (or emagent-mcp--token
      (setq emagent-mcp--token (emagent-mcp-make-token))))

(provide 'emagent-mcp-core)
;;; emagent-mcp-core.el ends here
