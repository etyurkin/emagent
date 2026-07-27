;;; emagent-mcp.el --- In-Emacs MCP server for emagent tools -*- lexical-binding: t; -*-

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
;; Exposes the `emagent-tool-*' functions to ACP agents as Model Context
;; Protocol (MCP) tools, served over a localhost HTTP listener hosted inside
;; the running Emacs.  Tool calls therefore execute in the live Emacs process
;; itself -- no `emacsclient', no subprocess, no second Emacs.
;;
;; The server is a refcounted singleton: started lazily when the first emagent
;; session connects and torn down only when the last session is gone.  Each
;; session registers a per-session token mapped to its project root; every
;; tool call carries that token in the request path
;; (http://127.0.0.1:PORT/mcp/TOKEN), so a shared, provider-agnostic server
;; can route each call to the right session and enforce its filesystem root.
;;
;; Two ways in, one server:
;;   - Claude: the token url is passed via `session/new' mcpServers (http).
;;   - Cursor: the cursor-agent CLI reads ~/.cursor/mcp.json, whose url uses
;;     ${env:EMAGENT_SESSION_TOKEN}; emagent sets that env var per session.
;;
;; Implementation is split across require-DAG leaves:
;;   `emagent-mcp-core'     — identity, port, sessions, tokens
;;   `emagent-mcp-registry' — tool table and dispatch
;;   `emagent-mcp-server'   — HTTP/JSON-RPC facade (http/lifecycle/cursor/gateway)
;;   `emagent-mcp-session'  — session feature shim
;;   `emagent-mcp-gateway'  — gateway feature shim

;;; Code:

(require 'emagent-mcp-core)
(require 'emagent-mcp-registry)
(require 'emagent-mcp-server)
(require 'emagent-mcp-session)
(require 'emagent-mcp-gateway)

(provide 'emagent-mcp)
;;; emagent-mcp.el ends here

