;;; emagent-cursor-command.el --- Cursor CLI command lookup  -*- lexical-binding: t; -*-

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

;; Resolves the Cursor ACP CLI command name, kept out of `emagent-cursor'
;; (which requires `emagent-chat') so `emagent-chat-mcp' can look up and
;; validate the Cursor command without a load cycle.

;;; Code:

(defcustom emagent-cursor-acp-command
  '("cursor-agent" "acp")
  "Command and parameters for the Cursor ACP agent.

Uses Cursor's own ACP server (the registry `cursor' entry, `cursor-agent
acp'), which speaks ACP natively.  Cursor discovers the in-Emacs MCP server
from ~/.cursor/mcp.json (see `emagent-mcp-ensure-cursor-config')."
  :type '(repeat string)
  :group 'emagent-cursor)

(defconst emagent-cursor-install-hint
  "Install Cursor's CLI (provides `cursor-agent acp'): https://cursor.com/cli")

(defun emagent-cursor-command ()
  "Return the Cursor ACP command name."
  (car emagent-cursor-acp-command))

(defun emagent-cursor-check-command ()
  "Signal a clear error when the Cursor agent is missing."
  (unless (executable-find (emagent-cursor-command))
    (error "Cursor ACP agent %s not found on PATH.\n%s"
           (emagent-cursor-command) emagent-cursor-install-hint)))

(provide 'emagent-cursor-command)
;;; emagent-cursor-command.el ends here