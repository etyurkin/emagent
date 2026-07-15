;;; emagent-mcp-gateway.el --- MCP gateway module  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; MCP gateway between emagent tools and MCP clients.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-tools)
(require 'emagent-acp-custom)
(defun emagent-mcp-gateway-system-prompt ()
  "Return gateway guidance when extra MCP servers are configured, or nil."
  (when (and emagent-acp-extra-mcp-config-file
             (emagent-mcp-config-file-servers))
    (bound-and-true-p emagent-acp-system-prompt-gateway)))

(provide 'emagent-mcp-gateway)
;;; emagent-mcp-gateway.el ends here
