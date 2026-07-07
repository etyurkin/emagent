;;; emagent-mcp-gateway.el --- MCP gateway module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

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
