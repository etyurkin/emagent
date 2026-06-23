;;; emagent-mcp-gateway.el --- MCP gateway module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
