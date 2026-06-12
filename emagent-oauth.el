;;; emagent-oauth.el --- OAuth support for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; MCP gateways initiate OAuth by showing a clickable authorize URL in the
;; emagent chat buffer.  The agent handles the browser and callback itself.
;; This module is a thin stub satisfying emagent-acp.el call sites.

;;; Code:

(defun emagent-oauth-watch-assistant-text (_buffer _text)
  "No-op: the agent opens the browser and handles the OAuth callback.")

(defun emagent-oauth-maybe-deliver-pending (_buffer)
  "No-op: the agent handles OAuth callbacks directly.")

(defun emagent-oauth-shutdown-buffer (_buffer)
  "No-op: no OAuth state to clean up.")

(provide 'emagent-oauth)

;;; emagent-oauth.el ends here
