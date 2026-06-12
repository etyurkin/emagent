;;; emagent-oauth.el --- OAuth browser launcher for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; When an MCP gateway initiates OAuth, this module detects the authorize URL
;; in the agent's response and opens it in a browser automatically.  The
;; agent's own OAuth server handles the callback and token exchange.

;;; Code:

(require 'emagent-log)

(defvar-local emagent-oauth--seen-authorize-urls nil
  "Authorize URLs already opened for the current emagent buffer.")

(defgroup emagent-oauth nil
  "OAuth browser launcher for emagent MCP gateways."
  :group 'emagent)

(defcustom emagent-oauth-auto-browse-url t
  "When non-nil, open OAuth authorize URLs in a browser automatically."
  :type 'boolean
  :group 'emagent-oauth)

(defun emagent-oauth--find-authorize-url (text)
  "Return the OAuth authorize URL in TEXT, or nil."
  (or (and (string-match
            "\\[\\[\\(https://[^]]*oauth/authorize[^]]*\\)\\]"
            text)
           (match-string 1 text))
      (and (string-match
            "\\(https://[^ \t\n<>\"'*]*oauth/authorize[^ \t\n<>\"'*]*\\)"
            text)
           (match-string 1 text))))

(defun emagent-oauth-watch-assistant-text (buffer text)
  "When TEXT contains an OAuth authorize URL, open it in a browser."
  (when (and emagent-oauth-auto-browse-url
             (buffer-live-p buffer)
             (stringp text)
             (not (string-empty-p text)))
    (when-let ((url (emagent-oauth--find-authorize-url text)))
      (with-current-buffer buffer
        (unless (member url emagent-oauth--seen-authorize-urls)
          (push url emagent-oauth--seen-authorize-urls)
          (browse-url url)
          (emagent-log "OAuth: opened browser for authorization"))))))

(defun emagent-oauth-maybe-deliver-pending (_buffer)
  "No-op: the agent handles OAuth callbacks directly.")

(defun emagent-oauth-shutdown-buffer (buffer)
  "Clear OAuth state for BUFFER."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (setq emagent-oauth--seen-authorize-urls nil))))

(provide 'emagent-oauth)

;;; emagent-oauth.el ends here
