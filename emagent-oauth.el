;;; emagent-oauth.el --- OAuth localhost callback capture for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; MCP gateways such as ebay-ai-gateway start OAuth with a browser URL whose
;; redirect_uri points at http://localhost:PORT/callback.  Nothing listens there
;; unless emagent does.  This module watches agent output for authorize URLs,
;; starts a short-lived HTTP listener on PORT, and submits the callback URL to
;; the agent when the browser redirects.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'emagent-log)

(declare-function emagent-chat--begin-response "emagent-chat")
(declare-function emagent-acp-send-prompt "emagent-acp")

(defvar emagent-oauth--servers (make-hash-table :test 'equal)
  "Hash table mapping localhost PORT to an OAuth listener plist.")

(defvar emagent-oauth--pending (make-hash-table :test 'eq)
  "Hash table mapping chat buffers to a pending OAuth callback URL.")

(defvar emagent-oauth--delivered (make-hash-table :test 'eq)
  "Hash table mapping chat buffers to lists of delivered callback URLs.")

(defvar-local emagent-oauth--seen-authorize-ports nil
  "Localhost ports already started as OAuth listeners for the current emagent buffer.")

(defvar emagent-oauth--http-ok
  "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n<!DOCTYPE html><html><head><title>Emagent</title></head><body><p>Authentication complete. You can close this tab and return to Emacs.</p></body></html>"
  "Minimal success page returned to the browser after OAuth.")

(defgroup emagent-oauth nil
  "OAuth callback capture for emagent MCP gateways."
  :group 'emagent)

(defcustom emagent-oauth-auto-browse-url t
  "When non-nil, open the OAuth authorize URL in a browser automatically.

Emagent still listens on the redirect localhost port; the browser step only
starts the corporate login flow without requiring you to click a link in Emacs."
  :type 'boolean
  :group 'emagent-oauth)

(defcustom emagent-oauth-timeout 600
  "Seconds to keep an OAuth localhost listener open before giving up."
  :type 'integer
  :group 'emagent-oauth)

(defun emagent-oauth--redirect-port (authorize-url)
  "Return the localhost redirect port encoded in AUTHORIZE-URL, or nil."
  (or (and (string-match
            "redirect_uri=\\(?:http%3A%2F%2F\\|http://\\)localhost%3A\\([0-9][0-9]*\\)%2Fcallback"
            authorize-url)
           (string-to-number (match-string 1 authorize-url)))
      (and (string-match "redirect_uri=http://localhost:\\([0-9][0-9]*\\)/callback"
                         authorize-url)
           (string-to-number (match-string 1 authorize-url)))))

(defun emagent-oauth--find-authorize (text)
  "Return plist (:authorize-url URL :port PORT) when TEXT contains OAuth start."
  (when-let* ((url (or (and (string-match
                             "\\[\\[\\(https://[^]]*oauth/authorize[^]]*\\)\\]"
                             text)
                            (match-string 1 text))
                       (and (string-match
                             "\\(https://[^ \t\n<>\"']*oauth/authorize[^ \t\n<>\"']*\\)"
                             text)
                            (match-string 1 text))))
              (port (emagent-oauth--redirect-port url)))
    (list :authorize-url url :port port)))

(defun emagent-oauth--server-entry (port)
  (gethash port emagent-oauth--servers))

(defun emagent-oauth--put-server-entry (port entry)
  (puthash port entry emagent-oauth--servers))

(defun emagent-oauth--remove-server-entry (port)
  (remhash port emagent-oauth--servers))

(defun emagent-oauth--stop-listener (port)
  "Stop the OAuth listener on PORT, if any."
  (when-let ((entry (emagent-oauth--server-entry port)))
    (when-let ((timer (plist-get entry :timer)))
      (cancel-timer timer))
    (when-let ((proc (plist-get entry :process)))
      (when (processp proc)
        (ignore-errors (delete-process proc))))
    (emagent-oauth--remove-server-entry port)))

(defun emagent-oauth--stop-buffer-listeners (buffer)
  "Stop every OAuth listener owned by BUFFER."
  (maphash (lambda (port entry)
             (when (eq (plist-get entry :buffer) buffer)
               (emagent-oauth--stop-listener port)))
           emagent-oauth--servers))

(defun emagent-oauth--local-port (proc)
  "Return the listening port for connection process PROC via its local address."
  (let ((local (process-contact proc :local)))
    (cond ((vectorp local) (aref local 1))
          ((consp local) (cadr local)))))

(defun emagent-oauth--connection-filter (proc string)
  "Handle an HTTP request on OAuth connection PROC."
  (when (string-match "^GET \\(/callback\\?[^ ]*\\) HTTP" string)
    (let* ((path-query (match-string 1 string))
           (port (emagent-oauth--local-port proc))
           (entry (and port (emagent-oauth--server-entry port)))
           (buffer (and entry (plist-get entry :buffer)))
           (callback-url (format "http://localhost:%s%s" port path-query)))
      (when entry
        (process-send-string proc emagent-oauth--http-ok)
        (delete-process proc)
        (emagent-oauth--stop-listener port)
        (emagent-oauth--queue-callback buffer callback-url)))))

(defun emagent-oauth--server-sentinel (proc _event)
  (when (memq (process-status proc) '(closed failed))
    (let ((port (process-get proc 'emagent-oauth-port)))
      (when (and port (eq proc (plist-get (emagent-oauth--server-entry port) :process)))
        (emagent-oauth--remove-server-entry port)))))

(defun emagent-oauth--start-listener (buffer port)
  "Listen on localhost PORT for an OAuth callback for BUFFER."
  (unless (emagent-oauth--server-entry port)
    (condition-case err
        (let* ((name (format "emagent-oauth-%d" port))
               (proc (make-network-process
                      :name name
                      :service (format "%d" port)
                      :family 'ipv4
                      :server t
                      :noquery t
                      :sentinel #'emagent-oauth--server-sentinel
                      :filter #'emagent-oauth--connection-filter
                      :coding 'no-conversion))
               (timer (run-at-time emagent-oauth-timeout nil
                                   #'emagent-oauth--stop-listener port)))
          (process-put proc 'emagent-oauth-port port)
          (process-put proc 'emagent-oauth-buffer buffer)
          (emagent-oauth--put-server-entry
           port (list :process proc :buffer buffer :timer timer :port port))
          (emagent-log "waiting for OAuth callback on localhost:%d…" port))
      (error
       (emagent-log "could not listen on localhost:%d (%s)"
                   port (error-message-string err))))))

(defun emagent-oauth--callback-delivered-p (buffer callback-url)
  (memq callback-url (gethash buffer emagent-oauth--delivered)))

(defun emagent-oauth--mark-callback-delivered (buffer callback-url)
  (let ((list (gethash buffer emagent-oauth--delivered)))
    (unless (memq callback-url list)
      (puthash buffer (cons callback-url list) emagent-oauth--delivered))))

(defun emagent-oauth--queue-callback (buffer callback-url)
  "Remember CALLBACK-URL for BUFFER and try to deliver it to the agent."
  (when (and (buffer-live-p buffer)
             (not (emagent-oauth--callback-delivered-p buffer callback-url)))
    (puthash buffer callback-url emagent-oauth--pending)
    (emagent-oauth-maybe-deliver-pending buffer)))

(defun emagent-oauth-maybe-deliver-pending (buffer)
  "Submit a queued OAuth callback URL when BUFFER's session is idle."
  (when-let ((callback-url (and (buffer-live-p buffer)
                                (gethash buffer emagent-oauth--pending))))
    (with-current-buffer buffer
      (let ((state (and (boundp 'emagent-acp--session) emagent-acp--session)))
        (when (and state
                   (map-elt state :ready)
                   (not (map-elt state :busy)))
          (remhash buffer emagent-oauth--pending)
          (emagent-oauth--mark-callback-delivered buffer callback-url)
          (emagent-oauth--submit-callback callback-url))))))

(defun emagent-oauth--submit-callback (callback-url)
  "Insert CALLBACK-URL into the chat buffer and send it to the agent."
  (let* ((prompt (format "Complete authentication with this callback URL:\n%s"
                         callback-url))
         (inhibit-read-only t))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert prompt "\n")
    (emagent-chat--begin-response (point))
    (emagent-log "OAuth callback received; completing authentication…")
    (emagent-acp-send-prompt prompt)))

(defun emagent-oauth-watch-assistant-text (buffer text)
  "When assistant TEXT contains an OAuth authorize URL, start listening."
  (when (and (buffer-live-p buffer) (stringp text) (not (string-empty-p text)))
    (when-let* ((info (emagent-oauth--find-authorize text))
                (url (plist-get info :authorize-url))
                (port (plist-get info :port)))
      (with-current-buffer buffer
        (unless (member port emagent-oauth--seen-authorize-ports)
          (push port emagent-oauth--seen-authorize-ports)
          (when emagent-oauth-auto-browse-url
            (browse-url url))
          (emagent-log "OAuth started — complete login in your browser")))
      (emagent-oauth--start-listener buffer port))))

(defun emagent-oauth-shutdown-buffer (buffer)
  "Stop OAuth listeners and pending state for BUFFER."
  (when buffer
    (emagent-oauth--stop-buffer-listeners buffer)
    (remhash buffer emagent-oauth--pending)
    (remhash buffer emagent-oauth--delivered)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq emagent-oauth--seen-authorize-ports nil)))))

(provide 'emagent-oauth)

;;; emagent-oauth.el ends here
