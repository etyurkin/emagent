;;; emagent-oauth.el --- OAuth localhost callback capture for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; MCP gateways such as ebay-ai-gateway start OAuth with a browser URL whose
;; redirect_uri points at http://localhost:PORT/callback.  This module watches
;; agent output for authorize URLs, opens a browser, starts a short-lived HTTP
;; listener on PORT, and submits the callback URL to the agent automatically.
;;
;; When the agent handles the callback itself (its own localhost server), the
;; bind may fail; emagent retries up to 5 times and gives up silently.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'emagent-log)

(declare-function emagent-chat--begin-response "emagent-chat")
(declare-function emagent-acp-send-prompt "emagent-acp")

(defvar emagent-oauth--servers (make-hash-table :test 'equal)
  "Map localhost PORT to an OAuth listener plist.")

(defvar emagent-oauth--authorize-urls (make-hash-table :test 'equal)
  "Map localhost PORT to the OAuth authorize URL used to start the listener.
Used to re-inject a missing state parameter when the OAuth server omits it.")

(defvar emagent-oauth--pending (make-hash-table :test 'eq)
  "Map chat buffer to a pending OAuth callback URL.")

(defvar emagent-oauth--delivered (make-hash-table :test 'eq)
  "Map chat buffer to lists of delivered callback URLs.")

(defvar-local emagent-oauth--seen-authorize-ports nil
  "Localhost ports already started as OAuth listeners for this buffer.")

(defvar emagent-oauth--http-ok
  "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n<!DOCTYPE html><html><head><title>Emagent</title></head><body><p>Authentication complete. You can close this tab and return to Emacs.</p></body></html>"
  "Success page returned to the browser after OAuth.")

(defgroup emagent-oauth nil
  "OAuth callback capture for emagent MCP gateways."
  :group 'emagent)

(defcustom emagent-oauth-auto-browse-url t
  "When non-nil, open the OAuth authorize URL in a browser automatically."
  :type 'boolean
  :group 'emagent-oauth)

(defcustom emagent-oauth-timeout 600
  "Seconds to keep an OAuth localhost listener open before giving up."
  :type 'integer
  :group 'emagent-oauth)

(defun emagent-oauth--extract-state (authorize-url)
  "Return the OAuth state parameter value from AUTHORIZE-URL, or nil."
  (when (and authorize-url
             (string-match "[?&]state=\\([^&#]*\\)" authorize-url))
    (match-string 1 authorize-url)))

(defun emagent-oauth--ensure-state (callback-url state)
  "Return CALLBACK-URL with STATE appended if state is missing.
Some OAuth servers (e.g. ebay-ai-gateway) omit the state parameter from
the redirect, breaking PKCE state validation.  This re-injects it."
  (if (and state
           (not (string-empty-p state))
           (not (string-match-p "[?&]state=" callback-url)))
      (concat callback-url
              (if (string-match-p "\\?" callback-url) "&" "?")
              "state=" state)
    callback-url))

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
                             "\\(https://[^ \t\n<>\"'*]*oauth/authorize[^ \t\n<>\"'*]*\\)"
                             text)
                            (match-string 1 text))))
              (port (emagent-oauth--redirect-port url)))
    (list :authorize-url url :port port)))

(defun emagent-oauth--server-entry (port)
  (gethash port emagent-oauth--servers))

(defun emagent-oauth--stop-listener (port)
  "Stop the OAuth listener on PORT, if any."
  (when-let ((entry (emagent-oauth--server-entry port)))
    (when-let ((timer (plist-get entry :timer)))
      (cancel-timer timer))
    (when-let ((proc (plist-get entry :process)))
      (when (processp proc)
        (ignore-errors (delete-process proc))))
    (remhash port emagent-oauth--servers)
    (remhash port emagent-oauth--authorize-urls)))

(defun emagent-oauth--stop-buffer-listeners (buffer)
  "Stop every OAuth listener owned by BUFFER."
  (maphash (lambda (port entry)
             (when (eq (plist-get entry :buffer) buffer)
               (emagent-oauth--stop-listener port)))
           emagent-oauth--servers))

(defun emagent-oauth--start-listener (buffer port &optional attempt)
  "Listen on localhost PORT for an OAuth callback for BUFFER.
Retries up to 5 times at 0.5 s intervals when the port is in use.
Port and buffer are captured in the filter closure — no process-get needed."
  (unless (emagent-oauth--server-entry port)
    (condition-case _
        (let* ((name (format "emagent-oauth-%d" port))
               (proc (make-network-process
                      :name name
                      :service (format "%d" port)
                      :family 'ipv4
                      :server t
                      :noquery t
                      :sentinel (lambda (p _e)
                                  (when (memq (process-status p) '(closed failed))
                                    (when (eq p (plist-get (emagent-oauth--server-entry port)
                                                           :process))
                                      (remhash port emagent-oauth--servers))))
                      :filter (lambda (conn string)
                                (process-put conn 'emagent-oauth-buf
                                             (concat (or (process-get conn 'emagent-oauth-buf) "")
                                                     string))
                                (let ((buf (process-get conn 'emagent-oauth-buf)))
                                  (when (string-match "^GET \\(/callback[^\r\n]*\\) HTTP" buf)
                                    (let* ((raw-url (format "http://localhost:%d%s"
                                                            port (match-string 1 buf)))
                                           (state (emagent-oauth--extract-state
                                                   (gethash port emagent-oauth--authorize-urls)))
                                           (callback-url (emagent-oauth--ensure-state raw-url state)))
                                      (ignore-errors (process-send-string conn emagent-oauth--http-ok))
                                      (ignore-errors (delete-process conn))
                                      (emagent-oauth--stop-listener port)
                                      (emagent-oauth--queue-callback buffer callback-url)))))
                      :coding 'no-conversion))
               (timer (run-at-time emagent-oauth-timeout nil
                                   #'emagent-oauth--stop-listener port)))
          (puthash port (list :process proc :buffer buffer :timer timer :port port)
                   emagent-oauth--servers)
          (emagent-log "waiting for OAuth callback on localhost:%d…" port))
      (error
       (let ((n (or attempt 0)))
         (if (< n 5)
             (run-at-time 0.5 nil #'emagent-oauth--start-listener buffer port (1+ n))
           (emagent-log "OAuth: port %d unavailable after retries — agent may handle callback directly"
                       port)))))))

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
  (let ((prompt (format "Complete authentication with this callback URL:\n%s" callback-url))
        (inhibit-read-only t))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert prompt "\n")
    (emagent-chat--begin-response (point))
    (emagent-log "OAuth callback received; completing authentication…")
    (emagent-acp-send-prompt prompt)))

(defun emagent-oauth-watch-assistant-text (buffer text)
  "When assistant TEXT contains an OAuth authorize URL, open browser and listen."
  (when (and (buffer-live-p buffer) (stringp text) (not (string-empty-p text)))
    (when-let* ((info (emagent-oauth--find-authorize text))
                (url (plist-get info :authorize-url))
                (port (plist-get info :port)))
      (with-current-buffer buffer
        (unless (member port emagent-oauth--seen-authorize-ports)
          (push port emagent-oauth--seen-authorize-ports)
          (puthash port url emagent-oauth--authorize-urls)
          (when emagent-oauth-auto-browse-url
            (browse-url url))
          (emagent-log "OAuth started — complete login in your browser")
          (emagent-oauth--start-listener buffer port))))))

(defun emagent-oauth-shutdown-buffer (buffer)
  "Stop OAuth listeners and pending state for BUFFER."
  (when buffer
    (emagent-oauth--stop-buffer-listeners buffer)
    (remhash buffer emagent-oauth--pending)
    (remhash buffer emagent-oauth--delivered)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (port emagent-oauth--seen-authorize-ports)
          (remhash port emagent-oauth--authorize-urls))
        (setq emagent-oauth--seen-authorize-ports nil)))))

(provide 'emagent-oauth)

;;; emagent-oauth.el ends here
