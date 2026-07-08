;;; emagent-test-utils.el --- Shared helpers for emagent ERT tests -*- lexical-binding: t; -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'ert)
(require 'cl-lib)
(require 'map)

(setq load-prefer-newer t)

(let* ((root (expand-file-name ".." (file-name-directory load-file-name)))
       (bootstrap (expand-file-name "lisp/core/emagent-load-path.el" root)))
  (add-to-list 'load-path root)
  (load bootstrap nil t)
  (emagent--register-load-path root))

(require 'emagent-acp-protocol)
(require 'emagent-mcp)

(defun emagent-test--temp-file (suffix)
  "Return a unique temp file path with SUFFIX."
  (make-temp-file "emagent-test-" nil suffix))

(defun emagent-test--temp-directory ()
  "Return a unique temporary directory."
  (make-temp-file "emagent-test-" t))

(defun emagent-test--sample-claude-root ()
  "Return a minimal ~/.claude.json-shaped alist for tests."
  (list (cons "projects"
              (list (cons "/tmp/proj"
                          (list (cons "hasTrustDialogAccepted" t)))))))

(defun emagent-test--with-busy-session (fn)
  "Run FN in a temp buffer with a busy ACP session."
  (with-temp-buffer
    (setq emagent-acp--session (make-hash-table :test 'eq))
    (puthash :busy t emagent-acp--session)
    (funcall fn)))

(defmacro emagent-test--with-mocks (bindings &rest body)
  "Run BODY with function symbols rebound via `cl-letf'."
  (declare (indent 1))
  `(cl-letf ,bindings ,@body))

(defun emagent-test--with-temp-project (fn)
  "Run FN with a temporary project directory bound as the tools root."
  (let ((dir (emagent-test--temp-directory)))
    (unwind-protect
        (progn
          (emagent-tools-set-project-directory dir)
          (funcall fn dir))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(defun emagent-test--make-test-client (&rest args)
  "Return an ACP client with a no-op process; ARGS override defaults."
  (unless (plist-member args :command)
    (setq args (append args '(:command "true"))))
  (apply #'emagent-acp-make-client
         :context-buffer (get-buffer-create "*emagent-test*")
         args))

(defun emagent-test--make-acp-state (&optional client chat-buffer)
  "Return a minimal ACP session state hash for integration tests."
  (let ((state (make-hash-table :test 'eq)))
    (puthash :client (or client (emagent-test--make-test-client)) state)
    (puthash :chat-buffer (or chat-buffer (get-buffer-create "*emagent-test-chat*")) state)
    (puthash :tool-call-titles (make-hash-table :test 'equal) state)
    (puthash :tool-call-inputs (make-hash-table :test 'equal) state)
    (puthash :tool-call-labels (make-hash-table :test 'equal) state)
    (puthash :tool-call-decisions (make-hash-table :test 'equal) state)
    (puthash :tool-call-pending (make-hash-table :test 'equal) state)
    (puthash :tool-resolve-queue nil state)
    (puthash :tool-resolve-worker nil state)
    (puthash :tool-resolve-attempts (make-hash-table :test 'equal) state)
    (puthash :cb-tool-call 'emagent-chat-show-tool-call state)
    (puthash :cb-permission 'emagent-chat-permission-prompt state)
    (puthash :permission-queue nil state)
    (puthash :permission-busy nil state)
    (puthash :permission-drain-timer nil state)
    (puthash :deferred-complete-response nil state)
    (puthash :external-tool-gate-reasons nil state)
    state))

(defun emagent-test--push-first-button (&optional buffer)
  "Click the first button in BUFFER, or the current buffer when omitted."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (while (and (not (eobp)) (not (button-at (point))))
      (forward-char 1))
    (when-let ((btn (button-at (point))))
      (button-activate btn t))))

(defun emagent-test--mock-buttons-prompt (choice &optional on-call)
  "Return a mock `emagent-tools--buttons-prompt' that invokes CALLBACK with CHOICE."
  (cl-function
   (lambda (&rest args)
     (when on-call (funcall on-call args))
     (when-let ((callback (nth 3 args)))
       (funcall callback choice)))))

(defun emagent-test--with-mcp-session (root fn)
  "Register a temporary MCP session at ROOT and run FN with TOKEN and BUFFER."
  (let ((token (emagent-mcp-make-token))
        (buffer (get-buffer-create "*emagent-mcp-test*")))
    (puthash token `(:root ,root :buffer ,buffer :prefer-emacs t) emagent-mcp--sessions)
    (unwind-protect
        (with-current-buffer buffer (funcall fn token buffer))
      (remhash token emagent-mcp--sessions))))

(defun emagent-test--tools-call-sync (id params token)
  "Run tools/call synchronously for tests; return a JSON-RPC response string."
  (let ((resp nil)
        (name (and (hash-table-p params) (gethash "name" params)))
        (args (or (and (hash-table-p params) (gethash "arguments" params))
                  (make-hash-table :test 'equal)))
        (session (and token (gethash token emagent-mcp--sessions))))
    (cond
     ((null token)
      (setq resp (emagent-mcp--rpc-result
                  id (emagent-mcp--tool-content
                      "No emagent session token in request path" t))))
     ((null session)
      (setq resp (emagent-mcp--rpc-result
                  id (emagent-mcp--tool-content
                      "Unknown or expired emagent session" t))))
     (t
      (emagent-mcp--run-tool-async name args session
                                   (lambda (result is-error)
                                     (setq resp (emagent-mcp--rpc-result
                                                 id (emagent-mcp--tool-content
                                                     result is-error)))))))
    resp))

(defun emagent-test--capturing-response-sender (responses)
  "Return a response sender that pushes each RESPONSE onto list RESPONSES."
  (cl-function
   (lambda (&key _client response &allow-other-keys)
     (push response responses))))

(defvar emagent-test--last-sent-request nil)

(cl-defun emagent-test--record-request-sender (&key request on-success &allow-other-keys)
  (setq emagent-test--last-sent-request request)
  (when on-success (funcall on-success '((ok . t)))))

(defvar emagent-test--captured-responses nil)

(cl-defun emagent-test--capture-response-sender (&key _client response &allow-other-keys)
  (push response emagent-test--captured-responses))

(defun emagent-test--response-content (response)
  "Return fs/read_text_file content from an ACP RESPONSE alist."
  (when-let* ((result (or (alist-get :result response)
                           (alist-get 'result response))))
    (or (alist-get 'content result)
        (alist-get :content result))))

(defun emagent-test--response-error-code (response)
  "Return JSON-RPC error code from an ACP RESPONSE alist."
  (when-let* ((error (or (alist-get :error response)
                          (alist-get 'error response))))
    (or (alist-get 'code error)
        (alist-get :code error)
        (when (and (consp error) (eq (car error) 'code)) (cdr error)))))

(defun emagent-test--initialize-response ()
  "Return a minimal ACP initialize result alist without MCP HTTP."
  '())

(defun emagent-test--session-new-response (&optional session-id)
  "Return a minimal session/new result alist."
  `((sessionId . ,(or session-id "test-session"))
    (configOptions . [((id . "model") (category . "model")
                      (currentValue . "auto")
                      (options . [((value . "auto") (name . "Auto"))]))])))

(cl-defun emagent-test--fake-request-sender (&key request on-success on-failure &allow-other-keys)
  "ACP request sender that auto-responds to the standard connect handshake."
  (pcase (map-elt request :method)
    ("initialize"
     (when on-success (funcall on-success (emagent-test--initialize-response))))
    ("session/new"
     (when on-success (funcall on-success (emagent-test--session-new-response))))
    ("session/load"
     (when on-success
       (funcall on-success (emagent-test--session-new-response "loaded-session"))))
    ("session/prompt"
     (when on-success (funcall on-success '((usage . ((totalTokens . 10)))))))
    ("session/set_config_option"
     (when on-success (funcall on-success '((ok . t)))))
    (_
     (when on-failure
       (funcall on-failure '((code . -32601) (message . "unsupported")) nil)))))

(defun emagent-test--make-connected-client (&rest args)
  "Return a test ACP client using `emagent-test--fake-request-sender'."
  (apply #'emagent-test--make-test-client
         :request-sender #'emagent-test--fake-request-sender
         args))

(defun emagent-test--notification-chunk (text &optional update-type)
  "Return a session/update notification alist carrying TEXT."
  `((method . "session/update")
    (params . ((update . ((sessionUpdate . ,(or update-type "agent_message_chunk"))
                          (content . ((type . "text") (text . ,text)))))))))

(defun emagent-test--simulate-notification (client notification)
  "Deliver NOTIFICATION to CLIENT's subscribed handlers."
  (dolist (handler (map-elt client :notification-handlers))
    (funcall handler notification)))

(defun emagent-test--with-emagent-buffer (fn)
  "Run FN with a fresh emagent-mode buffer bound to a temp project.
FN receives (BUFFER PROJECT-DIR)."
  (emagent-test--with-temp-project
   (lambda (dir)
     (let ((buffer (get-buffer-create "*emagent-integration-test*")))
       (unwind-protect
           (emagent-test--with-mocks
               (((symbol-function 'emagent-acp-ensure-connected) (lambda (&rest _args) nil))
                ((symbol-function 'emagent-mcp-ensure-server) (lambda () 8771))
                ((symbol-function 'emagent-mcp-ensure-cursor-config) (lambda () nil)))
             (with-current-buffer buffer
               (erase-buffer)
               (insert (format "#+TITLE: test\n#+EMAGENT_PROJECT: %s\n\n" dir))
               (unless (eq major-mode 'emagent-mode)
                 (require 'emagent)
                 (emagent-mode))
               (funcall fn buffer dir)))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(defun emagent-test--http-post (token body)
  "Return a complete HTTP/1.1 POST request string for /mcp/TOKEN with BODY."
  (let* ((payload (if (stringp body) body (json-serialize body)))
         (bytes (encode-coding-string payload 'utf-8)))
    (concat (format "POST /mcp/%s HTTP/1.1\r\n" token)
            "Content-Type: application/json\r\n"
            (format "Content-Length: %d\r\n\r\n" (length bytes))
            bytes)))

(defun emagent-test--sync-idle-timers ()
  "Run pending idle timers synchronously (for batch tests)."
  (let ((timers timer-list))
    (while timers
      (let ((timer (car timers)))
        (setq timers (cdr timers))
        (when (and (timerp timer) (timer-triggered timer))
          (timer-event-handler timer))))))

(defun emagent-test--recording-request-sender (prompt-request)
  "Return a request sender that records session/prompt in PROMPT-REQUEST cell."
  (cl-function
   (lambda (&key request on-success &allow-other-keys)
     (pcase (map-elt request :method)
       ("initialize"
        (when on-success (funcall on-success (emagent-test--initialize-response))))
       ("session/new"
        (when on-success (funcall on-success (emagent-test--session-new-response))))
       ("session/prompt"
        (setcar prompt-request request)
        (when on-success (funcall on-success '((usage . ((totalTokens . 10)))))))
       (_ nil)))))

(defun emagent-test--start-connected-session (buffer &optional on-ready)
  "Start a mocked ACP session in BUFFER; return the session state."
  (emagent-test--with-mocks
      (((symbol-function 'emagent-acp--client-started-p) (lambda (_client) t))
       ((symbol-function 'emagent-mcp-register-session) (lambda (&rest _args) nil))
       ((symbol-function 'emagent-mcp-ensure-server) (lambda () 8771))
       ((symbol-function 'emagent-acp--start-rss-timer) (lambda (_state) nil)))
    (let ((client (emagent-test--make-connected-client)))
      (setq emagent-acp--session nil)
      (setq emagent-acp--session
            (emagent-acp-start
             :client client
             :chat-buffer buffer
             :on-ready on-ready
             :callbacks
             `((:cb-chunk . ,#'emagent-chat-append-assistant)
               (:cb-finish . ,#'emagent-chat-finish-assistant)
               (:cb-fail . ,#'emagent-chat-fail-assistant))))
      emagent-acp--session)))

(provide 'emagent-test-utils)

;;; emagent-test-utils.el ends here
