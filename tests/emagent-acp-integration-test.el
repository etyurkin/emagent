;;; emagent-acp-integration-test.el --- ERT integration tests for ACP sessions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-acp)

(ert-deftest emagent-acp-integration-test-connect-handshake ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let ((state (emagent-test--start-connected-session buffer)))
       (should (map-elt state :ready))
       (should (map-elt state :initialized))
       (should (string= "test-session" (map-elt state :session-id))))))

(ert-deftest emagent-acp-integration-test-notification-streams-chunk ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let* ((client (emagent-test--make-test-client))
            (state (emagent-test--make-acp-state client buffer)))
       (setq emagent-acp-stream-to-buffer t)
       (map-put! state :cb-chunk #'emagent-chat-append-assistant)
       (map-put! state :busy t)
       (with-current-buffer buffer
         (goto-char (point-max))
         (emagent-chat--begin-response (point)))
       (emagent-acp--on-notification
        :state state
        :emagent-acp-notification (emagent-test--notification-chunk "Hello "))
       (emagent-acp--on-notification
        :state state
        :emagent-acp-notification (emagent-test--notification-chunk "world"))
       (with-current-buffer buffer
         (should (string-match-p "Hello world"
                                 (substring-no-properties (buffer-string)))))
       (should (string= "Hello world" (map-elt state :assistant-text))))))

(ert-deftest emagent-acp-integration-test-send-prompt ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let ((prompt-request (list nil)))
       (emagent-test--with-mocks
           (((symbol-function 'emagent-acp--client-started-p) (lambda (_client) t))
            ((symbol-function 'emagent-mcp-register-session) (lambda (&rest _args) nil))
            ((symbol-function 'emagent-mcp-ensure-server) (lambda () 8771))
            ((symbol-function 'emagent-acp--start-rss-timer) (lambda (_state) nil))
            ((symbol-function 'emagent-chat--spinner-start) (lambda () nil)))
         (let ((client (emagent-test--make-test-client
                        :request-sender
                        (emagent-test--recording-request-sender prompt-request))))
           (setq emagent-acp--session nil)
           (setq emagent-acp--session
                 (emagent-acp-start
                  :client client
                  :chat-buffer buffer
                  :callbacks
                  `((:cb-chunk . ,#'emagent-chat-append-assistant)
                    (:cb-finish . ,#'emagent-chat-finish-assistant)
                    (:cb-fail . ,#'emagent-chat-fail-assistant))))
           (with-current-buffer buffer
             (goto-char (point-max))
             (let ((at (emagent-chat--insert-user-heading-with-text "ping")))
               (emagent-chat--begin-response at))
             (emagent-acp-send-prompt "ping"))
           (should (map-elt emagent-acp--session :busy))
           (should (car prompt-request))
           (should (string= "session/prompt"
                            (map-elt (car prompt-request) :method)))))))))))

(ert-deftest emagent-acp-integration-test-send-slash-command ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let ((prompt-request (list nil)))
       (emagent-test--with-mocks
           (((symbol-function 'emagent-acp--client-started-p) (lambda (_client) t))
            ((symbol-function 'emagent-mcp-register-session) (lambda (&rest _args) nil))
            ((symbol-function 'emagent-mcp-ensure-server) (lambda () 8771))
            ((symbol-function 'emagent-acp--start-rss-timer) (lambda (_state) nil))
            ((symbol-function 'emagent-chat--spinner-start) (lambda () nil)))
         (let ((client (emagent-test--make-test-client
                        :request-sender
                        (emagent-test--recording-request-sender prompt-request))))
           (setq emagent-acp--session nil)
           (setq emagent-acp--session
                 (emagent-acp-start
                  :client client
                  :chat-buffer buffer
                  :callbacks
                  `((:cb-chunk . ,#'emagent-chat-append-assistant)
                    (:cb-finish . ,#'emagent-chat-finish-assistant)
                    (:cb-fail . ,#'emagent-chat-fail-assistant))))
           (with-current-buffer buffer
             (setq emagent-chat-provider 'cursor)
             (goto-char (point-max))
             (let ((at (emagent-chat--insert-user-heading-with-text "/plan")))
               (emagent-chat--begin-response at))
             (emagent-acp-send-prompt "/plan"))
           (let* ((params (cdr (assoc :params (car prompt-request))))
                  (prompt (map-elt params 'prompt))
                  (text (map-elt (aref prompt 0) 'text)))
             (should (string= "/plan" text))
             (should-not (string-match-p "\\[Emacs context\\]" text)))))))))

(ert-deftest emagent-acp-integration-test-on-request-unsupported ()
  (let* ((client (emagent-test--make-test-client
                  :response-sender #'emagent-test--capture-response-sender))
         (state (emagent-test--make-acp-state client))
         (request `((id . 42) (method . "nope") (params . nil))))
    (setq emagent-test--captured-responses nil)
    (emagent-acp--on-request :state state :emagent-acp-request request)
    (should (= 1 (length emagent-test--captured-responses)))
    (should (= -32601 (emagent-test--response-error-code
                       (car emagent-test--captured-responses))))))

(provide 'emagent-acp-integration-test)

;;; emagent-acp-integration-test.el ends here
