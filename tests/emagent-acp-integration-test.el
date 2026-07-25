;;; emagent-acp-integration-test.el --- ERT integration tests for ACP sessions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-acp)

(ert-deftest emagent-acp-integration-test-connect-handshake ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let ((state (emagent-test--start-connected-session buffer)))
       (should (emagent-acp-state-ready state))
       (should (emagent-acp-state-initialized state))
       (should (string= "test-session" (emagent-acp-state-session-id state))))))

(ert-deftest emagent-acp-integration-test-notification-streams-chunk ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let* ((client (emagent-test--make-test-client))
            (state (emagent-test--make-acp-state client buffer)))
       (setq emagent-acp-stream-to-buffer t)
       (setf (emagent-acp-state-cb-chunk state) #'emagent-chat-append-assistant)
       (setf (emagent-acp-state-busy state) t)
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
       (should (string= "Hello world" (emagent-acp-state-assistant-text state))))))

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
           (should (emagent-acp-state-busy emagent-acp--session))
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


(ert-deftest emagent-acp-integration-test-create-plan-auto-accepts ()
  "cursor/create_plan must return accepted with planUri and queue Build."
  (let* ((tmpdir (make-temp-file "emagent-plans" t))
         (home (getenv "HOME"))
         (client (emagent-test--make-test-client
                  :response-sender #'emagent-test--capture-response-sender))
         (state (emagent-test--make-acp-state client))
         (emagent-acp-auto-accept-plans t)
         (emagent-acp-auto-build-plans t)
         (request `((id . 99)
                    (method . "cursor/create_plan")
                    (params . ((name . "Test plan")
                               (overview . "Do the thing")
                               (plan . "1. One\n2. Two")
                               (todos . [((id . "t1")
                                          (content . "Step one")
                                          (status . "pending"))]))))))
    (setf (emagent-acp-state-session-id state) "abcd1234-sess")
    (unwind-protect
        (progn
          (setenv "HOME" tmpdir)
          (setq emagent-test--captured-responses nil)
          (emagent-acp--on-request :state state :emagent-acp-request request)
          (should (= 1 (length emagent-test--captured-responses)))
          (let* ((resp (car emagent-test--captured-responses))
                 (result (or (alist-get :result resp) (alist-get 'result resp)))
                 (outcome (alist-get 'outcome result))
                 (uri (alist-get 'planUri outcome))
                 (path (and (stringp uri)
                            (string-prefix-p "file://" uri)
                            (substring uri (length "file://")))))
            (should-not (emagent-test--response-error-code resp))
            (should (equal (alist-get 'outcome outcome) "accepted"))
            (should (stringp uri))
            (should (file-readable-p path))
            (should (string-match-p "1\\. One" (with-temp-buffer
                                                 (insert-file-contents path)
                                                 (buffer-string))))
            (should (stringp (emagent-acp-state-plan-build-prompt state)))
            (should (string-match-p "Build the approved plan"
                                    (emagent-acp-state-plan-build-prompt state)))))
      (setenv "HOME" home)
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

(ert-deftest emagent-acp-integration-test-create-plan-prompts-interactively ()
  "Interactive create_plan must show the plan preamble, not auto-accept."
  (let* ((client (emagent-test--make-test-client
                  :response-sender #'emagent-test--capture-response-sender))
         (state (emagent-test--make-acp-state client))
         (noninteractive nil)
         (emagent-acp-auto-accept-plans nil)
         (emagent-acp-auto-build-plans t)
         (seen nil)
         (request `((id . 101)
                    (method . "cursor/create_plan")
                    (params . ((name . "Review me")
                               (plan . "Step A"))))))
    (cl-letf (((symbol-function 'emagent-acp--prepare-interactive-context)
               #'ignore)
              ((symbol-function 'emagent-acp--clear-prompt-watchdog) #'ignore)
              ((symbol-function 'emagent-tools--buttons-prompt)
               (lambda (prompt choices buf callback &optional preamble)
                 (setq seen (list prompt choices buf callback preamble)))))
      (setq emagent-test--captured-responses nil)
      (emagent-acp--on-request :state state :emagent-acp-request request)
      (should (null emagent-test--captured-responses))
      (should (equal (nth 0 seen) "Accept and build this plan?"))
      (should (string-match-p "Review me" (nth 4 seen)))
      (should (string-match-p "Step A" (nth 4 seen)))
      (should (string-prefix-p "#+begin_quote" (nth 4 seen)))
      (funcall (nth 3 seen) :accept)
      (should (= 1 (length emagent-test--captured-responses)))
      (let* ((resp (car emagent-test--captured-responses))
             (result (or (alist-get :result resp) (alist-get 'result resp)))
             (outcome (alist-get 'outcome result)))
        (should (equal (alist-get 'outcome outcome) "accepted"))))))

(ert-deftest emagent-acp-integration-test-plan-build-arms-after-complete ()
  "Queued plan Build must arm a timer when the prompt completes."
  (let* ((client (emagent-test--make-test-client
                  :response-sender #'emagent-test--capture-response-sender))
         (state (emagent-test--make-acp-state client))
         (fired nil))
    (setf (emagent-acp-state-busy state) t)
    (setf (emagent-acp-state-plan-build-prompt state) "Build now")
    (cl-letf (((symbol-function 'emagent-acp--ensure-agent-mode) #'ignore)
              ((symbol-function 'run-with-timer)
               (lambda (_secs _rep fn &rest args)
                 (setq fired (cons fn args))
                 'fake-timer)))
      (emagent-acp--complete-prompt state nil))
    (should (equal (car fired) #'emagent-acp--fire-plan-build))
    (should (eq (nth 1 fired) state))
    (should (equal (nth 2 fired) "Build now"))
    (should-not (emagent-acp-state-plan-build-prompt state))))

(ert-deftest emagent-acp-integration-test-ask-question-defaults ()
  "cursor/ask_question answers with the first option in batch mode."
  (let* ((client (emagent-test--make-test-client
                  :response-sender #'emagent-test--capture-response-sender))
         (state (emagent-test--make-acp-state client))
         (request `((id . 100)
                    (method . "cursor/ask_question")
                    (params . ((title . "Choose")
                               (questions . [((id . "q1")
                                              (prompt . "Which?")
                                              (options . [((id . "a")
                                                           (label . "A"))
                                                          ((id . "b")
                                                           (label . "B"))]))]))))))
    (setq emagent-test--captured-responses nil)
    (emagent-acp--on-request :state state :emagent-acp-request request)
    (should (= 1 (length emagent-test--captured-responses)))
    (let* ((resp (car emagent-test--captured-responses))
           (result (or (alist-get :result resp) (alist-get 'result resp)))
           (outcome (alist-get 'outcome result)))
      (should-not (emagent-test--response-error-code resp))
      (should (equal (alist-get 'outcome outcome) "answered"))
      (should (equal (alist-get 'questionId (aref (alist-get 'answers outcome) 0))
                     "q1"))
      (should (equal (aref (alist-get 'selectedOptionIds
                                      (aref (alist-get 'answers outcome) 0))
                           0)
                     "a")))))

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
