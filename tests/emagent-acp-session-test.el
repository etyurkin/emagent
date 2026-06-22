;;; emagent-acp-session-test.el --- ERT tests for emagent-acp session logic -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-acp)
(require 'emagent-cursor)

;;;; Stderr and logging helpers

(ert-deftest emagent-acp-session-test-strip-pino-colors ()
  (should (string= "hello" (emagent-acp--strip-pino-colors "he[32mll[0mo"))))

(ert-deftest emagent-acp-session-test-agent-log-line-p ()
  (should (emagent-acp--agent-log-line-p
           "2026-01-01 12:00:00 [ info ]: started"))
  (should-not (emagent-acp--agent-log-line-p "ApiError: boom")))

(ert-deftest emagent-acp-session-test-stderr-notify-p ()
  (should (emagent-acp--stderr-notify-p '((message . "ApiError: denied"))))
  (should-not (emagent-acp--stderr-notify-p '((message . "finished"))))
  (should-not (emagent-acp--stderr-notify-p '((message . "")))))

;;;; External tool gates

(ert-deftest emagent-acp-session-test-external-refusal-text-p ()
  (should (emagent-acp--external-refusal-text-p "User refused permission to run tool"))
  (should-not (emagent-acp--external-refusal-text-p "all good")))

(ert-deftest emagent-acp-session-test-infer-external-tool-gate ()
  (let* ((client (emagent-test--make-test-client
                  :command "cursor-agent"
                  :command-params '("acp")))
         (state (emagent-test--make-acp-state client)))
    (let ((emagent-acp-external-tool-gate-hints t))
      (emagent-acp--infer-external-tool-gate-from-agent state)
      (should (string-match-p "cursor-agent" (emagent-acp--agent-launch-string state)))
      (should (member 'cursor-agent-cli (map-elt state :external-tool-gate-reasons))))))

(ert-deftest emagent-acp-session-test-format-external-tool-gate-hint ()
  (let ((msg (emagent-acp--format-external-tool-gate-proactive-hint
              '(claude-agent-sdk cursor-agent-cli))))
    (should (string-match-p "Claude Agent SDK" msg))
    (should (string-match-p "cursor-agent" msg))))

(ert-deftest emagent-acp-session-test-detect-external-refusal ()
  (let ((state (emagent-test--make-acp-state)))
    (emagent-acp--detect-external-refusal-in-text
     state "permission to run tool was denied")
    (should (memq 'observed-refusal-in-stream
                  (map-elt state :external-tool-gate-reasons)))))

;;;; Permission helpers

(ert-deftest emagent-acp-session-test-permission-option-allow-p ()
  (should (emagent-acp--permission-option-allow-p '((kind . "allow_once"))))
  (should (emagent-acp--permission-option-allow-p '((optionId . "yes"))))
  (should-not (emagent-acp--permission-option-allow-p '((kind . "deny")))))

(ert-deftest emagent-acp-session-test-permission-option-id ()
  (let ((options `[((optionId . "deny") (kind . "deny"))
                   ((optionId . "allow_once") (kind . "allow_once"))]))
    (should (string= "allow_once" (emagent-acp--permission-option-id options)))))

(ert-deftest emagent-acp-session-test-permission-interactive-drains-sync ()
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (prompted nil))
    (let ((emagent-acp-auto-approve-permissions nil))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-tools--buttons-prompt)
            (lambda (&rest _args) (setq prompted t) "allow_once"))
           ((symbol-function 'emagent-acp-send-response)
            (lambda (&rest _args) nil)))
        (emagent-acp--on-permission :state state :emagent-acp-request request)
        (should prompted)
        (should (null (map-elt state :permission-queue)))))))

(ert-deftest emagent-acp-session-test-permission-interactive-defers-until-resolve ()
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (prompted nil))
    (let ((emagent-acp-auto-approve-permissions nil))
      (map-put! state :cursor-tool-resolve-queue '("tool_x"))
      (map-put! state :cursor-tool-resolve-worker t)
      (emagent-test--with-mocks
          (((symbol-function 'emagent-acp--agent-launch-string)
            (lambda (_s) "cursor-agent acp"))
           ((symbol-function 'emagent-tools--buttons-prompt)
            (lambda (&rest _args) (setq prompted t) "allow_once"))
           ((symbol-function 'emagent-acp-send-response)
            (lambda (&rest _args) nil)))
        (emagent-acp--on-permission :state state :emagent-acp-request request)
        (should-not prompted)
        (should (= 1 (length (map-elt state :permission-queue))))
        (map-put! state :cursor-tool-resolve-worker nil)
        (map-put! state :cursor-tool-resolve-queue nil)
        (emagent-acp--drain-permission-queue state)
        (should prompted)
        (should (null (map-elt state :permission-queue)))))))

(ert-deftest emagent-acp-session-test-permission-handle-one-prompts ()
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (prompted nil))
    (let ((emagent-acp-auto-approve-permissions nil))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-tools--buttons-prompt)
            (lambda (&rest _args) (setq prompted t) "allow_once"))
           ((symbol-function 'emagent-acp-send-response)
            (lambda (&rest _args) nil)))
        (emagent-acp--handle-one-permission :state state :emagent-acp-request request)
        (should prompted)))))

(ert-deftest emagent-acp-session-test-permission-auto-approve-not-deferred ()
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req2")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (prompted nil)
         (responded nil))
    (let ((emagent-acp-auto-approve-permissions t))
      (map-put! state :permission-queue (list request))
      (map-put! state :cursor-tool-resolve-queue '("tool_x"))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-acp--agent-launch-string)
            (lambda (_s) "cursor-agent acp"))
           ((symbol-function 'emagent-tools--buttons-prompt)
            (lambda (&rest _args) (setq prompted t) "allow_once"))
           ((symbol-function 'emagent-acp-send-response)
            (lambda (&rest _args) (setq responded t))))
        (emagent-acp--drain-permission-queue state)
        (should-not prompted)
        (should responded)
        (should (null (map-elt state :permission-queue)))))))

(ert-deftest emagent-acp-session-test-permission-drains-after-tool-resolve ()
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req3")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (prompted nil))
    (let ((emagent-acp-auto-approve-permissions nil))
      (map-put! state :permission-queue (list request))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-acp--agent-launch-string)
            (lambda (_s) "cursor-agent acp"))
           ((symbol-function 'emagent-tools--buttons-prompt)
            (lambda (&rest _args) (setq prompted t) "allow_once"))
           ((symbol-function 'emagent-acp-send-response) (lambda (&rest _args) nil)))
        (emagent-acp--drain-cursor-tool-resolve-queue state)
        (should prompted)
        (should (null (map-elt state :permission-queue)))))))

(ert-deftest emagent-acp-session-test-permission-prompt-text ()
  (let* ((cmd "emacs --batch -l tests/emagent-test-runner.el 2>&1 | tail -30")
         (args (make-hash-table :test 'equal))
         (request `((params . ((title . "Allow compile?")
                                (toolCall . ((toolCallId . "tool_compile")
                                             (title . "compile")
                                             (arguments . ,args))))))))
    (puthash "command" cmd args)
    (should (string= (format "Allow compile?\n%s" cmd)
                     (emagent-acp--permission-prompt-text request))))
  (let* ((cmd "emacs --batch -l tests/emagent-test-runner.el")
         (raw (make-hash-table :test 'equal))
         (request `((params . ((title . "Allow compile?")
                                (toolCall . ((toolCallId . "tool_compile")
                                             (rawInput . ,raw))))))))
    (puthash "command" cmd raw)
    (should (string= (format "Allow compile?\n%s" cmd)
                     (emagent-acp--permission-prompt-text request))))
  (should (string= "Allow compile?"
                   (emagent-acp--permission-prompt-text
                    '((params . ((title . "Allow compile?")
                                 (toolCall . ((rawInput . "ignored")
                                              (subtitle . "#s(hash-table test equal data (command foo))")))))))))
  (let* ((cmd "make test")
         (args (make-hash-table :test 'equal))
         (request `((params . ((title . "Allow compile?")
                                (toolCall . ((toolCallId . "tool_compile")
                                             (title . "compile")
                                             (subtitle . "#s(hash-table test equal data (command make test))")
                                             (arguments . ,args))))))))
    (puthash "command" cmd args)
    (should (string= (format "Allow compile?\n%s" cmd)
                     (emagent-acp--permission-prompt-text request))))
  (let* ((cmd "emacs --batch -l tests/emagent-test-runner.el 2>&1 | tail -40")
         (prin1 (format "#s(hash-table test equal data (command %s))" cmd))
         (request `((params . ((title . "Allow compile?")
                                (toolCall . ((toolCallId . "tool_compile")
                                             (title . "compile")
                                             (rawInput . ,prin1))))))))
    (should (string= (format "Allow compile?\n%s" cmd)
                     (emagent-acp--permission-prompt-text request)))))

;;;; Tool-call display

(ert-deftest emagent-acp-session-test-tool-call-detail ()
  (let ((update '((title . "Read")
                  (rawInput . "{\"path\":\"foo.el\"}"))))
    (should (string= "foo.el" (emagent-acp--tool-call-detail update))))
  (let ((update '((title . "Edit File")
                  (rawInput . (("edits" . (((path . "bar.el")))))))))
    (should (string= "bar.el" (emagent-acp--tool-call-detail update))))
  (let ((update '((title . "MCP") (subtitle . "tool")
                  (rawInput . (("args" . "-5 --oneline"))))))
    (should (string= "-5 --oneline" (emagent-acp--tool-call-detail update))))
  (let* ((cmd "emacs --batch -l tests/emagent-test-runner.el")
         (args (make-hash-table :test 'equal)))
    (puthash "command" cmd args)
    (should (string= cmd
                     (emagent-acp--tool-call-detail
                      `((title . "compile") (arguments . ,args))))))
  (let* ((cmd "make test")
         (args (make-hash-table :test 'equal)))
    (puthash "command" cmd args)
    (should (string= cmd
                     (emagent-acp--tool-call-detail
                      `((title . "compile")
                        (subtitle . "#s(hash-table test equal data (command make test))")
                        (arguments . ,args)))))))

(ert-deftest emagent-acp-session-test-tool-call-label ()
  (let ((update '((title . "Grep") (rawInput . "{\"pattern\":\"defun\"}"))))
    (should (string-match-p "Grep" (emagent-acp--tool-call-label update)))
    (should (string-match-p "defun" (emagent-acp--tool-call-label update))))
  (let ((update '((title . "git_log")
                  (rawInput . (("args" . "-5 --oneline"))))))
    (should (string= "git_log: -5 --oneline"
                     (emagent-acp--tool-call-label update)))))

(ert-deftest emagent-acp-session-test-cursor-tool-call-deferred-until-detail ()
  (let ((state (emagent-test--make-acp-state))
        (shown nil))
    (puthash :session-id "sess" state)
    (emagent-test--with-mocks
        (((symbol-function 'emagent-acp--agent-launch-string)
          (lambda (_state) "cursor-agent acp"))
         ((symbol-function 'emagent-cursor-tool-call-from-store)
          (lambda (_sid _id) nil))
         ((symbol-function 'emagent-chat-show-tool-call)
          (lambda (_id label) (setq shown label))))
      (emagent-acp--on-tool-call
       state '((toolCallId . "tool_x") (title . "Edit") (rawInput . ())))
      (should (null shown))
      (should (= 1 (hash-table-count (map-elt state :tool-call-pending))))
      (emagent-acp--ingest-tool-call-request
       state '((toolCallId . "tool_x")
                (title . "Edit")
                (rawInput . "{\"path\":\"foo.el\"}")))
      (should (string-match-p "foo.el" shown))
      (should (= 0 (hash-table-count (map-elt state :tool-call-pending))))
      (setq shown nil)
      (emagent-acp--on-tool-call
       state '((toolCallId . "tool_y") (title . "Read") (rawInput . ())))
      (should (null shown))
      (let ((store (lambda (_sid _id) '("Read" . (("path" . "bar.el"))))))
        (cl-letf (((symbol-function 'emagent-cursor-tool-call-from-store) store))
          (emagent-acp--resolve-cursor-tool-from-store state "tool_y")))
      (should (string-match-p "bar.el" shown))
      (should (= 0 (hash-table-count (map-elt state :tool-call-pending)))))))

(ert-deftest emagent-acp-session-test-cursor-tool-call-completed-waits-for-store ()
  (let ((state (emagent-test--make-acp-state))
        (shown nil))
    (puthash :session-id "sess" state)
    (emagent-test--with-mocks
        (((symbol-function 'emagent-acp--agent-launch-string)
          (lambda (_state) "cursor-agent acp"))
         ((symbol-function 'emagent-cursor-tool-call-from-store)
          (lambda (_sid _id) nil))
         ((symbol-function 'emagent-chat-show-tool-call)
          (lambda (_id label) (setq shown label))))
      (emagent-acp--on-tool-call
       state '((toolCallId . "tool_z") (title . "Read File") (rawInput . ())))
      (should (null shown))
      (emagent-acp--on-tool-call
       state '((toolCallId . "tool_z") (title . "Read File")
               (status . "completed") (rawInput . ())))
      (should (null shown))
      (let ((store (lambda (_sid _id) '("Read" . (("path" . "done.el"))))))
        (cl-letf (((symbol-function 'emagent-cursor-tool-call-from-store) store))
          (emagent-acp--resolve-cursor-tool-from-store state "tool_z")))
      (should (string-match-p "done.el" shown))
      (should (= 0 (hash-table-count (map-elt state :tool-call-pending)))))))

(ert-deftest emagent-acp-session-test-tool-call-truncate ()
  (should (= 121 (length (emagent-acp--tool-call-truncate (make-string 200 ?x)))))
  (should (string-match-p "…\\'" (emagent-acp--tool-call-truncate (make-string 200 ?x)))))

;;;; Model resolution

(ert-deftest emagent-acp-session-test-model-entries-from-response ()
  (let* ((models (list (cons 'availableModels
                            (vector '((modelId . "gpt-4") (name . "GPT 4"))))
                       (cons 'currentModelId "gpt-4")))
         (response (list (cons 'models models)))
         (entries (emagent-acp--model-entries-from-response response)))
    (should (= 1 (length entries)))
    (should (string= "gpt-4" (map-elt (car entries) :model-id)))
    (should (string= "GPT 4" (map-elt (car entries) :name)))))

(ert-deftest emagent-acp-session-test-resolve-model-id ()
  (let* ((state (emagent-test--make-acp-state))
         (models '((availableModels . [((modelId . "auto") (name . "Auto"))
                                      ((modelId . "gpt-4") (name . "GPT 4"))])
                   (currentModelId . "gpt-4"))))
    (should (string= "gpt-4"
                     (emagent-acp--resolve-model-id state models "gpt-4")))
    (should (string= "auto"
                     (emagent-acp--resolve-model-id state models nil)))
    (map-put! state :config-options
              `((( :id . "model") (:category . "model")
                 (:current-value . "gpt-4")
                 (:options . (((:value . "gpt-4") (:name . "GPT 4")))))))
    (should (string= "GPT 4"
                     (emagent-acp--model-display-name state models "gpt-4")))))

;;;; Filesystem handlers

(ert-deftest emagent-acp-session-test-on-fs-read ()
  (emagent-test--with-temp-project
   (lambda (dir)
     (let* ((file (expand-file-name "foo.txt" dir))
            (client (emagent-test--make-test-client
                     :response-sender #'emagent-test--capture-response-sender))
            (state (emagent-test--make-acp-state client))
            (request `((id . 7) (method . "fs/read_text_file")
                       (params . ((path . ,file))))))
       (setq emagent-test--captured-responses nil)
       (write-region "hello world" nil file)
       (let ((emagent-acp-file-access t))
         (emagent-acp--on-fs-read :state state :emagent-acp-request request))
       (should (= 1 (length emagent-test--captured-responses)))
       (should (string= "hello world"
                         (emagent-test--response-content
                          (car emagent-test--captured-responses))))))))

(ert-deftest emagent-acp-session-test-on-fs-read-disabled ()
  (let* ((client (emagent-test--make-test-client
                  :response-sender #'emagent-test--capture-response-sender))
         (state (emagent-test--make-acp-state client)))
    (setq emagent-test--captured-responses nil)
    (let ((emagent-acp-file-access nil))
      (emagent-acp--on-fs-read
       :state state
       :emagent-acp-request
       `((id . 1) (method . "fs/read_text_file")
         (params . ((path . "foo.txt"))))))
    (should (= 1 (length emagent-test--captured-responses)))
    (should (= -32601 (emagent-test--response-error-code
                        (car emagent-test--captured-responses))))))

(ert-deftest emagent-acp-session-test-on-fs-write ()
  (emagent-test--with-temp-project
   (lambda (dir)
     (let* ((client (emagent-test--make-test-client
                     :response-sender #'emagent-test--capture-response-sender))
            (state (emagent-test--make-acp-state client))
            (request `((id . 2) (method . "fs/write_text_file")
                       (params . ((path . "out.txt") (content . "written"))))))
       (setq emagent-test--captured-responses nil)
       (let ((emagent-acp-file-access t)
             (emagent-acp-confirm-fs-writes nil))
         (emagent-acp--on-fs-write :state state :emagent-acp-request request))
       (should (= 1 (length emagent-test--captured-responses)))
       (should (file-exists-p (expand-file-name "out.txt" dir)))
       (should (string= "written"
                         (string-trim
                          (with-temp-buffer
                            (insert-file-contents (expand-file-name "out.txt" dir))
                            (buffer-string)))))))))

;;;; Images and prompts

(ert-deftest emagent-acp-session-test-image-media-type ()
  (should (string= "image/png" (emagent-acp--image-media-type "png")))
  (should (string= "image/jpeg" (emagent-acp--image-media-type "jpg")))
  (should-not (emagent-acp--image-media-type "txt")))

(ert-deftest emagent-acp-session-test-extract-image-links ()
  (emagent-test--with-temp-project
   (lambda (dir)
     (let* ((png (expand-file-name "pic.png" dir))
            (text (format "see [[file:%s]] here" png))
            (extracted nil))
       (write-region "\x89PNG" nil png)
       (setq extracted (emagent-acp--extract-image-links text))
       (should (string-match-p "here" (car extracted)))
       (should (= 1 (length (cdr extracted))))
       (should (string= "image/png"
                         (alist-get 'media-type (car (cdr extracted)))))))))

(ert-deftest emagent-acp-session-test-system-prompt ()
  (let ((prompt (emagent-acp--system-prompt)))
    (should (string-match-p "emagent" prompt))
    (should (string-match-p "org markup" prompt))
    (should (string-match-p "Tool preference" prompt))))

(ert-deftest emagent-acp-session-test-mcp-http-capable-p ()
  (should (emagent-acp--mcp-http-capable-p
           '((agentCapabilities . ((mcpCapabilities . ((http . t))))))))
  (should-not (emagent-acp--mcp-http-capable-p
               '((agentCapabilities . ((mcpCapabilities . ((http . :false)))))))))

(ert-deftest emagent-acp-session-test-fatal-agent-error-p ()
  (should (emagent-acp--fatal-agent-error-p "request timed out"))
  (should (emagent-acp--fatal-agent-error-p "failed with status 500"))
  (should-not (emagent-acp--fatal-agent-error-p "still working")))

(provide 'emagent-acp-session-test)

;;; emagent-acp-session-test.el ends here
