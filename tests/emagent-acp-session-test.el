;;; emagent-acp-session-test.el --- ERT tests for emagent-acp session logic -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-acp)

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

;;;; Tool-call display

(ert-deftest emagent-acp-session-test-tool-call-detail ()
  (let ((update '((title . "Read")
                  (rawInput . "{\"path\":\"foo.el\"}"))))
    (should (string= "foo.el" (emagent-acp--tool-call-detail update)))))

(ert-deftest emagent-acp-session-test-tool-call-label ()
  (let ((update '((title . "Grep") (rawInput . "{\"pattern\":\"defun\"}"))))
    (should (string-match-p "Grep" (emagent-acp--tool-call-label update)))
    (should (string-match-p "defun" (emagent-acp--tool-call-label update)))))

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
