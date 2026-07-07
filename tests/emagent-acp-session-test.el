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

(ert-deftest emagent-acp-session-test-permission-acp-allow-id-never-always ()
  (let ((options `[((optionId . "allow_always") (kind . "allow_always"))
                   ((optionId . "allow_once") (kind . "allow_once"))]))
    (should (string= "allow_once" (emagent-acp--permission-acp-allow-id options)))))

(ert-deftest emagent-acp-session-test-permission-fingerprint ()
  (let ((args (make-hash-table :test 'equal)))
    (puthash "command" "make test" args)
    (should (string= "execute:make"
                     (emagent-acp--permission-fingerprint
                      `((kind . "execute") (arguments . ,args)))))))

(ert-deftest emagent-acp-session-test-permission-validate-blocks-eval ()
  (let ((args (make-hash-table :test 'equal)))
    (puthash "form" "(kill-emacs)" args)
    (let ((result (emagent-acp--permission-validate
                   `((kind . "execute") (arguments . ,args)))))
      (should (eq (car result) :deny)))))

(ert-deftest emagent-acp-session-test-permission-validate-dangerous-eval ()
  (let ((args (make-hash-table :test 'equal)))
    (puthash "form" "(delete-file \"foo\")" args)
    (let ((result (emagent-acp--permission-validate
                   `((kind . "execute") (arguments . ,args)))))
      (should (eq (car result) :confirm)))))

(ert-deftest emagent-acp-session-test-permission-auto-allowed-session ()
  (emagent-test--with-mocks
      (((symbol-function 'emagent-permissions-global-fingerprints) (lambda () nil))
       ((symbol-function 'emagent-permissions-session-fingerprints) (lambda (_) nil)))
    (let ((state (emagent-test--make-acp-state)))
      (map-put! state :permission-auto-allow '("execute:make"))
      (should (emagent-acp--permission-auto-allowed-p state "execute:make" nil))
      (should-not (emagent-acp--permission-auto-allowed-p state "execute:git" nil)))))

(ert-deftest emagent-acp-session-test-permission-handle-one-replies-once-after-always ()
  (let* ((state (emagent-test--make-acp-state))
         (args (make-hash-table :test 'equal))
         (perms-dir (emagent-test--temp-directory))
         (request `((id . "req1")
                     (params . ((title . "Allow compile?")
                                (options . [((optionId . "allow_always")
                                             (kind . "allow_always"))
                                            ((optionId . "allow_once")
                                             (kind . "allow_once"))])
                                (toolCall . ((kind . "execute")
                                             (title . "compile")
                                             (arguments . ,args)))))))
         (sent-id nil))
    (puthash "command" "make test" args)
    (let ((emagent-acp-auto-approve-permissions nil)
          (emagent-permissions-directory perms-dir)
          (emagent-permissions--cache (make-hash-table :test 'equal)))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-chat--open-response-p)
            (lambda () nil))
           ((symbol-function 'emagent-tools--buttons-prompt)
            (emagent-test--mock-buttons-prompt :allow-always))
           ((symbol-function 'emagent-acp-send-response)
            (cl-function
             (lambda (&key response &allow-other-keys)
               (setq sent-id (map-nested-elt response '(:result outcome optionId)))))))
        (emagent-acp--handle-one-permission :state state :emagent-acp-request request)
        (should (string= "allow_once" sent-id))
        (should (member "execute:make" (emagent-permissions-global-fingerprints)))
        (should-not (member "execute:make" (or (map-elt state :permission-auto-allow) nil)))))))

(ert-deftest emagent-acp-session-test-permission-allow-session-persists-by-session-id ()
  (let ((perms-dir (emagent-test--temp-directory)))
    (let ((emagent-permissions-directory perms-dir)
          (emagent-permissions--cache (make-hash-table :test 'equal)))
      (let* ((state (emagent-test--make-acp-state))
             (session-id "sess-abc")
             (fingerprint "execute:make"))
        (puthash :session-id session-id state)
        (emagent-acp--permission-apply-choice state fingerprint nil :allow-session)
        (should (equal '("execute:make")
                       (emagent-permissions-session-fingerprints session-id)))
        (should-not (emagent-permissions-session-fingerprints "other-session"))
        (setq emagent-acp--session nil)
        (let ((fresh (emagent-test--make-acp-state)))
          (puthash :session-id session-id fresh)
          (emagent-acp--hydrate-session-permissions fresh session-id)
          (should (member fingerprint (map-elt fresh :permission-auto-allow))))))))

(ert-deftest emagent-acp-session-test-permission-allow-all-persists-by-session-id ()
  (let ((perms-dir (emagent-test--temp-directory)))
    (let ((emagent-permissions-directory perms-dir)
          (emagent-permissions--cache (make-hash-table :test 'equal)))
      (let* ((state (emagent-test--make-acp-state))
             (session-id "sess-all"))
        (puthash :session-id session-id state)
        (emagent-acp--permission-apply-choice state nil nil :allow-all)
        (should (emagent-permissions-session-auto-approve-p session-id))
        (setq emagent-acp--session nil)
        (let ((fresh (emagent-test--make-acp-state)))
          (emagent-acp--hydrate-session-permissions fresh session-id)
          (should (map-elt fresh :session-auto-approve)))))))

(ert-deftest emagent-acp-session-test-permission-decision-label ()
  (should (string= "Allow web search? (Allow: Always)"
                   (emagent-acp--permission-decision-label "Allow web search?" :allow-always)))
  (should (string= "Allow web search? (Allow: Session)"
                   (emagent-acp--permission-decision-label "Allow web search?" :allow-session)))
  (should (string= "Allow web search? (Allow: Once)"
                   (emagent-acp--permission-decision-label "Allow web search?" :allow-once)))
  (should (string= "Allow web search? (Allow)"
                   (emagent-acp--permission-decision-label "Allow web search?" :allow)))
  (should (string= "Allow web search? (Denied)"
                   (emagent-acp--permission-decision-label "Allow web search?" :deny))))

(ert-deftest emagent-acp-session-test-permission-auto-policy-shows-generic-allow ()
  "Policy auto-approval with no stored choice still records a generic (Allow)."
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req-edit")
                    (params . ((title . "Edit File: foo.rs")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])
                               (toolCall . ((toolCallId . "edit_x")
                                            (kind . "edit")
                                            (title . "Edit File: foo.rs")))))))
         (decided nil))
    (let ((emagent-acp-auto-approve-permissions t))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-acp--show-permission-decision)
            (lambda (_state _tool-call choice) (setq decided choice)))
           ((symbol-function 'emagent-acp-send-response)
            (cl-function (lambda (&rest _) nil))))
        (emagent-acp--handle-one-permission :state state :emagent-acp-request request)
        (should (eq decided :allow))))))

(ert-deftest emagent-acp-session-test-permission-stored-auto-choice ()
  (emagent-test--with-mocks
      (((symbol-function 'emagent-permissions-global-fingerprints)
        (lambda () '("tool:Allow web search?")))
       ((symbol-function 'emagent-permissions-session-fingerprints) (lambda (_) nil))
       ((symbol-function 'emagent-chat-allowed-permissions) (lambda () nil))
       ((symbol-function 'emagent-chat-project-directory) (lambda () nil))
       ((symbol-function 'emagent-permissions-project-fingerprints) (lambda (_) nil)))
    (let ((state (emagent-test--make-acp-state)))
      (should (eq :allow-always
                  (emagent-acp--permission-stored-auto-choice
                   state "tool:Allow web search?" nil)))
      (map-put! state :session-auto-approve t)
      (should (eq :allow-all
                  (emagent-acp--permission-stored-auto-choice
                   state "tool:Allow web search?" nil))))))

(ert-deftest emagent-acp-session-test-permission-auto-shows-stored-decision ()
  (let* ((state (emagent-test--make-acp-state))
         (tool-call '((toolCallId . "perm_web") (title . "Allow web search?")))
         (shown nil))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-chat-show-tool-call)
          (lambda (_id label &rest _) (setq shown label))))
      (emagent-acp--show-permission-decision state tool-call :allow-always)
      (should (string= "Allow web search? (Allow: Always)" shown))
      (setq shown nil)
      (emagent-acp--show-permission-decision state tool-call :allow-once)
      (should (string= "Allow web search? (Allow: Once)" shown)))))

(ert-deftest emagent-acp-session-test-permission-decision-persists-on-update ()
  "A later tool_call_update keeps the recorded decision suffix."
  (let* ((state (emagent-test--make-acp-state))
         (tool-call '((toolCallId . "edit_1") (title . "Edit File: foo.rs")))
         (shown nil))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-chat-show-tool-call)
          (lambda (_id label &rest _) (setq shown label)))
         ((symbol-function 'emagent-acp--detect-external-refusal-in-text)
          (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--notify-user) (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--refresh-mode-line) (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--schedule-prompt-watchdog)
          (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--chat-buffer)
          (lambda (_) (current-buffer))))
      (emagent-acp--show-permission-decision state tool-call :allow-session)
      (should (string= "Edit File: foo.rs (Allow: Session)" shown))
      (setq shown nil)
      (emagent-acp--emit-tool-call-display
       state "edit_1" 'edit nil "Edit File: foo.rs" "completed")
      (should (string= "Edit File: foo.rs (Allow: Session)" shown)))))

(ert-deftest emagent-acp-session-test-emagent-tool-tagged-emacs ()
  "Tools from emagent's own MCP server render with an (Allow: Emacs) tag."
  (let* ((state (emagent-test--make-acp-state))
         (merged '((toolCallId . "rf1") (title . "mcp_emagent_read_file")))
         (shown nil))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-chat-show-tool-call)
          (lambda (_id label &rest _) (setq shown label)))
         ((symbol-function 'emagent-acp--detect-external-refusal-in-text)
          (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--notify-user) (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--refresh-mode-line) (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--schedule-prompt-watchdog)
          (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--chat-buffer)
          (lambda (_) (current-buffer))))
      (emagent-acp--emit-tool-call-display
       state "rf1" 'read merged "read_file: foo.el" "completed")
      (should (string= "read_file: foo.el (Allow: Emacs)" shown)))))

(ert-deftest emagent-acp-session-test-agent-tool-not-tagged-emacs ()
  "Native agent tools (no emagent MCP origin) are tagged (Allow: Agent), not Emacs.
A completed tool that never hit the ACP permission path was allowed by the
agent's own allow-list, so the inferred decision is (Allow: Agent)."
  (let* ((state (emagent-test--make-acp-state))
         (merged '((toolCallId . "g1") (title . "Grep")))
         (shown nil))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-chat-show-tool-call)
          (lambda (_id label &rest _) (setq shown label)))
         ((symbol-function 'emagent-acp--detect-external-refusal-in-text)
          (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--notify-user) (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--refresh-mode-line) (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--schedule-prompt-watchdog)
          (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--chat-buffer)
          (lambda (_) (current-buffer))))
      (emagent-acp--emit-tool-call-display
       state "g1" 'search merged "Grep: pattern" "completed")
      (should (string= "Grep: pattern (Allow: Agent)" shown)))))

(defun emagent-test--run-at-time-immediately (_time _repeat fn)
  "Test helper: invoke FN synchronously instead of scheduling."
  (funcall fn)
  nil)

(ert-deftest emagent-acp-session-test-permission-interactive-schedules-drain ()
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (scheduled nil))
    (let ((emagent-acp-auto-approve-permissions nil))
      (emagent-test--with-mocks
          (((symbol-function 'run-at-time)
            (lambda (_time _repeat fn)
              (setq scheduled t)
              nil))
           ((symbol-function 'emagent-acp-send-response)
            (lambda (&rest _args) nil)))
        (emagent-acp--on-permission :state state :emagent-acp-request request)
        (should scheduled)
        (should (= 1 (length (map-elt state :permission-queue))))))))

(ert-deftest emagent-acp-session-test-permission-interactive-drains-deferred ()
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (prompted nil))
    (let ((emagent-acp-auto-approve-permissions nil))
      (emagent-test--with-mocks
          (((symbol-function 'run-at-time) #'emagent-test--run-at-time-immediately)
           ((symbol-function 'emagent-tools--buttons-prompt)
            (emagent-test--mock-buttons-prompt
             :allow-once
             (lambda (_) (setq prompted t))))
           ((symbol-function 'emagent-acp-send-response)
            (lambda (&rest _args) nil)))
        (emagent-acp--on-permission :state state :emagent-acp-request request)
        (should prompted)
        (should (null (map-elt state :permission-queue)))))))

(ert-deftest emagent-acp-session-test-permission-not-deferred-for-tool-resolve ()
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (prompted nil))
    (let ((emagent-acp-auto-approve-permissions nil))
      (map-put! state :tool-resolve-queue '("tool_x"))
      (map-put! state :tool-resolve-worker t)
      (emagent-test--with-mocks
          (((symbol-function 'run-at-time) #'emagent-test--run-at-time-immediately)
           ((symbol-function 'emagent-acp--agent-launch-string)
            (lambda (_s) "cursor-agent acp"))
           ((symbol-function 'emagent-tools--buttons-prompt)
            (emagent-test--mock-buttons-prompt
             :allow-once
             (lambda (_) (setq prompted t))))
           ((symbol-function 'emagent-acp-send-response)
            (lambda (&rest _args) nil)))
        (emagent-acp--on-permission :state state :emagent-acp-request request)
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
            (lambda (&rest _args) (setq prompted t)))
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
            (emagent-test--mock-buttons-prompt
             :allow-once
             (lambda (_) (setq prompted t))))
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
          (((symbol-function 'run-at-time) #'emagent-test--run-at-time-immediately)
           ((symbol-function 'emagent-acp--agent-launch-string)
            (lambda (_s) "cursor-agent acp"))
           ((symbol-function 'emagent-tools--buttons-prompt)
            (emagent-test--mock-buttons-prompt
             :allow-once
             (lambda (_) (setq prompted t))))
           ((symbol-function 'emagent-acp-send-response) (lambda (&rest _args) nil)))
        (emagent-acp-cursor--drain-tool-resolve-queue state)
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

(ert-deftest emagent-acp-session-test-tool-call-content-block-no-kind ()
  (let* ((cmd "make test")
         (args (make-hash-table :test 'equal)))
    (puthash "command" cmd args)
    (should (string-match-p "\\*\\* Allow execute"
                            (emagent-acp--tool-call-content-block
                             `((toolCallId . "tool_compile")
                               (title . "compile")
                               (arguments . ,args)))))
    (should (string-match-p (regexp-quote cmd)
                            (emagent-acp--tool-call-content-block
                             `((toolCallId . "tool_compile")
                               (title . "compile")
                               (arguments . ,args)))))))

(ert-deftest emagent-acp-session-test-tool-call-content-block-python-heredoc ()
  (let* ((cmd "python3 - <<'EOF'\nimport json\nprint(1)\nEOF")
         (args (make-hash-table :test 'equal)))
    (puthash "command" cmd args)
    (let ((block (emagent-acp--tool-call-content-block
                  `((toolCallId . "tool_compile")
                    (title . "compile")
                    (arguments . ,args)))))
      (should (string-match-p "\\*\\* Allow execute" block))
      (should (string-match-p "#\\+BEGIN_SRC python" block))
      (should (string-match-p "import json" block))
      (should-not (string-match-p "python3 - <<" block)))))

(ert-deftest emagent-acp-session-test-tool-call-content-block-eval ()
  (let ((args (make-hash-table :test 'equal)))
    (puthash "form" "(+ 1 2)" args)
    (let ((block (emagent-acp--tool-call-content-block
                  `((toolCallId . "tool_eval")
                    (title . "emagent-eval: eval")
                    (arguments . ,args)))))
      (should (string-match-p "\\*\\* Allow eval" block))
      (should (string-match-p "#\\+BEGIN_SRC elisp" block))
      (should (string-match-p "(\\+ 1 2)" block))
      (should-not (string-match-p "#\\+BEGIN_SRC sh" block)))))

(ert-deftest emagent-acp-session-test-tool-call-content-block-write-diff ()
  (let* ((dir (emagent-test--temp-directory))
         (path (expand-file-name "sample.py" dir)))
    (unwind-protect
        (progn
          (write-region "old line\n" nil path)
          (let ((block (emagent-acp--tool-call-content-block
                        `((toolCallId . "tool_write")
                          (title . "Edit File")
                          (rawInput . ((path . ,path)
                                       (content . "new line\n")))))))
            (should (string-match-p "\\*\\* Allow edit: sample.py" block))
            (should (string-match-p "#\\+BEGIN_SRC diff" block))
            (should (string-match-p "new line" block))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emagent-acp-session-test-tool-call-content-block-edit-patch ()
  (let* ((dir (emagent-test--temp-directory))
         (path (expand-file-name "foo.el" dir)))
    (unwind-protect
        (progn
          (write-region "old line\n" nil path)
          (let ((block (emagent-acp--tool-call-content-block
                        `((toolCallId . "tool_edit")
                          (title . "Edit File")
                          (rawInput . ((path . ,path)
                                       (edits . (((old_string . "old line")
                                                  (new_string . "new line"))))))))))
            (should (string-match-p "\\*\\* Allow edit: foo.el" block))
            (should (string-match-p "#\\+BEGIN_SRC diff" block))
            (should (string-match-p "new line" block))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emagent-acp-session-test-tool-call-content-block-claude-write ()
  (let* ((dir (emagent-test--temp-directory))
         (path (expand-file-name "module.py" dir)))
    (unwind-protect
        (progn
          (write-region "before\n" nil path)
          (let ((block (emagent-acp--tool-call-content-block
                        `((toolCallId . "tool_claude_write")
                          (kind . "edit")
                          (title . "write")
                          (locations . (((path . ,path))))
                          (rawInput . ((filePath . ,path)
                                       (content . "after\n")))))))
            (should (string-match-p "\\*\\* Allow edit: module.py" block))
            (should (string-match-p "#\\+BEGIN_SRC diff" block))
            (should (string-match-p "after" block))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emagent-acp-session-test-tool-call-content-block-claude-patch ()
  (let* ((dir (emagent-test--temp-directory))
         (path (expand-file-name "lib.el" dir)))
    (unwind-protect
        (progn
          (write-region "alpha\n" nil path)
          (let ((block (emagent-acp--tool-call-content-block
                        `((toolCallId . "tool_claude_patch")
                          (kind . "edit")
                          (title . "Edit")
                          (rawInput . ((file_path . ,path)
                                       (old_string . "alpha")
                                       (new_string . "beta")))))))
            (should (string-match-p "\\*\\* Allow edit: lib.el" block))
            (should (string-match-p "#\\+BEGIN_SRC diff" block))
            (should (string-match-p "beta" block))))
      (ignore-errors (delete-directory dir t)))))

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
                        (arguments . ,args))))))
  (let ((update '((title . "filesystem-create_directory")
                  (rawInput . "{\"name\":\"create_directory\",\"arguments\":{\"path\":\"/tmp/new-dir\"}}"))))
    (should (string= "/tmp/new-dir" (emagent-acp--tool-call-detail update))))
  (let ((update '((title . "filesystem-write_file")
                  (rawInput . (("name" . "write_file")
                               ("arguments" . (("path" . "/tmp/out.txt")
                                               ("content" . "hello"))))))))
    (should (string= "/tmp/out.txt" (emagent-acp--tool-call-detail update))))
  (let ((update '((title . "filesystem-list_directory")
                  (rawInput . (("arguments" . (("recursive" . t)
                                               ("maxDepth" . 3))))))))
    (should (string= "recursive=t maxdepth=3"
                     (emagent-acp--tool-call-detail update))))
  (let ((update '((title . "custom-tool")
                  (rawInput . (("mode" . "fast")
                               ("timeout" . 30))))))
    (should (string= "mode=fast timeout=30"
                     (emagent-acp--tool-call-detail update)))))

(ert-deftest emagent-acp-session-test-tool-call-label ()
  (let ((update '((title . "Grep") (rawInput . "{\"pattern\":\"defun\"}"))))
    (should (string-match-p "Grep" (emagent-acp--tool-call-label update)))
    (should (string-match-p "defun" (emagent-acp--tool-call-label update))))
  (let ((update '((title . "git_log")
                  (rawInput . (("args" . "-5 --oneline"))))))
    (should (string= "git_log: -5 --oneline"
                     (emagent-acp--tool-call-label update)))))

(ert-deftest emagent-acp-session-test-tool-call-block-spec ()
  ;; Explicit terminal command -> sh block carrying the command text.
  (let ((update '((title . "Terminal")
                  (rawInput . "{\"command\":\"cargo add foo\"}"))))
    (should (equal '("sh" . "cargo add foo")
                   (emagent-acp--tool-call-block-spec update))))
  ;; A structured grep tool carries only a pattern; reconstruct a grep command
  ;; line so it reads naturally and gets shell highlighting.  Patterns with
  ;; whitespace/metacharacters are quoted so they read unambiguously.
  (let ((update '((title . "grep")
                  (rawInput . "{\"pattern\":\"Edge \\\\{\"}"))))
    (should (equal '("sh" . "grep \"Edge \\{\"")
                   (emagent-acp--tool-call-block-spec update))))
  ;; grep with both a pattern and a path reconstructs the full command line so
  ;; the search term is not dropped in favor of the path.
  (let ((update '((title . "Grep")
                  (rawInput . "{\"pattern\":\"TODO\",\"path\":\"/Users/me/src/App.java\"}"))))
    (should (equal '("sh" . "grep TODO /Users/me/src/App.java")
                   (emagent-acp--tool-call-block-spec update))))
  ;; File read stays a compact arrow line (no block spec).
  (let ((update '((title . "Read")
                  (rawInput . "{\"path\":\"foo.el\"}"))))
    (should-not (emagent-acp--tool-call-block-spec update)))
  ;; A lone path is never a shell block, even with an execute kind.
  (let ((update '((title . "Read") (kind . "execute")
                  (rawInput . "{\"path\":\"/Users/me/dev/src/db/mod.rs\"}"))))
    (should-not (emagent-acp--tool-call-block-spec update)))
  ;; File write stays a compact arrow line too.
  (let ((update '((title . "Edit File")
                  (rawInput . (("edits" . (((path . "bar.el")))))))))
    (should-not (emagent-acp--tool-call-block-spec update)))
  ;; Multi-line detail on a non-shell tool -> text block.
  (let ((update '((title . "custom-tool")
                  (rawInput . "{\"description\":\"line one\\nline two\"}"))))
    (should (equal '("text" . "line one\nline two")
                   (emagent-acp--tool-call-block-spec update)))))

(ert-deftest emagent-acp-session-test-tool-call-block-spec-edit-diff ()
  "An auto-allowed edit renders a diff block, like the permission prompt.
The change is reconstructable from rawInput, so both the agent allow-list and
the emagent gate can show what was edited instead of a bare arrow line."
  (let* ((dir (emagent-test--temp-directory))
         (path (expand-file-name "sample.py" dir)))
    (unwind-protect
        (progn
          (write-region "old line\n" nil path)
          (let ((spec (emagent-acp--tool-call-block-spec
                       `((toolCallId . "tool_write")
                         (title . "Edit File")
                         (kind . "edit")
                         (rawInput . ((path . ,path)
                                      (content . "new line\n")))))))
            (should (equal "diff" (car spec)))
            (should (string-match-p "new line" (cdr spec)))))
      (ignore-errors (delete-directory dir t)))))

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
          (lambda (_id label &rest _) (setq shown label))))
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
          (emagent-acp-cursor--resolve-tool-from-store state "tool_y")))
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
          (lambda (_id label &rest _) (setq shown label))))
      (emagent-acp--on-tool-call
       state '((toolCallId . "tool_z") (title . "Read File") (rawInput . ())))
      (should (null shown))
      (emagent-acp--on-tool-call
       state '((toolCallId . "tool_z") (title . "Read File")
               (status . "completed") (rawInput . ())))
      (should (null shown))
      (let ((store (lambda (_sid _id) '("Read" . (("path" . "done.el"))))))
        (cl-letf (((symbol-function 'emagent-cursor-tool-call-from-store) store))
          (emagent-acp-cursor--resolve-tool-from-store state "tool_z")))
      (should (string-match-p "done.el" shown))
      (should (= 0 (hash-table-count (map-elt state :tool-call-pending)))))))

(ert-deftest emagent-acp-session-test-cursor-generic-title-stays-hidden ()
  (let ((state (emagent-test--make-acp-state))
        (shown nil))
    (puthash :session-id "sess" state)
    (emagent-test--with-mocks
        (((symbol-function 'emagent-acp--agent-launch-string)
          (lambda (_state) "cursor-agent acp"))
         ((symbol-function 'emagent-cursor-tool-call-from-store)
          (lambda (_sid _id) nil))
         ((symbol-function 'emagent-chat-show-tool-call)
          (lambda (_id label &rest _) (setq shown label))))
      (emagent-acp--on-tool-call
       state '((toolCallId . "tool_w") (title . "Read File") (rawInput . ())))
      (should (null shown))
      (dotimes (_ emagent-acp-cursor--tool-resolve-max-attempts)
        (emagent-acp-cursor--resolve-tool-from-store state "tool_w"))
      (should (null shown)))))

(ert-deftest emagent-acp-session-test-tool-call-redundant-detail ()
  (should (emagent-acp--tool-call-redundant-detail-p "git_log" "git_log"))
  (should (emagent-acp--tool-call-redundant-detail-p "emagent-git_log" "git_log"))
  (should-not (emagent-acp--tool-call-redundant-detail-p "compile" "make test")))

(ert-deftest emagent-acp-session-test-complete-prompt-defers-for-permission ()
  (let ((state (emagent-test--make-acp-state)))
    (map-put! state :busy t)
    (map-put! state :permission-queue '((id . "req")))
    (emagent-acp--complete-prompt state '((usage . nil)))
    (should (map-elt state :deferred-complete-response))
    (should (map-elt state :busy))))

(ert-deftest emagent-acp-session-test-permission-question-line ()
  (let* ((cmd "make test")
         (args (make-hash-table :test 'equal)))
    (puthash "command" cmd args)
    (should (string= cmd
                     (emagent-acp--permission-question-line
                      `((params . ((title . "Allow compile?")
                                    (toolCall . ((toolCallId . "tool_compile")
                                                 (arguments . ,args))))))))))
  (should (string= "compile"
                   (emagent-acp--permission-question-line
                    '((params . ((title . "Allow compile?")
                                 (toolCall . ((title . "compile"))))))))))

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

(ert-deftest emagent-acp-session-test-match-model-id-normalized ()
  (let* ((state (emagent-test--make-acp-state))
         (models '((availableModels . [((modelId . "grok-4.3[context=200k]")
                                        (name . "grok-4.3"))]))))
    (should (string= "grok-4.3[context=200k]"
                     (emagent-acp--match-model-id "grok-4.3" state models)))
    (should (string= "default[]"
                     (emagent-acp--match-model-id "auto" state
                      '((availableModels . [((modelId . "default[]")
                                              (name . "Auto"))])))))))

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

(ert-deftest emagent-acp-session-test-session-system-prompt-compressed ()
  (let ((summary "decided to use async runners"))
    (should (string-match-p "Compressed prior conversation context"
                            (emagent-acp--session-system-prompt summary)))
    (should (string-match-p summary (emagent-acp--session-system-prompt summary)))
    (should (string= (emagent-acp--system-prompt)
                     (emagent-acp--session-system-prompt nil)))
    (should (string= (emagent-acp--system-prompt)
                     (emagent-acp--session-system-prompt "")))))

(ert-deftest emagent-acp-session-test-mcp-http-capable-p ()
  (should (emagent-acp--mcp-http-capable-p
           '((agentCapabilities . ((mcpCapabilities . ((http . t))))))))
  (should-not (emagent-acp--mcp-http-capable-p
               '((agentCapabilities . ((mcpCapabilities . ((http . :false)))))))))

(ert-deftest emagent-acp-session-test-fatal-agent-error-p ()
  (should (emagent-acp--fatal-agent-error-p "request timed out"))
  (should (emagent-acp--fatal-agent-error-p "failed with status 500"))
  (should-not (emagent-acp--fatal-agent-error-p "still working")))

(ert-deftest emagent-acp-session-test-retriable-prompt-error-p ()
  (should (emagent-acp--retriable-prompt-error-p
           "Error: RetriableError: [unavailable] getaddrinfo ENOTFOUND api2.cursor.sh"))
  (should (emagent-acp--retriable-prompt-error-p "read ECONNRESET"))
  (should (emagent-acp--retriable-prompt-error-p "socket hang up"))
  (should-not (emagent-acp--retriable-prompt-error-p "failed with status 400"))
  (should-not (emagent-acp--retriable-prompt-error-p nil)))

(ert-deftest emagent-acp-session-test-agent-error-only-response-p ()
  (let ((state (emagent-test--make-acp-state)))
    ;; A turn whose whole output is a transient network error, with no
    ;; content or tool calls, is safe to re-issue.
    (puthash :assistant-text
             "Error: RetriableError: [unavailable] getaddrinfo ENOTFOUND api2.cursor.sh"
             state)
    (should (emagent-acp--agent-error-only-response-p state))
    ;; A real answer is never re-issued, even if it mentions a network word.
    (puthash :assistant-text
             "I finished the task; there was no network error along the way."
             state)
    (should-not (emagent-acp--agent-error-only-response-p state))
    ;; An error turn that also did tool work is left alone.
    (puthash :assistant-text "RetriableError: socket hang up" state)
    (puthash "call-1" "shell" (map-elt state :tool-call-titles))
    (should-not (emagent-acp--agent-error-only-response-p state))
    (clrhash (map-elt state :tool-call-titles))
    ;; Compression turns are never treated as retriable errors.
    (puthash :compress-pending t state)
    (should-not (emagent-acp--agent-error-only-response-p state))))

(ert-deftest emagent-acp-session-test-prompt-retry-delay ()
  (let ((emagent-acp-prompt-retry-base-delay 1.5))
    (should (= (emagent-acp--prompt-retry-delay 1) 1.5))
    (should (= (emagent-acp--prompt-retry-delay 2) 3.0))
    (should (= (emagent-acp--prompt-retry-delay 3) 6.0))))

(ert-deftest emagent-acp-session-test-tool-call-displayable-p ()
  (let* ((state (emagent-test--make-acp-state))
         (emagent-acp--tool-call-weak-details '("tool" "Tool" "running" "pending")))
    (puthash :provider 'cursor state)
    (should (emagent-acp--tool-call-displayable-p
             state '((title . "compile") (rawInput . "{\"command\":\"make test\"}"))))
    ;; A meaningful detail is displayable even when it duplicates the title;
    ;; redundancy is collapsed later in label-building, not hidden here.
    (should (emagent-acp--tool-call-displayable-p
             state '((title . "git_log") (rawInput . "{\"args\":\"git_log\"}"))))
    (should (emagent-acp--tool-call-displayable-p
             state '((title . "compile") (rawInput . "{}"))))
    (should-not (emagent-acp--tool-call-displayable-p
                 state '((title . "") (rawInput . "{}"))))
    (should-not (emagent-acp--tool-call-displayable-p
                 state '((title . "Read File") (rawInput . "{}"))))))

(ert-deftest emagent-acp-session-test-deferred-complete-fires-after-permission-drain ()
  "Deferred complete response fires once permission queue empties."
  (let* ((state (emagent-test--make-acp-state))
         (completed nil)
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))]))))))
    (map-put! state :busy t)
    (map-put! state :deferred-complete-response '((usage . ((totalTokens . 5)))))
    (map-put! state :permission-queue (list request))
    (let ((emagent-acp-auto-approve-permissions nil))
      (emagent-test--with-mocks
          (((symbol-function 'run-at-time) #'emagent-test--run-at-time-immediately)
           ((symbol-function 'emagent-tools--buttons-prompt)
            (emagent-test--mock-buttons-prompt :allow-once))
           ((symbol-function 'emagent-acp-send-response) (lambda (&rest _args) nil))
           ((symbol-function 'emagent-acp--render-prompt-response)
            (lambda (_state) (setq completed t))))
        (emagent-acp--drain-permission-queue state)
        (should completed)
        (should-not (map-elt state :busy))
        (should (null (map-elt state :deferred-complete-response)))))))

(ert-deftest emagent-acp-session-test-permission-question-line-ext ()
  ;; title without tool detail -> strips "Allow " prefix and trailing "?"
  (should (string= "compile"
                   (emagent-acp--permission-question-line
                    '((params . ((title . "Allow compile?")))))))
  ;; no title -> fallback text
  (should (string= "Permission request"
                   (emagent-acp--permission-question-line
                    '((params . ((title . ""))))))))

(ert-deftest emagent-acp-session-test-drain-permission-queue-clears-busy-on-error ()
  "drain-permission-queue-now' always clears :permission-busy even on error."
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))]))))))
    (map-put! state :permission-queue (list request))
    (let ((emagent-acp-auto-approve-permissions nil))
      (emagent-test--with-mocks
          (((symbol-function 'run-at-time) #'emagent-test--run-at-time-immediately)
           ((symbol-function 'emagent-acp-send-response) (lambda (&rest _args) nil))
           ((symbol-function 'emagent-acp--handle-one-permission)
            (lambda (&rest _args) (error "simulated permission crash"))))
        (emagent-acp--drain-permission-queue-now state)
        (should-not (map-elt state :permission-busy))
        (should (null (map-elt state :permission-queue)))))))

(ert-deftest emagent-acp-session-test-maybe-recover-stall-drains-queue ()
  "maybe-recover-stall' schedules permission drain when queue is nonempty."
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))]))))))
    (map-put! state :ready t)
    (map-put! state :busy nil)
    (map-put! state :permission-queue (list request))
    (let ((emagent-acp-auto-approve-permissions nil)
          (scheduled nil))
      (emagent-test--with-mocks
          (((symbol-function 'run-at-time)
            (lambda (_time _repeat fn) (setq scheduled t) (funcall fn) nil))
           ((symbol-function 'emagent-acp-send-response) (lambda (&rest _args) nil))
           ((symbol-function 'emagent-tools--buttons-prompt)
            (emagent-test--mock-buttons-prompt :allow-once)))
        (emagent-acp--maybe-recover-stall state)
        (should scheduled)
        (should (null (map-elt state :permission-queue)))))))

(provide 'emagent-acp-session-test)

;;; emagent-acp-session-test.el ends here
