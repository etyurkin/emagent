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
      (should (member 'cursor-agent-cli (emagent-acp-state-external-tool-gate-reasons state))))))

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
                  (emagent-acp-state-external-tool-gate-reasons state)))))

;;;; Permission helpers

(ert-deftest emagent-acp-session-test-permission-option-allow-p ()
  (should (emagent-acp--permission-option-allow-p '((kind . "allow_once"))))
  (should (emagent-acp--permission-option-allow-p '((optionId . "yes"))))
  (should-not (emagent-acp--permission-option-allow-p '((kind . "deny")))))

(ert-deftest emagent-acp-session-test-permission-acp-allow-id-never-always ()
  (let ((options `[((optionId . "allow_always") (kind . "allow_always"))
                   ((optionId . "allow_once") (kind . "allow_once"))]))
    (should (string= "allow_once" (emagent-acp--permission-acp-allow-id options)))))

(ert-deftest emagent-acp-session-test-permission-acp-allow-id-fail-closed ()
  "When the agent offers only allow_always (no one-shot), return nil so the
request is cancelled rather than escalated to a permanent agent-side grant."
  (let ((options `[((optionId . "allow_always") (kind . "allow_always"))
                   ((optionId . "reject") (kind . "reject"))]))
    (should (null (emagent-acp--permission-acp-allow-id options)))))

(defun emagent-test--exec-fingerprint (command)
  "Return the execute fingerprint for shell COMMAND."
  (let ((args (make-hash-table :test 'equal)))
    (puthash "command" command args)
    (emagent-acp--permission-fingerprint `((kind . "execute") (arguments . ,args)))))

(ert-deftest emagent-acp-session-test-permission-fingerprint ()
  ;; A non-subcommand program keys on the program name only.
  (should (string= "execute:ls" (emagent-test--exec-fingerprint "ls -la /tmp")))
  ;; A subcommand program keys on program:subverb, so different verbs get
  ;; distinct grants but argument variations share one.
  (should (string= "execute:make:test" (emagent-test--exec-fingerprint "make test")))
  (should (string= "execute:git:status" (emagent-test--exec-fingerprint "git status")))
  (should (string= "execute:git:push"
                   (emagent-test--exec-fingerprint "git push --force origin main")))
  ;; Leading short flags are skipped when finding the sub-verb.
  (should (string= "execute:npm:install"
                   (emagent-test--exec-fingerprint "npm install left-pad")))
  ;; A global flag WITH A VALUE (`git -C DIR', `kubectl -n NS') must not make
  ;; the value masquerade as the subcommand — else push/status/delete/get
  ;; collide and a grant for one auto-approves the others.
  (should (string= "execute:git:push"
                   (emagent-test--exec-fingerprint "git -C /tmp push --force")))
  (should-not (string= (emagent-test--exec-fingerprint "git -C /tmp status")
                       (emagent-test--exec-fingerprint "git -C /tmp push")))
  (should-not (string= (emagent-test--exec-fingerprint "kubectl -n prod get")
                       (emagent-test--exec-fingerprint "kubectl -n prod delete")))
  ;; A value-less long flag is handled: the subcommand still resolves.
  (should (string= "execute:git:status"
                   (emagent-test--exec-fingerprint "git --no-pager status")))
  ;; A grant for `git status' does not match `git push'.
  (should-not (string= (emagent-test--exec-fingerprint "git status")
                       (emagent-test--exec-fingerprint "git push"))))

(ert-deftest emagent-acp-session-test-permission-fingerprint-compound ()
  "Compound commands that differ only in path/glob arguments — or in how a
pipeline or `VAR=$(...)' assignment is composed — share one fingerprint, so a
single session grant covers both."
  ;; The motivating case: identical structure, different jar path + a leading
  ;; comment and a `-i' grep flag.  These must collapse to one grant.
  (let ((a (concat "JAR=$(find ~/.m2 -path \"*common*3.0.29*\" -name \"*.jar\" "
                   "! -name \"*sources*\" 2>/dev/null | head -1)\n"
                   "echo \"JAR: $JAR\"\n"
                   "jar tf \"$JAR\" 2>/dev/null | grep \"Foo\\|Provider\" | head -20"))
        (b (concat "# probe another artifact\n"
                   "JAR=$(find ~/.m2 -path \"*impl*\" -name \"*.jar\" "
                   "! -name \"*sources*\" 2>/dev/null | head -1)\n"
                   "echo \"JAR: $JAR\"\n"
                   "jar tf \"$JAR\" 2>/dev/null | grep -i \"foo\\|provider\" | head -20")))
    (should (string= (emagent-test--exec-fingerprint a)
                     (emagent-test--exec-fingerprint b))))
  ;; A simple pipeline collapses across path-only differences too.
  (should (string= (emagent-test--exec-fingerprint "find . -name '*.a' | head -1")
                   (emagent-test--exec-fingerprint "find /x -name '*.b' | head -1")))
  ;; Different tool sets still get distinct grants.
  (should-not (string= (emagent-test--exec-fingerprint "cat a | head")
                       (emagent-test--exec-fingerprint "rm a | head"))))

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

(ert-deftest emagent-acp-session-test-finalize-no-prompt-returns-nil ()
  "Finalizing with nothing in flight returns nil, not a `no-catch' error: the
function must be a cl-defun for its cl-return-from to have an enclosing block."
  (let ((emagent-acp--session (emagent-test--make-acp-state)))
    (should (null (emagent-acp--finalize-in-flight-prompt))))
  ;; And with NO session at all (buffer never connected): must not signal
  ;; wrong-type-argument on a struct accessor of nil.
  (let ((emagent-acp--session nil))
    (should (null (emagent-acp--finalize-in-flight-prompt)))))

(ert-deftest emagent-acp-session-test-turn-phase ()
  "The turn phase derives idle/streaming/finalizing/done from the turn flags."
  (let ((state (emagent-test--make-acp-state)))
    (should (eq 'idle (emagent-acp--turn-phase state)))
    (setf (emagent-acp-state-busy state) t)
    (should (eq 'streaming (emagent-acp--turn-phase state)))
    (setf (emagent-acp-state-busy state) nil)
    (setf (emagent-acp-state-prompt-finishing state) t)
    (should (eq 'finalizing (emagent-acp--turn-phase state)))
    (setf (emagent-acp-state-prompt-finalized state) t)
    (should (eq 'done (emagent-acp--turn-phase state)))))

(ert-deftest emagent-acp-session-test-permission-auto-allowed-session ()
  (emagent-test--with-mocks
      (((symbol-function 'emagent-permissions-global-fingerprints) (lambda () nil))
       ((symbol-function 'emagent-permissions-session-fingerprints) (lambda (_) nil)))
    (let ((state (emagent-test--make-acp-state)))
      (setf (emagent-acp-state-permission-auto-allow state) '("execute:make"))
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
        (should (member "execute:make:test" (emagent-permissions-global-fingerprints)))
        (should-not (member "execute:make:test" (or (emagent-acp-state-permission-auto-allow state) nil)))))))

(ert-deftest emagent-acp-session-test-permission-allow-session-persists-by-session-id ()
  (let ((perms-dir (emagent-test--temp-directory)))
    (let ((emagent-permissions-directory perms-dir)
          (emagent-permissions--cache (make-hash-table :test 'equal)))
      (let* ((state (emagent-test--make-acp-state))
             (session-id "sess-abc")
             (fingerprint "execute:make"))
        (setf (emagent-acp-state-session-id state) session-id)
        (emagent-acp--permission-apply-choice state fingerprint nil :allow-session)
        (should (equal '("execute:make")
                       (emagent-permissions-session-fingerprints session-id)))
        (should-not (emagent-permissions-session-fingerprints "other-session"))
        (setq emagent-acp--session nil)
        (let ((fresh (emagent-test--make-acp-state)))
          (setf (emagent-acp-state-session-id fresh) session-id)
          (emagent-acp--hydrate-session-permissions fresh session-id)
          (should (member fingerprint (emagent-acp-state-permission-auto-allow fresh))))))))

(ert-deftest emagent-acp-session-test-permission-allow-all-persists-by-session-id ()
  (let ((perms-dir (emagent-test--temp-directory)))
    (let ((emagent-permissions-directory perms-dir)
          (emagent-permissions--cache (make-hash-table :test 'equal)))
      (let* ((state (emagent-test--make-acp-state))
             (session-id "sess-all"))
        (setf (emagent-acp-state-session-id state) session-id)
        (emagent-acp--permission-apply-choice state nil nil :allow-all)
        (should (emagent-permissions-session-auto-approve-p session-id))
        (setq emagent-acp--session nil)
        (let ((fresh (emagent-test--make-acp-state)))
          (emagent-acp--hydrate-session-permissions fresh session-id)
          (should (emagent-acp-state-session-auto-approve fresh)))))))

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
       ((symbol-function 'emagent-session-allowed-permissions) (lambda () nil))
       ((symbol-function 'emagent-session-project-directory) (lambda () nil))
       ((symbol-function 'emagent-permissions-project-fingerprints) (lambda (_) nil)))
    (let ((state (emagent-test--make-acp-state)))
      (should (eq :allow-always
                  (emagent-acp--permission-stored-auto-choice
                   state "tool:Allow web search?" nil)))
      (setf (emagent-acp-state-session-auto-approve state) t)
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

(ert-deftest emagent-acp-session-test-emagent-tool-pending-untagged ()
  "A pending emagent tool call is untagged: it may await a permission prompt.
Once it runs (in_progress), its permission was granted and the tag applies."
  (let* ((state (emagent-test--make-acp-state))
         (merged '((toolCallId . "ss1")
                   (title . "mcp_emagent_structural_substitute")))
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
       state "ss1" 'edit merged "substitute: foo.el" "pending")
      (should (string= "substitute: foo.el" shown))
      (setq shown nil)
      (emagent-acp--emit-tool-call-display
       state "ss1" 'edit merged "substitute: foo.el" "in_progress")
      (should (string= "substitute: foo.el (Allow: Emacs)" shown)))))

(ert-deftest emagent-acp-session-test-merge-keeps-raw-input-across-status ()
  "Empty rawInput on in_progress must not wipe a stored web-search query."
  (let* ((state (emagent-test--make-acp-state))
         (id "web1")
         (with-query `((toolCallId . ,id)
                       (title . "Web Search")
                       (status . "pending")
                       (rawInput . ((searchTerm . "Swift SIGABRT")))))
         (status-only `((toolCallId . ,id)
                        (title . "Web Search")
                        (status . "in_progress")
                        (rawInput . nil))))
    (let ((merged1 (emagent-acp--merged-tool-call-update state with-query)))
      (should (string-match-p "Swift SIGABRT"
                              (or (emagent-acp--tool-call-detail merged1) "")))
      (should (string-match-p "Swift SIGABRT"
                              (emagent-acp--tool-call-label merged1))))
    (let ((merged2 (emagent-acp--merged-tool-call-update state status-only)))
      (should (string-match-p "Swift SIGABRT"
                              (or (emagent-acp--tool-call-detail merged2) "")))
      (should (string-match-p "Swift SIGABRT"
                              (emagent-acp--tool-call-label merged2))))))

(ert-deftest emagent-acp-session-test-emit-keeps-richer-web-search-label ()
  "in_progress Allow tag must not drop a previously shown search query."
  (let* ((state (emagent-test--make-acp-state))
         (merged '((toolCallId . "web2") (title . "Web Search")))
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
          (lambda (_) (current-buffer)))
         ((symbol-function 'emagent-acp--tool-call-block-spec)
          (lambda (&rest _) nil)))
      (emagent-acp--emit-tool-call-display
       state "web2" 'search merged
       "Web search: Swift \"Object was retained too many times\" SIGABRT"
       "pending")
      (should (string-match-p "Swift" shown))
      (emagent-acp--emit-tool-call-display
       state "web2" 'search merged "Web Search" "in_progress")
      (should (string-match-p "Swift" shown))
      (should (string-match-p "(Allow: Agent)" shown)))))

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
       state "g1" 'search merged "Grep: pattern" "pending")
      (should (string= "Grep: pattern" shown))
      (setq shown nil)
      (emagent-acp--emit-tool-call-display
       state "g1" 'search merged "Grep: pattern" "in_progress")
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
        (should (= 1 (length (emagent-acp-state-permission-queue state))))))))

(ert-deftest emagent-acp-session-test-permission-handler-error-cancels ()
  "When the permission handler errors, the popped request is replied to with
`cancelled' so the agent does not hang, and busy is released."
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req-err")
                    (params . ((title . "Allow?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (responses nil))
    (setf (emagent-acp-state-permission-queue state) (list request))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-acp--handle-one-permission)
          (lambda (&rest _) (error "boom in handler")))
         ((symbol-function 'emagent-acp-send-response)
          (cl-function (lambda (&key response &allow-other-keys)
                         (push response responses))))
         ((symbol-function 'emagent-acp--refresh-mode-line) (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--schedule-permission-drain)
          (lambda (&rest _) nil)))
      (emagent-acp--drain-permission-queue-now state)
      (should (null (emagent-acp-state-permission-busy state)))
      (should (= 1 (length responses)))
      (should (equal "cancelled"
                     (map-nested-elt (car responses) '(:result outcome outcome)))))))

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
        (should (null (emagent-acp-state-permission-queue state)))))))

(ert-deftest emagent-acp-session-test-permission-not-deferred-for-tool-resolve ()
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (prompted nil))
    (let ((emagent-acp-auto-approve-permissions nil))
      (setf (emagent-acp-state-tool-resolve-queue state) '("tool_x"))
      (setf (emagent-acp-state-tool-resolve-worker state) t)
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
        (should (null (emagent-acp-state-permission-queue state)))))))

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
      (setf (emagent-acp-state-permission-queue state) (list request))
      (setf (emagent-acp-state-tool-resolve-queue state) '("tool_x"))
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
        (should (null (emagent-acp-state-permission-queue state)))))))

(ert-deftest emagent-acp-session-test-permission-auto-approve-deep-queue ()
  "Auto-approve drains a long queue in batches (no Lisp stack overflow).

Cursor MCP auth/tool prompts can enqueue dozens of
`session/request_permission' messages.  Recursive on-complete drain used to
SIGSEGV Emacs; keep max-lisp-eval-depth artificially low to catch regressions.
Continuation uses `run-at-time'; pump those timers iteratively."
  (let* ((state (emagent-test--make-acp-state))
         (n 60)
         (responded 0)
         (pending nil)
         (emagent-acp-auto-approve-permissions t)
         (emagent-acp-permission-drain-batch-size 8)
         (max-lisp-eval-depth 40)
         (max-specpdl-size 600))
    (setf (emagent-acp-state-permission-queue state)
          (cl-loop for i from 1 to n
                   collect `((id . ,(format "req-%d" i))
                             (params . ((title . "Allow MCP?")
                                        (options . [((optionId . "allow_once")
                                                     (kind . "allow_once"))]))))))
    (emagent-test--with-mocks
        (((symbol-function 'run-at-time)
          (lambda (_time _repeat fn)
            (setq pending (append pending (list fn)))
            nil))
         ((symbol-function 'emagent-acp-send-response)
          (lambda (&rest _) (cl-incf responded)))
         ((symbol-function 'emagent-acp--show-permission-decision)
          (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--refresh-mode-line)
          (lambda (&rest _) nil)))
      (emagent-acp--drain-permission-queue state)
      (while pending
        (funcall (pop pending)))
      (should (= responded n))
      (should (null (emagent-acp-state-permission-queue state))))))

(ert-deftest emagent-acp-session-test-permission-auto-approve-batches ()
  "One drain turn handles at most `emagent-acp-permission-drain-batch-size'."
  (let* ((state (emagent-test--make-acp-state))
         (responded 0)
         (scheduled 0)
         (emagent-acp-auto-approve-permissions t)
         (emagent-acp-permission-drain-batch-size 2))
    (setf (emagent-acp-state-permission-queue state)
          (cl-loop for i from 1 to 5
                   collect `((id . ,(format "req-%d" i))
                             (params . ((title . "Allow MCP?")
                                        (options . [((optionId . "allow_once")
                                                     (kind . "allow_once"))]))))))
    (emagent-test--with-mocks
        (((symbol-function 'run-at-time)
          (lambda (_time _repeat _fn)
            (cl-incf scheduled)
            nil))
         ((symbol-function 'emagent-acp-send-response)
          (lambda (&rest _) (cl-incf responded)))
         ((symbol-function 'emagent-acp--show-permission-decision)
          (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--refresh-mode-line)
          (lambda (&rest _) nil)))
      (emagent-acp--drain-permission-queue-now state)
      (should (= responded 2))
      (should (= scheduled 1))
      (should (= (length (emagent-acp-state-permission-queue state)) 3)))))

(ert-deftest emagent-acp-session-test-permission-drains-after-tool-resolve ()
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req3")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))])))))
         (prompted nil))
    (let ((emagent-acp-auto-approve-permissions nil))
      (setf (emagent-acp-state-permission-queue state) (list request))
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
        (should (null (emagent-acp-state-permission-queue state)))))))

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

;;;; Agent-scheduled wakeup (ScheduleWakeup)

(defun emagent-acp-session-test--wakeup-update (args &optional title)
  "Return a ScheduleWakeup tool-call update with ARGS as rawInput."
  (let ((raw (make-hash-table :test 'equal)))
    (dolist (pair args)
      (puthash (car pair) (cdr pair) raw))
    `((toolCallId . "toolu_wakeup")
      (title . ,(or title "ScheduleWakeup"))
      (status . "completed")
      (rawInput . ,raw))))

(ert-deftest emagent-acp-session-test-wakeup-captured-and-armed ()
  "A ScheduleWakeup call is captured and armed when the turn completes."
  (let ((state (emagent-acp--state-create))
        (emagent-acp-honor-schedule-wakeup t))
    (emagent-acp--capture-schedule-wakeup
     state (emagent-acp-session-test--wakeup-update
            '(("delaySeconds" . 120) ("reason" . "waiting on test run")
              ("prompt" . "check the run again"))))
    (let ((request (emagent-acp-state-wakeup-request state)))
      (should request)
      (should (= 120 (plist-get request :delay)))
      (should (equal "check the run again" (plist-get request :prompt))))
    (unwind-protect
        (progn
          (emagent-acp--arm-wakeup state)
          (should (timerp (emagent-acp-state-wakeup-timer state)))
          (should-not (emagent-acp-state-wakeup-request state)))
      (emagent-acp--cancel-wakeup state))))

(ert-deftest emagent-acp-session-test-wakeup-stop-cancels ()
  "A ScheduleWakeup stop call cancels the pending request and timer."
  (let ((state (emagent-acp--state-create))
        (emagent-acp-honor-schedule-wakeup t))
    (emagent-acp--capture-schedule-wakeup
     state (emagent-acp-session-test--wakeup-update '(("delaySeconds" . 60))))
    (emagent-acp--arm-wakeup state)
    (emagent-acp--capture-schedule-wakeup
     state (emagent-acp-session-test--wakeup-update '(("stop" . t))))
    (should-not (emagent-acp-state-wakeup-timer state))
    (should-not (emagent-acp-state-wakeup-request state))))

(ert-deftest emagent-acp-session-test-wakeup-superseded-by-new-turn ()
  "A manual prompt (turn begin) cancels any pending or armed wakeup."
  (let ((state (emagent-acp--state-create))
        (emagent-acp-honor-schedule-wakeup t))
    (emagent-acp--capture-schedule-wakeup
     state (emagent-acp-session-test--wakeup-update '(("delaySeconds" . 60))))
    (emagent-acp--arm-wakeup state)
    (should (timerp (emagent-acp-state-wakeup-timer state)))
    (emagent-acp--turn-begin state)
    (should-not (emagent-acp-state-wakeup-timer state))
    (should-not (emagent-acp-state-wakeup-request state))))

(ert-deftest emagent-acp-session-test-wakeup-disabled ()
  "With `emagent-acp-honor-schedule-wakeup' nil, nothing is captured."
  (let ((state (emagent-acp--state-create))
        (emagent-acp-honor-schedule-wakeup nil))
    (emagent-acp--capture-schedule-wakeup
     state (emagent-acp-session-test--wakeup-update '(("delaySeconds" . 60))))
    (should-not (emagent-acp-state-wakeup-request state))))

(ert-deftest emagent-acp-session-test-wakeup-fire-sends-prompt ()
  "Firing a wakeup inserts a user turn and calls the buffer's send callback."
  (let ((state (emagent-acp--state-create))
        (sent nil)
        (token nil))
    (with-temp-buffer
      (setq-local emagent-chat--on-send
                  (lambda (text)
                    (setq sent text
                          token emagent-chat--send-token)))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-acp--chat-buffer)
            (lambda (_) (current-buffer)))
           ((symbol-function 'emagent-chat--insert-user-heading-with-text)
            (lambda (_text) (point)))
           ((symbol-function 'emagent-chat--begin-response)
            (lambda (&rest _) nil)))
        (emagent-acp--fire-wakeup state "check the run again")
        (should (equal "check the run again" sent))
        ;; Regression: without send-pending-begin, emagent-acp-send's
        ;; send-active-p gate silently drops Build/wakeup turns.
        (should emagent-chat--send-pending)
        (should token)
        (should (emagent-chat--send-active-p token))))))

(ert-deftest emagent-acp-session-test-wakeup-fire-skips-when-busy ()
  "Firing a wakeup does nothing when a prompt is already running."
  (let ((state (emagent-acp--state-create))
        (sent nil))
    (setf (emagent-acp-state-busy state) t)
    (with-temp-buffer
      (setq-local emagent-chat--on-send (lambda (text) (setq sent text)))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-acp--chat-buffer)
            (lambda (_) (current-buffer))))
        (emagent-acp--fire-wakeup state "should not send")
        (should-not sent)))))

(ert-deftest emagent-acp-session-test-plan-build-fire-sends-quietly ()
  "Plan Build sends to the agent without inventing a * user> heading."
  (let ((state (emagent-acp--state-create))
        (sent nil)
        (token nil)
        (headed nil)
        (began nil)
        (scrolled nil))
    (with-temp-buffer
      (setq-local emagent-chat--on-send
                  (lambda (text)
                    (setq sent text
                          token emagent-chat--send-token)))
      (setq-local emagent-chat--defer-user-stub t)
      (setq-local emagent-chat--follow-output nil)
      (emagent-test--with-mocks
          (((symbol-function 'emagent-acp--chat-buffer)
            (lambda (_) (current-buffer)))
           ((symbol-function 'emagent-chat--insert-user-heading-with-text)
            (lambda (text) (setq headed text) (point)))
           ((symbol-function 'emagent-chat--user-zone-start)
            (lambda () (point-max)))
           ((symbol-function 'emagent-chat--begin-response)
            (lambda (&rest _)
              (setq began t
                    emagent-chat--follow-output t)))
           ((symbol-function 'emagent-chat--ensure-follow-window)
            (lambda (&rest _)
              (setq scrolled t
                    emagent-chat--follow-output t))))
        (emagent-acp--fire-plan-build
         state "Build the approved plan \"X\" (file:///tmp/x.plan.md).")
        (should (string-match-p "Build the approved plan" sent))
        (should-not headed)
        (should began)
        (should scrolled)
        (should emagent-chat--follow-output)
        (should-not emagent-chat--defer-user-stub)
        (should emagent-chat--send-pending)
        (should token)
        (should (emagent-chat--send-active-p token))))))

(ert-deftest emagent-acp-session-test-wakeup-abort-clears ()
  "Aborting a turn drops a captured ScheduleWakeup so it cannot arm later."
  (let ((state (emagent-acp--state-create))
        (emagent-acp-honor-schedule-wakeup t))
    (setf (emagent-acp-state-busy state) t)
    (emagent-acp--capture-schedule-wakeup
     state (emagent-acp-session-test--wakeup-update '(("delaySeconds" . 60))))
    (should (emagent-acp-state-wakeup-request state))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-acp--chat-buffer) (lambda (_) nil))
         ((symbol-function 'emagent-acp--refresh-mode-line) (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--flush-thought-buffer) (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--clear-prompt-watchdog) (lambda (&rest _) nil))
         ((symbol-function 'emagent-acp--cancel-prompt-render) (lambda (&rest _) nil)))
      (emagent-acp--abort-prompt state "boom"))
    (should-not (emagent-acp-state-wakeup-request state))
    (should-not (emagent-acp-state-wakeup-timer state))))

(ert-deftest emagent-acp-session-test-wakeup-interrupt-clears ()
  "Finalize-in-flight (interrupt) cancels a pending or armed wakeup."
  (let ((state (emagent-acp--state-create))
        (emagent-acp-honor-schedule-wakeup t)
        (emagent-acp--session nil))
    (emagent-acp--capture-schedule-wakeup
     state (emagent-acp-session-test--wakeup-update '(("delaySeconds" . 60))))
    (emagent-acp--arm-wakeup state)
    (should (timerp (emagent-acp-state-wakeup-timer state)))
    (setf (emagent-acp-state-busy state) t)
    (setq emagent-acp--session state)
    (unwind-protect
        (emagent-test--with-mocks
            (((symbol-function 'emagent-acp--clear-prompt-watchdog)
              (lambda (&rest _) nil))
             ((symbol-function 'emagent-acp--cancel-prompt-render)
              (lambda (&rest _) nil))
             ((symbol-function 'emagent-acp--flush-thought-buffer)
              (lambda (&rest _) nil))
             ((symbol-function 'emagent-acp--reset-permission-gate)
              (lambda (&rest _) nil))
             ((symbol-function 'emagent-acp--render-prompt-response)
              (lambda (&rest _) nil))
             ((symbol-function 'emagent-acp--refresh-mode-line)
              (lambda (&rest _) nil)))
          (should (emagent-acp--finalize-in-flight-prompt "stopped"))
          (should-not (emagent-acp-state-wakeup-timer state))
          (should-not (emagent-acp-state-wakeup-request state)))
      (emagent-acp--cancel-wakeup state)
      (setq emagent-acp--session nil))))

;;;; Tool-call display

(ert-deftest emagent-acp-session-test-edit-patch-string-keeps-blank-lines ()
  "The fallback edit preview must not swallow empty lines.
Blank lines separating functions are content; OMIT-NULLS in the line
split silently dropped them."
  (let ((patch (emagent-acp--tool-call-edit-patch-string
                "/tmp/x.clef" nil "(defun f (a) a)\n\n(defun g (b) b)\n")))
    (should (string-match-p "\\+(defun f (a) a)\n\\+\n\\+(defun g (b) b)" patch))
    ;; A single trailing newline must not become a spurious empty + line.
    (should (string-suffix-p "+(defun g (b) b)" patch)))
  (let ((patch (emagent-acp--tool-call-edit-patch-string
                "/tmp/x.clef" "(old)\n\n(lines)\n" "(new)\n\n(lines)\n")))
    (should (string-match-p "-(old)\n-\n-(lines)" patch))
    (should (string-match-p "\\+(new)\n\\+\n\\+(lines)" patch))))

(ert-deftest emagent-acp-session-test-edit-diff-real-diff-cached-post-write ()
  "A post-write re-render must reuse the real diff computed pre-write.
The agent writes the file right after permission is granted; later status
updates re-render the same tool call against the already-edited file, where
diffing yields nothing and the display degraded to an all-`+' preview."
  (skip-unless (executable-find "diff"))
  (let* ((dir (make-temp-file "emagent-diff-test" t))
         (file (expand-file-name "audit.clef" dir))
         (content "(defun f (a) a)\n\n(defun g (b) b)\n")
         (data `((content . ,content)))
         (id "toolu_cache_test")
         (emagent-acp--edit-diff-cache (make-hash-table :test 'equal))
         (emagent-acp--edit-diff-cache-order nil)
         (emagent-tools--project-directory dir))
    (unwind-protect
        (progn
          ;; Pre-write render: real diff, blank line preserved as lone "+".
          (let ((diff (emagent-acp--tool-call-edit-diff-string file data id)))
            (should (string-match-p "@@" diff))
            (should (string-match-p "\\+(defun f (a) a)\n\\+\n\\+(defun g (b) b)" diff))
            ;; The write happens; the file now equals the proposed content.
            (write-region content nil file nil 'quiet)
            ;; Post-write re-render: same diff, from the cache.
            (should (equal diff (emagent-acp--tool-call-edit-diff-string file data id)))))
      (delete-directory dir t))))

(ert-deftest emagent-acp-session-test-edit-diff-reverse-reconstruction ()
  "Without a cached diff, old/new-string edits reconstruct a real diff
by reverse-applying the edit to the already-written file."
  (skip-unless (executable-find "diff"))
  (let* ((dir (make-temp-file "emagent-diff-test" t))
         (file (expand-file-name "code.el" dir))
         (data '((edits . (((old_string . "(old impl)")
                            (new_string . "(new impl)"))))))
         (emagent-acp--edit-diff-cache (make-hash-table :test 'equal))
         (emagent-acp--edit-diff-cache-order nil)
         (emagent-tools--project-directory dir))
    (unwind-protect
        (progn
          ;; File already contains the applied edit; no cache entry exists.
          (write-region "(keep)\n(new impl)\n(keep)\n" nil file nil 'quiet)
          (let ((diff (emagent-acp--tool-call-edit-diff-string file data "toolu_rev")))
            (should diff)
            (should (string-match-p "@@" diff))
            (should (string-match-p "^-(old impl)" diff))
            (should (string-match-p "^\\+(new impl)" diff))))
      (delete-directory dir t))))

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
    (setf (emagent-acp-state-session-id state) "sess")
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
      (should (= 1 (hash-table-count (emagent-acp-state-tool-call-pending state))))
      (emagent-acp--ingest-tool-call-request
       state '((toolCallId . "tool_x")
                (title . "Edit")
                (rawInput . "{\"path\":\"foo.el\"}")))
      (should (string-match-p "foo.el" shown))
      (should (= 0 (hash-table-count (emagent-acp-state-tool-call-pending state))))
      (setq shown nil)
      (emagent-acp--on-tool-call
       state '((toolCallId . "tool_y") (title . "Read") (rawInput . ())))
      (should (null shown))
      (let ((store (lambda (_sid _id) '("Read" . (("path" . "bar.el"))))))
        (cl-letf (((symbol-function 'emagent-cursor-tool-call-from-store) store))
          (emagent-acp-cursor--resolve-tool-from-store state "tool_y")))
      (should (string-match-p "bar.el" shown))
      (should (= 0 (hash-table-count (emagent-acp-state-tool-call-pending state)))))))

(ert-deftest emagent-acp-session-test-cursor-tool-call-completed-waits-for-store ()
  (let ((state (emagent-test--make-acp-state))
        (shown nil))
    (setf (emagent-acp-state-session-id state) "sess")
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
      (should (= 0 (hash-table-count (emagent-acp-state-tool-call-pending state)))))))

(ert-deftest emagent-acp-session-test-cursor-generic-title-stays-hidden ()
  (let ((state (emagent-test--make-acp-state))
        (shown nil))
    (setf (emagent-acp-state-session-id state) "sess")
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
    (setf (emagent-acp-state-busy state) t)
    (setf (emagent-acp-state-permission-queue state) '((id . "req")))
    (emagent-acp--complete-prompt state '((usage . nil)))
    (should (emagent-acp-state-deferred-complete-response state))
    (should (emagent-acp-state-busy state))))

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
                                 (toolCall . ((title . "compile")))))))))
  ;; An MCP tool's bare input fragment gets the tool name prepended —
  ;; "? --oneline -10" alone says nothing about what is being allowed.
  (let ((args (make-hash-table :test 'equal)))
    (puthash "command" "--oneline -10" args)
    (should (string= "mcp__emagent__git_log --oneline -10"
                     (emagent-acp--permission-question-line
                      `((params . ((title . "Allow mcp__emagent__git_log?")
                                   (toolCall . ((toolCallId . "tool_gl")
                                                (arguments . ,args)))))))))))

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
    (setf (emagent-acp-state-config-options state)
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

(ert-deftest emagent-acp-session-test-fs-read-confined-without-session-root ()
  "With no session root (propertyless chat buffer) but a known project directory,
an absolute path outside it is still denied — the boundary falls back to the
project directory rather than opening up unconfined access."
  (emagent-test--with-temp-project
   (lambda (dir)
     (let* ((client (emagent-test--make-test-client
                     :response-sender #'emagent-test--capture-response-sender))
            (state (emagent-test--make-acp-state client))
            ;; A sibling of DIR under the temp root, outside the project.
            (outside (expand-file-name
                      "emagent-escape.txt"
                      (file-name-directory (directory-file-name dir))))
            (request `((id . 9) (method . "fs/read_text_file")
                       (params . ((path . ,outside))))))
       (setq emagent-test--captured-responses nil)
       (write-region "secret" nil outside)
       (unwind-protect
           (progn
             ;; The test chat buffer carries no project property.
             (should (null (emagent-acp--fs-session-root state)))
             (let ((emagent-acp-file-access t))
               (emagent-acp--on-fs-read :state state :emagent-acp-request request))
             (let ((resp (car emagent-test--captured-responses)))
               (should resp)
               (should-not (emagent-test--response-content resp))
               (should (emagent-test--response-error-code resp))))
         (ignore-errors (delete-file outside)))))))

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

(ert-deftest emagent-acp-session-test-quiet-prompt-skips-buffer-stream ()
  (let ((state (emagent-test--make-acp-state))
        (emagent-acp-stream-to-buffer t)
        (emagent-acp-thought-progress 'both))
    (setf (emagent-acp-state-busy state) t
          (emagent-acp-state-quiet-prompt state) t)
    (should-not (emagent-acp--stream-to-buffer-p state))
    (should-not (emagent-acp--stream-thought-to-buffer-p state))))

(ert-deftest emagent-acp-session-test-materialize-dispatches-quiet-prompt ()
  "After compact, materialize sends a quiet session/prompt for durability."
  (let* ((requests nil)
         (client (emagent-test--make-test-client
                  :request-sender
                  (cl-function
                   (lambda (&key request on-success &allow-other-keys)
                     (push (map-elt request :method) requests)
                     (when on-success
                       (funcall on-success '((stopReason . "end_turn"))))))))
         (state (emagent-test--make-acp-state client)))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-acp--client-started-p) (lambda (_client) t))
         ((symbol-function 'emagent-acp--schedule-prompt-watchdog) (lambda (_state) nil))
         ((symbol-function 'emagent-acp--refresh-mode-line) (lambda (_state) nil)))
      (setf (emagent-acp-state-ready state) t
            (emagent-acp-state-session-id state) "sess-compact"
            (emagent-acp-state-prompt-generation state) 0)
      (emagent-acp--materialize-session state)
      (should (equal (car requests) "session/prompt"))
      (should (emagent-acp-state-quiet-prompt state))
      (should (emagent-acp-state-prompt-finishing state)))))

(ert-deftest emagent-acp-session-test-quiet-render-skips-finish-callback ()
  (let* ((finished nil)
         (state (emagent-test--make-acp-state)))
    (setf (emagent-acp-state-quiet-prompt state) t
          (emagent-acp-state-prompt-finishing state) t
          (emagent-acp-state-assistant-text state) "ready"
          (emagent-acp-state-cb-finish state)
          (lambda (&rest _) (setq finished t)))
    (emagent-acp--render-prompt-response state)
    (should-not finished)
    (should-not (emagent-acp-state-quiet-prompt state))
    (should (emagent-acp-state-prompt-finalized state))
    (should (string-empty-p (emagent-acp-state-assistant-text state)))))

(ert-deftest emagent-acp-session-test-mcp-http-capable-p ()
  (should (emagent-acp--mcp-http-capable-p
           '((agentCapabilities . ((mcpCapabilities . ((http . t))))))))
  (should-not (emagent-acp--mcp-http-capable-p
               '((agentCapabilities . ((mcpCapabilities . ((http . :false)))))))))

(ert-deftest emagent-acp-session-test-fatal-agent-error-p ()
  (should (emagent-acp--fatal-agent-error-p "request timed out"))
  (should (emagent-acp--fatal-agent-error-p "failed with status 500"))
  (should (emagent-acp--fatal-agent-error-p
           "Internal error: You've hit your session limit · resets 11:10pm"))
  (should-not (emagent-acp--fatal-agent-error-p "still working"))
  (should-not
   (emagent-acp--fatal-agent-error-p
    "Internal error: API Error: Unable to connect to API (ConnectionRefused)")))

(ert-deftest emagent-acp-session-test-quota-error-p ()
  (should (emagent-acp--quota-error-p
           "Internal error: You've hit your session limit · resets 11:10pm (America/Toronto)"))
  (should (emagent-acp--quota-error-p "rate limit exceeded"))
  (should-not (emagent-acp--quota-error-p "request timed out"))
  (should-not (emagent-acp--quota-error-p nil)))

(ert-deftest emagent-acp-session-test-abort-prompt-surfaces-quota-after-finalize ()
  "Quota errors still reach cb-fail after the watchdog cleared busy."
  (let* ((seen nil)
         (state (emagent-test--make-acp-state)))
    (setf (emagent-acp-state-busy state) nil
          (emagent-acp-state-prompt-finishing state) nil
          (emagent-acp-state-cb-fail state)
          (lambda (message) (setq seen message)))
    (emagent-acp--abort-prompt
     state
     "prompt failed: Internal error: You've hit your session limit · resets 11:10pm")
    (should (string-match-p "session limit" seen))))

(ert-deftest emagent-acp-session-test-watchdog-extends-when-pending ()
  "Watchdog must not finalize while ACP requests are still pending."
  (let* ((state (emagent-test--make-acp-state))
         (completed nil)
         (emagent-acp-watchdog-timeout 0.01))
    (setf (emagent-acp-state-busy state) t
          (emagent-acp-state-assistant-text state) "partial"
          (map-elt (emagent-acp-state-client state) :pending-requests)
          '(("1" . t)))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-acp--complete-prompt)
          (lambda (&rest _) (setq completed t)))
         ((symbol-function 'emagent-acp--abort-prompt)
          (lambda (&rest _) (setq completed 'aborted)))
         ((symbol-function 'emagent-acp--refresh-mode-line) (lambda (_s) nil)))
      (emagent-acp--schedule-prompt-watchdog state)
      (sleep-for 0.05)
      (should-not completed)
      (should (emagent-acp-state-prompt-watchdog-timer state))
      (emagent-acp--clear-prompt-watchdog state))))

(ert-deftest emagent-acp-session-test-retriable-prompt-error-p ()
  (should (emagent-acp--retriable-prompt-error-p
           "Error: RetriableError: [unavailable] getaddrinfo ENOTFOUND api2.cursor.sh"))
  (should (emagent-acp--retriable-prompt-error-p "read ECONNRESET"))
  (should (emagent-acp--retriable-prompt-error-p "socket hang up"))
  (should (emagent-acp--retriable-prompt-error-p
           "Internal error: API Error: Unable to connect to API (ConnectionRefused)"))
  (should-not (emagent-acp--retriable-prompt-error-p "failed with status 400"))
  (should-not (emagent-acp--retriable-prompt-error-p nil)))

(ert-deftest emagent-acp-session-test-agent-error-only-response-p ()
  (let ((state (emagent-test--make-acp-state)))
    ;; A turn whose whole output is a transient network error, with no
    ;; content or tool calls, is safe to re-issue.
    (setf (emagent-acp-state-assistant-text state) "Error: RetriableError: [unavailable] getaddrinfo ENOTFOUND api2.cursor.sh")
    (should (emagent-acp--agent-error-only-response-p state))
    ;; A real answer is never re-issued, even if it mentions a network word.
    (setf (emagent-acp-state-assistant-text state) "I finished the task; there was no network error along the way.")
    (should-not (emagent-acp--agent-error-only-response-p state))
    ;; An error turn that also did tool work is left alone.
    (setf (emagent-acp-state-assistant-text state) "RetriableError: socket hang up")
    (puthash "call-1" "shell" (emagent-acp-state-tool-call-titles state))
    (should-not (emagent-acp--agent-error-only-response-p state))
    (clrhash (emagent-acp-state-tool-call-titles state))
    ;; Compression turns are never treated as retriable errors.
    (setf (emagent-acp-state-compress-pending state) t)
    (should-not (emagent-acp--agent-error-only-response-p state))))

(ert-deftest emagent-acp-session-test-turn-did-no-work-p ()
  (let ((state (emagent-test--make-acp-state)))
    (setf (emagent-acp-state-assistant-text state) "")
    (should (emagent-acp--turn-did-no-work-p state))
    ;; A tool call means side effects may have happened.
    (puthash "call-1" "shell" (emagent-acp-state-tool-call-titles state))
    (should-not (emagent-acp--turn-did-no-work-p state))
    (clrhash (emagent-acp-state-tool-call-titles state))
    ;; Substantial content also counts as work.
    (setf (emagent-acp-state-assistant-text state) (make-string 500 ?x))
    (should-not (emagent-acp--turn-did-no-work-p state))))

(ert-deftest emagent-acp-session-test-turn-hit-transient-error-p ()
  (let ((state (emagent-test--make-acp-state)))
    ;; Detected even when the turn also ran tool calls (unlike error-only).
    (setf (emagent-acp-state-assistant-text state) "Committed and pushed.\n\nError: RetriableError: WritableIterable is closed")
    (puthash "call-1" "shell" (emagent-acp-state-tool-call-titles state))
    (should (emagent-acp--turn-hit-transient-error-p state))
    ;; This work-turn must NOT be treated as safe-to-replay.
    (should-not (emagent-acp--agent-error-only-response-p state))
    ;; A clean answer is not a transient error.
    (setf (emagent-acp-state-assistant-text state) "All done, pushed successfully.")
    (should-not (emagent-acp--turn-hit-transient-error-p state))
    ;; Compression turns are excluded.
    (setf (emagent-acp-state-assistant-text state) "RetriableError: socket hang up")
    (setf (emagent-acp-state-compress-pending state) t)
    (should-not (emagent-acp--turn-hit-transient-error-p state))))

(ert-deftest emagent-acp-session-test-prompt-retry-delay ()
  (let ((emagent-acp-prompt-retry-base-delay 1.5))
    (should (= (emagent-acp--prompt-retry-delay 1) 1.5))
    (should (= (emagent-acp--prompt-retry-delay 2) 3.0))
    (should (= (emagent-acp--prompt-retry-delay 3) 6.0))))

(ert-deftest emagent-acp-session-test-prompt-retry-pending-guards-abort ()
  "Stderr for a retriable failure must not abort while a retry is scheduled."
  (let* ((state (emagent-test--make-acp-state))
         (message "Internal error: API Error: Unable to connect to API (ConnectionRefused)")
         (aborted nil))
    (setf (emagent-acp-state-busy state) t
          (emagent-acp-state-prompt-generation state) 2
          (emagent-acp-state-prompt-retry-gen state) 2)
    (cl-letf (((symbol-function 'emagent-acp--abort-prompt)
               (lambda (_s _m) (setq aborted t))))
      (when (and (emagent-acp-state-busy state)
                 (emagent-acp--fatal-agent-error-p message)
                 (not (emagent-acp--prompt-retry-pending-p state)))
        (emagent-acp--abort-prompt state message)))
    (should-not aborted)
    (should-not (emagent-acp--fatal-agent-error-p message))))

(ert-deftest emagent-acp-session-test-tool-call-displayable-p ()
  (let* ((state (emagent-test--make-acp-state))
         (emagent-acp--tool-call-weak-details '("tool" "Tool" "running" "pending")))
    (setf (emagent-acp-state-provider state) 'cursor)
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
    (setf (emagent-acp-state-busy state) t)
    (setf (emagent-acp-state-deferred-complete-response state) '((usage . ((totalTokens . 5)))))
    (setf (emagent-acp-state-permission-queue state) (list request))
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
        (should-not (emagent-acp-state-busy state))
        (should (null (emagent-acp-state-deferred-complete-response state)))))))

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
    (setf (emagent-acp-state-permission-queue state) (list request))
    (let ((emagent-acp-auto-approve-permissions nil))
      (emagent-test--with-mocks
          (((symbol-function 'run-at-time) #'emagent-test--run-at-time-immediately)
           ((symbol-function 'emagent-acp-send-response) (lambda (&rest _args) nil))
           ((symbol-function 'emagent-acp--handle-one-permission)
            (lambda (&rest _args) (error "simulated permission crash"))))
        (emagent-acp--drain-permission-queue-now state)
        (should-not (emagent-acp-state-permission-busy state))
        (should (null (emagent-acp-state-permission-queue state)))))))

(ert-deftest emagent-acp-session-test-permission-on-complete-error-no-double-answer ()
  "When the post-reply continuation throws, the request is not answered twice:
`respond' already fired, so the outer handler must not send a second `cancelled'."
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1") (params . ((title . "Allow?")))))
         (responses 0))
    (setf (emagent-acp-state-permission-queue state) (list request))
    (emagent-test--with-mocks
        (((symbol-function 'run-at-time) #'emagent-test--run-at-time-immediately)
         ((symbol-function 'emagent-acp-send-response)
          (lambda (&rest _) (setq responses (1+ responses))))
         ((symbol-function 'emagent-acp--maybe-complete-deferred-prompt)
          (lambda (&rest _) (error "continuation crash")))
         ((symbol-function 'emagent-acp--handle-one-permission)
          (lambda (&rest args)
            ;; Simulate an auto-decision: reply, then run the continuation.
            (emagent-acp-send-response :client nil :response nil)
            (funcall (plist-get args :on-complete)))))
      (emagent-acp--drain-permission-queue-now state)
      (should (= responses 1))
      (should-not (emagent-acp-state-permission-busy state)))))

(ert-deftest emagent-acp-session-test-maybe-recover-stall-drains-queue ()
  "maybe-recover-stall' schedules permission drain when queue is nonempty."
  (let* ((state (emagent-test--make-acp-state))
         (request '((id . "req1")
                    (params . ((title . "Allow compile?")
                               (options . [((optionId . "allow_once")
                                            (kind . "allow_once"))]))))))
    (setf (emagent-acp-state-ready state) t)
    (setf (emagent-acp-state-busy state) nil)
    (setf (emagent-acp-state-permission-queue state) (list request))
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
        (should (null (emagent-acp-state-permission-queue state)))))))



(ert-deftest emagent-acp-session-test-switch-mode-returns-exact-option-id ()
  "ExitPlanMode-shaped switch_mode returns the selected mode optionId."
  (let* ((state (emagent-test--make-acp-state))
         (options [((optionId . "auto")
                    (name . "Yes, and use auto mode")
                    (kind . "allow_always"))
                   ((optionId . "default")
                    (name . "Yes, and manually approve edits")
                    (kind . "allow_once"))
                   ((optionId . "plan")
                    (name . "No, keep planning")
                    (kind . "reject_once"))])
         (tool-call `((toolCallId . "exit1")
                      (kind . "switch_mode")
                      (title . "Ready to code?")
                      (content . [((type . "content")
                                   (content . ((type . "text")
                                               (text . "Do the thing"))))])))
         (request `((id . "req-exit-plan")
                    (params . ((title . "Ready to code?")
                               (options . ,options)
                               (toolCall . ,tool-call)))))
         (sent-id nil)
         (seen-choices nil)
         (seen-preamble nil))
    (let ((emagent-acp-auto-approve-permissions t))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-chat--open-response-p) (lambda () nil))
           ((symbol-function 'emagent-tools--buttons-prompt)
            (emagent-test--mock-buttons-prompt
             "default"
             (lambda (args)
               (setq seen-choices (nth 1 args)
                     seen-preamble (nth 4 args)))))
           ((symbol-function 'emagent-acp-send-response)
            (cl-function
             (lambda (&key response &allow-other-keys)
               (setq sent-id (map-nested-elt response '(:result outcome optionId)))))))
        (emagent-acp--handle-one-permission :state state :emagent-acp-request request)
        (should (string= "default" sent-id))
        (should (equal (mapcar #'cdr seen-choices) '("auto" "default" "plan")))
        (should (string-match-p "Do the thing" (or seen-preamble "")))))))

(ert-deftest emagent-acp-session-test-switch-mode-stay-plan-option ()
  "Selecting keep-planning returns optionId plan."
  (let* ((state (emagent-test--make-acp-state))
         (options [((optionId . "default")
                    (name . "Yes")
                    (kind . "allow_once"))
                   ((optionId . "plan")
                    (name . "No, keep planning")
                    (kind . "reject_once"))])
         (request `((id . "req-stay")
                    (params . ((options . ,options)
                               (toolCall . ((toolCallId . "exit2")
                                            (kind . "switch_mode")
                                            (title . "Ready to code?")))))))
         (sent-id nil))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-tools--buttons-prompt)
          (emagent-test--mock-buttons-prompt "plan"))
         ((symbol-function 'emagent-acp-send-response)
          (cl-function
           (lambda (&key response &allow-other-keys)
             (setq sent-id (map-nested-elt response '(:result outcome optionId)))))))
      (emagent-acp--handle-one-permission :state state :emagent-acp-request request)
      (should (string= "plan" sent-id)))))

(ert-deftest emagent-acp-session-test-switch-mode-label-rewrites-unknown ()
  (let ((update '((kind . "switch_mode")
                  (title . "Switch Mode: unknown")
                  (rawInput . ((targetModeId . "plan")))))
        )
    (should (string= "Switch to plan"
                     (emagent-acp--switch-mode-display-title update)))
    (should (string= "Switch to plan"
                     (emagent-acp--tool-call-label update))))
  (let ((update '((kind . "switch_mode")
                  (title . "Switch Mode: unknown")
                  (rawInput . ((explanation . "Need ask mode for this")))))
        )
    (should (string-match-p "Need ask mode"
                            (emagent-acp--switch-mode-display-title update)))
    (should-not (string-match-p "unknown"
                                (emagent-acp--switch-mode-display-title update)))))

(ert-deftest emagent-acp-session-test-current-mode-update-sets-state ()
  (let ((state (emagent-test--make-acp-state)))
    (emagent-acp--on-notification
     :state state
     :emagent-acp-notification
     '((method . "session/update")
       (params . ((update . ((sessionUpdate . "current_mode_update")
                             (currentModeId . "plan")))))))
    (should (string= "plan" (emagent-acp-state-session-mode-id state)))
    (emagent-acp--on-notification
     :state state
     :emagent-acp-notification
     '((method . "session/update")
       (params . ((update . ((sessionUpdate . "current_mode_update")
                             (modeId . "agent")))))))
    (should (string= "agent" (emagent-acp-state-session-mode-id state)))))

(provide 'emagent-acp-session-test)

;;; emagent-acp-session-test.el ends here
