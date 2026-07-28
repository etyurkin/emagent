;;; emagent-providers-test.el --- ERT tests for Claude/Cursor providers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-acp)
(require 'emagent-cursor)

(ert-deftest emagent-providers-test-claude-command ()
  (should (string= "claude-agent-acp" (emagent-claude-command)))
  (should (listp (emagent-claude-command-params))))

(ert-deftest emagent-providers-test-cursor-command ()
  (should (string= "cursor-agent" (emagent-cursor-command)))
  (should (member "acp" (emagent-cursor-command-params)))
  (should (equal emagent-cursor-acp-extra-args '("--sandbox" "disabled"))))

(ert-deftest emagent-providers-test-cursor-buffer-local-extra-args ()
  (with-temp-buffer
    (setq-local emagent-chat-cursor-acp-extra-args '("--sandbox" "enabled"))
    (should (equal '("--sandbox" "enabled")
                   (cdr (emagent-cursor-command-params-for-context (current-buffer)))))))

(ert-deftest emagent-providers-test-cursor-environment-token ()
  (with-temp-buffer
    (let ((env (emagent-cursor--environment (current-buffer))))
      (should (string-match-p "EMAGENT_SESSION_TOKEN="
                              (car (last env))))
      (should (string-match-p (emagent-mcp-buffer-token)
                              (car (last env)))))))

(ert-deftest emagent-providers-test-claude-make-client-mock ()
  (emagent-test--with-mocks
      (((symbol-function 'executable-find) (lambda (_cmd) "/bin/true"))
       ((symbol-function 'emagent-claude-check-command) (lambda () nil)))
    (let ((client (emagent-claude-make-client :context-buffer (current-buffer))))
      (should (hash-table-p client))
      (should (string= "claude-agent-acp" (map-elt client :command))))))

(ert-deftest emagent-providers-test-cursor-make-client-mock ()
  (emagent-test--with-mocks
      (((symbol-function 'executable-find) (lambda (_cmd) "/bin/true"))
       ((symbol-function 'emagent-cursor-check-command) (lambda () nil))
       ((symbol-function 'emagent-mcp-ensure-cursor-config) (lambda () nil)))
    (let ((client (emagent-cursor-make-client :context-buffer (current-buffer))))
      (should (hash-table-p client))
      (should (string= "cursor-agent" (map-elt client :command)))
      (should (member "acp" (map-elt client :command-params))))))

(ert-deftest emagent-providers-test-context-usage-unavailable-hook ()
  "Cursor reports context usage as unavailable via its provider hook; a
provider without the hook (claude) reports it as available."
  (let ((cursor (emagent-test--make-acp-state)))
    (setf (emagent-acp-state-provider cursor) 'cursor)
    (should (emagent-acp--provider-context-usage-unavailable-p cursor)))
  (let ((claude (emagent-test--make-acp-state)))
    (setf (emagent-acp-state-provider claude) 'claude)
    (should-not (emagent-acp--provider-context-usage-unavailable-p claude))))

(ert-deftest emagent-providers-test-turn-usage-does-not-zero-context ()
  "Prompt response usage with per-turn inputTokens must not wipe ctx fill."
  (let ((state (emagent-test--make-acp-state)))
    (emagent-acp--update-usage-from-notification
     state '((used . 50000) (size . 200000)))
    (emagent-acp--save-usage-from-response
     state '((inputTokens . 0) (outputTokens . 42) (totalTokens . 42)))
    (let ((usage (emagent-acp-state-usage state)))
      (should (equal 50000 (map-elt usage :context-used)))
      (should (equal 200000 (map-elt usage :context-size))))))

(ert-deftest emagent-providers-test-usage-notification-ignores-input-tokens ()
  "usage_update must not treat inputTokens as cumulative context fill."
  (let ((state (emagent-test--make-acp-state)))
    (emagent-acp--update-usage-from-notification
     state '((used . 12000) (size . 200000)))
    (emagent-acp--update-usage-from-notification
     state '((inputTokens . 0) (size . 200000)))
    (let ((usage (emagent-acp-state-usage state)))
      (should (equal 12000 (map-elt usage :context-used)))
      (should (equal 200000 (map-elt usage :context-size))))))

(provide 'emagent-providers-test)

;;; emagent-providers-test.el ends here
