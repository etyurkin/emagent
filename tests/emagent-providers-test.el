;;; emagent-providers-test.el --- ERT tests for Claude/Cursor providers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-claude)
(require 'emagent-cursor)

(ert-deftest emagent-providers-test-claude-command ()
  (should (string= "claude-agent-acp" (emagent-claude-command)))
  (should (listp (emagent-claude-command-params))))

(ert-deftest emagent-providers-test-cursor-command ()
  (should (string= "cursor-agent" (emagent-cursor-command)))
  (should (member "acp" (emagent-cursor-command-params))))

(ert-deftest emagent-providers-test-cursor-buffer-local-extra-args ()
  (with-temp-buffer
    (setq-local emagent-chat-cursor-acp-extra-args '("--trust"))
    (should (equal '("--trust")
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

(provide 'emagent-providers-test)

;;; emagent-providers-test.el ends here
