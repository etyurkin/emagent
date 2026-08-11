;;; emagent-providers-test.el --- ERT tests for Claude/Cursor providers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emagent-test-utils)
(require 'emagent-acp)
(require 'emagent-claude)

(ert-deftest emagent-providers-test-claude-command ()
  (should (string= "claude-agent-acp" (emagent-claude-command)))
  (should (listp (emagent-claude-command-params))))

(ert-deftest emagent-providers-test-claude-builtin-agents ()
  (let ((agents (emagent-claude-agents nil)))
    (dolist (name '("general-purpose" "Explore" "Plan"))
      (let ((entry (cl-find name agents :key (lambda (a) (map-elt a 'name)) :test #'string=)))
        (should entry)
        (should (not (string-empty-p (map-elt entry 'description))))))))

(ert-deftest emagent-providers-test-claude-custom-agents-from-project ()
  (let* ((root (emagent-test--temp-directory))
         (agents-dir (expand-file-name ".claude/agents" root))
         (file (expand-file-name "reviewer.md" agents-dir)))
    (unwind-protect
        (progn
          (make-directory agents-dir t)
          (with-temp-file file
            (insert "---\nname: reviewer\ndescription: Reviews code for correctness and style\n---\nYou are a careful code reviewer.\n"))
          (let* ((agents (emagent-claude-agents root))
                 (entry (cl-find "reviewer" agents :key (lambda (a) (map-elt a 'name)) :test #'string=)))
            (should entry)
            (should (string= "Reviews code for correctness and style"
                             (map-elt entry 'description)))))
      (when (file-exists-p root)
        (delete-directory root t)))))

(ert-deftest emagent-providers-test-claude-custom-agent-overrides-builtin ()
  (let* ((root (emagent-test--temp-directory))
         (agents-dir (expand-file-name ".claude/agents" root))
         (file (expand-file-name "general-purpose.md" agents-dir)))
    (unwind-protect
        (progn
          (make-directory agents-dir t)
          (with-temp-file file
            (insert "---\nname: general-purpose\ndescription: Custom override\n---\nBody.\n"))
          (let* ((agents (emagent-claude-agents root))
                 (entry (cl-find "general-purpose" agents :key (lambda (a) (map-elt a 'name)) :test #'string=)))
            (should entry)
            (should (string= "Custom override" (map-elt entry 'description)))))
      (when (file-exists-p root)
        (delete-directory root t)))))

(ert-deftest emagent-providers-test-claude-agent-frontmatter-parsing ()
  (let ((root (emagent-test--temp-directory)))
    (unwind-protect
        (progn
          (make-directory root t)
          (let ((quoted (expand-file-name "quoted.md" root))
                (no-dashes (expand-file-name "no-dashes.md" root))
                (no-name (expand-file-name "no-name.md" root))
                (extra-keys (expand-file-name "extra-keys.md" root)))
            (with-temp-file quoted
              (insert "---\nname: \"reviewer\"\ndescription: 'Quoted description'\n---\nBody.\n"))
            (should (equal '("reviewer" . "Quoted description")
                           (emagent-claude--agent-frontmatter quoted)))
            (with-temp-file no-dashes
              (insert "name: reviewer\ndescription: No frontmatter here\n"))
            (should-not (emagent-claude--agent-frontmatter no-dashes))
            (with-temp-file no-name
              (insert "---\ndescription: Missing a name\n---\nBody.\n"))
            (should-not (emagent-claude--agent-frontmatter no-name))
            (with-temp-file extra-keys
              (insert "---\nname: reviewer\ntools: Read, Grep\nmodel: sonnet\ndescription: Ignores unknown keys\n---\nBody.\n"))
            (should (equal '("reviewer" . "Ignores unknown keys")
                           (emagent-claude--agent-frontmatter extra-keys)))))
      (when (file-exists-p root)
        (delete-directory root t)))))

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
