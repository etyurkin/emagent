;;; emagent-chat-mcp-test.el --- ERT tests for client /mcp -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emagent-test-utils)
(require 'emagent-chat-mcp)
(require 'emagent-acp-state)
(require 'emagent-acp-send)

(ert-deftest emagent-chat-mcp-test-command-p ()
  (should (emagent-chat--mcp-command-p "/mcp"))
  (should (emagent-chat--mcp-command-p "  /mcp  "))
  (should (emagent-chat--mcp-command-p "/mcp sentry"))
  (should-not (emagent-chat--mcp-command-p "/model"))
  (should-not (emagent-chat--mcp-command-p "/mcp\nmore")))

(ert-deftest emagent-chat-mcp-test-arg ()
  (should-not (emagent-chat--mcp-arg "/mcp"))
  (should (string= "sentry" (emagent-chat--mcp-arg "/mcp sentry")))
  (should (string= "my-http" (emagent-chat--mcp-arg "/mcp  my-http  "))))

(ert-deftest emagent-chat-mcp-test-parse-cursor-list ()
  (let ((servers (emagent-chat--mcp-parse-list
                  "emagent: ready\nlisp-sitter: ready\nsentry: requires_authentication\n")))
    (should (equal servers
                   '(("emagent" . "ready")
                     ("lisp-sitter" . "ready")
                     ("sentry" . "requires_authentication"))))))

(ert-deftest emagent-chat-mcp-test-parse-claude-list ()
  (let ((servers
         (emagent-chat--mcp-parse-list
          (concat
           "⚠ warning line\n"
           "Checking MCP server health…\n"
           "sentry: https://example/mcp (HTTP) - ✔ Connected\n"
           "lisp-sitter: /bin/lisp-sitter mcp serve - ✔ Connected\n"))))
    (should (equal (assoc-string "sentry" servers)
                   '("sentry" . "ready")))
    (should (equal (assoc-string "lisp-sitter" servers)
                   '("lisp-sitter" . "ready")))))

(ert-deftest emagent-chat-mcp-test-needs-auth-p ()
  (should (emagent-chat--mcp-needs-auth-p "requires_authentication"))
  (should (emagent-chat--mcp-needs-auth-p "needs_authentication"))
  (should-not (emagent-chat--mcp-needs-auth-p "ready"))
  (should-not (emagent-chat--mcp-needs-auth-p "disabled")))

(ert-deftest emagent-chat-mcp-test-send-does-not-dispatch ()
  "Bare /mcp is handled client-side and never dispatches an ACP prompt."
  (let ((dispatched nil)
        (ran nil)
        (state (emagent-test--make-acp-state)))
    (setf (emagent-acp-state-ready state) t
          (emagent-acp-state-busy state) nil
          (emagent-acp-state-session-id state) "s1")
    (emagent-test--with-mocks
        (((symbol-function 'emagent-acp--session) (lambda () state))
         ((symbol-function 'emagent-chat--slash-mcp-apply)
          (lambda (&optional _text) (setq ran t)))
         ((symbol-function 'emagent-acp--dispatch-prompt-request)
          (lambda (&rest _) (setq dispatched t)))
         ((symbol-function 'emagent-acp--provider-normalize-slash-prompt)
          (lambda (_s text) text)))
      (emagent-acp-send-prompt "/mcp")
      (should ran)
      (should-not dispatched))))

(ert-deftest emagent-chat-mcp-test-select-lists-async ()
  "Interactive select starts mcp list asynchronously (no sync call-process)."
  (let ((started nil)
        (emagent-chat-provider 'cursor))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-chat--mcp-cli)
          (lambda () (cons "cursor-agent" default-directory)))
         ((symbol-function 'emagent-chat--mcp-start)
          (lambda (_program _directory args on-done)
            (setq started args)
            (when on-done
              (funcall on-done 0 "emagent: ready\nsentry: requires_authentication\n"))
            nil))
         ((symbol-function 'emagent-chat--mcp-pick-server)
          (lambda (&rest _) nil))
         ((symbol-function 'run-with-idle-timer)
          (lambda (_secs _repeat fn) (funcall fn) nil)))
      (emagent-chat--mcp-select-and-act)
      (should (equal started '("mcp" "list"))))))
;;; emagent-chat-mcp-test.el ends here
