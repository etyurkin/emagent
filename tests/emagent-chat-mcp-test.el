;;; emagent-chat-mcp-test.el --- ERT tests for client /mcp -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emagent-test-utils)
(require 'emagent-chat)
(require 'emagent-acp-protocol)
(require 'emagent-acp)

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
  "Bare /mcp is intercepted by `emagent-chat-send' and never reaches ACP."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let (dispatched ran)
       (with-current-buffer buffer
         (goto-char (point-max))
         (insert (emagent-chat--user-heading-prefix) "/mcp")
         (emagent-test--with-mocks
             (((symbol-function 'emagent-chat--slash-mcp-apply)
               (lambda (&optional _text) (setq ran t)))
              ((symbol-function 'emagent-acp-send)
               (lambda (&rest _args) (setq dispatched t))))
           (emagent-chat-send))
         (should ran)
         (should-not dispatched))))))

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

(ert-deftest emagent-chat-mcp-test-reload-session ()
  "Reload shuts down a live session then ensure-connects."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let (shutdown connected)
       (with-current-buffer buffer
         (setq emagent-acp--session (list :dummy t))
         (emagent-test--with-mocks
             (((symbol-function 'emagent-acp-shutdown-buffer)
               (lambda ()
                 (setq shutdown t)
                 (setq emagent-acp--session nil)))
              ((symbol-function 'emagent-acp-ensure-connected)
               (lambda (&rest args)
                 (setq connected t)
                 (when-let ((on-ready (plist-get args :on-ready)))
                   (funcall on-ready))))
              ((symbol-function 'emagent-chat-seed-cursor-slash-commands)
               (lambda () nil)))
           (emagent-chat--mcp-reload-session buffer "sentry"))
         (should shutdown)
         (should connected))))))

(ert-deftest emagent-chat-mcp-test-login-claude-reloads ()
  "Claude MCP login success reconnects the ACP session."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let ((reloaded nil)
           (emagent-chat-provider 'claude))
       (with-current-buffer buffer
         (emagent-test--with-mocks
             (((symbol-function 'emagent-chat--mcp-cli)
               (lambda () (cons "claude" default-directory)))
              ((symbol-function 'emagent-chat--mcp-start)
               (lambda (_program _directory args on-done &optional _pty)
                 (should (equal args '("mcp" "login" "sentry")))
                 (when on-done (funcall on-done 0 ""))
                 nil))
              ((symbol-function 'emagent-chat--mcp-reload-session)
               (lambda (buf name)
                 (setq reloaded (list buf name))))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) nil)))
           (emagent-chat--mcp-login "sentry" 'claude))
         (should (equal reloaded (list buffer "sentry"))))))))

(ert-deftest emagent-chat-mcp-test-login-cursor-reloads-after-enable ()
  "Cursor MCP login waits for enable before reconnecting."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let ((reloaded nil)
           (enable-cb nil)
           (emagent-chat-provider 'cursor))
       (with-current-buffer buffer
         (emagent-test--with-mocks
             (((symbol-function 'emagent-chat--mcp-cli)
               (lambda () (cons "cursor-agent" default-directory)))
              ((symbol-function 'emagent-chat--mcp-start)
               (lambda (_program _directory args on-done &optional _pty)
                 (should (equal args '("mcp" "login" "sentry")))
                 (when on-done (funcall on-done 0 ""))
                 nil))
              ((symbol-function 'emagent-cursor-write-mcp-approvals)
               (lambda (&rest _) nil))
              ((symbol-function 'emagent-chat--mcp-enable)
               (lambda (name on-done)
                 (should (string= name "sentry"))
                 (setq enable-cb on-done)))
              ((symbol-function 'emagent-chat--mcp-reload-session)
               (lambda (buf name)
                 (setq reloaded (list buf name))))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) nil)))
           (emagent-chat--mcp-login "sentry" 'cursor)
           (should-not reloaded)
           (should enable-cb)
           (funcall enable-cb 0 "")
           (should (equal reloaded (list buffer "sentry")))))))))

;;; emagent-chat-mcp-test.el ends here
