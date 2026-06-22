;;; emagent-mcp-test.el --- ERT tests for emagent MCP server -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-mcp)
(require 'emagent-tools)

;;;; Tokens

(ert-deftest emagent-mcp-test-make-token ()
  (let ((t1 (emagent-mcp-make-token))
        (t2 (emagent-mcp-make-token)))
    (should (= 24 (length t1)))
    (should (string-match-p "\\`[0-9a-f]\\{24\\}\\'" t1))
    (should (not (string= t1 t2)))))

(ert-deftest emagent-mcp-test-buffer-token ()
  (with-temp-buffer
    (should (string= (emagent-mcp-buffer-token) (emagent-mcp-buffer-token)))
    (should (string-match-p "\\`[0-9a-f]\\{24\\}\\'" emagent-mcp--token))))

;;;; Args helpers

(ert-deftest emagent-mcp-test-arg ()
  (let ((args (make-hash-table :test 'equal)))
    (puthash "path" "/tmp/foo" args)
    (puthash "flag" t args)
    (puthash "empty" :null args)
    (should (equal (emagent-mcp--arg args "path") "/tmp/foo"))
    (should (equal (emagent-mcp--arg args "missing" "default") "default"))
    (should (equal (emagent-mcp--arg args "empty" "default") "default"))))

(ert-deftest emagent-mcp-test-bool ()
  (let ((args (make-hash-table :test 'equal)))
    (puthash "on" t args)
    (puthash "off" :false args)
    (should (emagent-mcp--bool args "on"))
    (should-not (emagent-mcp--bool args "off"))))

(ert-deftest emagent-mcp-test-string-result ()
  (should (string= "hi" (emagent-mcp--string-result "hi")))
  (should (string= "" (emagent-mcp--string-result nil)))
  (should (string= "42" (emagent-mcp--string-result 42))))

;;;; Tool metadata

(ert-deftest emagent-mcp-test-truncate-detail ()
  (should (string= "short" (emagent-mcp--truncate-detail "short" 10)))
  (should (string-match-p "…\\'" (emagent-mcp--truncate-detail (make-string 20 ?x) 10))))

(ert-deftest emagent-mcp-test-tool-confirm-symbol ()
  (should (eq 'emagent-tool-read-file (emagent-mcp--tool-confirm-symbol "read_file")))
  (should (eq 'emagent-tool-run-shell-command
              (emagent-mcp--tool-confirm-symbol "run_shell_command"))))

(ert-deftest emagent-mcp-test-tool-prompt-detail ()
  (let ((args (make-hash-table :test 'equal)))
    (puthash "command" "make test" args)
    (puthash "directory" "src" args)
    (let ((detail (emagent-mcp--tool-prompt-detail "run_shell_command" args "/proj")))
      (should (string-match-p "make test" detail))
      (should (string-match-p "src" detail))))
  (let ((args (make-hash-table :test 'equal)))
    (puthash "command" "emacs --batch -l tests/emagent-test-runner.el" args)
    (should (string-match-p "emacs --batch"
                            (emagent-mcp--tool-prompt-detail "compile" args "/proj")))))

(ert-deftest emagent-mcp-test-acp-session-skips-confirm ()
  (let* ((token "abc")
         (session (list :root "/proj" :buffer (get-buffer-create "*mcp-test*") :acp t))
         (args (make-hash-table :test 'equal))
         (confirmed nil))
    (puthash "command" "make test" args)
    (puthash token session emagent-mcp--sessions)
    (unwind-protect
        (emagent-test--with-mocks
            (((symbol-function 'emagent-mcp--confirm-tool)
              (lambda (&rest _args) (setq confirmed t) t))
             ((symbol-function 'emagent-tool-compile)
              (lambda (&rest _args) "ok")))
          (should (string= "ok" (emagent-mcp--run-tool "compile" args session)))
          (should-not confirmed))
      (remhash token emagent-mcp--sessions))))

;;;; RPC helpers

(ert-deftest emagent-mcp-test-rpc-result ()
  (let ((resp (emagent-mcp--rpc-result 1 '((ok . t)))))
    (should (stringp resp))
    (should (string-match-p "\"id\":1" resp))
    (should (string-match-p "\"ok\":true" resp))))

(provide 'emagent-mcp-test)

;;; emagent-mcp-test.el ends here
