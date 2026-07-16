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

(ert-deftest emagent-mcp-test-run-tool-executes-handler ()
  (let* ((token "abc")
         (session (list :root "/proj" :buffer (get-buffer-create "*mcp-test*") :acp t))
         (args (make-hash-table :test 'equal)))
    (puthash "command" "make test" args)
    (puthash token session emagent-mcp--sessions)
    (unwind-protect
        (emagent-test--with-mocks
            (((symbol-function 'emagent-tool-compile)
              (lambda (&rest _args) "ok")))
          (should (string= "ok" (emagent-mcp--run-tool "compile" args session))))
      (remhash token emagent-mcp--sessions))))

(ert-deftest emagent-mcp-test-acp-session-skips-eval-dangerous-confirm ()
  (let ((emagent-tools--acp-session-p t))
    (should (emagent-tools--eval-form-dangerous-allowed-p "(load-file \"x\")" '(load-file)))))

;;;; RPC helpers

(ert-deftest emagent-mcp-test-rpc-result ()
  (let ((resp (emagent-mcp--rpc-result 1 '((ok . t)))))
    (should (stringp resp))
    (should (string-match-p "\"id\":1" resp))
    (should (string-match-p "\"ok\":true" resp))))

(ert-deftest emagent-mcp-test-cursor-project-slug ()
  (should (string= "Users-alice-config-my-app-project"
                   (emagent-cursor-project-slug
                    "/Users/alice/.config/my-app/project")))
  (should (string= "Users-alice-src-demo-app"
                   (emagent-cursor-project-slug
                    "/Users/alice/src/demo-app/"))))

(ert-deftest emagent-mcp-test-cursor-mcp-approval-key ()
  "Match Cursor's name-sha256prefix format for http MCP approvals."
  (should
   (string=
    "mcp-gateway-790a71ad022ef2d7"
    (emagent-cursor--mcp-approval-key
     "mcp-gateway"
     "/Users/alice/src/demo-app"
     "https://mcp.example.com/mcp"))))

(ert-deftest emagent-mcp-test-cursor-write-mcp-approvals ()
  (let* ((tmpdir (make-temp-file "emagent-mcp-approvals" t))
         (cfg (expand-file-name "mcp.json" tmpdir))
         (projects (expand-file-name "projects" tmpdir))
         (cwd (expand-file-name "proj" tmpdir))
         (emagent-mcp-cursor-config-file cfg))
    (unwind-protect
        (progn
          (make-directory cwd t)
          (with-temp-file cfg
            (insert
             (json-serialize
              '((mcpServers
                 (emagent . ((url . "http://127.0.0.1:9/mcp/x")))
                 (mcp-gateway . ((url . "https://example.test/mcp"))))))))
          (emagent-test--with-mocks
              (((symbol-function 'emagent-cursor-mcp-approvals-file)
                (lambda (dir)
                  (expand-file-name
                   "mcp-approvals.json"
                   (expand-file-name (emagent-cursor-project-slug dir)
                                     projects)))))
            (let ((out (emagent-cursor-write-mcp-approvals cwd)))
              (should (file-readable-p out))
              (let* ((keys (json-parse-string
                            (with-temp-buffer
                              (insert-file-contents out)
                              (buffer-string))
                            :object-type 'alist
                            :array-type 'list))
                     (expected
                      (emagent-cursor--mcp-approval-key
                       "mcp-gateway"
                       (directory-file-name (expand-file-name cwd))
                       "https://example.test/mcp")))
                (should (equal keys (list expected)))))))
      (ignore-errors (delete-directory tmpdir t)))))

(provide 'emagent-mcp-test)

;;; emagent-mcp-test.el ends here
