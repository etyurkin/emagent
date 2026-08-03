;;; emagent-mcp-test.el --- ERT tests for emagent MCP server -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-chat)
(require 'emagent-tools)

;;;; Tokens

(ert-deftest emagent-mcp-test-make-token ()
  (let ((t1 (emagent-mcp-make-token))
        (t2 (emagent-mcp-make-token)))
    (should (= 24 (length t1)))
    (should (string-match-p "\\`[0-9a-f]\\{24\\}\\'" t1))
    (should (not (string= t1 t2)))))

(ert-deftest emagent-mcp-test-unresolved-env-token-p ()
  (should (emagent-mcp--unresolved-env-token-p
           "${env:EMAGENT_SESSION_TOKEN}"))
  (should (emagent-mcp--unresolved-env-token-p ""))
  (should (emagent-mcp--unresolved-env-token-p "  "))
  (should-not (emagent-mcp--unresolved-env-token-p "abcdef0123456789abcdef01"))
  (should-not (emagent-mcp--unresolved-env-token-p emagent-mcp-external-token)))

(ert-deftest emagent-mcp-test-resolve-session-token-external ()
  (let ((emagent-mcp-external-root "/tmp")
        (emagent-mcp--sessions (make-hash-table :test 'equal)))
    (emagent-test--with-mocks
        (((symbol-function 'emagent-mcp-ensure-server)
          (lambda ()
            (emagent-mcp--install-external-session)
            8771)))
      (should (equal (emagent-mcp--resolve-session-token
                      "${env:EMAGENT_SESSION_TOKEN}")
                     emagent-mcp-external-token))
      (should (gethash emagent-mcp-external-token emagent-mcp--sessions))
      (should (equal (plist-get (gethash emagent-mcp-external-token
                                         emagent-mcp--sessions)
                                :root)
                     (expand-file-name "/tmp"))))))

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
    (puthash "str" "true" args)
    (puthash "yes" "YES" args)
    (puthash "no" "false" args)
    (should (emagent-mcp--bool args "on"))
    (should (emagent-mcp--bool args "str"))
    (should (emagent-mcp--bool args "yes"))
    (should-not (emagent-mcp--bool args "off"))
    (should-not (emagent-mcp--bool args "no"))
    (should-not (emagent-mcp--bool args "missing"))))

(ert-deftest emagent-mcp-test-string-result ()
  (should (string= "hi" (emagent-mcp--string-result "hi")))
  (should (string= "" (emagent-mcp--string-result nil)))
  (should (string= "42" (emagent-mcp--string-result 42))))

;;;; Tool metadata

(ert-deftest emagent-mcp-test-run-tool-executes-handler ()
  (let* ((token "abc")
         (session (list :root "/proj" :buffer (get-buffer-create "*mcp-test*") :acp t))
         (args (make-hash-table :test 'equal)))
    (puthash "op" "compile" args)
    (puthash "command" "make test" args)
    (puthash token session emagent-mcp--sessions)
    (unwind-protect
        (emagent-test--with-mocks
            (((symbol-function 'emagent-tool-compile-async)
              (lambda (callback &rest _args) (funcall callback "ok" nil))))
          (should (string= "ok" (emagent-mcp--run-tool "shell" args session))))
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


(ert-deftest emagent-mcp-test-external-root-prefers-selected ()
  "External auto-root prefers the selected window's emagent project."
  (let* ((dir-a (make-temp-file "emagent-root-a" t))
         (dir-b (make-temp-file "emagent-root-b" t))
         (buf-a (generate-new-buffer " *emagent-root-a*"))
         (buf-b (generate-new-buffer " *emagent-root-b*"))
         (emagent-mcp-external-root nil))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (delay-mode-hooks (emagent-mode))
            (setq emagent-chat-project-directory dir-a))
          (with-current-buffer buf-b
            (delay-mode-hooks (emagent-mode))
            (setq emagent-chat-project-directory dir-b))
          (set-window-buffer (selected-window) buf-b)
          (should (equal (file-name-as-directory
                          (expand-file-name dir-b))
                         (file-name-as-directory
                          (emagent-mcp--external-root)))))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b))
      (ignore-errors (delete-directory dir-a t))
      (ignore-errors (delete-directory dir-b t)))))

(provide 'emagent-mcp-test)

;;; emagent-mcp-test.el ends here
