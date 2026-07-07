;;; emagent-acp-test.el --- ERT tests for emagent ACP protocol -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-acp-protocol)

;;;; JSON helpers

(ert-deftest emagent-acp-test-json-roundtrip ()
  (let* ((obj '((:method . "ping") (:params . ((n . 1)))))
         (json (emagent-acp--serialize-json obj))
         (again (emagent-acp--parse-json (string-trim json))))
    (should (string-match-p "\n\\'" json))
    (should (equal (alist-get :method again) "ping"))
    (should (= (alist-get 'n (alist-get :params again)) 1))))

;;;; Request builders

(ert-deftest emagent-acp-test-initialize-request ()
  (let ((req (emagent-acp-make-initialize-request
              :protocol-version 1
              :client-info '((name . "emagent") (version . "1.0.2"))
              :read-text-file-capability t
              :write-text-file-capability nil)))
    (should (equal (alist-get :method req) "initialize"))
    (should (= (alist-get 'protocolVersion (alist-get :params req)) 1))
    (should (equal (alist-get 'readTextFile
                              (alist-get 'fs (alist-get 'clientCapabilities
                                                        (alist-get :params req))))
                   t))
    (should (eq (alist-get 'writeTextFile
                           (alist-get 'fs (alist-get 'clientCapabilities
                                                     (alist-get :params req))))
                :false))))

(ert-deftest emagent-acp-test-session-new-request ()
  (let* ((cwd (expand-file-name "~/"))
         (req (emagent-acp-make-session-new-request :cwd cwd :mcp-servers '())))
    (should (equal (alist-get :method req) "session/new"))
    (should (string= (alist-get 'cwd (alist-get :params req))
                     (directory-file-name cwd)))))

(ert-deftest emagent-acp-test-session-prompt-request ()
  (let ((req (emagent-acp-make-session-prompt-request
              :session-id "sess-1"
              :prompt `[((type . "text") (text . "hello"))])))
    (should (equal (alist-get :method req) "session/prompt"))
    (should (equal (alist-get 'sessionId (alist-get :params req)) "sess-1"))
    (should (equal (length (alist-get 'prompt (alist-get :params req))) 1))))

(ert-deftest emagent-acp-test-make-error ()
  (let ((err (emagent-acp-make-error :code -32600 :message "bad request")))
    (should (= (alist-get 'code err) -32600))
    (should (equal (alist-get 'message err) "bad request"))))

;;;; Stderr parsing

(ert-deftest emagent-acp-test-parse-stderr-api-error ()
  (should-not (emagent-acp--parse-stderr-api-error "finished")))

(require 'emagent-acp-model)

(ert-deftest emagent-acp-test-model-history-variable ()
  "Helm reads completing-read history via symbol-value; the variable must exist."
  (should (boundp 'emagent-model-history))
  (should (listp (symbol-value 'emagent-model-history))))

(provide 'emagent-acp-test)

;;; emagent-acp-test.el ends here
