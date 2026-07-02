;;; emagent-mcp-integration-test.el --- ERT tests for MCP dispatch and HTTP -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-mcp)

;;;; HTTP parsing

(ert-deftest emagent-mcp-integration-test-path-token ()
  (should (string= "abc123" (emagent-mcp--path-token "/mcp/abc123")))
  (should (string= "abc123" (emagent-mcp--path-token "/mcp/abc123/tools")))
  (should-not (emagent-mcp--path-token "/health")))

(ert-deftest emagent-mcp-integration-test-parse-headers ()
  (let ((headers (emagent-mcp--parse-headers
                  '("Content-Type: application/json"
                    "Content-Length: 42"))))
    (should (string= "application/json" (cdr (assoc "content-type" headers))))
    (should (string= "42" (cdr (assoc "content-length" headers))))))

(ert-deftest emagent-mcp-integration-test-reason-phrase ()
  (should (string= "OK" (emagent-mcp--reason-phrase 200)))
  (should (string= "Not Found" (emagent-mcp--reason-phrase 404))))

;;;; RPC dispatch

(ert-deftest emagent-mcp-integration-test-dispatch-initialize ()
  (let* ((proc (generate-new-buffer " *fake*"))
         (sent nil)
         (params (make-hash-table :test 'equal))
         (message (make-hash-table :test 'equal)))
    (puthash "protocolVersion" "2025-01-01" params)
    (puthash "id" 1 message)
    (puthash "method" "initialize" message)
    (puthash "params" params message)
    (emagent-test--with-mocks
        (((symbol-function 'process-live-p) (lambda (p) (eq p proc)))
         ((symbol-function 'process-send-string)
          (lambda (_proc data) (setq sent (concat (or sent "") data)))))
      (emagent-mcp--dispatch proc "tok" message))
    (should (string-match-p "HTTP/1.1 200" sent))
    (should (string-match-p "2025-01-01" sent))))

(ert-deftest emagent-mcp-integration-test-dispatch-unknown-method ()
  (let ((proc (generate-new-buffer " *fake*"))
        (sent nil)
        (message (make-hash-table :test 'equal)))
    (puthash "id" 9 message)
    (puthash "method" "nope" message)
    (emagent-test--with-mocks
        (((symbol-function 'process-live-p) (lambda (p) (eq p proc)))
         ((symbol-function 'process-send-string)
          (lambda (_proc data) (setq sent (concat (or sent "") data)))))
      (emagent-mcp--dispatch proc "tok" message))
    (should (string-match-p "-32601" sent))
    (should (string-match-p "Method not found" sent))))

(ert-deftest emagent-mcp-integration-test-tools-list-payload ()
  (let ((payload (emagent-mcp--tools-list-payload)))
    (should (> (length (alist-get 'tools payload)) 10))
    (should (string= "read_file"
                     (alist-get 'name (aref (alist-get 'tools payload) 0))))))

;;;; Tool execution

(ert-deftest emagent-mcp-integration-test-handle-tools-call-project-directory ()
  (emagent-test--with-temp-project
   (lambda (dir)
     (emagent-test--with-mcp-session dir
       (lambda (token _buffer)
         (let* ((params (make-hash-table :test 'equal))
                (resp nil)
                (parsed nil))
           (puthash "name" "project_directory" params)
           (setq resp (emagent-test--tools-call-sync 3 params token))
           (setq parsed (json-parse-string resp :object-type 'alist))
           (should (string-match-p (regexp-quote dir)
                                   (map-nested-elt parsed '(result content 0 text))))))))))

(ert-deftest emagent-mcp-integration-test-handle-tools-call-missing-token ()
  (let* ((params (make-hash-table :test 'equal))
         (resp nil)
         (parsed nil))
    (puthash "name" "project_directory" params)
    (setq resp (emagent-test--tools-call-sync 1 params nil))
    (setq parsed (json-parse-string resp :object-type 'alist))
    (should (string-match-p "No emagent session token"
                            (map-nested-elt parsed '(result content 0 text))))))

(ert-deftest emagent-mcp-integration-test-handle-tools-call-unknown-tool ()
  (emagent-test--with-mcp-session "/tmp"
   (lambda (token _buffer)
     (let* ((params (make-hash-table :test 'equal))
            (resp nil)
            (parsed nil))
       (puthash "name" "not_a_real_tool" params)
       (setq resp (emagent-test--tools-call-sync 2 params token))
       (setq parsed (json-parse-string resp :object-type 'alist))
       (should (string-match-p "Unknown tool"
                               (map-nested-elt parsed '(result content 0 text))))))))

;;;; HTTP request handling

(ert-deftest emagent-mcp-integration-test-handle-request-post ()
  (let* ((proc (generate-new-buffer " *fake*"))
         (sent nil)
         (body (json-serialize '((jsonrpc . "2.0") (id . 5) (method . "ping"))))
         (content (encode-coding-string body 'utf-8)))
    (emagent-test--with-mocks
        (((symbol-function 'process-live-p) (lambda (p) (eq p proc)))
         ((symbol-function 'process-send-string)
          (lambda (_proc data) (setq sent (concat (or sent "") data)))))
      (emagent-test--with-mcp-session "/tmp"
       (lambda (token _buffer)
         (emagent-mcp--handle-request
          proc (format "POST /mcp/%s HTTP/1.1" token)
          nil
          content))))
    (should (string-match-p "HTTP/1.1 200" sent))))

(ert-deftest emagent-mcp-integration-test-dispatch-tools-list ()
  (let ((proc (generate-new-buffer " *fake*"))
        (sent nil)
        (message (make-hash-table :test 'equal)))
    (puthash "id" 3 message)
    (puthash "method" "tools/list" message)
    (emagent-test--with-mocks
        (((symbol-function 'process-live-p) (lambda (p) (eq p proc)))
         ((symbol-function 'process-send-string)
          (lambda (_proc data) (setq sent (concat (or sent "") data)))))
      (emagent-mcp--dispatch proc "tok" message))
    (should (string-match-p "HTTP/1.1 200" sent))
    (should (string-match-p "read_file" sent))))

(ert-deftest emagent-mcp-integration-test-handle-request-options ()
  (let ((proc (generate-new-buffer " *fake*"))
        (sent nil))
    (emagent-test--with-mocks
        (((symbol-function 'process-live-p) (lambda (p) (eq p proc)))
         ((symbol-function 'process-send-string)
          (lambda (_proc data) (setq sent (concat (or sent "") data)))))
      (emagent-mcp--handle-request proc "OPTIONS /mcp/tok HTTP/1.1" nil ""))
    (should (string-match-p "HTTP/1.1 204" sent))))

(ert-deftest emagent-mcp-integration-test-handle-request-invalid-json ()
  (let ((proc (generate-new-buffer " *fake*"))
        (sent nil))
    (emagent-test--with-mocks
        (((symbol-function 'process-live-p) (lambda (p) (eq p proc)))
         ((symbol-function 'process-send-string)
          (lambda (_proc data) (setq sent (concat (or sent "") data)))))
      (emagent-mcp--handle-request proc "POST /mcp/tok HTTP/1.1" nil "not-json"))
    (should (string-match-p "-32700" sent))))

(ert-deftest emagent-mcp-integration-test-handle-tools-call-read-file ()
  (emagent-test--with-temp-project
   (lambda (dir)
     (let ((file (expand-file-name "readme.txt" dir)))
       (write-region "integration readme" nil file)
       (emagent-test--with-mcp-session dir
        (lambda (token _buffer)
          (let* ((params (make-hash-table :test 'equal))
                 (parsed nil))
            (puthash "name" "read_file" params)
            (puthash "arguments"
                     (let ((args (make-hash-table :test 'equal)))
                       (puthash "path" "readme.txt" args)
                       args)
                     params)
            (setq parsed (json-parse-string
                           (emagent-test--tools-call-sync 4 params token)
                           :object-type 'alist))
            (should (string-match-p "integration readme"
                                    (map-nested-elt parsed '(result content 0 text)))))))))))

(ert-deftest emagent-mcp-integration-test-handle-request-tools-call ()
  (let* ((proc (generate-new-buffer " *fake*"))
         (sent nil)
         (body (json-serialize `((jsonrpc . "2.0") (id . 8) (method . "tools/call")
                                  (params . ((name . "project_directory")
                                             (arguments . ()))))))
         (content (encode-coding-string body 'utf-8)))
    (emagent-test--with-mocks
        (((symbol-function 'process-live-p) (lambda (p) (eq p proc)))
         ((symbol-function 'process-send-string)
          (lambda (_proc data) (setq sent (concat (or sent "") data))))
         ((symbol-function 'run-with-idle-timer)
          (lambda (_delay _repeat function)
            (funcall function))))
      (emagent-test--with-mcp-session "/tmp"
       (lambda (token _buffer)
         (emagent-mcp--handle-request
          proc (format "POST /mcp/%s HTTP/1.1" token)
          nil
          content))))
    (should (string-match-p "HTTP/1.1 200" sent))
    (should (string-match-p "/tmp" sent))))

(provide 'emagent-mcp-integration-test)

;;; emagent-mcp-integration-test.el ends here
