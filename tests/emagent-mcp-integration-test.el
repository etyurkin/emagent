;;; emagent-mcp-integration-test.el --- ERT tests for MCP dispatch and HTTP -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'seq)
(require 'emagent-test-utils)
(require 'emagent-chat)

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
  (let* ((payload (emagent-mcp--tools-list-payload))
         (tools (alist-get 'tools payload))
         (names (mapcar (lambda (tool) (alist-get 'name tool))
                        (append tools nil))))
    (should (<= (length tools) 20))
    (should (member "fs" names))
    (should (member "search" names))
    (let* ((search (seq-find (lambda (tool)
                               (string= "search" (alist-get 'name tool)))
                             (append tools nil)))
           (schema (alist-get 'inputSchema search))
           (props (alist-get 'properties schema))
           (required (append (alist-get 'required schema) nil)))
      (should (gethash "pattern" props))
      (should-not (gethash "op" props))
      (should (member "pattern" required))
      (should-not (member "op" required)))
    (should (member "list_buffers" names))
    (should (member "buffer_info" names))
    (should (member "list_windows" names))
    (should (member "imenu_index" names))
    (should (member "list_frames" names))
    (should (member "list_marks" names))
    (should (member "list_registers" names))
    (should (member "list_bookmarks" names))
    (should (member "list_diagnostics" names))
    (should (member "list_processes" names))
    (should (member "project_directory" names))
    (should (member "where_is" names))
    (should-not (member "emacs" names))
    (should (string= "fs" (alist-get 'name (aref tools 0))))))

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
           (puthash "arguments" (make-hash-table :test 'equal) params)
           (setq resp (emagent-test--tools-call-sync 3 params token))
           (setq parsed (json-parse-string resp :object-type 'alist))
           (should (string-match-p (regexp-quote dir)
                                   (map-nested-elt parsed '(result content 0 text))))))))))

(ert-deftest emagent-mcp-integration-test-handle-tools-call-missing-token ()
  (let* ((params (make-hash-table :test 'equal))
         (resp nil)
         (parsed nil))
    (puthash "name" "project_directory" params)
    (puthash "arguments" (make-hash-table :test 'equal) params)
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
    (should (string-match-p "\"fs\"" sent))))

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
            (puthash "name" "fs" params)
            (puthash "arguments"
                     (let ((args (make-hash-table :test 'equal)))
                       (puthash "op" "read" args)
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
         (args (make-hash-table :test 'equal))
         (body (json-serialize `((jsonrpc . "2.0") (id . 8) (method . "tools/call")
                                 (params . ((name . "project_directory")
                                            (arguments . ,args))))))
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

;;;; Timer-yield drain

(ert-deftest emagent-mcp-integration-test-filter-defers-dispatch-to-timer ()
  "Filter only buffers DATA and schedules a drain tick; it never dispatches inline."
  (let* ((proc (generate-new-buffer " *fake*"))
         (store (make-hash-table :test 'eq))
         (scheduled nil)
         (request (emagent-test--http-post
                   "tok" (json-serialize '((jsonrpc . "2.0") (id . 1) (method . "ping"))))))
    (emagent-test--with-mocks
        (((symbol-function 'process-get)
          (lambda (p prop) (and (eq p proc) (gethash prop store))))
         ((symbol-function 'process-put)
          (lambda (p prop val) (when (eq p proc) (puthash prop val store)) val))
         ((symbol-function 'process-live-p) (lambda (p) (eq p proc)))
         ((symbol-function 'run-with-timer)
          (lambda (_delay _repeat fn &rest args) (setq scheduled (cons fn args)) 'fake-timer))
         ((symbol-function 'process-send-string) (lambda (&rest _) nil)))
      (emagent-mcp--filter proc request)
      ;; Data is buffered but not yet parsed/dispatched by the filter itself.
      (should (equal (gethash 'emagent-mcp-data store) request))
      (should scheduled)
      ;; Running the scheduled tick drains and clears the buffered request.
      (apply (car scheduled) (cdr scheduled))
      (should (string-empty-p (gethash 'emagent-mcp-data store))))))

(ert-deftest emagent-mcp-integration-test-drain-handles-one-request-per-tick ()
  "Drain handles exactly one buffered request per tick, then reschedules for more."
  (let* ((proc (generate-new-buffer " *fake*"))
         (store (make-hash-table :test 'eq))
         (sent "")
         (scheduled nil)
         (req1 (emagent-test--http-post
                "tok" (json-serialize '((jsonrpc . "2.0") (id . 1) (method . "ping")))))
         (req2 (emagent-test--http-post
                "tok" (json-serialize '((jsonrpc . "2.0") (id . 2) (method . "ping"))))))
    (emagent-test--with-mocks
        (((symbol-function 'process-get)
          (lambda (p prop) (and (eq p proc) (gethash prop store))))
         ((symbol-function 'process-put)
          (lambda (p prop val) (when (eq p proc) (puthash prop val store)) val))
         ((symbol-function 'process-live-p) (lambda (p) (eq p proc)))
         ((symbol-function 'run-with-timer)
          (lambda (_delay _repeat fn &rest args) (setq scheduled (cons fn args)) 'fake-timer))
         ((symbol-function 'process-send-string)
          (lambda (_proc data) (setq sent (concat sent data)))))
      (puthash 'emagent-mcp-data (concat req1 req2) store)
      (emagent-mcp--drain proc)
      (should (= 1 (length (string-split sent "HTTP/1.1" t))))
      (should scheduled)
      (setq sent "")
      (let ((next scheduled))
        (setq scheduled nil)
        (apply (car next) (cdr next)))
      (should (= 1 (length (string-split sent "HTTP/1.1" t))))
      (should (string-empty-p (gethash 'emagent-mcp-data store))))))

(ert-deftest emagent-mcp-integration-test-sentinel-cancels-drain-timer ()
  "Sentinel cancels PROC's pending drain timer when the connection closes."
  (let* ((proc (generate-new-buffer " *fake*"))
         (store (make-hash-table :test 'eq))
         (cancelled nil))
    (puthash 'emagent-mcp-drain-timer 'fake-timer store)
    (emagent-test--with-mocks
        (((symbol-function 'process-get)
          (lambda (p prop) (and (eq p proc) (gethash prop store))))
         ((symbol-function 'process-put)
          (lambda (p prop val) (when (eq p proc) (puthash prop val store)) val))
         ((symbol-function 'process-live-p) (lambda (_p) nil))
         ((symbol-function 'cancel-timer) (lambda (timer) (setq cancelled timer))))
      (emagent-mcp--sentinel proc "closed"))
    (should (eq cancelled 'fake-timer))
    (should-not (gethash 'emagent-mcp-drain-timer store))))

(provide 'emagent-mcp-integration-test)

;;; emagent-mcp-integration-test.el ends here
