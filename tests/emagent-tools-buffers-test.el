;;; emagent-tools-buffers-test.el --- ERT for wide buffer MCP tools -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
(require 'emagent-test-utils)
(require 'emagent-chat)
(require 'emagent-tools-buffers)

(ert-deftest emagent-tools-buffers-test-list-filters-and-limit ()
  (let* ((file-buf (generate-new-buffer "emagent-buf-file.el"))
         (tmp (make-temp-file "emagent-buf-" nil ".el" "x"))
         (internal (get-buffer-create " *emagent-internal*")))
    (unwind-protect
        (progn
          (with-current-buffer file-buf
            (setq buffer-file-name tmp)
            (emacs-lisp-mode))
          (let ((rows (emagent-tool-list-buffers
                       "file" nil nil "emagent-buf-file" nil nil nil nil 10)))
            (should (= 1 (length rows)))
            (should (equal "file"
                           (alist-get "type" (car rows) nil nil #'equal))))
          (let ((rows (emagent-tool-list-buffers
                       "internal" nil nil "emagent-internal" nil nil nil nil 10)))
            (should (>= (length rows) 1))
            (should (seq-every-p
                     (lambda (row)
                       (equal "internal"
                              (alist-get "type" row nil nil #'equal)))
                     rows))))
      (when (buffer-live-p file-buf) (kill-buffer file-buf))
      (when (buffer-live-p internal) (kill-buffer internal))
      (ignore-errors (delete-file tmp)))))

(ert-deftest emagent-tools-buffers-test-info-selectors ()
  (let ((buf (get-buffer-create "*emagent-buf-info*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (erase-buffer)
            (insert "hello")
            (goto-char (point-min)))
          (let ((row (emagent-tool-buffer-info "*emagent-buf-info*")))
            (should (equal "*emagent-buf-info*"
                           (alist-get "name" row nil nil #'equal)))
            (should (equal 1 (alist-get "point" row nil nil #'equal))))
          (should-error (emagent-tool-buffer-info "no-such-buffer-xyz")
                        :type 'error)
          (should-error (emagent-tool-buffer-info "*emagent-buf-info*" 0)
                        :type 'error))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest emagent-mcp-test-string-result-json ()
  (should (string= "hi" (emagent-mcp--string-result "hi")))
  (should (string= "" (emagent-mcp--string-result nil)))
  (should (string= "42" (emagent-mcp--string-result 42)))
  (let ((json (emagent-mcp--string-result
               '(("name" . "x") ("path" . nil) ("selected" . :false)))))
    (should (string-match-p "\"name\":\"x\"" json))
    (should (string-match-p "\"path\":null" json))
    (should (string-match-p "\"selected\":false" json)))
  (should (string= "[]" (emagent-mcp--string-result (vector)))))

(ert-deftest emagent-mcp-test-list-buffers-tool-registered ()
  (should (emagent-mcp--tool-entry "list_buffers"))
  (should (emagent-mcp--tool-entry "buffer_info"))
  (let* ((payload (emagent-mcp--tools-list-payload))
         (tools (mapcar (lambda (tool) (alist-get 'name tool))
                        (append (alist-get 'tools payload) nil))))
    (should (member "list_buffers" tools))
    (should (member "buffer_info" tools))))

(ert-deftest emagent-mcp-test-list-buffers-run-tool-json ()
  (let* ((buf (get-buffer-create "*emagent-mcp-list*"))
         (session (list :root "/tmp" :buffer buf))
         (args (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (puthash "name_filter_regex" "emagent-mcp-list" args)
          (puthash "limit" 5 args)
          (let* ((out (emagent-mcp--run-tool "list_buffers" args session))
                 (parsed (json-parse-string out :object-type 'alist
                                            :array-type 'list)))
            (should (listp parsed))
            (should (seq-every-p #'listp parsed))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'emagent-tools-buffers-test)

;;; emagent-tools-buffers-test.el ends here
