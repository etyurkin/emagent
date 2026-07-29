;;; emagent-tools-buffers-test.el --- ERT for wide buffer MCP tools -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
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
  (should (emagent-mcp--tool-entry "list_windows"))
  (should (emagent-mcp--tool-entry "list_frames"))
  (should (emagent-mcp--tool-entry "list_marks"))
  (should (emagent-mcp--tool-entry "list_registers"))
  (should (emagent-mcp--tool-entry "list_bookmarks"))
  (should (emagent-mcp--tool-entry "list_diagnostics"))
  (should (emagent-mcp--tool-entry "imenu_index"))
  (should (emagent-mcp--tool-entry "project_directory"))
  (should (emagent-mcp--tool-entry "where_is"))
  (should-not (emagent-mcp--tool-entry "emacs"))
  (should (eq (nth 4 (emagent-mcp--tool-entry "fs"))
              'emagent-mcp-tool-fs))
  (let* ((entry (emagent-mcp--tool-entry "where_is"))
         (props (nth 2 entry))
         (required (nth 3 entry)))
    (should (assoc-string "command" props t))
    (should (equal required '("command"))))
  (let* ((payload (emagent-mcp--tools-list-payload))
         (tools (mapcar (lambda (tool) (alist-get 'name tool))
                        (append (alist-get 'tools payload) nil))))
    (should (<= (length tools) 19))
    (should (member "list_buffers" tools))
    (should (member "buffer_info" tools))
    (should (member "list_windows" tools))
    (should (member "list_frames" tools))
    (should (member "list_marks" tools))
    (should (member "list_registers" tools))
    (should (member "list_bookmarks" tools))
    (should (member "list_diagnostics" tools))
    (should (member "imenu_index" tools))
    (should (member "project_directory" tools))
    (should (member "where_is" tools))))

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

(ert-deftest emagent-mcp-test-cont-sync-and-async ()
  (should (emagent-mcp-cont-p
           (emagent-mcp-cont (reply) (funcall reply "x"))))
  (let ((got nil))
    (funcall (emagent-mcp-cont-function
              (emagent-mcp-cont (reply) (funcall reply "done")))
             (lambda (result &optional is-error)
               (setq got (list result is-error))))
    (should (equal got '("done" nil))))
  (let* ((session (list :root "/tmp" :buffer (current-buffer)))
         (args (make-hash-table :test 'equal))
         (got nil))
    (puthash "form" "42" args)
    (emagent-mcp--run-tool-async
     "eval" args session
     (lambda (result is-error) (setq got (list result is-error))))
    (should (equal got '("42" nil)))))

(ert-deftest emagent-mcp-test-cont-cancel-drops-reply ()
  (let* ((session (list :root "/tmp" :buffer (current-buffer)))
         (args (make-hash-table :test 'equal))
         (got 'unset)
         (entry (emagent-mcp--tool-entry "eval")))
    (cl-letf (((symbol-function (nth 4 entry))
               (lambda (_args)
                 (let ((c nil))
                   (setq c (emagent-mcp-cont (reply)
                             (emagent-mcp-cont-cancel c)
                             (funcall reply "should-drop")))
                   c))))
      (emagent-mcp--run-tool-async
       "eval" args session
       (lambda (result is-error) (setq got (list result is-error))))
      (should (equal got '("Cancelled" t))))))

(ert-deftest emagent-mcp-test-cont-reply-hook ()
  (let* ((session (list :root "/tmp" :buffer (current-buffer)))
         (args (make-hash-table :test 'equal))
         (hooked nil)
         (got nil))
    (puthash "form" "7" args)
    (let ((emagent-mcp-cont-reply-functions
           (list (lambda (cont result is-error)
                   (setq hooked (list (emagent-mcp-cont-p cont)
                                      result is-error))))))
      ;; Sync return does not use cont; wrap eval to return a cont.
      (let ((entry (emagent-mcp--tool-entry "eval")))
        (cl-letf (((symbol-function (nth 4 entry))
                   (lambda (_args)
                     (emagent-mcp-cont (reply)
                       (funcall reply "hooked-ok")))))
          (emagent-mcp--run-tool-async
           "eval" args session
           (lambda (result is-error) (setq got (list result is-error))))
          (should (equal got '("hooked-ok" nil)))
          (should (equal hooked '(t "hooked-ok" nil))))))))

(ert-deftest emagent-mcp-test-typed-deftool-positional ()
  (let* ((session (list :root "/tmp" :buffer (current-buffer)))
         (args (make-hash-table :test 'equal))
         (seen nil))
    (cl-letf (((symbol-function 'emagent-tool-where-is)
               (lambda (command) (setq seen command) "bound")))
      (puthash "command" "find-file" args)
      (should (string= "bound"
                       (emagent-mcp--run-tool "where_is" args session)))
      (should (string= "find-file" seen)))))

(ert-deftest emagent-tools-buffers-test-list-windows-selected ()
  (let ((rows (emagent-tool-list-windows nil nil t 5)))
    (should (= 1 (length rows)))
    (should (eq t (alist-get "selected" (car rows) nil nil #'equal)))))

(ert-deftest emagent-tools-buffers-test-imenu-records ()
  (let ((buf (generate-new-buffer "emagent-imenu.el")))
    (unwind-protect
        (with-current-buffer buf
          (emacs-lisp-mode)
          (insert "(defun emagent-imenu-demo ()\n  1)\n")
          (setq imenu-create-index-function #'imenu-default-create-index-function)
          (let ((rows (emagent-tool-imenu-records)))
            (should (consp rows))
            (should (alist-get "name" (car rows) nil nil #'equal))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest emagent-tools-buffers-test-list-frames-selected ()
  (let ((rows (emagent-tool-list-frames nil t 5)))
    (should (= 1 (length rows)))
    (should (eq t (alist-get "selected" (car rows) nil nil #'equal)))
    (should (alist-get "buffer_names" (car rows) nil nil #'equal))))

(ert-deftest emagent-tools-buffers-test-list-marks ()
  (let ((buf (get-buffer-create "*emagent-marks*")))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "abcdef")
          (goto-char 2)
          (push-mark 5 t t)
          (let ((rows (emagent-tool-list-marks "*emagent-marks*" nil 10)))
            (should (>= (length rows) 1))
            (should (eq t (alist-get "current" (car rows) nil nil #'equal)))
            (should (equal "*emagent-marks*"
                           (alist-get "buffer_name" (car rows) nil nil #'equal)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest emagent-tools-buffers-test-list-registers ()
  (let ((register-alist `((?a . "hello") (?b . 42))))
    (let ((rows (emagent-tool-list-registers "string" 10)))
      (should (= 1 (length rows)))
      (should (equal "a" (alist-get "name" (car rows) nil nil #'equal)))
      (should (equal "string" (alist-get "type" (car rows) nil nil #'equal))))
    (let ((rows (emagent-tool-list-registers nil 1)))
      (should (= 1 (length rows))))))


(ert-deftest emagent-mcp-test-cont-cancel-kills-process ()
  (let* ((session (list :root "/tmp" :buffer (current-buffer)))
         (args (make-hash-table :test 'equal))
         (got 'unset)
         (live-proc nil)
         (cont nil)
         (entry (emagent-mcp--tool-entry "eval")))
    (cl-letf (((symbol-function (nth 4 entry))
               (lambda (_args)
                 (setq cont
                       (emagent-mcp-cont (reply)
                         (emagent-tools--run-process-async
                          (lambda (output is-error)
                            (funcall reply output is-error))
                          "sleep" "30")))
                 cont)))
      (let ((emagent-mcp--current-session-token "tok-cancel-proc"))
        (emagent-mcp--run-tool-async
         "eval" args session
         (lambda (result is-error) (setq got (list result is-error)))))
      (setq live-proc
            (car (seq-filter
                  (lambda (p)
                    (and (process-live-p p)
                         (string-prefix-p "emagent-proc" (process-name p))))
                  (process-list))))
      (should (process-live-p live-proc))
      (should (emagent-mcp-cont-p cont))
      (emagent-mcp-cont-cancel cont)
      (should (equal got '("Cancelled" t)))
      (should-not (process-live-p live-proc)))))

(ert-deftest emagent-mcp-test-cont-cancel-fetch-url-buffer ()
  (let* ((session (list :root "/tmp" :buffer (current-buffer)))
         (args (make-hash-table :test 'equal))
         (got 'unset)
         (retrieve-buf (generate-new-buffer " *emagent-url-test*"))
         (entry (emagent-mcp--tool-entry "fetch_url")))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (&rest _)
                     retrieve-buf)))
          (puthash "url" "https://example.com/" args)
          (let ((emagent-mcp--current-session-token "tok-cancel-fetch"))
            (emagent-mcp--run-tool-async
             "fetch_url" args session
             (lambda (result is-error) (setq got (list result is-error)))))
          (let ((conts (gethash "tok-cancel-fetch" emagent-mcp--session-inflight)))
            (should conts)
            (emagent-mcp-cont-cancel (car conts)))
          (should (equal got '("Cancelled" t)))
          (should (null (buffer-name retrieve-buf))))
      (when (buffer-name retrieve-buf)
        (kill-buffer retrieve-buf)))))

(ert-deftest emagent-mcp-test-cancel-session-tools ()
  (let* ((token "tok-session-cancel")
         (got nil)
         (cont (emagent-mcp-cont (reply) (ignore reply))))
    (emagent-mcp-cont-put
     cont :cancel-notify
     (lambda () (setq got 'notified)))
    (puthash token (list cont) emagent-mcp--session-inflight)
    (emagent-mcp-cancel-session-tools token)
    (should (eq got 'notified))
    (should (emagent-mcp-cont-cancelled-p cont))
    (should-not (gethash token emagent-mcp--session-inflight))))

(ert-deftest emagent-mcp-test-sentinel-cancels-inflight-conts ()
  (let* ((proc (generate-new-buffer " *fake-mcp-client*"))
         (store (make-hash-table :test 'eq))
         (got nil)
         (cont (emagent-mcp-cont (reply) (ignore reply))))
    (unwind-protect
        (progn
          (emagent-mcp-cont-put
           cont :cancel-notify
           (lambda () (setq got 'cancelled)))
          (puthash 'emagent-mcp-inflight-conts (list cont) store)
          (emagent-test--with-mocks
              (((symbol-function 'process-get)
                (lambda (p prop) (and (eq p proc) (gethash prop store))))
               ((symbol-function 'process-put)
                (lambda (p prop val)
                  (when (eq p proc) (puthash prop val store))
                  val))
               ((symbol-function 'process-live-p) (lambda (_p) nil)))
            (emagent-mcp--sentinel proc "deleted"))
          (should (eq got 'cancelled))
          (should (emagent-mcp-cont-cancelled-p cont)))
      (when (buffer-live-p proc)
        (kill-buffer proc)))))

(ert-deftest emagent-mcp-test-finalize-cancels-session-tools ()
  (require 'emagent-acp)
  (let* ((token "tok-finalize-cancel")
         (got nil)
         (cont (emagent-mcp-cont (reply) (ignore reply)))
         (chat (generate-new-buffer " *emagent-finalize-cancel*"))
         (state (emagent-acp--make-state :chat-buffer chat :client nil))
         (emagent-acp--session nil))
    (unwind-protect
        (progn
          (emagent-mcp-cont-put
           cont :cancel-notify
           (lambda () (setq got 'from-finalize)))
          (puthash token (list cont) emagent-mcp--session-inflight)
          (with-current-buffer chat
            (setq-local emagent-mcp--token token))
          (setf (emagent-acp-state-busy state) t)
          (setf (emagent-acp-state-session-id state) "s1")
          (setq emagent-acp--session state)
          (emagent-test--with-mocks
              (((symbol-function 'emagent-acp--clear-prompt-watchdog)
                (lambda (&rest _) nil))
               ((symbol-function 'emagent-acp--cancel-prompt-render)
                (lambda (&rest _) nil))
               ((symbol-function 'emagent-acp--flush-thought-buffer)
                (lambda (&rest _) nil))
               ((symbol-function 'emagent-acp--reset-permission-gate)
                (lambda (&rest _) nil))
               ((symbol-function 'emagent-acp--render-prompt-response)
                (lambda (&rest _) nil))
               ((symbol-function 'emagent-acp--refresh-mode-line)
                (lambda (&rest _) nil)))
            (let ((emagent-mcp--token token))
              (should (emagent-acp--finalize-in-flight-prompt "stopped"))))
          (should (eq got 'from-finalize))
          (should (emagent-mcp-cont-cancelled-p cont)))
      (remhash token emagent-mcp--session-inflight)
      (setq emagent-acp--session nil)
      (when (buffer-live-p chat)
        (kill-buffer chat)))))


(ert-deftest emagent-mcp-test-search-no-op-arg ()
  (let* ((session (list :root "/tmp" :buffer (current-buffer)))
         (args (make-hash-table :test 'equal))
         (got nil)
         (entry (emagent-mcp--tool-entry "search")))
    (should entry)
    (should-not (assoc-string "op" (nth 2 entry) t))
    (should (equal (nth 3 entry) '("pattern")))
    (cl-letf (((symbol-function 'emagent-tool-grep-async)
               (lambda (cb pattern path)
                 (funcall cb (format "pattern=%s path=%s" pattern path) nil))))
      (puthash "pattern" "foo" args)
      (emagent-mcp--run-tool-async
       "search" args session
       (lambda (result is-error) (setq got (list result is-error))))
      (should (equal got '("pattern=foo path=nil" nil))))))


(ert-deftest emagent-tools-buffers-test-list-diagnostics-filter ()
  (require 'flymake)
  (let ((buf (generate-new-buffer "emagent-diag.el")))
    (unwind-protect
        (with-current-buffer buf
          (setq buffer-file-name (expand-file-name "emagent-diag.el" temporary-file-directory))
          (insert "(+ 1 2)\n")
          (emacs-lisp-mode)
          (flymake-mode 1)
          (cl-letf (((symbol-function 'flymake-diagnostics)
                     (lambda (&optional _beg _end)
                       (list (flymake-make-diagnostic
                              (current-buffer) 1 2
                              'flymake-error "boom")))))
            (let ((rows (emagent-tool-list-diagnostics
                         "emagent-diag.el" nil nil "error" 10)))
              (should (= 1 (length rows)))
              (should (equal "error"
                             (alist-get "severity" (car rows) nil nil #'equal)))
              (should (equal "boom"
                             (alist-get "message" (car rows) nil nil #'equal))))
            (let ((rows (emagent-tool-list-diagnostics
                         nil "emagent-diag" nil "warning" 10)))
              (should (= 1 (length rows))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest emagent-tools-buffers-test-list-bookmarks-filter ()
  (require 'bookmark)
  (let ((bookmark-alist nil)
        (tmp (make-temp-file "emagent-bm-" nil ".el" "(+ 1 2)\n")))
    (unwind-protect
        (progn
          (bookmark-store "emagent-bm-one"
                          `((filename . ,tmp) (position . 1)
                            (front-context-string . "(")
                            (rear-context-string . "+"))
                          nil)
          (bookmark-store "other-bm"
                          '((filename . "/no/such/file.el") (position . 1))
                          nil)
          (let ((rows (emagent-tool-list-bookmarks "emagent-bm" nil "file" 10)))
            (should (= 1 (length rows)))
            (should (equal "emagent-bm-one"
                           (alist-get "name" (car rows) nil nil #'equal)))
            (should (equal "file"
                           (alist-get "type" (car rows) nil nil #'equal)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest emagent-mcp-test-list-diagnostics-run-tool-json ()
  (require 'flymake)
  (let* ((buf (get-buffer-create "*emagent-mcp-diag*"))
         (session (list :root "/tmp" :buffer buf))
         (args (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (puthash "limit" 5 args)
          (cl-letf (((symbol-function 'emagent-tool-list-diagnostics)
                     (lambda (&rest _)
                       '((("buffer_name" . "*x*")
                          ("severity" . "error")
                          ("message" . "m"))))))
            (let* ((out (emagent-mcp--run-tool "list_diagnostics" args session))
                   (parsed (json-parse-string out :object-type 'alist
                                              :array-type 'list)))
              (should (listp parsed))
              (should (= 1 (length parsed))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))


(provide 'emagent-tools-buffers-test)

;;; emagent-tools-buffers-test.el ends here
