;;; emagent-acp-routing-test.el --- ERT tests for ACP message routing -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-acp-protocol)

(defun emagent-test--route-client (on-success &optional on-failure)
  "Return a test client with pending request id 1."
  (let ((client (emagent-test--make-test-client)))
    (map-put! client :pending-requests
              (list (cons 1
                          (list (cons :on-success on-success)
                                (cons :on-failure on-failure)
                                (cons :buffer (current-buffer))))))
    client))

(ert-deftest emagent-acp-routing-test-result ()
  (let* ((box (list nil))
         (client (emagent-test--route-client (lambda (r) (setcar box r))))
         (msg (emagent-acp--make-message
               :json "{}"
               :object '((jsonrpc . "2.0") (id . 1) (result . ((sessionId . "sess-1")))))))
    (emagent-acp--route-incoming-message
     :client client :message msg
     :on-notification (lambda (_msg) nil)
     :on-request (lambda (_req) nil))
    (should (equal '((sessionId . "sess-1")) (car box)))
    (should (null (map-elt client :pending-requests)))))

(ert-deftest emagent-acp-routing-test-error ()
  (let* ((box (list nil))
         (client (emagent-test--route-client nil (lambda (e _m) (setcar box e))))
         (msg (emagent-acp--make-message
               :json "{}"
               :object '((jsonrpc . "2.0") (id . 1)
                         (error . ((code . -32600) (message . "bad")))))))
    (emagent-acp--route-incoming-message
     :client client :message msg
     :on-notification (lambda (_msg) nil)
     :on-request (lambda (_req) nil))
    (should (= -32600 (alist-get 'code (car box))))
    (should (string= "bad" (alist-get 'message (car box))))))

(ert-deftest emagent-acp-routing-test-notification ()
  (let ((got nil)
        (client (emagent-test--make-test-client))
        (msg (emagent-acp--make-message
              :json "{}"
              :object '((jsonrpc . "2.0") (method . "session/update")
                        (params . ((update . ((sessionUpdate . "agent_message_chunk")
                                              (content . ((type . "text") (text . "hi")))))))))))
    (emagent-acp--route-incoming-message
     :client client :message msg
     :on-notification (lambda (n) (setq got n))
     :on-request (lambda (_req) nil))
    (should (equal (alist-get 'method got) "session/update"))))

(ert-deftest emagent-acp-routing-test-incoming-request ()
  (let ((got nil)
        (client (emagent-test--make-test-client))
        (msg (emagent-acp--make-message
              :json "{}"
              :object '((jsonrpc . "2.0") (id . 9) (method . "fs/read_text_file")
                        (params . ((path . "foo.txt")))))))
    (emagent-acp--route-incoming-message
     :client client :message msg
     :on-notification (lambda (_msg) nil)
     :on-request (lambda (r) (setq got r)))
    (should (= 9 (alist-get 'id got)))
    (should (equal (alist-get 'method got) "fs/read_text_file"))))

(ert-deftest emagent-acp-routing-test-fail-pending ()
  (let* ((failures nil)
         (client (emagent-test--route-client
                  (lambda (_result) (error "should not succeed"))
                  (lambda (e _m) (push e failures))))
         (pending (map-elt client :pending-requests)))
    (should (= 1 (length pending)))
    (should (functionp
             (alist-get :on-failure (cdr (assq 1 pending)))))
    (emagent-acp--fail-pending-requests :client client :event "exit")
    (should (= 1 (length failures)))
    (should (string-match-p "Agent process ended" (alist-get 'message (car failures))))
    (should (null (map-elt client :pending-requests)))))

(ert-deftest emagent-acp-routing-test-fake-request-sender ()
  (let ((result nil)
        (request (emagent-acp-make-session-new-request :cwd "/tmp")))
    (emagent-test--record-request-sender
     :request request
     :on-success (lambda (r) (setq result r)))
    (should (equal request emagent-test--last-sent-request))
    (should (equal (alist-get :method request) "session/new"))
    (should (equal result '((ok . t))))))

(provide 'emagent-acp-routing-test)

;;; emagent-acp-routing-test.el ends here
