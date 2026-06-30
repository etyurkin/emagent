;;; emagent-chat-integration-test.el --- ERT integration tests for chat rendering -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-acp)
(require 'emagent-chat)

(ert-deftest emagent-chat-integration-test-response-cycle ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (let ((at (emagent-chat--insert-user-heading-with-text "hello")))
         (emagent-chat--begin-response at)
         (emagent-chat-append-assistant "partial ")
         (emagent-chat-finish-assistant "Hello *world*"))
       (let ((text (substring-no-properties (buffer-string))))
         (should (string-match-p "hello" text))
         (should (string-match-p "Hello" text))
         (should (string-match-p "^\\*\\* Response" text))
         (should-not (string-match-p emagent-chat--progress-line text)))))))

(ert-deftest emagent-chat-integration-test-fail-assistant ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (let ((at (emagent-chat--insert-user-heading-with-text "boom")))
         (emagent-chat--begin-response at)
         (emagent-chat-fail-assistant "agent died"))
       (let ((text (substring-no-properties (buffer-string))))
         (should (string-match-p "\\*Error:\\* agent died" text))
         (should (string-match-p "^\\*\\* Response" text)))))))

(ert-deftest emagent-chat-integration-test-acp-chunk-to-buffer ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let* ((client (emagent-test--make-test-client))
            (state (emagent-test--make-acp-state client buffer)))
       (setq emagent-acp-stream-to-buffer t)
       (map-put! state :cb-chunk #'emagent-chat-append-assistant)
       (map-put! state :busy t)
       (with-current-buffer buffer
         (goto-char (point-max))
         (emagent-chat--begin-response (point))
         (emagent-acp--on-notification
          :state state
          :emagent-acp-notification (emagent-test--notification-chunk "streamed "))
         (emagent-acp--on-notification
          :state state
          :emagent-acp-notification (emagent-test--notification-chunk "text"))
         (should (string-match-p "streamed text"
                                 (substring-no-properties (buffer-string)))))))))

(provide 'emagent-chat-integration-test)

;;; emagent-chat-integration-test.el ends here
