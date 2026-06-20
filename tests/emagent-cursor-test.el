;;; emagent-cursor-test.el --- ERT tests for Cursor provider -*- lexical-binding: t; -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'emagent-chat)
(require 'emagent-cursor)
(require 'emagent-acp)

(ert-deftest emagent-cursor-test-tool-call-from-blob-json ()
  (let ((json "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool-call\",\"toolCallId\":\"tool_abc\",\"toolName\":\"StrReplace\",\"args\":{\"path\":\"foo.el\"}}]}"))
    (should (string= "foo.el"
                     (emagent-acp--tool-call-raw-input-detail
                      (cdr (emagent-cursor--tool-call-from-blob-json json "tool_abc")))))))

(ert-deftest emagent-cursor-test-enrich-tool-call-update ()
  (let ((update '((toolCallId . "tool_abc") (title . "Edit"))))
    (should (eq update
                (emagent-cursor-enrich-tool-call-update "missing-session" update)))))

(ert-deftest emagent-cursor-test-normalize-slash-prompt ()
  (should (string= "/compress" (emagent-cursor-normalize-slash-prompt "/compact")))
  (should (string= "/compress now" (emagent-cursor-normalize-slash-prompt "/compact now")))
  (should (string= "/run-everything on"
                   (emagent-cursor-normalize-slash-prompt "/auto-run on")))
  (should (string= "/plan" (emagent-cursor-normalize-slash-prompt "/plan")))
  (should (string= "not a command" (emagent-cursor-normalize-slash-prompt "not a command"))))

(ert-deftest emagent-cursor-test-builtin-slash-commands ()
  (let ((cmds (emagent-cursor-slash-commands nil)))
    (should (> (length cmds) 10))
    (should (cl-find "compress" cmds :key (lambda (c) (map-elt c 'name)) :test #'string=))
    (should (cl-find "compact" cmds :key (lambda (c) (map-elt c 'name)) :test #'string=))
    (should (cl-find "plan" cmds :key (lambda (c) (map-elt c 'name)) :test #'string=))))

(ert-deftest emagent-cursor-test-custom-slash-commands ()
  (let* ((root (emagent-test--temp-directory))
         (commands-dir (expand-file-name ".cursor/commands" root))
         (file (expand-file-name "deploy.md" commands-dir)))
    (unwind-protect
        (progn
          (make-directory commands-dir t)
          (with-temp-file file
            (insert "# Deploy to staging\nDo the thing."))
          (let ((cmds (emagent-cursor-slash-commands root)))
            (should (cl-find "deploy" cmds :key (lambda (c) (map-elt c 'name)) :test #'string=))
            (should (string= "Deploy to staging"
                             (map-elt (cl-find "deploy" cmds
                                               :key (lambda (c) (map-elt c 'name))
                                               :test #'string=)
                                      'description)))))
      (when (file-exists-p root)
        (delete-directory root t)))))

(provide 'emagent-cursor-test)

;;; emagent-cursor-test.el ends here
