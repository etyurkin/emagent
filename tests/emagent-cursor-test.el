;;; emagent-cursor-test.el --- ERT tests for Cursor provider -*- lexical-binding: t; -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'emagent-chat)
(require 'emagent-cursor)
(require 'emagent-acp)

(ert-deftest emagent-cursor-test-enrich-tool-call-update ()
  (let ((update '((toolCallId . "tool_abc") (title . "Edit"))))
    (should (eq update
                (emagent-cursor-enrich-tool-call-update "missing-session" update))))
  (let ((store (lambda (_sid _id)
                 '("mcp_emagent_git_log" . (("args" . "-5 --oneline"))))))
    (cl-letf (((symbol-function 'emagent-cursor-tool-call-from-store) store))
      (let* ((update '((toolCallId . "tool_abc") (title . "MCP")
                       (subtitle . "tool") (rawInput . ())))
             (enriched (emagent-cursor-enrich-tool-call-update "sess" update)))
        (should (equal "git_log" (map-elt enriched 'title)))
        (should (equal "-5 --oneline"
                       (emagent-acp--tool-call-detail enriched)))))))

(ert-deftest emagent-cursor-test-tool-display-name ()
  (should (string= "git_log" (emagent-cursor--tool-display-name "mcp_emagent_git_log")))
  (should (string= "Read" (emagent-cursor--tool-display-name "Read"))))

(ert-deftest emagent-cursor-test-tool-call-from-blob-empty-args ()
  (let ((json "{\"content\":[{\"type\":\"tool-call\",\"toolCallId\":\"tool_mcp\",\"toolName\":\"mcp_emagent_git_diff\",\"args\":{}}]}"))
    (let ((result (emagent-cursor--tool-call-from-blob-json json "tool_mcp")))
      (should (string= "mcp_emagent_git_diff" (car result)))
      (should (null (cdr result))))))

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
