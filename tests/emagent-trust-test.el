;;; emagent-trust-test.el --- ERT tests for emagent trust -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-trust)
(require 'emagent-trust-claude)
(require 'emagent-trust-cursor)

;;;; Path normalization

(ert-deftest emagent-trust-test-normalize-dir ()
  (should (string= (emagent-trust--normalize-dir "/tmp/foo/")
                   (emagent-trust--normalize-dir "/tmp/foo")))
  (should (string= "/" (emagent-trust--normalize-dir "/"))))

;;;; JSON round-trip

(ert-deftest emagent-trust-test-json-roundtrip ()
  (let ((path (emagent-test--temp-file ".json"))
        (root (emagent-test--sample-claude-root)))
    (unwind-protect
        (progn
          (emagent-trust--json-write-file root path)
          (let ((again (emagent-trust--json-read-file path)))
            (should
             (emagent-trust-claude--has-trust-accepted-p
              (alist-get "/tmp/proj"
                         (alist-get "projects" again nil nil #'equal)
                         nil nil #'equal)))))
      (when (file-exists-p path)
        (delete-file path)))))

;;;; Claude trust

(ert-deftest emagent-trust-test-claude-has-trust-accepted-p ()
  (should (emagent-trust-claude--has-trust-accepted-p
           (list (cons "hasTrustDialogAccepted" t))))
  (should-not (emagent-trust-claude--has-trust-accepted-p
               (list (cons "hasTrustDialogAccepted" :json-false))))
  (should-not (emagent-trust-claude--has-trust-accepted-p
               (list (cons "allowedTools" (vector))))))

(ert-deftest emagent-trust-test-claude-trusted-p-walks-parent ()
  (let* ((path (emagent-test--temp-file ".json"))
         (root (list (cons "projects"
                           (list (cons "/tmp"
                                       (list (cons "hasTrustDialogAccepted" t))))))))
    (unwind-protect
        (progn
          (emagent-trust--json-write-file root path)
          (let ((emagent-trust-claude-json-file path))
            (should (emagent-trust-claude-trusted-p "/tmp/deep/nested"))
            (should-not (emagent-trust-claude-trusted-p "/other"))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest emagent-trust-test-claude-record-trust ()
  (let* ((path (emagent-test--temp-file ".json"))
         (dir "/tmp/emagent-trust-test"))
    (unwind-protect
        (progn
          (emagent-trust--json-write-file nil path)
          (let ((emagent-trust-claude-json-file path))
            (emagent-trust-claude-record-trust dir)
            (should (emagent-trust-claude-trusted-p dir))))
      (when (file-exists-p path)
        (delete-file path)))))

;;;; Cursor trust

(ert-deftest emagent-trust-test-cursor-project-slug ()
  (should (string= "" (emagent-trust-cursor--project-slug "/")))
  (should (string= "Users-etyurkin-dev"
                   (emagent-trust-cursor--project-slug "/Users/etyurkin/dev"))))

(ert-deftest emagent-trust-test-cursor-trust-file ()
  (let ((tf (emagent-trust-cursor--trust-file "/Users/foo/bar")))
    (should (string-match-p "projects/Users-foo-bar/\\.workspace-trusted\\'" tf))))

(provide 'emagent-trust-test)

;;; emagent-trust-test.el ends here
