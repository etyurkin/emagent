;;; emagent-tools-test.el --- ERT tests for emagent tools -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-tools)

;;;; Session root boundary

(ert-deftest emagent-tools-test-within-boundary-p ()
  (let ((emagent-tools--root-boundary "/tmp/project"))
    (should (emagent-tools--within-boundary-p "/tmp/project/src/foo.el"))
    (should (emagent-tools--within-boundary-p "/tmp/project"))
    (should-not (emagent-tools--within-boundary-p "/tmp/other"))))

(ert-deftest emagent-tools-test-root-directory ()
  (let ((emagent-tools--root-boundary "/tmp/project")
        (emagent-tools--project-directory "/tmp/project"))
    (should (string= (emagent-tools--root-directory "src/foo.el")
                     (expand-file-name "src/foo.el" "/tmp/project")))))

;;;; Glob conversion

(ert-deftest emagent-tools-test-glob-to-regexp ()
  (should (string-match-p (emagent-tools--glob-to-regexp "*.el") "./foo.el"))
  (should (string-match-p (emagent-tools--glob-to-regexp "**/*.el") "./dir/foo.el"))
  (should (string-match-p (emagent-tools--glob-to-regexp "foo?.el") "./foox.el")))

;;;; Write diff

(ert-deftest emagent-tools-test-write-diff-string ()
  (let* ((dir (make-temp-file "emagent-tools-test-" t))
         (path (expand-file-name "sample.txt" dir))
         (diff (emagent-tools--write-diff-string path "new\ncontent")))
    (unwind-protect
        (progn
          (write-region "old\ncontent" nil path)
          (should (string-match-p "^---" diff))
          (should (string-match-p "new" diff)))
      (delete-directory dir t))))

;;;; Elisp syntax check

(ert-deftest emagent-tools-test-check-elisp ()
  (should (string= "OK" (emagent-tool-check-elisp "(+ 1 2)")))
  (should (string-match-p "SYNTAX ERROR" (emagent-tool-check-elisp "(+ 1 2"))))

;;;; Blocked symbols in eval

(ert-deftest emagent-tools-test-symbols-in-form ()
  (should (equal '(delete-file)
                 (emagent-tools--symbols-in-form '(delete-file "x")
                                                 '(delete-file))))
  (should (equal nil (emagent-tools--symbols-in-form '(+ 1 2) '(delete-file)))))

(provide 'emagent-tools-test)

;;; emagent-tools-test.el ends here
