;;; emagent-elisp-test.el --- ERT tests for emagent-elisp -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-elisp)

(ert-deftest emagent-elisp-test-check-form-ok ()
  (should (string= "OK" (emagent-elisp-check-form "(+ 1 2)"))))

(ert-deftest emagent-elisp-test-check-form-error ()
  (let ((result (emagent-elisp-check-form "(+ 1 2")))
    (should (string-match-p "SYNTAX ERROR" result))
    (should (string-match-p "line [0-9]+, column [0-9]+" result))))

(ert-deftest emagent-elisp-test-check-file-ok ()
  (let ((content "(defun foo ()\n  (+ 1 2))\n(provide 'foo)\n"))
    (should (string= "OK" (emagent-elisp-check-file-content content "foo.el")))))

(ert-deftest emagent-elisp-test-check-file-read-error ()
  (let ((content "(defun foo ()\n  (+ 1 2\n"))
    (should (string-match-p "SYNTAX ERROR" (emagent-elisp-check-file-content content)))))

(provide 'emagent-elisp-test)

;;; emagent-elisp-test.el ends here