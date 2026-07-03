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

(ert-deftest emagent-elisp-test-check-form-docstring-ok ()
  "Short docstring lines pass."
  (should (string= "OK"
    (emagent-elisp-check-form "(defun foo (x)\n  \"Short doc.\"\n  x)"))))

(ert-deftest emagent-elisp-test-check-form-docstring-long ()
  "Docstring line >80 chars yields STYLE WARNING."
  (let ((result (emagent-elisp-check-form
                  (concat "(defun foo (x)\n"
                          "  \"" (make-string 81 ?x) "\"\n"
                          "  x)"))))
    (should (string-match-p "STYLE WARNING" result))
    (should (string-match-p "80 chars" result))))

(ert-deftest emagent-elisp-test-check-form-defvar-docstring-long ()
  "defvar with long docstring line is also flagged."
  (let ((result (emagent-elisp-check-form
                  (concat "(defvar foo nil\n"
                          "  \"" (make-string 81 ?x) "\")"))))
    (should (string-match-p "STYLE WARNING" result))))

(ert-deftest emagent-elisp-test-syntax-error-before-docstring-check ()
  "Syntax errors are reported before docstring style warnings."
  (let ((result (emagent-elisp-check-form "(defun broken (")))
    (should (string-match-p "SYNTAX ERROR" result))
    (should-not (string-match-p "STYLE WARNING" result))))

(provide 'emagent-elisp-test)

;;; emagent-elisp-test.el ends here