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

(ert-deftest emagent-elisp-test-defun-bounds ()
  (let ((content "(defun alpha () 1)\n\n(defun beta () 2)\n"))
    (should (string-match-p "^[0-9]+:[0-9]+$" (emagent-elisp-defun-bounds content "beta")))
    (should (string-match-p "No defun" (emagent-elisp-defun-bounds content "missing")))))

(ert-deftest emagent-elisp-test-replace-defun ()
  (let* ((content "(defun old-f ()\n  1)\n(provide 'x)\n")
         (new-body "(defun old-f ()\n  2)\n")
         (updated (emagent-elisp-replace-defun content "old-f" new-body)))
    (should (string-match-p "2)" updated))
    (should-not (string-match-p "SYNTAX ERROR" updated))
    (should (string= "OK" (emagent-elisp-check-file-content updated)))))

(ert-deftest emagent-elisp-test-insert-after-form ()
  (let* ((content "(defun first () 1)\n(provide 'x)\n")
         (form "(defun second () 2)")
         (updated (emagent-elisp-insert-after-form content "first" form)))
    (should (string-match-p "defun second" updated))
    (should (string= "OK" (emagent-elisp-check-file-content updated)))))

(ert-deftest emagent-elisp-test-insert-at-start ()
  (let* ((updated (emagent-elisp-insert-after-form "" emagent-elisp-anchor-start
                                                   "(defun first () 1)")))
    (should (string-match-p "defun first" updated))
    (should (string= "OK" (emagent-elisp-check-file-content updated)))))

(ert-deftest emagent-elisp-test-insert-at-end ()
  (let* ((content "(defun first () 1)\n")
         (updated (emagent-elisp-insert-after-form content emagent-elisp-anchor-end
                                                   "(provide 'x)")))
    (should (string-match-p "provide" updated))
    (should (string= "OK" (emagent-elisp-check-file-content updated)))))

(ert-deftest emagent-elisp-test-sexp-tree ()
  (let ((content "(defun a () 1)\n(defvar b 2)\n(defconst c 3)\n"))
    (let ((tree (emagent-elisp-sexp-tree content)))
      (should (string-match-p "defun:a" tree))
      (should (string-match-p "defvar:b" tree))
      (should (string-match-p "defconst:c" tree)))))

(ert-deftest emagent-elisp-test-treesit-grammar-recipe ()
  (emagent-elisp--ensure-treesit-grammar-recipe)
  (should (assq 'elisp treesit-language-source-alist)))

(ert-deftest emagent-elisp-test-treesit-available-p ()
  (should (booleanp (emagent-elisp-treesit-available-p))))

(ert-deftest emagent-elisp-test-treesit-broken-buffer ()
  (skip-unless (emagent-elisp-treesit-available-p))
  (let ((content "(defun alpha () 1)\n(defun broken ()\n(+ 1\n(defun beta () 2)\n"))
    (should (string-match-p "^[0-9]+:[0-9]+$" (emagent-elisp-defun-bounds content "broken")))
    (let ((tree (emagent-elisp-sexp-tree content)))
      (should (string-match-p "alpha" tree))
      (should (string-match-p "broken" tree)))))

(ert-deftest emagent-elisp-test-replace-defun-multi-form ()
  (let* ((content "(defun alpha () 1)\n\n(defun beta () 2)\n(provide 'x)\n")
         (new-body "(defun beta () 3)\n")
         (updated (emagent-elisp-replace-defun content "beta" new-body)))
    (should (string-match-p "3)" updated))
    (should-not (string-match-p "SYNTAX ERROR" updated))
    (should (string= "OK" (emagent-elisp-check-file-content updated)))))

(provide 'emagent-elisp-test)

;;; emagent-elisp-test.el ends here
