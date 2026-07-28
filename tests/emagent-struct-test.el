;;; emagent-struct-test.el --- ERT tests for lisp-sitter CLI proxy -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the lisp-sitter CLI proxy.  All tests are skipped when the
;; lisp-sitter binary is not installed.

;;; Code:

(require 'ert)
(require 'emagent-tools)

(ert-deftest emagent-struct-test-available-p ()
  (should (booleanp (emagent-struct-available-p))))

(ert-deftest emagent-struct-test-lang-for ()
  (should (string= "elisp" (emagent-struct--lang-for "foo.el")))
  (should (string= "commonlisp" (emagent-struct--lang-for "foo.lisp")))
  (should (string= "commonlisp" (emagent-struct--lang-for "foo.cl")))
  (should (string= "scheme" (emagent-struct--lang-for "foo.scm")))
  (should (string= "scheme" (emagent-struct--lang-for "foo.ss")))
  (should (string= "scheme" (emagent-struct--lang-for "foo.sld"))))

(ert-deftest emagent-struct-test-lisp-file-p ()
  (should (emagent-struct--lisp-file-p "foo.el"))
  (should (emagent-struct--lisp-file-p "foo.lisp"))
  (should (emagent-struct--lisp-file-p "foo.cl"))
  (should (emagent-struct--lisp-file-p "foo.scm"))
  (should-not (emagent-struct--lisp-file-p "foo.txt"))
  (should-not (emagent-struct--lisp-file-p "foo.rs")))

(ert-deftest emagent-struct-test-tree ()
  (skip-unless (emagent-struct-available-p))
  (let ((out (emagent-struct-tree "(defun foo () 1)" "test.el")))
    (should (string-match-p "foo" out))))

(ert-deftest emagent-struct-test-bounds ()
  (skip-unless (emagent-struct-available-p))
  (let ((out (emagent-struct-bounds "(defun foo () 1)" "test.el" "foo")))
    (should (string-match-p "^[0-9]+:[0-9]+$" out))))

(ert-deftest emagent-struct-test-replace ()
  (skip-unless (emagent-struct-available-p))
  (let ((out (emagent-struct-replace "(defun foo () 1)" "test.el" "foo"
                                     "(defun foo () 2)")))
    (should (string-match-p "2" out))))

(ert-deftest emagent-struct-test-insert ()
  (skip-unless (emagent-struct-available-p))
  (let ((out (emagent-struct-insert "(defun foo () 1)" "test.el" "__end__"
                                    "(defun bar () 2)")))
    (should (string-match-p "bar" out))))

(ert-deftest emagent-struct-test-check ()
  (skip-unless (emagent-struct-available-p))
  (should (string= "OK"
                   (emagent-struct-check "(defun foo () 1)" "test.el"))))

(ert-deftest emagent-struct-test-check-node ()
  (skip-unless (emagent-struct-available-p))
  (should (string= "OK"
                   (emagent-struct-check-node "(defun foo () 1)" "elisp"))))

(ert-deftest emagent-struct-test-get ()
  (skip-unless (emagent-struct-available-p))
  (let ((out (emagent-struct-get "(defun foo () 1)" "test.el" "foo")))
    (should (string-match-p "defun foo" out))))

(ert-deftest emagent-struct-test-replace-error ()
  (skip-unless (emagent-struct-available-p))
  (should-error (emagent-struct-replace "(defun foo () 1)" "test.el" "nonexistent"
                                        "(defun bar () 2)")))

(provide 'emagent-struct-test)

;;; emagent-struct-test.el ends here