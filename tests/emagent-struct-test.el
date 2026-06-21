;;; emagent-struct-test.el --- ERT tests for structural editing plugins -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-struct)

(ert-deftest emagent-struct-test-plugin-ids ()
  (let ((ids (mapcar (lambda (p) (plist-get p :id)) emagent-struct--plugins)))
    (should (member 'elisp ids))
    (should (member 'python ids))
    (should (member 'commonlisp ids))))

(ert-deftest emagent-struct-test-plugin-for-path ()
  (should (emagent-struct-plugin-for-path "foo.el"))
  (should (emagent-struct-plugin-for-path "bar.py"))
  (should (emagent-struct-plugin-for-path "baz.lisp"))
  (should (emagent-struct-plugin-for-path "qux.cl"))
  (should-not (emagent-struct-plugin-for-path "readme.txt")))

(ert-deftest emagent-struct-test-check-file-unknown ()
  (should (string-match-p "No structural plugin"
                          (emagent-struct-check-file "readme.txt" "hello"))))

(ert-deftest emagent-struct-test-anchors ()
  (should (emagent-struct--anchor-start-p "__start__"))
  (should (emagent-struct--anchor-start-p ""))
  (should (emagent-struct--anchor-end-p "__end__"))
  (should-not (emagent-struct--anchor-end-p "__start__")))

(ert-deftest emagent-struct-test-write-blocked-when-treesit ()
  (let* ((dir (make-temp-file "emagent-struct-" t))
         (path (expand-file-name "blocked.el" dir))
         (emagent-struct-require-edits t))
    (unwind-protect
        (progn
          (write-region "(defun x () 1)" nil path)
          (cl-letf (((symbol-function 'emagent-elisp-treesit-available-p) (lambda () t)))
            (should (string-match-p "structural_\\*"
                                    (emagent-struct-write-blocked-message path)))
            (let ((emagent-struct--structural-write-p t))
              (should-not (emagent-struct-write-blocked-message path)))))
      (delete-directory dir t))))

(provide 'emagent-struct-test)

;;; emagent-struct-test.el ends here
