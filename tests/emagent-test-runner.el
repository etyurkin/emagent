;;; emagent-test-runner.el --- Load and run all emagent ERT tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run all tests:
;;
;;   emacs --batch -L . -L lisp/core -L lisp/acp ... -l tests/emagent-test-runner.el
;;
;;   make test
;;
;; From inside Emacs:
;;
;;   M-x load-file RET tests/emagent-test-runner.el RET

;;; Code:

(require 'ert)

(let ((dir (file-name-directory load-file-name)))
  (add-to-list 'load-path (expand-file-name ".." dir))
  (load (expand-file-name "emagent-test-utils.el" dir) nil t)
  (dolist (file (sort (directory-files dir t "^emagent-.*-test\\.el$") #'string<))
    (unless (string-match-p "emagent-test-utils\\.el$" file)
      (load file nil t))))

(ert-run-tests-batch-and-exit)

;;; emagent-test-runner.el ends here
