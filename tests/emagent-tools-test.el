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

(ert-deftest emagent-tools-test-buttons-prompt-removes-block ()
  (with-temp-buffer
    (insert "before\n")
    (let ((buf (current-buffer)))
      (run-at-time 0.01 nil
                   (lambda ()
                     (with-current-buffer buf
                       (save-excursion
                         (goto-char (point-min))
                         (while (and (not (eobp)) (not (button-at (point))))
                           (forward-char 1))
                         (when (button-at (point))
                           (push-button (point)))))))
      (let ((result (emagent-tools--buttons-prompt
                      "Allow test?"
                      '(("Allow" . ok))
                      buf)))
        (should (eq result 'ok))
        (should (string= "before\n" (buffer-string)))))))

(ert-deftest emagent-tools-test-buttons-prompt-survives-insert-before ()
  "Prompt cleanup still works when streaming inserts before the block."
  (with-temp-buffer
    (insert "before\n")
    (let ((buf (current-buffer)))
      (run-at-time 0.005 nil
                   (lambda ()
                     (with-current-buffer buf
                       (goto-char (point-min))
                       (insert "streamed "))))
      (run-at-time 0.01 nil
                   (lambda ()
                     (with-current-buffer buf
                       (save-excursion
                         (goto-char (point-min))
                         (while (and (not (eobp)) (not (button-at (point))))
                           (forward-char 1))
                         (when (button-at (point))
                           (push-button (point)))))))
      (let ((result (emagent-tools--buttons-prompt
                      "Allow compile?"
                      '(("Allow" . ok))
                      buf)))
        (should (eq result 'ok))
        (should (string-match-p "\\`streamed before\n\\'" (buffer-string)))
        (should-not (string-match-p "Allow compile?" (buffer-string)))
        (should-not (string-match-p "\\[Allow\\]" (buffer-string)))))))

;;;; Elisp syntax check

(ert-deftest emagent-tools-test-check-elisp ()
  (let ((bad "(+ 1 2"))
    (should (string= "OK" (emagent-tool-check-elisp "(+ 1 2)")))
    (should (string-match-p "SYNTAX ERROR" (emagent-tool-check-elisp bad)))
    (should (string-match-p "line [0-9]+, column [0-9]+"
                            (emagent-tool-check-elisp bad)))))

(ert-deftest emagent-tools-test-write-elisp-validation ()
  (let* ((dir (make-temp-file "emagent-tools-elisp-" t))
         (path (expand-file-name "bad.el" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-elisp-validate-on-write t)
         (emagent-elisp-byte-compile-on-check nil))
    (unwind-protect
        (should-error
         (emagent-tools--write-file-content path "(defun x ()\n  (+ 1"))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-write-elisp-structural-required ()
  (let* ((dir (make-temp-file "emagent-tools-struct-" t))
         (path (expand-file-name "existing.el" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-struct-require-edits t))
    (unwind-protect
        (progn
          (write-region "(defun old () 1)" nil path)
          (cl-letf (((symbol-function 'emagent-elisp-treesit-available-p) (lambda () t)))
            (should-error
             (emagent-tools--write-file-content path "(defun old () 2)"))
            (let ((emagent-struct--structural-write-p t))
              (should (string= path
                               (emagent-tools--write-file-content path "(defun old () 2)"))))))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-write-elisp-new-file-blocked ()
  (let* ((dir (make-temp-file "emagent-tools-new-" t))
         (path (expand-file-name "new.el" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-struct-require-edits t)
         (emagent-elisp-byte-compile-on-check nil))
    (unwind-protect
        (cl-letf (((symbol-function 'emagent-elisp-treesit-available-p) (lambda () t)))
          (should-error
           (emagent-tools--write-file-content path "(provide 'new)\n")))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-structural-elisp-eval ()
  (let* ((dir (make-temp-file "emagent-tools-eval-" t))
         (file "loaded.el")
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-elisp-byte-compile-on-check nil)
         (emagent-elisp-eval-after-structural-edit t))
    (unwind-protect
        (let ((result (emagent-tool-structural-insert
                       file "__start__" "(defun emagent-tools-eval-test () 'evaluated)")))
          (should (string-match-p "Wrote and evaluated" result))
          (should (fboundp 'emagent-tools-eval-test))
          (should (eq (emagent-tools-eval-test) 'evaluated)))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-structural-elisp-eval-blocked ()
  (let* ((dir (make-temp-file "emagent-tools-eval-block-" t))
         (file "evil.el")
         (path (expand-file-name file dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-elisp-byte-compile-on-check nil)
         (emagent-elisp-eval-after-structural-edit t))
    (unwind-protect
        (progn
          (should-error
           (emagent-tool-structural-insert
            file "__start__" "(kill-emacs)"))
          (should-not (file-exists-p path)))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-eval-form-guard-blocked ()
  (should (string-match-p "Eval blocked (kill-emacs)"
                          (emagent-tools--eval-form-guard "(kill-emacs)"))))

(ert-deftest emagent-tools-test-symbols-in-form ()
  (should (equal '(delete-file)
                 (emagent-tools--symbols-in-form '(delete-file "x")
                                                 '(delete-file))))
  (should (equal nil (emagent-tools--symbols-in-form '(+ 1 2) '(delete-file)))))

(provide 'emagent-tools-test)

;;; emagent-tools-test.el ends here
