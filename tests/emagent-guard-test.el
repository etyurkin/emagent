;;; emagent-guard-test.el --- ERT tests for the authorization guard -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-acp-request)

(ert-deftest emagent-guard-test-shell-verdict ()
  "Shell operations return the policy verdict, or allow when clean."
  (should (eq :confirm (car (emagent-guard-check 'shell "rm -rf /tmp/x"))))
  (should (eq :confirm (car (emagent-guard-check 'shell "true && rm -rf ~"))))
  (should (equal '(:allow . t) (emagent-guard-check 'shell "ls -la"))))

(ert-deftest emagent-guard-test-eval-verdict ()
  "Elisp eval operations return the policy verdict, or allow when clean."
  (should (eq :deny (car (emagent-guard-check 'eval "(kill-emacs)"))))
  (should (eq :confirm (car (emagent-guard-check 'eval "(delete-file \"x\")"))))
  (should (equal '(:allow . t) (emagent-guard-check 'eval "(+ 1 2)"))))

(ert-deftest emagent-guard-test-path-verdict-confines-to-root ()
  "A path inside the bound root is allowed (with its canonical form); a path
outside, or reached through a symlink, is denied."
  (let* ((root (file-truename (make-temp-file "emagent-guard-" t)))
         (outside (file-truename (make-temp-file "emagent-guard-out-" t)))
         (link (expand-file-name "escape" root))
         (emagent-tools--root-boundary root)
         (emagent-tools--project-directory root))
    (unwind-protect
        (progn
          (make-symbolic-link outside link)
          ;; Inside the root: allowed, resolved to a canonical path.
          (let ((v (emagent-guard-check 'read (expand-file-name "a.txt" root))))
            (should (emagent-guard-allow-p v))
            (should (string-prefix-p root (emagent-guard-resolved v))))
          ;; Through the symlink: resolves outside the root → denied.
          (let ((v (emagent-guard-check 'write (expand-file-name "x" link))))
            (should (emagent-guard-deny-p v))
            (should (stringp (emagent-guard-reason v)))))
      (ignore-errors (delete-file link))
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-directory outside t)))))

(ert-deftest emagent-guard-test-unknown-op-denied ()
  (should (emagent-guard-deny-p (emagent-guard-check 'teleport "/tmp"))))

(provide 'emagent-guard-test)

;;; emagent-guard-test.el ends here
