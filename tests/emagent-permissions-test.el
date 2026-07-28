;;; emagent-permissions-test.el --- ERT tests for ~/.emagent permissions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-trust)

(defun emagent-test--with-permissions-dir (fn)
  "Run FN with a temporary `emagent-permissions-directory'."
  (let ((dir (emagent-test--temp-directory)))
    (unwind-protect
        (let ((emagent-permissions-directory dir)
              (emagent-permissions--cache (make-hash-table :test 'equal)))
          (funcall fn dir))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest emagent-permissions-test-global-fingerprint ()
  (emagent-test--with-permissions-dir
   (lambda (_dir)
     (emagent-permissions-add-global-fingerprint "execute:make")
     (should (member "execute:make" (emagent-permissions-global-fingerprints))))))

(ert-deftest emagent-permissions-test-session-fingerprint ()
  (emagent-test--with-permissions-dir
   (lambda (_dir)
     (emagent-permissions-add-session-fingerprint "sess-1" "read:/tmp/foo")
     (should (equal '("read:/tmp/foo")
                    (emagent-permissions-session-fingerprints "sess-1"))))))

(ert-deftest emagent-permissions-test-project-tool ()
  (emagent-test--with-temp-project
   (lambda (dir)
     (let ((emagent-permissions-directory (emagent-test--temp-directory))
           (emagent-permissions--cache (make-hash-table :test 'equal)))
       (emagent-permissions-add-project-tool dir 'grep)
       (should (memq 'grep (emagent-permissions-project-tools dir)))))))

(ert-deftest emagent-permissions-test-legacy-global-migration ()
  (emagent-test--with-permissions-dir
   (lambda (dir)
     (with-temp-buffer
       (insert "execute:legacy\n")
       (write-region (point-min) (point-max)
                     (expand-file-name "allowed-permissions" dir)
                     nil 'nomessage))
     (should (member "execute:legacy" (emagent-permissions-global-fingerprints)))
     (should (file-readable-p (expand-file-name "global.json" dir))))))

(provide 'emagent-permissions-test)
;;; emagent-permissions-test.el ends here
