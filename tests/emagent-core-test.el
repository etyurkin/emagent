;;; emagent-core-test.el --- ERT tests for emagent.el entry points -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent)

(ert-deftest emagent-core-test-find-executable-extra-path ()
  (let* ((dir (emagent-test--temp-directory))
         (bin (expand-file-name "fake-agent" dir)))
    (unwind-protect
        (progn
          (write-region "#!/bin/sh\necho ok" nil bin)
          (chmod bin #o755)
          (let ((emagent-extra-exec-paths (list dir))
                (exec-path nil))
            (should (string= bin (emagent--find-executable "fake-agent")))
            (should (member dir exec-path))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest emagent-core-test-agent-model-label ()
  (should (string= "cursor - GPT 4"
                   (emagent--agent-model-label
                    'cursor '((:model-id . "gpt-4") (:name . "GPT 4"))))))

(ert-deftest emagent-core-test-available-providers-mock ()
  (emagent-test--with-mocks
      (((symbol-function 'emagent--provider-available-p)
        (lambda (provider) (eq provider 'cursor))))
    (should (equal '(cursor) (emagent--available-providers)))))

(ert-deftest emagent-core-test-project-directory-initial-file-buffer ()
  (emagent-test--with-temp-project
   (lambda (dir)
     (let ((file (expand-file-name "src/foo.el" dir)))
       (make-directory (file-name-directory file) t)
       (write-region "" nil file)
       (with-current-buffer (find-file-noselect file)
         (should (string= (file-name-directory file)
                          (emagent--project-directory-initial))))))))

(ert-deftest emagent-core-test-make-client-cursor-mock ()
  (emagent-test--with-mocks
      (((symbol-function 'emagent--find-executable) (lambda (_cmd) "/bin/true"))
       ((symbol-function 'emagent-cursor-check-command) (lambda () nil))
       ((symbol-function 'emagent-mcp-ensure-cursor-config) (lambda () nil))
       ((symbol-function 'emagent-chat--session-directory) (lambda () "/tmp/proj")))
    (with-temp-buffer
      (let ((client (emagent--make-client 'cursor (current-buffer))))
        (should (hash-table-p client))
        (should (string= "cursor-agent" (map-elt client :command)))))))

(provide 'emagent-core-test)

;;; emagent-core-test.el ends here
