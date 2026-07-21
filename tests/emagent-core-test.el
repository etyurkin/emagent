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
  (should (string= "cursor - gpt-4 (GPT 4)"
                   (emagent--agent-model-label
                    'cursor '((:model-id . "gpt-4") (:name . "GPT 4")))))
  (should (string= "grok-4.3[context=200k]"
                   (emagent--agent-model-label
                    'cursor '((:model-id . "grok-4.3[context=200k]")
                              (:name . "grok-4.3")) t)))
  (should (string= "default[] (Auto)"
                   (emagent--agent-model-label
                    'cursor '((:model-id . "default[]") (:name . "Auto")) t)))
  (let ((label (emagent--agent-model-label
                'cursor '((:model-id . "composer-2.5[fast=true]")
                          (:name . "composer-2.5")))))
    (should (string= "cursor - composer-2.5[fast=true]"
                     (substring-no-properties label)))
    (should (eq 'emagent-model-choice-agent (get-text-property 0 'face label)))
    (should (eq 'emagent-model-choice-model (get-text-property 9 'face label)))
    (should (eq 'emagent-model-choice-detail (get-text-property 21 'face label)))))

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
      (let ((client (emagent-acp--make-client 'cursor (current-buffer))))
        (should (hash-table-p client))
        (should (string= "cursor-agent" (map-elt client :command)))))))

(ert-deftest emagent-core-test-defer-session-until-displayed ()
  "A session file opened off-screen stays in `org-mode' until first displayed."
  (emagent-test--with-temp-project
   (lambda (dir)
     (let* ((file (expand-file-name "session.org" dir))
            (emagent--pending-buffers nil)
            (emagent-activate-on-display t)
            buf)
       (unwind-protect
           (progn
             (write-region
              (format "# -*- mode: emagent -*-\n#+EMAGENT_PROJECT: %s\n" dir)
              nil file)
             ;; `find-file-noselect' does not display the buffer, so the cookie
             ;; must defer instead of activating `emagent-mode'.
             (setq buf (find-file-noselect file))
             (with-current-buffer buf
               (should (derived-mode-p 'org-mode))
               (should-not (derived-mode-p 'emagent-mode))
               (should emagent--session-pending)
               (should (memq buf emagent--pending-buffers))
               (should-not org-element-use-cache))
             ;; Pretend the buffer is now shown in a window.
             (emagent-test--with-mocks
                 (((symbol-function 'emagent-chat--buffer-displayed-p)
                   (lambda (&optional _b) t)))
               (emagent--activate-displayed-pending))
             (with-current-buffer buf
               (should (derived-mode-p 'emagent-mode))
               (should-not emagent--session-pending)
               (should-not (memq buf emagent--pending-buffers))))
         (when (buffer-live-p buf) (kill-buffer buf))
         (when (file-exists-p file) (delete-file file)))))))

(ert-deftest emagent-core-test-explicit-open-activates-immediately ()
  "Explicit activation bypasses display deferral in the mode entry wrapper."
  (emagent-test--with-temp-project
   (lambda (dir)
     (let* ((file (expand-file-name "session.org" dir))
            (emagent--pending-buffers nil)
            (emagent-activate-on-display t)
            buf)
       (unwind-protect
           (progn
             (write-region
              (format "# -*- mode: emagent -*-\n#+EMAGENT_PROJECT: %s\n" dir)
              nil file)
             (setq buf (find-file-noselect file))
             (with-current-buffer buf
               (should emagent--session-pending)
               (emagent-mode-force)
               (should (derived-mode-p 'emagent-mode))))
         (when (buffer-live-p buf) (kill-buffer buf))
         (when (file-exists-p file) (delete-file file)))))))


(ert-deftest emagent-core-test-mode-toggle-off-does-not-defer ()
  "Calling `emagent-mode' while already active must not mark the buffer pending."
  (emagent-test--with-temp-project
   (lambda (dir)
     (let* ((file (expand-file-name "session.org" dir))
            (emagent--pending-buffers nil)
            (emagent-activate-on-display t)
            buf)
       (unwind-protect
           (progn
             (write-region
              (format "# -*- mode: emagent -*-\n#+EMAGENT_PROJECT: %s\n" dir)
              nil file)
             (setq buf (find-file-noselect file))
             (with-current-buffer buf
               (emagent-mode-force)
               (should (derived-mode-p 'emagent-mode))
               (setq emagent--pending-buffers nil)
               (kill-local-variable 'emagent--session-pending)
               ;; Re-enter while already in emagent-mode; must not defer.
               (emagent-mode)
               (should (derived-mode-p 'emagent-mode))
               (should-not emagent--session-pending)
               (should-not (memq buf emagent--pending-buffers))))
         (when (buffer-live-p buf) (kill-buffer buf))
         (when (file-exists-p file) (delete-file file)))))))

(provide 'emagent-core-test)

;;; emagent-core-test.el ends here
