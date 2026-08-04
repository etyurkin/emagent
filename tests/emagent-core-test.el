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


(ert-deftest emagent-core-test-open-does-not-mark-buffer-modified ()
  "Opening/activating a saved session must not mark the buffer modified.

Regression: mode entry rewrote #+EMAGENT_PROJECT into the abbreviated
trailing-slash form on every visit, so reopening a session always dirtied
the buffer.  Cookie housekeeping had the same bug when inserting."
  (emagent-test--with-temp-project
   (lambda (dir)
     (let* ((file (expand-file-name "session.org" dir))
            ;; Absolute path without trailing slash — the form that used to
            ;; be rewritten (and dirty the buffer) on every activation.
            (project (directory-file-name (expand-file-name dir)))
            (emagent--pending-buffers nil)
            (emagent-activate-on-display t)
            buf)
       (unwind-protect
           (progn
             (write-region
              (format "# -*- mode: emagent -*-\n#+EMAGENT_PROJECT: %s\n* user> \n"
                      project)
              nil file)
             (setq buf (find-file-noselect file))
             (with-current-buffer buf
               (should-not (buffer-modified-p))
               (emagent-mode-force)
               (should (derived-mode-p 'emagent-mode))
               (should-not (buffer-modified-p))))
         (when (buffer-live-p buf) (kill-buffer buf))
         (when (file-exists-p file) (delete-file file)))))))

(ert-deftest emagent-core-test-ensure-mode-cookie-preserves-unmodified ()
  "Inserting a missing mode cookie must not mark an unmodified buffer dirty."
  (with-temp-buffer
    (insert "#+EMAGENT_SESSION: abc\n#+EMAGENT_PROJECT: /tmp/proj/\n")
    (set-buffer-modified-p nil)
    (emagent-chat--ensure-mode-cookie)
    (goto-char (point-min))
    (should (looking-at-p "#[ \t]*-\\*-.*\\bmode:[ \t]*emagent\\b"))
    (should-not (buffer-modified-p))))

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

(ert-deftest emagent-core-test-probe-claude-model-catalog ()
  "The startup Claude probe switches models and collects per-model siblings."
  (let* ((response
          '((sessionId . "sess")
            (configOptions
             . [((id . "mode") (category . "mode") (type . "select")
                 (currentValue . "default")
                 (options . [((value . "default") (name . "Default"))]))
                ((id . "model") (category . "model") (type . "select")
                 (currentValue . "fable[1m]")
                 (options . [((value . "fable[1m]") (name . "Fable"))
                             ((value . "haiku") (name . "Haiku"))]))
                ((id . "effort") (category . "thought_level") (type . "select")
                 (currentValue . "high")
                 (options . [((value . "default") (name . "Default"))
                             ((value . "high") (name . "High"))]))])))
         (sent nil)
         (catalog
          (emagent-test--with-mocks
              (((symbol-function 'emagent-acp-send-request)
                (cl-function
                 (lambda (&key request &allow-other-keys)
                   (push (map-elt (map-elt request :params) 'value) sent)
                   ;; Haiku has no effort option.
                   '((configOptions
                      . [((id . "model") (category . "model") (type . "select")
                          (currentValue . "haiku")
                          (options . [((value . "fable[1m]") (name . "Fable"))
                                      ((value . "haiku") (name . "Haiku"))]))]))))))
            (emagent--probe-claude-model-catalog nil response))))
    (should (equal '("haiku") sent))
    (should (= 2 (length catalog)))
    (let ((fable (seq-find (lambda (e) (equal "fable[1m]" (map-elt e :value)))
                           catalog))
          (haiku (seq-find (lambda (e) (equal "haiku" (map-elt e :value)))
                           catalog)))
      (should (equal "effort"
                     (map-elt (car (map-elt fable :config-options)) :id)))
      (should (null (map-elt haiku :config-options))))
    (let ((rows (emagent-acp--model-variant-choices-from-catalog catalog)))
      ;; fable: default+high effort rows; haiku: bare row.
      (should (= 3 (length rows))))))

(ert-deftest emagent-core-test-existing-buffer-agent ()
  "An existing session buffer pins the startup picker to its agent."
  (emagent-test--with-emagent-buffer
   (lambda (buffer dir)
     (with-current-buffer buffer
       (emagent-session-set-agent 'claude))
     (should (eq buffer (emagent-chat-find-project-buffer dir)))
     (should (eq 'claude (emagent--existing-buffer-agent dir)))
     (should-not (emagent--existing-buffer-agent
                  "/nonexistent/emagent-test-dir")))))

(ert-deftest emagent-core-test-chat-open-reuses-project-buffer ()
  "`emagent-chat-open' returns the existing buffer for a project."
  (emagent-test--with-temp-project
   (lambda (dir)
     (let (b1 b2)
       (unwind-protect
           (progn
             (setq b1 (emagent-chat-open :project-dir dir)
                   b2 (emagent-chat-open :project-dir dir))
             (should (eq b1 b2)))
         (when (buffer-live-p b1) (kill-buffer b1))
         (when (buffer-live-p b2) (kill-buffer b2)))))))


(ert-deftest emagent-core-test-project-directory-initial-emagent-buffer ()
  "Emagent buffers keep the session project even when visited under scratch/."
  (emagent-test--with-temp-project
   (lambda (dir)
     (let* ((scratch (emagent-test--temp-directory))
            (file (expand-file-name "sess.org" scratch))
            buf)
       (unwind-protect
           (progn
             (write-region
              (format "# -*- mode: emagent -*-\n#+EMAGENT_PROJECT: %s\n" dir)
              nil file)
             (setq buf (find-file-noselect file))
             (with-current-buffer buf
               (emagent-mode-force)
               (setq default-directory scratch)
               (should (equal (file-name-as-directory (expand-file-name dir))
                              (file-name-as-directory
                               (emagent--project-directory-initial))))))
         (when (buffer-live-p buf) (kill-buffer buf))
         (ignore-errors (delete-directory scratch t)))))))

(ert-deftest emagent-core-test-start-with-provider-pops-when-connecting ()
  "M-x emagent must pop the existing buffer even while connect is in flight."
  (emagent-test--with-emagent-buffer
   (lambda (buffer dir)
     (let ((popped nil))
       (with-current-buffer buffer
         (emagent-session-set-agent 'claude)
         (setq emagent-acp--session
               (emagent-acp--make-state :client nil :chat-buffer buffer)))
       (cl-letf (((symbol-function 'emagent-acp-ensure-connected)
                  (lambda (&rest _) nil))
                 ((symbol-function 'pop-to-buffer)
                  (lambda (b &rest _)
                    (setq popped b)
                    b)))
         (emagent--start-with-provider 'claude dir t)
         (should (eq popped buffer)))))))

(provide 'emagent-core-test)

;;; emagent-core-test.el ends here
