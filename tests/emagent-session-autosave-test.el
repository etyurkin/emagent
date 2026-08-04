;;; emagent-session-autosave-test.el --- ERT for session autosave -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-session)
(require 'emagent-chat)
(require 'emagent-test-utils)

(ert-deftest emagent-session-autosave-test-maybe-skips-unmodified ()
  "maybe-autosave must not call save when the buffer is unmodified."
  (let ((saved nil)
        (emagent-session-autosave t)
        (file (emagent-test--temp-file ".org")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert "hello\n")
          (basic-save-buffer)
          (should-not (buffer-modified-p))
          (emagent-test--with-mocks
              (((symbol-function 'basic-save-buffer)
                (lambda (&rest _) (setq saved t))))
            (emagent-session-maybe-autosave))
          (should-not saved))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest emagent-session-autosave-test-maybe-saves-modified ()
  "maybe-autosave saves a modified visited file."
  (let ((emagent-session-autosave t)
        (file (emagent-test--temp-file ".org")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert "v1\n")
          (basic-save-buffer)
          (insert "v2\n")
          (should (buffer-modified-p))
          (emagent-session-maybe-autosave)
          (should-not (buffer-modified-p))
          (should (string-match-p "v2"
                                  (with-temp-buffer
                                    (insert-file-contents file)
                                    (buffer-string)))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest emagent-session-autosave-test-maybe-skips-pathless ()
  "maybe-autosave is a no-op without buffer-file-name."
  (let ((saved nil)
        (emagent-session-autosave t))
    (with-temp-buffer
      (insert "x\n")
      (emagent-test--with-mocks
          (((symbol-function 'basic-save-buffer)
            (lambda (&rest _) (setq saved t))))
        (emagent-session-maybe-autosave))
      (should-not saved))))

(ert-deftest emagent-session-autosave-test-ensure-scratch-assigns-file ()
  "ensure-scratch-file assigns ~/.emagent/scratch under a temp root."
  (let* ((dir (emagent-test--temp-directory))
         (proj (expand-file-name "proj" dir))
         (emagent-session-autosave t)
         (emagent-session-scratch-directory
          (expand-file-name "scratch" dir)))
    (make-directory proj t)
    (unwind-protect
        (with-temp-buffer
          (insert "# -*- mode: emagent -*-\n* user> hi\n")
          (setq emagent-chat-project-directory proj
                default-directory proj)
          (should-not buffer-file-name)
          (let ((path (emagent-session-ensure-scratch-file)))
            (should (stringp path))
            (should (equal path buffer-file-name))
            (should (file-readable-p path))
            (should (string-prefix-p
                     (file-name-as-directory
                      emagent-session-scratch-directory)
                     (file-name-as-directory
                      (file-name-directory path))))
            ;; Visited file is under scratch/, but cwd stays on the project.
            (should (equal (file-name-as-directory (expand-file-name proj))
                           (file-name-as-directory default-directory)))))
      (delete-directory dir t))))

(ert-deftest emagent-session-autosave-test-ensure-scratch-noop-when-off ()
  "ensure-scratch-file does nothing when autosave is disabled."
  (let ((emagent-session-autosave nil))
    (with-temp-buffer
      (should-not (emagent-session-ensure-scratch-file))
      (should-not buffer-file-name))))

(ert-deftest emagent-session-autosave-test-ensure-scratch-noop-when-visited ()
  "ensure-scratch-file leaves an existing visited file alone."
  (let ((emagent-session-autosave t)
        (file (emagent-test--temp-file ".org")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (should-not (emagent-session-ensure-scratch-file))
          (should (equal file buffer-file-name)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest emagent-session-autosave-test-enable-arms-timer ()
  "enable-autosave arms a buffer-local idle timer for visited files."
  (let ((emagent-session-autosave t)
        (emagent-session-autosave-idle-seconds 30)
        (file (emagent-test--temp-file ".org")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (emagent-session-enable-autosave)
          (should (timerp emagent-session--autosave-timer))
          (emagent-session-disable-autosave)
          (should-not emagent-session--autosave-timer))
      (when (file-exists-p file) (delete-file file)))))

(provide 'emagent-session-autosave-test)

;;; emagent-session-autosave-test.el ends here
