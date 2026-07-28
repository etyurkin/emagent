;;; emagent-archive-test.el --- ERT tests for session archive -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-archive)
(require 'emagent-test-utils)

(defun emagent-archive-test--fill-turns (n)
  "Insert N completed user turns into the current buffer."
  (dotimes (i n)
    (insert (format "%sheading-%d\n\n** Response\nanswer-%d\n\n"
                    (emagent-chat--user-heading-prefix) i i))))

(ert-deftest emagent-archive-test-refuses-unsaved ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (let ((emagent-archive-threshold-bytes 10)
             (emagent-archive-keep-turns 1))
         (emagent-archive-test--fill-turns 4)
         (should-not (emagent-archive-try t))
         (should-not (file-directory-p
                      (expand-file-name "emagent-archive"
                                        default-directory))))))))

(ert-deftest emagent-archive-test-under-threshold-skips ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer dir)
     (with-current-buffer buffer
       (let* ((file (expand-file-name "sess.org" dir))
              (emagent-archive-threshold-bytes 1000000)
              (emagent-archive-keep-turns 1))
         (write-region (point-min) (point-max) file)
         (setq buffer-file-name file)
         (emagent-archive-test--fill-turns 4)
         (should-not (emagent-archive-try nil))
         (should-not (file-directory-p
                      (emagent-archive--dir file))))))))

(ert-deftest emagent-archive-test-moves-turns-and-toc ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer dir)
     (with-current-buffer buffer
       (let* ((file (expand-file-name "sess.org" dir))
              (emagent-archive-threshold-bytes 50)
              (emagent-archive-keep-turns 1)
              (user (user-login-name)))
         (erase-buffer)
         (insert (format "#+TITLE: test\n#+EMAGENT_PROJECT: %s\n\n" dir))
         (emagent-archive-test--fill-turns 4)
         (write-region (point-min) (point-max) file nil 'silent)
         (setq buffer-file-name file)
         (should (emagent-archive-try t))
         (let ((chunk (expand-file-name "001.org"
                                        (emagent-archive--dir file))))
           (should (file-readable-p chunk))
           (with-temp-buffer
             (insert-file-contents chunk)
             (should (string-match-p "Back to \\[\\[file:\\.\\./sess\\.org\\]"
                                     (buffer-string)))
             (should (string-match-p "heading-0" (buffer-string)))
             (should (string-match-p "answer-0" (buffer-string))))
           (should (string-match-p "\\* Archive" (buffer-string)))
           (should (string-match-p "sess-archive/001\\.org" (buffer-string)))
           (should (string-match-p "Moved .* turns out of this buffer"
                                   (buffer-string)))
           ;; Hot buffer keeps the tail turn (blurb may mention heading-0).
           (should (string-match-p "heading-3" (buffer-string)))
           (should-not (string-match-p "\\* .*heading-0" (buffer-string)))
           (should-not (string-match-p "answer-0" (buffer-string)))
           ;; Second archive with nothing new under keep-turns is empty skip.
           (should-not (emagent-archive-try t))
           (should-not (file-exists-p
                        (expand-file-name "002.org"
                                          (emagent-archive--dir file))))))))))

(ert-deftest emagent-archive-test-command-p ()
  (should (emagent-archive-command-p "/archive"))
  (should (emagent-archive-command-p "/archive force"))
  (should-not (emagent-archive-command-p "/compact")))

(provide 'emagent-archive-test)

;;; emagent-archive-test.el ends here
