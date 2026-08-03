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
                                          (emagent-archive--dir file))))
           (should-not (buffer-modified-p))
           (with-temp-buffer
             (insert-file-contents file)
             (should (string-match-p "heading-3" (buffer-string)))
             (should-not (string-match-p "\\* .*heading-0" (buffer-string))))))))))


(ert-deftest emagent-archive-test-save-failure-rolls-back ()
  "A failed session save must undo the hot-buffer edit and drop the chunk."
  (emagent-test--with-emagent-buffer
   (lambda (buffer dir)
     (with-current-buffer buffer
       (let* ((file (expand-file-name "sess.org" dir))
              (emagent-archive-threshold-bytes 50)
              (emagent-archive-keep-turns 1))
         (erase-buffer)
         (insert (format "#+TITLE: test\n#+EMAGENT_PROJECT: %s\n\n" dir))
         (emagent-archive-test--fill-turns 4)
         (write-region (point-min) (point-max) file nil 'silent)
         (setq buffer-file-name file)
         (cl-letf (((symbol-function 'basic-save-buffer)
                    (lambda (&rest _) (error "disk full"))))
           (should-error (emagent-archive-try t)))
         (should (string-match-p "heading-0" (buffer-string)))
         (should (string-match-p "answer-0" (buffer-string)))
         (should-not (file-exists-p
                      (expand-file-name "001.org"
                                        (emagent-archive--dir file)))))))))

(ert-deftest emagent-archive-test-chunk-claim-is-exclusive ()
  "Claiming a chunk path must fail when the file already exists."
  (let* ((dir (make-temp-file "emagent-chunk" t))
         (file (expand-file-name "sess.org" dir))
         (archive (expand-file-name "sess-archive" dir))
         (chunk (expand-file-name "001.org" archive)))
    (unwind-protect
        (progn
          (write-region "" nil file)
          (make-directory archive t)
          (write-region "taken\n" nil chunk)
          (let ((next (emagent-archive--next-chunk-path file)))
            (should (string-suffix-p "002.org" next))
            (should (file-exists-p next))
            (should (= 0 (file-attribute-size (file-attributes next))))))
      (delete-directory dir t))))

(ert-deftest emagent-archive-test-buffer-bytes-multibyte ()
  "Threshold measurement uses bytes, not character count."
  (with-temp-buffer
    (insert "αβγ") ;; 3 chars, 6 UTF-8 bytes
    (should (= 6 (emagent-archive--buffer-bytes)))
    (should (= 3 (buffer-size)))))

(ert-deftest emagent-archive-test-command-p ()
  (should (emagent-archive-command-p "/archive"))
  (should (emagent-archive-command-p "/archive force"))
  (should-not (emagent-archive-command-p "/compact")))


(ert-deftest emagent-archive-test-on-turn-end-swallows-save-error ()
  "Auto-archive failure must not escape turn-end finalization."
  (emagent-test--with-emagent-buffer
   (lambda (buffer dir)
     (with-current-buffer buffer
       (let* ((file (expand-file-name "sess.org" dir))
              (emagent-archive-auto t)
              (emagent-archive-threshold-bytes 50)
              (emagent-archive-keep-turns 1))
         (erase-buffer)
         (insert (format "#+TITLE: test\n#+EMAGENT_PROJECT: %s\n\n" dir))
         (emagent-archive-test--fill-turns 4)
         (write-region (point-min) (point-max) file nil 'silent)
         (setq buffer-file-name file)
         (cl-letf (((symbol-function 'basic-save-buffer)
                    (lambda (&rest _) (error "disk full"))))
           (should-not (emagent-archive-on-turn-end))))))))

(provide 'emagent-archive-test)

;;; emagent-archive-test.el ends here
