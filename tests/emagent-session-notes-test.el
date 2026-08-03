;;; emagent-session-notes-test.el --- ERT tests for session notes -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-session)
(require 'emagent-test-utils)

(ert-deftest emagent-session-notes-test-roundtrip-and-facts ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (emagent-session-notes-write "path: a.java")
       (should (string-match-p "a.java" (emagent-session-notes-read)))
       (emagent-session-notes-merge-from-summary
        "Summary text.\n\nFACTS:\n- decided on Maven\n- open: wire tests")
       (let ((notes (emagent-session-notes-read)))
         (should (string-match-p "a.java" notes))
         (should (string-match-p "Maven" notes)))
       (should (string-match-p "Session notes"
                               (emagent-session-notes-prompt-block)))))))

(ert-deftest emagent-session-notes-test-literal-backslash-n-roundtrip ()
  "Literal backslash-n in notes must survive encode/decode."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (let ((note "path: a\\nb"))
         (emagent-session-notes-write note)
         (should (equal note (emagent-session-notes-read))))
       (emagent-session-notes-write "line1\nline2")
       (should (equal "line1\nline2" (emagent-session-notes-read)))))))

(ert-deftest emagent-session-notes-test-strip-facts ()
  (let* ((raw "SUMMARY:\n- used Maven\n\nFACTS:\n- path: pom.xml")
         (stripped (emagent-session-notes-strip-facts raw)))
    (should (string-match-p "Maven" stripped))
    (should-not (string-match-p "FACTS:" stripped))
    (should-not (string-match-p "pom.xml" stripped))))

(ert-deftest emagent-session-notes-test-project-notes-under-emagent-home ()
  (let* ((perms (make-temp-file "emagent-perms-" t))
         (proj (make-temp-file "emagent-proj-" t))
         (emagent-permissions-directory perms))
    (unwind-protect
        (emagent-test--with-emagent-buffer
         (lambda (buffer _dir)
           (with-current-buffer buffer
             (emagent-session-set-project-directory proj)
             (let ((path (emagent-session-project-notes-file)))
               (should (stringp path))
               (should (string-prefix-p (expand-file-name "projects" perms)
                                       (expand-file-name path)))
               (should (string-suffix-p ".notes.org" path))
               (should-not (string-prefix-p (expand-file-name proj)
                                           (expand-file-name path)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert "build: Maven\n"))
               (should (string-match-p "Maven"
                                      (emagent-session-project-notes-read)))
               (should (string-match-p "project:\n"
                                      (emagent-session-notes-prompt-block)))))))
      (ignore-errors (delete-directory perms t))
      (ignore-errors (delete-directory proj t)))))


(ert-deftest emagent-session-notes-test-legacy-backslash-compat ()
  "Unmarked notes keep legacy newline-only escaping; marked round-trips paths."
  ;; Legacy unmarked: only `\n' pairs expand.  Paths without that pair are
  ;; unchanged; real multiline used the same `\n' encoding as before.
  (let ((legacy-path "Server path: C:\\foo\\bar")
        (legacy-multiline "line1\\nline2"))
    (should (equal legacy-path (emagent-session-notes--decode legacy-path)))
    (should (equal "line1\nline2"
                   (emagent-session-notes--decode legacy-multiline))))
  ;; Marked reversible encoder preserves `\nginx'-style sequences.
  (let ((note "Server path: C:\\nginx\\conf, see \\notes.txt"))
    (should (equal note
                   (emagent-session-notes--decode
                    (emagent-session-notes--encode note))))))

(provide 'emagent-session-notes-test)

;;; emagent-session-notes-test.el ends here
