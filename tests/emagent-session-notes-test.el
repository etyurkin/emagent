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

(ert-deftest emagent-session-notes-test-strip-facts ()
  (let* ((raw "SUMMARY:\n- used Maven\n\nFACTS:\n- path: pom.xml")
         (stripped (emagent-session-notes-strip-facts raw)))
    (should (string-match-p "Maven" stripped))
    (should-not (string-match-p "FACTS:" stripped))
    (should-not (string-match-p "pom.xml" stripped))))

(provide 'emagent-session-notes-test)

;;; emagent-session-notes-test.el ends here
