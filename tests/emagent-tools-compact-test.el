;;; emagent-tools-compact-test.el --- ERT tests for tool output compacting -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-tools-compact)

(ert-deftest emagent-tools-compact-test-strip-ansi ()
  (should (string= "hello"
                   (emagent-tools-compact--strip-ansi "\e[31mhello\e[0m"))))

(ert-deftest emagent-tools-compact-test-head-tail ()
  (let ((text (apply #'concat (make-list 100 "abcdefghij"))))
    (let ((out (emagent-tools-compact--head-tail text 80)))
      (should (< (length out) (length text)))
      (should (string-match-p "compacted" out)))))

(ert-deftest emagent-tools-compact-test-git-diff-drops-index ()
  (let* ((raw (string-join
               '("diff --git a/f b/f"
                 "index abc..def 100644"
                 "--- a/f"
                 "+++ b/f"
                 "@@ -1 +1 @@"
                 "-old"
                 "+new")
               "\n"))
         (out (emagent-tools-compact-git-diff raw)))
    (should-not (string-match-p "^index " out))
    (should (string-match-p "diff --git" out))
    (should (string-match-p "\\+new" out))))

(ert-deftest emagent-tools-compact-test-failures-focused ()
  (let* ((raw (string-join
               '("ok test_a"
                 "FAIL test_b"
                 "AssertionError: boom"
                 "ok test_c"
                 "FAILED (failures=1)")
               "\n"))
         (out (emagent-tools-compact-shell raw "pytest -q" t)))
    (should (string-match-p "FAIL test_b" out))
    (should (string-match-p "AssertionError" out))
    (should (string-match-p "FAILED" out))
    (should-not (string-match-p "ok test_a" out))
    (should (string-match-p "omitted" out))))

(ert-deftest emagent-tools-compact-test-file-list-cap ()
  (let* ((emagent-tools-compact-list-max-lines 3)
         (raw (string-join '("a" "b" "c" "d" "e") "\n"))
         (out (emagent-tools-compact-file-list raw)))
    (should (string-match-p "a\nb\nc\n" out))
    (should (string-match-p "2 more files" out))))

(ert-deftest emagent-tools-compact-test-read-default-cap ()
  (let* ((emagent-tools-compact-read-max-lines 2)
         (raw "one\ntwo\nthree\nfour")
         (out (emagent-tools-compact-read raw nil)))
    (should (string-prefix-p "one\ntwo\n" out))
    (should (string-match-p "more lines" out)))
  (should (string= "one\ntwo\nthree"
                   (emagent-tools-compact-read "one\ntwo\nthree" t))))

(ert-deftest emagent-tools-compact-test-grep-line-truncate ()
  (let* ((emagent-tools-compact-grep-line-max 10)
         (out (emagent-tools-compact-grep "file.el:1:abcdefghijklmnop")))
    (should (string-match-p "…" out))
    (should (< (length out)
               (length "file.el:1:abcdefghijklmnop")))))

(ert-deftest emagent-tools-compact-test-grep-fold-by-file ()
  (let* ((emagent-tools-compact-grep-max-per-file 2)
         (emagent-tools-compact-grep-line-max 200)
         (raw (string-join
               '("a.el:1:one"
                 "a.el:2:two"
                 "a.el:3:three"
                 "b.el:1:only")
               "\n"))
         (out (emagent-tools-compact-grep raw)))
    (should (string-match-p "a.el:1:one" out))
    (should (string-match-p "a.el:2:two" out))
    (should (string-match-p "1 more hits" out))
    (should-not (string-match-p "a.el:3:three" out))
    (should (string-match-p "b.el:1:only" out))))

(ert-deftest emagent-tools-compact-test-mvn-success-ok-line ()
  (let* ((raw (string-join
               '("[INFO] Scanning for projects..."
                 "[INFO] Tests run: 10, Failures: 0, Errors: 0"
                 "[INFO] BUILD SUCCESS"
                 "[INFO] Total time: 12 s")
               "\n"))
         (out (emagent-tools-compact-shell raw "mvn test" nil)))
    (should (string-match-p "OK (compacted)" out))
    (should (string-match-p "mvn" out))
    (should-not (string-match-p "Scanning for projects" out))))

(ert-deftest emagent-tools-compact-test-mvn-failure-keeps-errors ()
  (let* ((raw (string-join
               '("[INFO] Scanning..."
                 "[ERROR] Failed to execute goal"
                 "[INFO] BUILD FAILURE")
               "\n"))
         (out (emagent-tools-compact-shell raw "mvn test" t)))
    (should (string-match-p "ERROR" out))
    (should (string-match-p "BUILD FAILURE" out))
    (should-not (string-match-p "OK (compacted)" out))))

(provide 'emagent-tools-compact-test)

;;; emagent-tools-compact-test.el ends here
