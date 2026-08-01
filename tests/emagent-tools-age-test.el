;;; emagent-tools-age-test.el --- ERT tests for tool-result aging -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-tools-age)

(ert-deftest emagent-tools-age-test-stub-on-repeat ()
  (emagent-tools-age-reset 'all)
  (let* ((emagent-tools-age t)
         (emagent-tools-age-min-bytes 10)
         (payload (make-string 100 ?x))
         (first (emagent-tools-age-note "fs-read" "/tmp/a" "line=nil" payload))
         (second (emagent-tools-age-note "fs-read" "/tmp/a" "line=nil" payload)))
    (should (string= first payload))
    (should (string-match-p "\\[aged:" second))
    (should (string-match-p "refresh=1" second))))

(ert-deftest emagent-tools-age-test-refresh-bypasses-stub ()
  (emagent-tools-age-reset 'all)
  (let* ((emagent-tools-age t)
         (emagent-tools-age-min-bytes 10)
         (payload (make-string 100 ?y))
         (_ (emagent-tools-age-note "fs-read" "/tmp/b" "" payload))
         (again (emagent-tools-age-note "fs-read" "/tmp/b" "" payload t)))
    (should (string= again payload))))

(ert-deftest emagent-tools-age-test-bytes-hint ()
  (emagent-tools-age-reset 'all)
  (let ((emagent-tools-age-bytes-threshold 50)
        (emagent-tools-age-min-bytes 1000))
    (emagent-tools-age-note "fs-read" "/tmp/c" "" (make-string 60 ?z))
    (should (emagent-tools-age-bytes-hint-p))
    (emagent-tools-age-clear-bytes-hint)
    (should-not (emagent-tools-age-bytes-hint-p))))

(ert-deftest emagent-tools-age-test-tick-cache ()
  (emagent-tools-age-reset 'all)
  (let ((emagent-tools-age-tick-cache t))
    (should (string= "body"
                     (emagent-tools-age-tick-note "/tmp/t" "line=nil" "abc" "body")))
    (should (string-match-p "unchanged since tick abc"
                            (emagent-tools-age-tick-note "/tmp/t" "line=nil" "abc" "body")))
    (should (string= "body2"
                     (emagent-tools-age-tick-note "/tmp/t" "line=nil" "abc" "body2" t)))))

(provide 'emagent-tools-age-test)

;;; emagent-tools-age-test.el ends here
