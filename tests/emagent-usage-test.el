;;; emagent-usage-test.el --- ERT tests for usage logging -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-usage)

(ert-deftest emagent-usage-test-record-and-report ()
  (let* ((dir (make-temp-file "emagent-usage-" t))
         (emagent-usage-enabled t)
         (emagent-usage-file (expand-file-name "usage.jsonl" dir)))
    (unwind-protect
        (progn
          (emagent-usage-record-usage
           '((totalTokens . 1200) (costUSD . 0.01))
           '((provider . "claude") (model . "test")))
          (emagent-usage-record-usage
           '(:total-tokens 800)
           '((provider . "claude")))
          (let ((report (emagent-usage-report-string 30)))
            (should (string-match-p "events: 2" report))
            (should (string-match-p "2\\.0k\\|2000" report))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emagent-usage-test-format-tokens ()
  (should (string= "n/a" (emagent-usage--format-tokens nil)))
  (should (string= "12" (emagent-usage--format-tokens 12)))
  (should (string= "1.5k" (emagent-usage--format-tokens 1500))))

(ert-deftest emagent-usage-test-disabled-noop ()
  (let* ((dir (make-temp-file "emagent-usage-" t))
         (emagent-usage-enabled nil)
         (emagent-usage-file (expand-file-name "usage.jsonl" dir)))
    (unwind-protect
        (progn
          (emagent-usage-record '((day . "2026-01-01") (total-tokens . 1)))
          (should-not (file-exists-p emagent-usage-file)))
      (ignore-errors (delete-directory dir t)))))

(provide 'emagent-usage-test)

;;; emagent-usage-test.el ends here
