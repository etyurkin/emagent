;;; emagent-log-test.el --- ERT tests for emagent log buffer -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-log)

(ert-deftest emagent-log-test-truncate-line-short ()
  (should (string= "short" (emagent-log-truncate-line "short" 20))))

(ert-deftest emagent-log-test-truncate-line-long ()
  (let ((truncated (emagent-log-truncate-line (make-string 40 ?x) 10)))
    (should (<= (string-width truncated) 10))
    (should (string-match-p "…" truncated))))

(ert-deftest emagent-log-test-truncate-line-keep-tail ()
  (let ((truncated (emagent-log-truncate-line "abcdefghij" 6 t)))
    (should (string-match-p "…" truncated))
    (should (string-match-p "ghij\\'" truncated))))

(ert-deftest emagent-log-test-append ()
  (let ((buffer (emagent-log--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq emagent-log--line-count 0))
      (let ((emagent-log-echo-minibuffer nil))
        (emagent-log "test line %d" 42))
      (should (string-match-p "test line 42" (buffer-string))))))

(provide 'emagent-log-test)

;;; emagent-log-test.el ends here
