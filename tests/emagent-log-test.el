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

(ert-deftest emagent-log-test-font-lock-keywords-only ()
  "Log mode uses keywords-only font-lock (no syntactic pass)."
  (let ((buffer (emagent-log--get-buffer)))
    (with-current-buffer buffer
      (should (eq (nth 1 font-lock-defaults) t))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "[12:34:56] permission denied; mcp enable foo: ok\n"))
      (font-lock-ensure (point-min) (point-max))
      (should (eq (get-text-property 1 'face) 'emagent-log-timestamp))
      (goto-char (point-min))
      (search-forward "denied")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'emagent-log-error))
      (search-forward "ok")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'emagent-log-success)))))

(provide 'emagent-log-test)

;;; emagent-log-test.el ends here
