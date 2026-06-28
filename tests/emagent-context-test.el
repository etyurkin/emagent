;;; emagent-context-test.el --- ERT tests for emagent context -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-context)

;;;; Formatting

(ert-deftest emagent-context-test-format ()
  (let ((ctx (list (cons :buffer "foo.org")
                   (cons :file "/tmp/foo.org")
                   (cons :major-mode "org-mode")
                   (cons :default-directory "/tmp/")
                   (cons :point (list (cons :line 10) (cons :column 3)))
                   (cons :enclosing-function "my-fun"))))
    (let ((text (emagent-context-format ctx)))
      (should (string-match-p "\\[Emacs context\\]" text))
      (should (string-match-p "buffer: foo.org" text))
      (should (string-match-p "point: line 10, column 3" text))
      (should (string-match-p "enclosing-function: my-fun" text)))))

(ert-deftest emagent-context-test-build-prompt ()
  (let ((prompt (emagent-context-build-prompt "hello" '("extra block"))))
    (should (string-match-p "\\`hello" prompt))
    (should (string-match-p "\\[Emacs context\\]" prompt))
    (should (string-match-p "extra block" prompt))))

(ert-deftest emagent-context-test-buffer-summary ()
  (with-temp-buffer
    (insert "line1\nline2\n")
    (let ((summary (emagent-context-buffer-summary)))
      (should (string-match-p "lines: 2" summary))
      (should (string-match-p "chars: 12" summary)))))

(provide 'emagent-context-test)

;;; emagent-context-test.el ends here
