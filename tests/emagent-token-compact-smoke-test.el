;;; emagent-token-compact-smoke-test.el --- Smoke checks for token-compact -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emagent-archive)
(require 'emagent-tools-age)
(require 'emagent-usage)
(require 'emagent-test-utils)

(defun emagent-token-compact-smoke--fill-turns (n)
  "Insert N completed user turns into the current buffer."
  (dotimes (i n)
    (insert (format "%sheading-%d\n\n** Response\nanswer-%d\n\n"
                    (emagent-chat--user-heading-prefix) i i))))

(ert-deftest emagent-token-compact-smoke-tick-cache ()
  (emagent-tools-age-reset 'all)
  (let ((emagent-tools-age-tick-cache t))
    (should (string= "body"
                     (emagent-tools-age-tick-note "/tmp/t" "line=nil" "abc" "body")))
    (should (string-match-p "unchanged since tick abc"
                            (emagent-tools-age-tick-note "/tmp/t" "line=nil"
                                                        "abc" "body")))
    (should (string= "body2"
                     (emagent-tools-age-tick-note "/tmp/t" "line=nil"
                                                 "abc" "body2" t)))
    (should (string= "new"
                     (emagent-tools-age-tick-note "/tmp/t" "line=nil"
                                                 "def" "new")))))

(ert-deftest emagent-token-compact-smoke-usage-tax-and-baseline ()
  (emagent-usage-tax-reset 'all)
  (emagent-usage-tax-add 'user 100)
  (emagent-usage-tax-add 'mcp-bytes 4000)
  (let ((report (emagent-usage-report-string 7)))
    (should (string-match-p "session tax:" report))
    (should-not (string-match-p "since baseline:" report)))
  (emagent-usage-baseline-set)
  (emagent-usage-tax-add 'user 50)
  (emagent-usage-tax-add 'mcp-bytes 2000)
  (let ((report (emagent-usage-report-string 7))
        (delta (emagent-usage-tax-delta-string)))
    (should (string-match-p "session tax:" report))
    (should (string-match-p "since baseline:" report))
    (should (stringp delta))
    (should (= 50 (emagent-usage--tax-delta 'user)))
    (should (= 2000 (emagent-usage--tax-delta 'mcp-bytes))))
  (emagent-usage-baseline-clear)
  (should-not (emagent-usage-tax-delta-string)))

(ert-deftest emagent-token-compact-smoke-archive ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer dir)
     (with-current-buffer buffer
       (let ((emagent-archive-threshold-bytes 10)
             (emagent-archive-keep-turns 1))
         (emagent-token-compact-smoke--fill-turns 4)
         (should-not (emagent-archive-try t)))
       (let* ((file (expand-file-name "sess.org" dir))
              (emagent-archive-threshold-bytes 50)
              (emagent-archive-keep-turns 1))
         (erase-buffer)
         (insert (format "#+TITLE: test\n#+EMAGENT_PROJECT: %s\n\n" dir))
         (emagent-token-compact-smoke--fill-turns 4)
         (write-region (point-min) (point-max) file nil 'silent)
         (setq buffer-file-name file)
         (should (emagent-archive-try t))
         (should (string-match-p "\\* Archive" (buffer-string)))
         (should (string-match-p "sess-archive/001\\.org" (buffer-string)))
         (should (string-match-p "Moved .* turns out of this buffer"
                                 (buffer-string))))))))

(ert-deftest emagent-token-compact-smoke-ctx-proxy ()
  (emagent-tools-age-reset 'all)
  (emagent-test--with-mocks
      (((symbol-function 'emagent-acp-context-usage) (lambda () nil)))
    (let ((emagent-acp-ctx-proxy-size 200000)
          (emagent-acp-ctx-proxy-buffer-divisor 40))
      (emagent-tools-age--account-bytes 80000)
      (with-temp-buffer
        (delay-mode-hooks (emagent-mode))
        (insert (make-string 500000 ?x))
        (setq emagent-chat--status nil)
        (let* ((pct (emagent-chat--context-fill-percent))
               (ml (emagent-chat--mode-line-context-usage))
               (esc (emagent-chat--mode-line-escape ml)))
          (should (numberp pct))
          ;; Soft curve: mcp+buffer raw ≈ 32500 → ~15%
          (should (< pct 20.0))
          (should (> pct 10.0))
          (should (string-match-p "ctx:~1[0-9]%" (string-trim-left ml)))
          (should (eq 'shadow (get-text-property 0 'face ml)))
          (should (eq 'shadow (get-text-property (1- (length ml)) 'face ml)))
          (should (string-match-p "ctx:~1[0-9]%%" (string-trim-left esc)))
          (should (eq 'shadow
                      (get-text-property (1- (length esc)) 'face esc))))))))

(ert-deftest emagent-token-compact-smoke-ctx-proxy-grows ()
  (emagent-tools-age-reset 'all)
  (emagent-test--with-mocks
      (((symbol-function 'emagent-acp-context-usage) (lambda () nil)))
    (let ((emagent-acp-ctx-proxy-size 200000)
          (emagent-acp-ctx-proxy-buffer-divisor 40)
          small large)
      (with-temp-buffer
        (delay-mode-hooks (emagent-mode))
        (insert (make-string 80000 ?x))
        (setq small (emagent-chat--context-fill-percent))
        (insert (make-string 1920000 ?x))
        (setq large (emagent-chat--context-fill-percent))
        (should (numberp small))
        (should (numberp large))
        (should (> large small))
        (should (< large 100.0))))))

(ert-deftest emagent-token-compact-smoke-ctx-proxy-no-mcp ()
  (emagent-tools-age-reset 'all)
  (emagent-test--with-mocks
      (((symbol-function 'emagent-acp-context-usage) (lambda () nil)))
    (let ((emagent-acp-ctx-proxy-size 200000)
          (emagent-acp-ctx-proxy-buffer-divisor 40))
      (with-temp-buffer
        (delay-mode-hooks (emagent-mode))
        (insert (make-string 500000 ?x))
        (setq emagent-chat--status '(:ctx-unavailable t))
        (let* ((pct (emagent-chat--context-fill-percent))
               (ml (emagent-chat--mode-line-context-usage))
               (esc (emagent-chat--mode-line-escape ml)))
          (should (numberp pct))
          (should (< pct 10.0))
          (should (> pct 0.0))
          (should (string-match-p "ctx:~[0-9]+%" (string-trim-left ml)))
          (should (string-match-p "ctx:~[0-9]+%%" (string-trim-left esc)))
          (should (eq 'shadow
                      (get-text-property (1- (length esc)) 'face esc))))))))

(provide 'emagent-token-compact-smoke-test)

;;; emagent-token-compact-smoke-test.el ends here
