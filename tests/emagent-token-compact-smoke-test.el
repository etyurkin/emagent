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
  (emagent-tools-age-reset)
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
  (emagent-usage-tax-reset)
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
  (emagent-tools-age-reset)
  (emagent-test--with-mocks
      (((symbol-function 'emagent-acp-context-usage) (lambda () nil)))
    (let ((emagent-acp-ctx-proxy-size 200000))
      (setq emagent-tools-age--bytes 80000)
      (with-temp-buffer
        (delay-mode-hooks (emagent-mode))
        ;; Large transcript must not inflate the proxy (not model context).
        (insert (make-string 500000 ?x))
        (setq emagent-chat--status nil)
        (let ((pct (emagent-chat--context-fill-percent))
              (ml (emagent-chat--mode-line-context-usage)))
          (should (numberp pct))
          ;; 80000 MCP bytes / 4 / 200000 ≈ 10%
          (should (< pct 15.0))
          (should (> pct 5.0))
          (should (stringp ml))
          ;; Mode-line needs %% so format-mode-line shows a single %.
          ;; Doubled %% so mode-line-format keeps one face on the "%".
          (should (string-match-p "ctx:~10%%\\'" ml))
          (should (eq 'shadow (get-text-property 0 'face ml)))
          (should (eq 'shadow
                      (get-text-property (1- (length ml)) 'face ml))))))))

(ert-deftest emagent-token-compact-smoke-ctx-proxy-no-mcp ()
  (emagent-tools-age-reset)
  (emagent-test--with-mocks
      (((symbol-function 'emagent-acp-context-usage) (lambda () nil)))
    (let ((emagent-acp-ctx-proxy-size 200000))
      (with-temp-buffer
        (delay-mode-hooks (emagent-mode))
        (insert (make-string 500000 ?x))
        (setq emagent-chat--status '(:ctx-unavailable t))
        (should-not (emagent-chat--context-fill-percent))
        (should (string-match-p "ctx:n/a"
                                (emagent-chat--mode-line-context-usage)))))))

(provide 'emagent-token-compact-smoke-test)

;;; emagent-token-compact-smoke-test.el ends here
