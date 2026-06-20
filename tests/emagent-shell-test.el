;;; emagent-shell-test.el --- ERT tests for emagent shell routing -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-shell)

;;;; Git guards

(ert-deftest emagent-shell-test-git-no-verify-p ()
  (should (emagent-shell--git-no-verify-p "git commit --no-verify -m x"))
  (should-not (emagent-shell--git-no-verify-p
               "git commit -m \"--no-verify inside quotes\"")))

(ert-deftest emagent-shell-test-git-push-p ()
  (should (emagent-shell--git-push-p "git push origin main"))
  (should-not (emagent-shell--git-push-p "git status")))

;;;; Command parsing

(ert-deftest emagent-shell-test-strip-quoted ()
  (should (string-match-p "git commit" (emagent-shell--strip-quoted "git commit -m 'secret'")))
  (should-not (string-match-p "secret" (emagent-shell--strip-quoted "git commit -m 'secret'"))))

(ert-deftest emagent-shell-test-unquote ()
  (should (string= "foo" (emagent-shell--unquote "\"foo\"")))
  (should (string= "bar" (emagent-shell--unquote "'bar'")))
  (should (string= "plain" (emagent-shell--unquote "plain"))))

(ert-deftest emagent-shell-test-words ()
  (should (equal (emagent-shell--words "git status --short")
                 '("git" "status" "--short")))
  (should (equal (emagent-shell--words "echo 'hello world'")
                 '("echo" "'hello" "world'"))))

;;;; Build detection

(ert-deftest emagent-shell-test-build-command-p ()
  (should (emagent-shell--build-command-p '("make")))
  (should (emagent-shell--build-command-p '("cargo" "build")))
  (should-not (emagent-shell--build-command-p '("ls"))))

;;;; Suggestions

(ert-deftest emagent-shell-test-suggest-alternative ()
  (let ((emagent-acp-prefer-emacs t)
        (emagent-shell-suggest t))
    (should (string-match-p "grep" (emagent-shell--suggest-alternative "rg foo")))
    (should (string-match-p "find_files"
                            (emagent-shell--suggest-alternative "find . -name foo")))
    (should (string-match-p "git_status"
                            (emagent-shell--suggest-alternative "git commit -m x")))
    (should-not (emagent-shell--suggest-alternative "git status"))))

;;;; Network policy

(ert-deftest emagent-shell-test-read-only-network-p ()
  (should (emagent-shell--read-only-network-p "curl -s https://example.com"))
  (should-not (emagent-shell--read-only-network-p "curl -X POST https://example.com")))

(provide 'emagent-shell-test)

;;; emagent-shell-test.el ends here
