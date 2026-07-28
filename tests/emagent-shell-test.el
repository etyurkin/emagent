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

;;;; fetch_url

(require 'emagent-test-utils)
(require 'emagent-tools)
(require 'url)  ;; pre-load so cl-letf mocks aren't overwritten by require inside the functio)

(defmacro emagent-shell-test--with-fake-url-retrieve (response &rest body)
  "Run BODY with `url-retrieve' mocked to deliver RESPONSE string.
RESPONSE should use \\n\\n (not \\r\\n\\r\\n) as the header/body separator,
matching what the Emacs url package puts in its response buffers."
  (declare (indent 1))
  `(emagent-test--with-mocks
       (((symbol-function 'url-retrieve)
         (lambda (_url callback &optional _cbargs &rest _)
           (with-temp-buffer
             (insert ,response)
             (funcall callback nil)))))
     ,@body))

(ert-deftest emagent-tools-fetch-url-test-invalid-url ()
  "Non-http URLs are rejected synchronously without touching url-retrieve."
  (let (got-result got-error)
    (emagent-tool-fetch-url-async
     (lambda (result is-error) (setq got-result result got-error is-error))
     "ftp://example.com")
    (should got-error)
    (should (string-match-p "http" got-result))))

(ert-deftest emagent-tools-fetch-url-test-cbargs-is-nil ()
  "Regression: timer object must not be passed as CBARGS to url-retrieve.
The bug caused Wrong type argument: listp in url-http-chunked-encoding-after-change-function."
  (let (captured-cbargs called)
    (emagent-test--with-mocks
        (((symbol-function 'url-retrieve)
          (lambda (_url _cb &optional cbargs &rest _)
            (setq captured-cbargs cbargs called t))))
      (emagent-tool-fetch-url-async #'ignore "http://example.com"))
    (should called)
    (should (null captured-cbargs))))

(ert-deftest emagent-tools-fetch-url-test-success ()
  "Body after the blank line separator is returned."
  (emagent-shell-test--with-fake-url-retrieve "HTTP/1.1 200 OK\n\nHello!"
    (let ((result (emagent-tool-fetch-url "http://example.com")))
      (should (string= "Hello!" result)))))

(ert-deftest emagent-tools-fetch-url-test-no-separator-yields-error ()
  "Missing blank-line separator returns is-error=t, not an uncaught throw."
  (let (got-error)
    (emagent-shell-test--with-fake-url-retrieve "HTTP/1.1 200 OK\n"
      (emagent-tool-fetch-url-async
       (lambda (_result is-error) (setq got-error is-error))
       "http://example.com"))
    (should got-error)))

(ert-deftest emagent-tools-fetch-url-test-truncation ()
  "Body exceeding max-bytes is truncated and marked."
  (emagent-shell-test--with-fake-url-retrieve
      (concat "HTTP/1.1 200 OK\n\n" (make-string 200 ?x))
    (let ((result (emagent-tool-fetch-url "http://example.com" 10)))
      (should (string-match-p "truncated" result)))))

(provide 'emagent-shell-test)

;;; emagent-shell-test.el ends here
