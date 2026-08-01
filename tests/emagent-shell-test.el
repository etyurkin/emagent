;;; emagent-shell-test.el --- ERT tests for emagent shell routing -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-tools)

;;;; Git guards

(ert-deftest emagent-shell-test-git-no-verify-p ()
  (should (emagent-shell--git-no-verify-p "git commit --no-verify -m x"))
  (should (emagent-shell--git-no-verify-p
           "git commit '--no-verify' -m x"))
  (should (emagent-shell--git-no-verify-p
           "git commit \"--no-verify\" -m x"))
  (should-not (emagent-shell--git-no-verify-p
               "git commit -m \"--no-verify inside quotes\""))
  (should-not (emagent-shell--git-no-verify-p
               "git commit -m '--no-verify inside quotes'")))

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
    (should (string-match-p "search" (emagent-shell--suggest-alternative "rg foo")))
    (should (string-match-p "fs op=find"
                            (emagent-shell--suggest-alternative "find . -name foo")))
    (should (string-match-p "git op="
                            (emagent-shell--suggest-alternative "git commit -m x")))
    (should-not (emagent-shell--suggest-alternative "git status"))))

(ert-deftest emagent-shell-test-compound-command-p ()
  (should (emagent-shell--compound-command-p "grep x Makefile | head"))
  (should (emagent-shell--compound-command-p "make test && echo ok"))
  (should (emagent-shell--compound-command-p "cmd1 || cmd2"))
  (should (emagent-shell--compound-command-p "echo hi; ls"))
  (should (emagent-shell--compound-command-p "echo $(date)"))
  (should-not (emagent-shell--compound-command-p "grep x Makefile"))
  (should-not (emagent-shell--compound-command-p "echo \"a|b\""))
  (should-not (emagent-shell--compound-command-p
               "echo \"don't | touch\""))
  (should-not (emagent-shell--compound-command-p
               "echo 'a|b'")))

(ert-deftest emagent-shell-test-compound-skips-suggest-and-redirect ()
  "Pipelines must not be redirected or refused as simple grep/cat."
  (let ((emagent-acp-prefer-emacs t)
        (emagent-shell-redirect t)
        (emagent-shell-suggest t))
    (should-not (emagent-shell--suggest-alternative "grep x Makefile | head"))
    (should-not (emagent-shell--try-redirect "grep x Makefile | head" default-directory))
    (should-not (emagent-shell--try-redirect "cat foo | wc -l" default-directory))
    (should (string-match-p "search" (emagent-shell--suggest-alternative "rg foo")))))

;;;; Network policy

(ert-deftest emagent-shell-test-read-only-network-p ()
  (should (emagent-shell--read-only-network-p "curl -s https://example.com"))
  (should-not (emagent-shell--read-only-network-p "curl -X POST https://example.com")))

;;;; fetch_url

(require 'emagent-test-utils)
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

(ert-deftest emagent-shell-test-compile-timeout-detaches ()
  "Compile timeout leaves the process running and names the buffer."
  (let* ((emagent-tools-subprocess-timeout 0.05)
         (emagent-tools-subprocess-timeout-max 1800)
         (got nil)
         (got-err nil)
         (killed nil)
         (fake-proc (start-process "emagent-compile-test" nil "sleep" "30")))
    (unwind-protect
        (emagent-test--with-mocks
            (((symbol-function 'compilation-start)
              (lambda (_cmd &rest _)
                (get-buffer-create "*emagent-compile*")))
             ((symbol-function 'get-buffer-process)
              (lambda (_buf) fake-proc))
             ((symbol-function 'delete-process)
              (lambda (_p) (setq killed t)))
             ((symbol-function 'emagent-tools--cont-register-cancel) #'ignore)
             ((symbol-function 'ansi-color-compilation-filter) #'ignore)
             ((symbol-function 'emagent-tools--clamp-timeout) #'identity))
          (emagent-tool-compile-async
           (lambda (text is-error)
             (setq got text got-err is-error))
           "make ansi CHAPTER=printer")
          (sleep-for 0.2)
          (should got-err)
          (should (string-match-p "\\*emagent-compile" got))
          (should (string-match-p "left running" got))
          (should (string-match-p "do not relaunch" got))
          (should-not killed)
          (should (process-live-p fake-proc)))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))


(ert-deftest emagent-shell-test-compile-buffer-names-unique ()
  "Each compile call gets a distinct buffer name."
  (let ((emagent-tools--compile-buffer-seq 0))
    (should (string= "*emagent-compile*<1>"
                     (emagent-tools--compile-buffer-name "make")))
    (should (string= "*emagent-compile*<2>"
                     (emagent-tools--compile-buffer-name "make")))))

(provide 'emagent-shell-test)

;;; emagent-shell-test.el ends here
