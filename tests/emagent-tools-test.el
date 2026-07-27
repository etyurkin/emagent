;;; emagent-tools-test.el --- ERT tests for emagent tools -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-tools)
(require 'emagent-tools-shell)

;;;; Async subprocess runner

(ert-deftest emagent-tools-test-run-process-async-completes ()
  "A normally-exiting subprocess fires its callback.
Regression: the sentinel matched `exited' (never returned by `process-status',
which yields `exit'), so every subprocess tool hung until it timed out."
  (skip-unless (executable-find "echo"))
  (let (result done)
    (emagent-tools--run-process-async
     (lambda (out err) (setq result (cons (string-trim out) err) done t))
     "echo" "hello")
    (let ((deadline (+ (float-time) 5)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should done)
    (should (equal "hello" (car result)))
    (should-not (cdr result))))

(ert-deftest emagent-tools-test-git-uses-no-pager ()
  "git runs with `--no-pager' so log/diff/show never hang on a pager prompt."
  (let (captured)
    (cl-letf (((symbol-function 'emagent-tools--run-process-async)
               (lambda (_cb program &rest args) (setq captured (cons program args))))
              ((symbol-function 'emagent-tools--root-directory) (lambda (_) "/tmp")))
      (emagent-tools--run-git-async #'ignore "log" "--oneline")
      (should (equal '("git" "--no-pager" "log" "--oneline") captured)))))

;;;; Session root boundary

(ert-deftest emagent-tools-test-within-boundary-p ()
  (let ((emagent-tools--root-boundary "/tmp/project"))
    (should (emagent-tools--within-boundary-p "/tmp/project/src/foo.el"))
    (should (emagent-tools--within-boundary-p "/tmp/project"))
    (should-not (emagent-tools--within-boundary-p "/tmp/other"))))

(ert-deftest emagent-tools-test-root-directory ()
  (let ((emagent-tools--root-boundary "/tmp/project")
        (emagent-tools--project-directory "/tmp/project"))
    (should (string= (emagent-tools--root-directory "src/foo.el")
                     (expand-file-name "src/foo.el" "/tmp/project")))))

(ert-deftest emagent-tools-test-boundary-rejects-symlink-escape ()
  "A symlink inside the root pointing outside must not pass the boundary."
  (let* ((root (file-truename (make-temp-file "emagent-root-" t)))
         (outside (file-truename (make-temp-file "emagent-outside-" t)))
         (link (expand-file-name "escape" root))
         (emagent-tools--root-boundary root))
    (unwind-protect
        (progn
          (make-symbolic-link outside link)
          (should (emagent-tools--within-boundary-p
                   (expand-file-name "real.el" root)))
          (should-not (emagent-tools--within-boundary-p
                       (expand-file-name "x.el" link))))
      (ignore-errors (delete-file link))
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-directory outside t)))))

(ert-deftest emagent-tools-test-protected-truename-p ()
  "Protected macOS trees are detected on the resolved truename."
  (should (emagent-tools--protected-truename-p
           (expand-file-name "~/Library/Containers/com.example/x")))
  (should (emagent-tools--protected-truename-p
           (expand-file-name "~/Library/Mobile Documents/foo")))
  (should-not (emagent-tools--protected-truename-p "/tmp/x")))

;;;; Glob conversion

(ert-deftest emagent-tools-test-glob-to-regexp ()
  (should (string-match-p (emagent-tools--glob-to-regexp "*.el") "./foo.el"))
  (should (string-match-p (emagent-tools--glob-to-regexp "**/*.el") "./dir/foo.el"))
  (should (string-match-p (emagent-tools--glob-to-regexp "foo?.el") "./foox.el")))

(ert-deftest emagent-tools-test-find-files ()
  "A name glob matches basenames recursively; a path glob (with `/') matches
relative paths and does not cross directory boundaries on `*'."
  (let ((dir (make-temp-file "emagent-find-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src" dir))
          (write-region "" nil (expand-file-name "src/a.el" dir))
          (write-region "" nil (expand-file-name "src/b.py" dir))
          (write-region "" nil (expand-file-name "top.el" dir))
          (let ((emagent-tools--project-directory dir))
            (should (equal "src/a.el" (emagent-tool-find-files "src/*.el")))
            (should (equal '("src/a.el" "top.el")
                           (sort (split-string (emagent-tool-find-files "*.el") "\n")
                                 #'string<)))
            (should (equal "No matches" (emagent-tool-find-files "*.rb")))))
      (delete-directory dir t))))

;;;; Write diff

(ert-deftest emagent-tools-test-write-diff-string ()
  (let* ((dir (make-temp-file "emagent-tools-test-" t))
         (path (expand-file-name "sample.txt" dir))
         (diff (emagent-tools--write-diff-string path "new\ncontent")))
    (unwind-protect
        (progn
          (write-region "old\ncontent" nil path)
          (should (string-match-p "^---" diff))
          (should (string-match-p "new" diff)))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-buttons-prompt-removes-block ()
  (with-temp-buffer
    (insert "before\n")
    (let ((buf (current-buffer))
          (result nil))
      (emagent-tools--buttons-prompt
       "Allow test?"
       '(("Allow" . ok))
       buf
       (lambda (v) (setq result v)))
      (emagent-test--push-first-button buf)
      (should (eq result 'ok))
      (should (string= "before\n" (buffer-string))))))

(ert-deftest emagent-tools-test-buttons-prompt-survives-insert-before ()
  "Prompt cleanup still works when streaming inserts before the block."
  (with-temp-buffer
    (insert "before\n")
    (let ((buf (current-buffer))
          (result nil))
      (emagent-tools--buttons-prompt
       "Allow compile?"
       '(("Allow" . ok))
       buf
       (lambda (v) (setq result v)))
      (with-current-buffer buf
        (goto-char (point-min))
        (insert "streamed "))
      (emagent-test--push-first-button buf)
      (should (eq result 'ok))
      (should (string-match-p "\\`streamed before\n\\'" (buffer-string)))
      (should-not (string-match-p "Allow compile?" (buffer-string)))
      (should-not (string-match-p "\\[Allow\\]" (buffer-string))))))

(ert-deftest emagent-tools-test-buttons-prompt-yn-shortcuts ()
  "Accept/reject dialogs expose Y/N labels and uppercase key bindings."
  (with-temp-buffer
    (insert "plan\n")
    (let ((buf (current-buffer))
          (result nil))
      (emagent-tools--buttons-prompt
       "Accept and build this plan?"
       '(("Accept & Build" . :accept) ("Reject" . :reject))
       buf
       (lambda (v) (setq result v)))
      (should (string-match-p "\\[Accept & Build (y)\\]" (buffer-string)))
      (should (string-match-p "\\[Reject (n)\\]" (buffer-string)))
      (with-current-buffer buf
        (emagent-tools--goto-first-button (point-min))
        (let ((map (get-text-property (point) 'keymap)))
          (should (commandp (lookup-key map (kbd "y"))))
          (should (commandp (lookup-key map (kbd "Y"))))
          (should (commandp (lookup-key map (kbd "n"))))
          (should (commandp (lookup-key map (kbd "N"))))
          (call-interactively (lookup-key map (kbd "Y")))))
      (should (eq result :accept))
      (should-not (string-match-p "Accept and build" (buffer-string))))))

(ert-deftest emagent-tools-test-buttons-prompt-before-user-stub ()
  "Dialog inserts above a trailing user stub, not after it."
  (with-temp-buffer
    (let* ((stub (emagent-chat--user-heading-prefix))
           (stub-re (regexp-quote (string-trim-right stub))))
      (insert "thought\n" stub "\n")
      (let ((buf (current-buffer))
            (result nil))
        (cl-letf (((symbol-function 'emagent-chat--user-zone-start)
                   (lambda ()
                     (save-excursion
                       (goto-char (point-min))
                       (re-search-forward (emagent-chat--user-heading-re))
                       (line-beginning-position)))))
          (emagent-tools--buttons-prompt
           "Accept and build this plan?"
           '(("Accept & Build" . :accept) ("Reject" . :reject))
           buf
           (lambda (v) (setq result v)))
          (should (< (string-match "Accept and build" (buffer-string))
                     (string-match stub-re (buffer-string))))
          (emagent-test--push-first-button buf)
          (should (eq result :accept))
          (should (string-match-p stub-re (buffer-string))))))))

;;;; Elisp syntax check

(ert-deftest emagent-tools-test-check-elisp ()
  (let ((bad "(+ 1 2"))
    (should (string= "OK" (emagent-tool-check-elisp "(+ 1 2)")))
    (should (string-match-p "SYNTAX ERROR" (emagent-tool-check-elisp bad)))
    (should (string-match-p "line [0-9]+, column [0-9]+"
                            (emagent-tool-check-elisp bad)))))

(ert-deftest emagent-tools-test-write-elisp-validation ()
  (let* ((dir (make-temp-file "emagent-tools-elisp-" t))
         (path (expand-file-name "bad.el" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-elisp-validate-on-write t)
         (emagent-elisp-byte-compile-on-check nil))
    (unwind-protect
        (should-error
         (emagent-tools--write-file-content path "(defun x ()\n  (+ 1"))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-structural-elisp-eval ()
  :tags '(lisp-sitter)
  (skip-unless (emagent-struct-available-p))
  (let* ((dir (make-temp-file "emagent-tools-eval-" t))
         (file "loaded.el")
         (resolved (expand-file-name file dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-elisp-byte-compile-on-check nil)
         (emagent-struct-eval-after-structural-edit t))
    (unwind-protect
        (progn
          (write-region "" nil resolved)
          (let ((result (emagent-tool-structural-insert
                         file "__start__" "(defun emagent-tools-eval-test () 'evaluated)")))
            (should (string-match-p "Wrote" result))
            (should (fboundp 'emagent-tools-eval-test))
            (should (eq (emagent-tools-eval-test) 'evaluated))))
      (fmakunbound 'emagent-tools-eval-test)
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-structural-elisp-eval-blocked ()
  :tags '(lisp-sitter)
  (skip-unless (emagent-struct-available-p))
  (let* ((dir (make-temp-file "emagent-tools-eval-block-" t))
         (file "evil.el")
         (resolved (expand-file-name file dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-elisp-byte-compile-on-check nil))
    (unwind-protect
        (progn
          (write-region "" nil resolved)
          (should-error
           (emagent-tool-structural-insert
            file "__start__" "(kill-emacs)")))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-write-file-blocked-for-lisp ()
  :tags '(lisp-sitter)
  (skip-unless (emagent-struct-available-p))
  (let* ((dir (make-temp-file "emagent-tools-write-block-" t))
         (file "blocked.el")
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir))
    (unwind-protect
        (should-error
         (emagent-tool-write-file file "(defun foo () 1)"))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-eval-form-guard-blocked ()
  (should (string-match-p "Eval blocked (kill-emacs)"
                          (emagent-tools--eval-form-guard "(kill-emacs)"))))

(ert-deftest emagent-tools-test-symbols-in-form ()
  (should (equal '(delete-file)
                 (emagent-tools--symbols-in-form '(delete-file "x")
                                                 '(delete-file))))
  (should (equal nil (emagent-tools--symbols-in-form '(+ 1 2) '(delete-file)))))

(provide 'emagent-tools-test)

;;; emagent-tools-test.el ends here
