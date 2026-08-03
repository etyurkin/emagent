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

(ert-deftest emagent-tools-test-structural-eval-skips-side-effects ()
  "Post-write eval only reloads definition heads, not delete-file/make-process."
  (let ((ran nil))
    (cl-letf (((symbol-function 'delete-file)
               (lambda (&rest _) (setq ran t))))
      (emagent-tools--structural-eval-after-edit
       "(delete-file \"/tmp/should-not-run\")")
      (should-not ran)
      (emagent-tools--structural-eval-after-edit
       "(progn (delete-file \"/tmp/should-not-run\"))")
      (should-not ran))
    (emagent-tools--structural-eval-after-edit
     "(defun emagent-tools-eval-side-test () 1)")
    (should (fboundp 'emagent-tools-eval-side-test))
    (fmakunbound 'emagent-tools-eval-side-test)))

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

(ert-deftest emagent-tools-test-apply-string-edit ()
  (should (string= "hello world"
                   (emagent-tools--apply-string-edit "hello there" "there" "world")))
  (should (string= "aXaX"
                   (emagent-tools--apply-string-edit "abab" "ab" "aX" t)))
  (should-error (emagent-tools--apply-string-edit "abab" "ab" "aX"))
  (should-error (emagent-tools--apply-string-edit "hello" "missing" "x")))

(ert-deftest emagent-tools-test-edit-file ()
  (let* ((dir (make-temp-file "emagent-tools-edit-" t))
         (file "poc.qmd")
         (resolved (expand-file-name file dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p nil)
         (emagent-struct-require-for-lisp-files nil))
    (unwind-protect
        (progn
          (write-region "alpha\nbeta\ngamma\n" nil resolved)
          (emagent-tool-edit-file file "beta" "BETA")
          (should (string= "alpha\nBETA\ngamma\n"
                           (with-temp-buffer
                             (insert-file-contents resolved)
                             (buffer-string))))
          (should-error (emagent-tool-edit-file file "missing" "x"))
          (emagent-tool-edit-file file "alpha" "ALPHA")
          (should (string= "ALPHA\nBETA\ngamma\n"
                           (with-temp-buffer
                             (insert-file-contents resolved)
                             (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-eval-form-guard-blocked ()
  (should (string-match-p "Eval blocked (kill-emacs)"
                          (emagent-tools--eval-form-guard "(kill-emacs)"))))

(ert-deftest emagent-tools-test-symbols-in-form ()
  (should (equal '(delete-file)
                 (emagent-tools--symbols-in-form '(delete-file "x")
                                                 '(delete-file))))
  (should (equal nil (emagent-tools--symbols-in-form '(+ 1 2) '(delete-file)))))


(ert-deftest emagent-tools-test-file-tick-stable-for-same-content ()
  (let* ((dir (make-temp-file "emagent-tick-" t))
         (file (expand-file-name "note.txt" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir))
    (unwind-protect
        (progn
          (write-region "hello\n" nil file nil 'silent)
          (let ((tick (emagent-tools--file-tick file)))
            (should (stringp tick))
            (should (string= tick (emagent-tools--file-tick file)))
            (write-region "hello\n" nil file nil 'silent)
            (should (string= tick (emagent-tools--file-tick file)))
            (write-region "other\n" nil file nil 'silent)
            (should-not (string= tick (emagent-tools--file-tick file)))))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-directory-tick-tracks-nested-content ()
  "Directory tick must change when a nested file's size or mtime changes."
  (let* ((dir (make-temp-file "emagent-dir-tick" t))
         (nested (expand-file-name "nested.txt" dir))
         (emagent-tools--project-directory dir))
    (unwind-protect
        (progn
          (with-temp-file nested (insert "one\n"))
          (let ((tick (emagent-tools--file-tick dir)))
            (should (string-prefix-p "dir:" tick))
            (should (string= tick (emagent-tools--file-tick dir)))
            (with-temp-file nested (insert "two-chars-longer\n"))
            (should-not (string= tick (emagent-tools--file-tick dir)))))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-mcp-write-requires-tick ()
  (let* ((dir (make-temp-file "emagent-tick-req-" t))
         (file (expand-file-name "note.txt" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p t)
         (emagent-tools--expected-file-tick nil))
    (unwind-protect
        (progn
          (write-region "v1\n" nil file nil 'silent)
          (should-error (emagent-tools--write-file-content file "v2\n")))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-mcp-write-stale-tick ()
  (let* ((dir (make-temp-file "emagent-tick-stale-" t))
         (file (expand-file-name "note.txt" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p t))
    (unwind-protect
        (progn
          (write-region "v1\n" nil file nil 'silent)
          (let ((tick (emagent-tools--file-tick file)))
            (write-region "v1b\n" nil file nil 'silent)
            (let ((emagent-tools--expected-file-tick tick))
              (should-error (emagent-tools--write-file-content file "v2\n")))))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-mcp-write-matching-tick ()
  (let* ((dir (make-temp-file "emagent-tick-ok-" t))
         (file (expand-file-name "note.txt" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p t)
         (emagent-tools-show-written-buffer nil))
    (unwind-protect
        (progn
          (write-region "v1\n" nil file nil 'silent)
          (let* ((tick (emagent-tools--file-tick file))
                 (emagent-tools--expected-file-tick tick))
            (emagent-tools--write-file-content file "v2\n")
            (should (string= "v2\n"
                             (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string))))
            (should-not (string= tick (emagent-tools--file-tick file)))))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-mcp-read-file-includes-tick ()
  (let* ((dir (make-temp-file "emagent-tick-read-" t))
         (file (expand-file-name "note.txt" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p t))
    (unwind-protect
        (progn
          (write-region "hello\n" nil file nil 'silent)
          (let ((out (emagent-tool-read-file file)))
            (should (string-match-p "\\`emagent-tick: " out))
            (should (string-match-p "---\nhello\n\\'" out))))
      (delete-directory dir t))))


(ert-deftest emagent-tools-test-mcp-undo-includes-tick ()
  (let* ((dir (make-temp-file "emagent-tick-undo-" t))
         (file (expand-file-name "note.txt" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p t)
         (emagent-tools-show-written-buffer nil))
    (unwind-protect
        (progn
          (write-region "v1\n" nil file nil 'silent)
          (let* ((tick (emagent-tools--file-tick file))
                 (emagent-tools--expected-file-tick tick))
            (emagent-tools--write-file-content file "v2\n")
            (let ((out (emagent-tool-undo-file file)))
              (should (string-match-p "Undid 1 change" out))
              (should (string-match-p "emagent-tick: " out))
              (should (string= "v1\n"
                               (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-string)))))))
      (delete-directory dir t))))



(ert-deftest emagent-tools-test-reconcile-preserves-dirty-deleted ()
  "Deleted-on-disk dirty buffers are kept for read; require-clean errors."
  (let* ((dir (make-temp-file "emagent-reconcile-del-" t))
         (file (expand-file-name "note.txt" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p t)
         buf)
    (unwind-protect
        (progn
          (write-region "disk\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (with-current-buffer buf
            (erase-buffer)
            (insert "unsaved\n")
            (set-buffer-modified-p t))
          (delete-file file)
          (should-error
           (emagent-tools--reconcile-visited-file file t)
           :type 'user-error)
          (should (buffer-live-p buf))
          (should (buffer-modified-p buf))
          ;; Without require-clean, keep dirty so read can return edits.
          (emagent-tools--reconcile-visited-file file nil)
          (should (buffer-live-p buf))
          (should (equal "unsaved\n"
                         (emagent-tools--read-file-content file)))
          ;; Clean ghost buffers are still dropped.
          (with-current-buffer buf
            (set-buffer-modified-p nil))
          (emagent-tools--reconcile-visited-file file nil)
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emagent-tools-test-reconcile-reverts-clean-external ()
  "Unmodified buffer picks up external disk edits on read."
  (let* ((dir (make-temp-file "emagent-reconcile-" t))
         (file (expand-file-name "note.txt" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir))
    (unwind-protect
        (progn
          (write-region "v1\n" nil file nil 'silent)
          (find-file-noselect file)
          (write-region "v2\n" nil file nil 'silent)
          (should (string= "v2\n" (emagent-tools--read-file-content file)))
          (with-current-buffer (find-buffer-visiting file)
            (should (string= "v2\n" (buffer-string)))
            (should (verify-visited-file-modtime))))
      (ignore-errors (kill-buffer (find-buffer-visiting file)))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-mcp-write-stale-on-external-disk ()
  "ACP write with pre-external tick fails after disk changes under buffer."
  (let* ((dir (make-temp-file "emagent-ext-tick-" t))
         (file (expand-file-name "note.txt" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p t)
         (emagent-tools-show-written-buffer nil))
    (unwind-protect
        (progn
          (write-region "v1\n" nil file nil 'silent)
          (find-file-noselect file)
          (let ((tick (emagent-tools--file-tick file)))
            (write-region "v2\n" nil file nil 'silent)
            (let ((emagent-tools--expected-file-tick tick))
              (should-error
               (emagent-tools--write-file-content file "v3\n")
               :type 'user-error))))
      (ignore-errors (kill-buffer (find-buffer-visiting file)))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-sync-errors-dirty-plus-external ()
  "Structural sync must not flush a dirty buffer over a newer disk file."
  (let* ((dir (make-temp-file "emagent-dirty-ext-" t))
         (file (expand-file-name "note.txt" dir))
         (emagent-tools--root-boundary dir)
         (emagent-tools--project-directory dir))
    (unwind-protect
        (progn
          (write-region "v1\n" nil file nil 'silent)
          (with-current-buffer (find-file-noselect file)
            (insert "dirty")
            (set-buffer-modified-p t)
            (write-region "external\n" nil file nil 'silent)
            (should-error (emagent-tools--structural-sync-path file)
                          :type 'user-error)
            (should (string-match-p "dirty" (buffer-string)))
            (should (string=
                     "external\n"
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))))
      (ignore-errors (kill-buffer (find-buffer-visiting file)))
      (delete-directory dir t))))


(ert-deftest emagent-tools-test-session-context-survives-unbind ()
  "Captured session root is restored in wrapped async callbacks."
  (let* ((dir-a (make-temp-file "emagent-ctx-a-" t))
         (dir-b (make-temp-file "emagent-ctx-b-" t))
         (rel "note.txt")
         (seen nil)
         (ctx nil))
    (unwind-protect
        (progn
          (write-region "AAA\n" nil (expand-file-name rel dir-a) nil 'silent)
          (write-region "BBB\n" nil (expand-file-name rel dir-b) nil 'silent)
          (let ((emagent-tools--project-directory dir-a)
                (emagent-tools--root-boundary dir-a)
                (emagent-tools--acp-session-p t))
            (setq ctx (emagent-tools--capture-session-context)))
          (setq default-directory dir-b)
          (let ((emagent-tools--project-directory dir-b)
                (emagent-tools--root-boundary dir-b))
            (funcall (emagent-tools--wrap-session-callback
                      ctx
                      (lambda (_r _e)
                        (setq seen (emagent-tools--read-file-content rel))))
                     "ok" nil))
          (should (string= "AAA\n" seen)))
      (delete-directory dir-a t)
      (delete-directory dir-b t))))

(ert-deftest emagent-tools-test-age-sessions-isolated ()
  "Age ledger reset for one session leaves another intact."
  (emagent-tools-age-reset 'all)
  (let ((emagent-tools-age t)
        (emagent-tools-age-min-bytes 10)
        (payload (make-string 100 ?q)))
    (let ((emagent-tools-age--session-key "tok-a"))
      (emagent-tools-age-note "fs-read" "/tmp/a" "" payload)
      (should (string-match-p "\\[aged:"
                              (emagent-tools-age-note "fs-read" "/tmp/a" "" payload))))
    (let ((emagent-tools-age--session-key "tok-b"))
      (should (string= payload
                       (emagent-tools-age-note "fs-read" "/tmp/a" "" payload))))
    (let ((emagent-tools-age--session-key "tok-a"))
      (emagent-tools-age-reset)
      (should (string= payload
                       (emagent-tools-age-note "fs-read" "/tmp/a" "" payload))))
    (let ((emagent-tools-age--session-key "tok-b"))
      (should (string-match-p "\\[aged:"
                              (emagent-tools-age-note "fs-read" "/tmp/a" "" payload))))))


(ert-deftest emagent-tools-test-age-ui-uses-buffer-token ()
  "Mode-line age reads follow the chat buffer MCP token."
  (emagent-tools-age-reset 'all)
  (let ((emagent-tools-age t)
        (emagent-tools-age-min-bytes 10)
        (payload (make-string 80 ?z)))
    (with-temp-buffer
      (setq-local emagent-mcp--token "tok-ui")
      (let ((emagent-tools-age--session-key "tok-ui"))
        (emagent-tools-age-note "fs-read" "/tmp/x" "" payload))
      (should (= (string-bytes payload) (emagent-tools-age-bytes)))
      (let ((emagent-tools-age--session-key "other"))
        (should (= 0 (emagent-tools-age-bytes))))
      ;; No dynamic key: fall back to buffer-local token
      (should (= (string-bytes payload) (emagent-tools-age-bytes))))))

(ert-deftest emagent-tools-test-buffer-project-directory ()
  "Per-buffer project survives a later global setq from another chat."
  (let* ((dir-a (make-temp-file "emagent-proj-a-" t))
         (dir-b (make-temp-file "emagent-proj-b-" t))
         (buf (generate-new-buffer " *emagent-proj*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local emagent-mcp--token "tok-a")
          (emagent-tools-set-project-directory dir-a)
          (should (string= (file-truename dir-a)
                           (file-truename emagent-tools--buffer-project-directory)))
          (let ((emagent-tools--project-directory nil))
            (should (string-prefix-p
                     (file-truename dir-a)
                     (file-truename (emagent-tools--root-directory nil)))))
          (setq emagent-tools--project-directory dir-b)
          (let ((emagent-tools--project-directory nil))
            (should (string-prefix-p
                     (file-truename dir-a)
                     (file-truename (emagent-tools--root-directory nil))))))
      (kill-buffer buf)
      (delete-directory dir-a t)
      (delete-directory dir-b t))))





(ert-deftest emagent-tools-test-delete-directory-preserves-buffers-on-failure ()
  "delete_directory must refuse unsaved nested buffers; keep them intact."
  (let* ((dir (make-temp-file "emagent-del-dir" t))
         (sub (expand-file-name "sub" dir))
         (path (expand-file-name "x.txt" sub))
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p nil)
         buf)
    (unwind-protect
        (progn
          (make-directory sub)
          (write-region "disk\n" nil path nil 'quiet)
          (setq buf (find-file-noselect path))
          (with-current-buffer buf
            (erase-buffer)
            (insert "unsaved\n")
            (set-buffer-modified-p t))
          ;; Directory tick ignores nested edits; still must refuse.
          (should-error (emagent-tool-delete-directory sub t)
                        :type 'user-error)
          (should (file-directory-p sub))
          (should (buffer-live-p buf))
          (should (buffer-modified-p buf))
          (should (equal "unsaved\n"
                         (with-current-buffer buf
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
          ;; Clean buffer: non-recursive fails without killing; recursive works.
          (with-current-buffer buf
            (set-buffer-modified-p nil))
          (should-error (emagent-tool-delete-directory sub nil))
          (should (buffer-live-p buf))
          (emagent-tool-delete-directory sub t)
          (should-not (file-directory-p sub))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emagent-tools-test-delete-kills-visiting-buffer ()
  "fs delete must drop the visiting buffer so recreate is not stuck on ghost."
  (let* ((dir (make-temp-file "emagent-del-buf" t))
         (path (expand-file-name "x.txt" dir))
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p nil)
         buf)
    (unwind-protect
        (progn
          (write-region "hi\n" nil path nil 'quiet)
          (setq buf (find-file-noselect path))
          (should (buffer-live-p buf))
          (emagent-tool-delete-file path)
          (should-not (file-exists-p path))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-write-rejects-directory ()
  "Writing file content to a directory path must error with a clear message."
  (let* ((dir (make-temp-file "emagent-write-dir" t))
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p nil))
    (unwind-protect
        (progn
          (should-error (emagent-tools--write-file-content dir "nope\n"))
          (let ((err (should-error (emagent-tool-write-file dir "nope\n"))))
            (should (string-match-p "Cannot write file content to directory"
                                    (error-message-string err)))))
      (delete-directory dir t))))

(ert-deftest emagent-tools-test-write-creates-nested-parents ()
  "fs write must create missing parent directories before visiting the file."
  (let* ((dir (make-temp-file "emagent-write-nested" t))
         (path (expand-file-name "a/b/c.txt" dir))
         (emagent-tools--project-directory dir)
         (emagent-tools--acp-session-p nil)
         (emagent-tools-show-written-buffer nil))
    (unwind-protect
        (progn
          (should-not (file-directory-p (expand-file-name "a/b" dir)))
          (emagent-tools--write-file-content path "hello\n")
          (should (file-readable-p path))
          (should (string= "hello\n"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string)))))
      (delete-directory dir t))))

(provide 'emagent-tools-test)

;;; emagent-tools-test.el ends here
