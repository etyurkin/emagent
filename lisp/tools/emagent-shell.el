;;; emagent-shell.el --- Shell command routing for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.8
;; This file is part of emagent.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Intercepts `emagent-tool-run-shell-command' to block unsafe git usage,
;; suggest Emacs-native alternatives, and auto-redirect simple commands.
;;
;;; Code:

(require 'emagent-log)
(require 'emagent-policy)
(require 'emagent-tools)
(require 'emagent-tools-shell)

(defvar emagent-acp-prefer-emacs)

(defgroup emagent-shell nil
  "Shell command routing for emagent."
  :group 'emagent)

(defcustom emagent-shell-block-no-verify t
  "When non-nil, refuse git commands that use --no-verify."
  :type 'boolean
  :group 'emagent-shell)

(defcustom emagent-shell-guard-push t
  "When non-nil, refuse git push when gh reports the branch PR is merged."
  :type 'boolean
  :group 'emagent-shell)

(defcustom emagent-shell-redirect t
  "When non-nil with `emagent-acp-prefer-emacs', redirect simple shell commands."
  :type 'boolean
  :group 'emagent-shell)

(defcustom emagent-shell-suggest t
  "When non-nil with `emagent-acp-prefer-emacs', refuse substitutable shell."
  :type 'boolean
  :group 'emagent-shell)

(defun emagent-shell--prefer-emacs-p ()
  "Return non-nil when Emacs-native routing is active."
  (and (boundp 'emagent-acp-prefer-emacs)
       emagent-acp-prefer-emacs
       emagent-shell-redirect))

(defun emagent-shell--suggest-p ()
  "Return non-nil when shell suggestions are active."
  (and (boundp 'emagent-acp-prefer-emacs)
       emagent-acp-prefer-emacs
       emagent-shell-suggest))

(defun emagent-shell--strip-quoted (command)
  "Remove single- and double-quoted spans from COMMAND."
  (replace-regexp-in-string "[\"'][^\"']*[\"']" "" command))

(defun emagent-shell--git-no-verify-p (command)
  "Return non-nil when COMMAND is git with --no-verify."
  (and (string-match-p "\\<git\\>" command)
       (string-match-p "--no-verify" (emagent-shell--strip-quoted command))))

(defun emagent-shell--git-push-p (command)
  "Return non-nil when COMMAND is a git push."
  (string-match-p "\\`git[[:space:]]+push\\>" (string-trim command)))

(defconst emagent-shell--build-executables
  '("mvn" "./mvnw" "gradle" "./gradlew" "make" "cmake" "ninja"
    "cargo" "go" "pytest" "python" "python3"
    "npm" "yarn" "pnpm" "bun")
  "Executable names that produce compiler-style output.
These are always redirected to `emagent-tool-compile' for navigable errors.")

(defun emagent-shell--build-command-p (words)
  "Return non-nil when WORDS names a build/test/compile executable."
  (member (car words) emagent-shell--build-executables))

(defvar emagent-tools--timeout-override)

(defvar emagent-tools--shell-output-limit)

(defun emagent-shell--call-with-timeout (timeout thunk)
  "Call THUNK with TIMEOUT bound as `emagent-tools--timeout-override'."
  (if timeout
      (let ((emagent-tools--timeout-override timeout))
        (funcall thunk))
    (funcall thunk)))

(defun emagent-shell--captured-timeout ()
  "Return the per-call timeout from the current dynamic binding, or nil."
  (and emagent-tools--timeout-override
       (emagent-tools--clamp-timeout emagent-tools--timeout-override)))

(defun emagent-shell--run-in-directory (directory fn)
  "Run FN with `default-directory' set to DIRECTORY."
  (let ((default-directory (emagent-tools--root-directory directory)))
    (funcall fn)))

(defun emagent-shell--unquote (text)
  "Strip one layer of shell quotes from TEXT."
  (if (and (stringp text) (>= (length text) 2))
      (pcase (aref text 0)
        (?\" (if (= (aref text (1- (length text))) ?\")
                 (substring text 1 -1)
               text))
        (?\' (if (= (aref text (1- (length text))) ?\')
                 (substring text 1 -1)
               text))
        (_ text))
    text))

(defun emagent-shell--words (command)
  "Split COMMAND into words, respecting simple quotes."
  (if (fboundp 'split-string-shell-argument)
      (split-string-shell-argument command)
    (split-string command "[[:space:]]+" t)))

(defun emagent-shell--command-to-string (command)
  "Like `shell-command-to-string' for COMMAND, yielding to the event loop."
  (let ((buf (generate-new-buffer " *emagent-shell*"))
        done)
    (unwind-protect
        (progn
          (let ((proc (start-process-shell-command "emagent-shell" buf command)))
            (set-process-sentinel proc (lambda (_p _e) (setq done t))))
          (while (not done)
            (accept-process-output nil 0.05))
          (with-current-buffer buf
            (buffer-string)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun emagent-shell--read-only-network-p (command)
  "Return non-nil when COMMAND is a read-only HTTP GET via curl or wget."
  (let ((cmd (emagent-shell--strip-quoted (string-trim command))))
    (cond
     ((string-match-p "\\`curl\\>" cmd)
      (and (string-match-p "https?://" cmd)
           (not (string-match-p
                 "\\(?:-X[[:space:]]*\\(?:POST\\|PUT\\|DELETE\\|PATCH\\)\\|-d\\|--data\\|-F\\|--form\\|-o[[:space:]]\\|-O\\|>[[:space:]]\\)"
                 cmd))))
     ((string-match-p "\\`wget\\>" cmd)
      (and (string-match-p "https?://" cmd)
           (not (string-match-p "\\(?:--post\\|-O\\|--output-document\\|>[[:space:]]\\)" cmd))))
     (t nil))))

(defun emagent-shell--current-branch ()
  "Return the current git branch name, or nil."
  (string-trim (apply #'emagent-tools--run-git "branch" "--show-current")))

(defun emagent-shell--branch-pr-merged-p (branch)
  "Return non-nil when gh reports BRANCH has a merged PR."
  (when (and branch (not (string-empty-p branch)) (executable-find "gh"))
    (let ((state (string-trim
                  (emagent-shell--command-to-string
                   (format "gh pr view --head %s --json state -q .state 2>/dev/null"
                           (shell-quote-argument branch))))))
      (string= state "MERGED"))))

(defun emagent-shell--guard-git-push (directory)
  "Signal an error when pushing a branch whose PR is already merged.

Arguments: DIRECTORY."
  (when emagent-shell-guard-push
    (emagent-shell--run-in-directory
     directory
     (lambda ()
       (let ((branch (emagent-shell--current-branch)))
         (when (and branch (not (string-empty-p branch)))
           (if (executable-find "gh")
               (when (emagent-shell--branch-pr-merged-p branch)
                 (user-error
                  "Branch '%s' has an already-merged PR; checkout main, pull, and create a new branch instead"
                  branch))
             (require 'emagent-log)
             (emagent-log "emagent: gh CLI not found; skipping merged-PR check for push"))))))))

(defun emagent-shell--guard-git-push-async (directory callback &optional timeout)
  "Call CALLBACK with nil on success or an error string when push is blocked.

Arguments: DIRECTORY, TIMEOUT."
  (if (not emagent-shell-guard-push)
      (funcall callback nil)
    (emagent-shell--call-with-timeout timeout
     (lambda ()
       (emagent-tools--run-git-async
        (lambda (branch-out is-error)
          (if is-error
              (funcall callback branch-out)
            (let ((branch (string-trim branch-out)))
              (cond
               ((or (null branch) (string-empty-p branch))
                (funcall callback nil))
               ((not (executable-find "gh"))
                (require 'emagent-log)
                (emagent-log "emagent: gh CLI not found; skipping merged-PR check for push")
                (funcall callback nil))
               (t
                (emagent-shell--call-with-timeout timeout
                 (lambda ()
                   (emagent-tools--run-shell-async
                    (lambda (state-out is-error-gh)
                      (if is-error-gh
                          (funcall callback nil)
                        (if (string= (string-trim state-out) "MERGED")
                            (funcall callback
                                     (format "Branch '%s' has an already-merged PR. Checkout main, pull, and create a new branch instead."
                                             branch))
                          (funcall callback nil))))
                    (format "gh pr view --head %s --json state -q .state 2>/dev/null"
                            (shell-quote-argument branch))
                    directory))))))))
        "branch" "--show-current")))))

(defun emagent-shell--redirect-git (words directory)
  "Internal helper for WORDS and DIRECTORY."
  (emagent-shell--run-in-directory
   directory
   (lambda ()
     (pcase words
       (`("git" "status" . ,_)
        (emagent-tool-git-status))
       (`("git" "diff" . ,rest)
        (emagent-tool-git-diff (and rest (string-join rest " "))))
       (`("git" "log" . ,rest)
        (emagent-tool-git-log (and rest (string-join rest " "))))
       (_ nil)))))

(defun emagent-shell--redirect-cat (words directory)
  "Internal helper for WORDS and DIRECTORY."
  (emagent-shell--run-in-directory
   directory
   (lambda ()
     (pcase words
       (`("cat" ,path)
        (emagent-tool-read-file (emagent-shell--unquote path)))
       (_ nil)))))

(defun emagent-shell--redirect-head (words directory)
  "Internal helper for WORDS and DIRECTORY."
  (emagent-shell--run-in-directory
   directory
   (lambda ()
     (pcase words
       (`("head" "-n" ,n ,path)
        (emagent-tool-read-file (emagent-shell--unquote path)
                                1 (string-to-number n)))
       (`("head" ,path)
        (emagent-tool-read-file (emagent-shell--unquote path) 1 10))
       (_ nil)))))

(defun emagent-shell--redirect-grep (words directory)
  "Internal helper for WORDS and DIRECTORY."
  (emagent-shell--run-in-directory
   directory
   (lambda ()
     (let ((pattern nil)
           (path nil)
           (skip-next nil))
       (dolist (word (cdr words))
         (cond
          (skip-next
           (setq skip-next nil))
          ((member word '("-r" "-R" "-n" "-H" "-h" "--color=auto" "--color=never"))
           nil)
          ((string-prefix-p "-" word)
           (setq skip-next t))
          ((null pattern)
           (setq pattern (emagent-shell--unquote word)))
          (t
           (setq path (emagent-shell--unquote word)))))
       (when pattern
         (emagent-tool-grep pattern path))))))

(defun emagent-shell--redirect-rg (words directory)
  "Internal helper for WORDS and DIRECTORY."
  (emagent-shell--run-in-directory
   directory
   (lambda ()
     (let ((pattern nil)
           (path nil))
       (dolist (word (cdr words))
         (cond
          ((and (string-prefix-p "-" word) (not (string-match-p "^-[0-9]+$" word)))
           nil)
          ((null pattern)
           (setq pattern (emagent-shell--unquote word)))
          (t
           (setq path (emagent-shell--unquote word)))))
       (when pattern
         (emagent-tool-grep pattern path))))))

(defun emagent-shell--redirect-find (command directory)
  "Internal helper for COMMAND and DIRECTORY."
  (when (string-match
         "\\`find\\(?:[[:space:]]+\\([^[:space:]]+\\)\\)?[[:space:]]+-name[[:space:]]+\\([^[:space:]]+\\)"
         command)
    (let ((root (match-string 1 command))
          (glob (emagent-shell--unquote (match-string 2 command))))
      (emagent-tool-find-files glob (or root directory)))))

(defun emagent-shell--try-redirect (command directory)
  "Run COMMAND via an emagent tool when it matches a simple pattern.

Arguments: DIRECTORY."
  (when (emagent-shell--prefer-emacs-p)
    (let* ((trimmed (string-trim command))
           (words (emagent-shell--words trimmed))
           (tool (pcase (car words)
                   ("git" (emagent-shell--redirect-git words directory))
                   ("cat" (emagent-shell--redirect-cat words directory))
                   ("head" (emagent-shell--redirect-head words directory))
                   ("grep" (emagent-shell--redirect-grep words directory))
                   ((or "rg" "ag") (emagent-shell--redirect-rg words directory))
                   (_ nil))))
      (or tool
          (emagent-shell--redirect-find trimmed directory)))))

(defun emagent-shell--suggest-alternative (command)
  "Return a user-facing hint when COMMAND should use an emagent tool."
  (when (emagent-shell--suggest-p)
    (let ((cmd (string-trim command)))
    (cond
     ((string-match-p "\\`git[[:space:]]+status\\>" cmd) nil)
     ((string-match-p "\\`git[[:space:]]+diff\\>" cmd) nil)
     ((string-match-p "\\`git[[:space:]]+log\\>" cmd) nil)
     ((string-match-p "\\<git\\>" cmd)
      "Use emagent git_status, git_diff, or git_log instead of shell git.")
     ((string-match-p "\\`\\(?:grep\\|rg\\|ag\\)\\>" cmd)
      "Use emagent grep instead of shell search commands.")
     ((string-match-p "\\`find\\>" cmd)
      "Use emagent find_files or list_files instead of shell find.")
     ((string-match-p "\\`\\(?:cat\\|head\\|tail\\)\\>" cmd)
      "Use emagent read_file (optional line and limit) instead of cat/head/tail.")
     ((string-match-p "\\`jq\\>" cmd)
      "Use emagent eval with json-parse-string / json-read instead of jq.")
     ((string-match-p "\\`open[[:space:]]" cmd)
      "Use emagent eval with browse-url instead of open.")
     (t nil)))))

(defun emagent-shell--redirect-git-async (words callback &optional timeout)
  "Internal helper for WORDS and CALLBACK and TIMEOUT."
  (emagent-shell--call-with-timeout timeout
   (lambda ()
     (pcase words
       (`("git" "status" . ,_)
        (emagent-tool-git-status-async callback))
       (`("git" "diff" . ,rest)
        (emagent-tool-git-diff-async callback (and rest (string-join rest " "))))
       (`("git" "log" . ,rest)
        (emagent-tool-git-log-async callback (and rest (string-join rest " "))))
       (_ (funcall callback nil nil))))))

(defun emagent-shell--redirect-cat-async (words callback)
  "Internal helper for WORDS and CALLBACK."
  (pcase words
    (`("cat" ,path)
     (funcall callback (emagent-tool-read-file (emagent-shell--unquote path)) nil))
    (_ (funcall callback nil nil))))

(defun emagent-shell--redirect-head-async (words callback)
  "Internal helper for WORDS and CALLBACK."
  (pcase words
    (`("head" "-n" ,n ,path)
     (funcall callback
              (emagent-tool-read-file (emagent-shell--unquote path)
                                      1 (string-to-number n))
              nil))
    (`("head" ,path)
     (funcall callback
              (emagent-tool-read-file (emagent-shell--unquote path) 1 10)
              nil))
    (_ (funcall callback nil nil))))

(defun emagent-shell--redirect-grep-async (words _directory callback &optional timeout)
  "Internal helper for WORDS and CALLBACK and TIMEOUT."
  (let ((pattern nil)
        (path nil)
        (skip-next nil))
    (dolist (word (cdr words))
      (cond
       (skip-next (setq skip-next nil))
       ((member word '("-r" "-R" "-n" "-H" "-h" "--color=auto" "--color=never"))
        nil)
       ((string-prefix-p "-" word)
        (setq skip-next t))
       ((null pattern)
        (setq pattern (emagent-shell--unquote word)))
       (t
        (setq path (emagent-shell--unquote word)))))
    (if pattern
        (emagent-shell--call-with-timeout timeout
         (lambda ()
           (emagent-tool-grep-async callback pattern path)))
      (funcall callback nil nil))))

(defun emagent-shell--redirect-rg-async (words _directory callback &optional timeout)
  "Internal helper for WORDS and CALLBACK and TIMEOUT."
  (let ((pattern nil)
        (path nil))
    (dolist (word (cdr words))
      (cond
       ((and (string-prefix-p "-" word) (not (string-match-p "^-[0-9]+$" word)))
        nil)
       ((null pattern)
        (setq pattern (emagent-shell--unquote word)))
       (t
        (setq path (emagent-shell--unquote word)))))
    (if pattern
        (emagent-shell--call-with-timeout timeout
         (lambda ()
           (emagent-tool-grep-async callback pattern path)))
      (funcall callback nil nil))))

(defun emagent-shell--try-redirect-async (command directory callback &optional timeout)
  "Run COMMAND via an emagent tool when it matches; call CALLBACK with result.

Arguments: DIRECTORY, TIMEOUT."
  (if (not (emagent-shell--prefer-emacs-p))
      (funcall callback nil nil)
    (let* ((trimmed (string-trim command))
           (words (emagent-shell--words trimmed))
           (first (car words)))
      (pcase first
        ("git"
         (emagent-shell--redirect-git-async words callback timeout))
        ("cat"
         (emagent-shell--redirect-cat-async words callback))
        ("head"
         (emagent-shell--redirect-head-async words callback))
        ("grep"
         (emagent-shell--redirect-grep-async words directory callback timeout))
        ((or "rg" "ag")
         (emagent-shell--redirect-rg-async words directory callback timeout))
        (_
         (let ((found (emagent-shell--redirect-find trimmed directory)))
           (if found
               (funcall callback found nil)
             (funcall callback nil nil))))))))

(defun emagent-shell--run-command-body-async (cmd words directory callback
                                                  &optional timeout)
  "Run guarded shell CMD asynchronously; deliver via CALLBACK.

Arguments: WORDS, DIRECTORY, TIMEOUT."
  (if (emagent-shell--build-command-p words)
      (emagent-shell--call-with-timeout timeout
       (lambda ()
         (emagent-tool-compile-async callback cmd directory)))
    (emagent-shell--try-redirect-async cmd directory
     (lambda (redirected is-error)
       (if redirected
           (funcall callback redirected is-error)
         (let ((suggestion (emagent-shell--suggest-alternative cmd)))
           (if suggestion
               (funcall callback suggestion t)
             (emagent-shell--call-with-timeout timeout
              (lambda ()
                (emagent-tools--run-shell-async callback cmd directory)))))))
     timeout)))

(defun emagent-shell-run-command-async (command directory callback)
  "Like `emagent-shell-run-command' for COMMAND via CALLBACK.
CALLBACK is called as \(CALLBACK OUTPUT IS-ERROR).  Synchronous guards
\(policy, --no-verify) run immediately; push guard, redirects, compile,
and shell fallback are non-blocking.

Arguments: DIRECTORY."
  (let* ((cmd (string-trim command))
         (words (emagent-shell--words cmd))
         (timeout (emagent-shell--captured-timeout))
         (guard-error
          (condition-case err
              (progn
                (emagent-policy-enforce (emagent-policy-check-shell cmd) cmd)
                (when (and emagent-shell-block-no-verify
                           (emagent-shell--git-no-verify-p cmd))
                  (user-error
                   "--no-verify bypasses pre-commit hooks; fix the pre-commit issue instead"))
                nil)
            (error (error-message-string err)))))
    (if guard-error
        (funcall callback guard-error t)
      (if (emagent-shell--git-push-p cmd)
          (emagent-shell--guard-git-push-async
           directory
           (lambda (push-err)
             (if push-err
                 (funcall callback push-err t)
               (emagent-shell--run-command-body-async
                cmd words directory callback timeout)))
           timeout)
        (emagent-shell--run-command-body-async
         cmd words directory callback timeout)))))

(defun emagent-shell-run-command (command &optional directory)
  "Run COMMAND with Emacs-native routing, guards, and redirects.

Arguments: DIRECTORY."
  (let* ((cmd (string-trim command))
         (words (emagent-shell--words cmd)))
    (emagent-policy-enforce (emagent-policy-check-shell cmd) cmd)
    (when (and emagent-shell-block-no-verify
               (emagent-shell--git-no-verify-p cmd))
      (user-error
       "--no-verify bypasses pre-commit hooks; fix the pre-commit issue instead"))
    (when (emagent-shell--git-push-p cmd)
      (emagent-shell--guard-git-push directory))
    ;; Build/test commands always go through compilation-mode for navigable errors,
    ;; regardless of the prefer-emacs setting.
    (if (emagent-shell--build-command-p words)
        (emagent-tool-compile cmd directory)
      (or (emagent-shell--try-redirect cmd directory)
          (let ((suggestion (emagent-shell--suggest-alternative cmd)))
            (when suggestion
              (user-error "%s" suggestion))
            (let* ((default-directory (emagent-tools--root-directory directory))
                   (output (emagent-shell--command-to-string cmd)))
              (if (> (length output) emagent-tools--shell-output-limit)
                  (concat (substring output 0 emagent-tools--shell-output-limit)
                          "\n… (output truncated)")
                output)))))))

(provide 'emagent-shell)
;;; emagent-shell.el ends here
