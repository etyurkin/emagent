;;; emagent-tools-shell.el --- Shell and grep tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

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
;; Shared tool helpers plus shell/process/search/git tool handlers.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'emagent-log)
(require 'emagent-policy)

(eval-when-compile
  (require 'cl-lib))

(defgroup emagent-elisp nil
  "Elisp validation helpers for emagent."
  :group 'emagent-tools)

(defcustom emagent-elisp-validate-on-write t
  "When non-nil, reject writes to .el files that fail Elisp validation."
  :type 'boolean
  :group 'emagent-elisp)

(defcustom emagent-elisp-byte-compile-on-check nil
  "When non-nil, run `byte-compile-file' during .el file validation.

WARNING: byte-compiling expands macros, which EXECUTES arbitrary code from the
validated content — a `(defmacro m () (delete-file …)) (m)' payload runs during
the check.  Because this validation runs on agent-supplied file content (via
`check_structural_file' and, with `emagent-elisp-validate-on-write', every .el
write), enabling it lets a misbehaving or prompt-injected agent run code before
any permission gate.  Leave nil unless you fully trust the content being
validated; syntax and paren checks run regardless."
  :type 'boolean
  :group 'emagent-elisp)

;; ── Position helpers ──────────────────────────────────────────────

(defun emagent-elisp--position-line-column (content pos)
  "Return (LINE . COLUMN) one-based for zero-based POS in CONTENT."
  (let ((line 1) (col 1) (i 0))
    (while (< i pos)
      (pcase (aref content i)
        (?\n (setq line (1+ line) col 1))
        (?\r nil)
        (_ (setq col (1+ col))))
      (setq i (1+ i)))
    (cons line col)))

(defun emagent-elisp--error-at (content pos message)
  "Format MESSAGE with line:column for POS in CONTENT."
  (let ((lc (emagent-elisp--position-line-column content (max 0 pos))))
    (format "line %d, column %d: %s" (car lc) (cdr lc) message)))

;; ── Validation ────────────────────────────────────────────────────

(defun emagent-elisp--scan-parens (content)
  "Return nil when CONTENT balances parens, or an error string."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (condition-case err
        (progn
          (while (< (point) (point-max))
            (skip-chars-forward " \t\n")
            (when (< (point) (point-max))
              (goto-char (scan-sexps (point) 1))))
          (skip-chars-forward " \t\n")
          (when (< (point) (point-max))
            (emagent-elisp--error-at content (point)
                                     "extra text after last form")))
      (scan-error
       (emagent-elisp--error-at content (max 0 (nth 2 err)) (nth 1 err))))))

(defun emagent-elisp--read-forms (content)
  "Read all top-level forms from CONTENT.
Return a list of (POS . FORM) or signal with read error string."
  (let ((pos 0) (len (length content)) (forms nil))
    (while (< pos len)
      (while (and (< pos len)
                  (memq (aref content pos) '(?\s ?\t ?\n ?\r)))
        (setq pos (1+ pos)))
      (when (< pos len)
        (condition-case err
            (let ((parsed (read-from-string content pos)))
              (push (cons pos (car parsed)) forms)
              (setq pos (cdr parsed)))
          (end-of-file
           (error "%s" (emagent-elisp--error-at content pos "unexpected end of file")))
          (error
           (error "%s" (emagent-elisp--error-at content pos (error-message-string err)))))))
    (nreverse forms)))

(defun emagent-elisp--byte-compile-content (content)
  "Return nil when CONTENT byte-compiles, or an error string."
  (let ((tmp (make-temp-file "emagent-elisp-" nil ".el")))
    (unwind-protect
        (progn
          (write-region content nil tmp nil 'silent)
          (let ((byte-compile-debug 1) (inhibit-message t))
            (condition-case err
                (progn
                  (byte-compile-file tmp)
                  (ignore-errors (delete-file (concat tmp "c")))
                  nil)
              (error
               (format "byte-compile: %s" (error-message-string err))))))
      (ignore-errors (delete-file tmp)))))

(defun emagent-elisp--validate-content (content &optional _path)
  "Return nil when CONTENT is valid Elisp, or an error description string."
  (or (emagent-elisp--scan-parens content)
      (condition-case err
          (progn (emagent-elisp--read-forms content) nil)
        (error (error-message-string err))
        (user-error (error-message-string err)))))

(defun emagent-elisp--validate-content-strict (content &optional path)
  "Like `emagent-elisp--validate-content' but also byte-compile CONTENT.

Arguments: PATH."
  (or (emagent-elisp--validate-content content path)
      (when emagent-elisp-byte-compile-on-check
        (emagent-elisp--byte-compile-content content))))

(defun emagent-elisp--wrap-form (form-str)
  "Return FORM-STR wrapped for single-expression validation."
  (concat "(progn " form-str ")"))

(defun emagent-elisp-check-form (form-str)
  "Validate FORM-STR.  Return \"OK\" or an error description."
  (let* ((trimmed (string-trim (or form-str "")))
         (wrapped (emagent-elisp--wrap-form trimmed))
         (err (emagent-elisp--validate-content wrapped))
         (doc-warn (unless err (emagent-elisp--check-docstrings trimmed))))
    (cond
     (err
      (format "SYNTAX ERROR -- %s\n\nFix the form and call check_elisp again before eval."
              err))
     (doc-warn
      (format "STYLE WARNING -- %s\n\nShorten docstring lines to ≤%d chars."
              doc-warn emagent-elisp--docstring-max-line))
     (t "OK"))))

(defun emagent-elisp-check-file-content (content &optional path)
  "Validate Elisp file CONTENT.  Return \"OK\" or an error description.

Arguments: PATH."
  (let ((err (emagent-elisp--validate-content-strict content path))
        (doc-warn (emagent-elisp--check-docstrings content)))
    (cond
     (err
      (format "SYNTAX ERROR -- %s\n\nFix the file and call check_structural_file before writing."
              err))
     (doc-warn
      (format "STYLE WARNING -- %s\n\nShorten docstring lines to ≤%d chars."
              doc-warn emagent-elisp--docstring-max-line))
     (t "OK"))))

;; ── Path helpers ──────────────────────────────────────────────────

(defun emagent-elisp-elisp-file-p (path)
  "Return non-nil when PATH resembles an Emacs Lisp file."
  (and (stringp path) (string-match-p "\\.el\\'" path)))

(defun emagent-elisp--defun-name-p (form)
  "Return defined name when FORM is a defun-like top-level form."
  (when (and (listp form) (memq (car form) '(defun cl-defun))
             (symbolp (nth 1 form)))
    (nth 1 form)))

(defconst emagent-elisp--docstring-max-line 80
  "Maximum allowed length for any single line of an Emacs Lisp docstring.")

(defun emagent-elisp--form-docstring (form)
  "Return the docstring of FORM as a string, or nil when absent."
  (when (listp form)
    (pcase (car form)
      ((or 'defun 'cl-defun 'defmacro 'cl-defmacro)
       (when (stringp (nth 3 form)) (nth 3 form)))
      ((or 'defvar 'defconst 'defcustom 'defgroup 'defface)
       (when (stringp (nth 3 form)) (nth 3 form))))))

(defun emagent-elisp--check-docstrings (content)
  "Return a warning string when any docstring line in CONTENT exceeds 80 chars.
Returns nil when all docstrings are within the limit."
  (condition-case nil
      (let ((forms (emagent-elisp--read-forms content)))
        (catch 'found
          (dolist (pos-form forms)
            (let* ((form (cdr pos-form))
                   (name (and (listp form) (symbolp (nth 1 form)) (nth 1 form)))
                   (doc (emagent-elisp--form-docstring form)))
              (when doc
                (dolist (line (split-string doc "\n"))
                  (when (> (length line) emagent-elisp--docstring-max-line)
                    (throw 'found
                           (format "docstring line >%d chars in `%s': \"%s\""
                                   emagent-elisp--docstring-max-line
                                   (or name "?")
                                   (truncate-string-to-width
                                    line 60 nil nil "…"))))))))
          nil))
    (error nil)))

;; Bound by the MCP dispatcher and by `emagent-tools-set-project-directory'
;; (both in `emagent-tools', which requires this file); forward-declared
;; here rather than required back to avoid a cycle.
(defvar emagent-tools--project-directory)

(defvar emagent-tools--root-boundary)

(defconst emagent-tools--icloud-dir
  (expand-file-name "~/Library/Mobile Documents/"))

(defconst emagent-tools--containers-dir
  (expand-file-name "~/Library/Containers/"))

(defconst emagent-tools--group-containers-dir
  (expand-file-name "~/Library/Group Containers/"))

(defun emagent-tools--protected-truename-p (truename)
  "Return non-nil when TRUENAME is in a protected macOS tree.
TRUENAME is an absolute, symlink-resolved path (iCloud or another app
container).  Pure predicate with no session resolution, so
`emagent-tools--root-directory' can call it safely."
  (or (string-prefix-p emagent-tools--icloud-dir truename)
      (string-prefix-p emagent-tools--containers-dir truename)
      (string-prefix-p emagent-tools--group-containers-dir truename)))

(defun emagent-tools--within-boundary-p (resolved)
  "Return non-nil when RESOLVED is inside `emagent-tools--root-boundary'.

Compares symlink-resolved truenames so a symlink inside the root that points
outside it cannot pass the check.  `file-truename' resolves the existing prefix
of a not-yet-created path, so a symlinked parent directory is caught too."
  (or (null emagent-tools--root-boundary)
      (let ((root (file-name-as-directory
                   (file-truename (expand-file-name emagent-tools--root-boundary))))
            (true (file-truename resolved)))
        (or (string-prefix-p root (file-name-as-directory true))
            (string= (directory-file-name true)
                     (directory-file-name root))))))

(defun emagent-tools--root-directory (path)
  "Return PATH resolved against the active emagent session project directory.

A relative PATH is resolved against the session project directory (not the
process `default-directory'), and an omitted PATH yields that directory.
Signal an error when the result escapes `emagent-tools--root-boundary' or lands
in a protected macOS tree (iCloud or another app's container)."
  (let* ((base (or emagent-tools--project-directory default-directory))
         (resolved (expand-file-name (or path base) base)))
    (unless (emagent-tools--within-boundary-p resolved)
      (user-error "Path %s is outside the session root %s"
                  resolved emagent-tools--root-boundary))
    (when (emagent-tools--protected-truename-p (file-truename resolved))
      (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                  resolved))
    resolved))

(defun emagent-tools--eval-form-read (form-str)
  "Return FORM-STR parsed as `(progn ,@forms)'."
  (read (concat "(progn " (string-trim (or form-str "")) ")")))

(defun emagent-tools--eval-form-guard (form-str)
  "Return nil when FORM-STR passes eval guardrails, else an error string."
  (emagent-policy-enforce-string (emagent-policy-check-elisp form-str) form-str))

(defun emagent-tools--eval-form-safely (form-str)
  "Evaluate FORM-STR with syntax and symbol guardrails; return a result string."
  (let ((check-result (emagent-elisp-check-form form-str)))
    (unless (string= "OK" check-result)
      (user-error "%s" check-result))
    (when-let ((err (emagent-tools--eval-form-guard form-str)))
      (user-error "%s" err))
    (condition-case err
        (let ((result (eval (emagent-tools--eval-form-read form-str))))
          (if (null result) "nil" (prin1-to-string result)))
      (error (format "Eval error: %s" (error-message-string err))))))

(defcustom emagent-tools-subprocess-timeout 60
  "Default seconds before killing an agent subprocess.
Agent tools may override this per call up to
`emagent-tools-subprocess-timeout-max'."
  :type 'integer
  :group 'emagent-tools)

(defcustom emagent-tools-subprocess-timeout-max 300
  "Maximum seconds an agent may request as a per-call subprocess timeout."
  :type 'integer
  :group 'emagent-tools)

(defcustom emagent-tools-display-compile-buffer nil
  "When non-nil, display the `*emagent-compile*' buffer when a build starts.
When nil (the default) the buffer fills in the background without
touching the window layout; switch to it any time for navigable errors
\\(\\[next-error])."
  :type 'boolean
  :group 'emagent-tools)

(defvar emagent-tools--timeout-override nil
  "When non-nil, the per-call subprocess timeout requested by the agent.
Bound dynamically around a tool call and read synchronously when a runner
starts, so it is captured before any process wait.")

(defconst emagent-tools--shell-output-limit 100000
  "Max characters returned from shell/process tool output.")

(defun emagent-tools--clamp-timeout (secs)
  "Clamp SECS to [1, `emagent-tools-subprocess-timeout-max']."
  (max 1 (min secs emagent-tools-subprocess-timeout-max)))

(defun emagent-tools--subprocess-timeout ()
  "Return the effective agent subprocess timeout in seconds.
Honors `emagent-tools--timeout-override' when set, clamped to the max."
  (emagent-tools--clamp-timeout
   (or emagent-tools--timeout-override emagent-tools-subprocess-timeout)))

(defun emagent-tools--timeout-message (secs &optional shell)
  "Return a timeout error string for limit SECS.
When SHELL is non-nil, also suggest background execution."
  (concat
   (format
    "Timed out after %ds. Retry with a larger `timeout` argument (up to %ds)."
    secs emagent-tools-subprocess-timeout-max)
   (when shell
     (concat
      " For genuinely long-running work, use background execution"
      " (append ' > /tmp/out.txt 2>&1 & echo \"PID: $!\"') and read the"
      " output file later with read_file."))))

(defun emagent-tools--run-async-sync (async-fn &rest args)
  "Run ASYNC-FN with ARGS and a result callback; block until it finishes.
For tests and internal callers only — MCP agent tools use the async path."
  (let (result is-error done)
    (apply async-fn
           (lambda (r e)
             (setq result r is-error e done t))
           args)
    (while (not done)
      (accept-process-output nil 0.05))
    (if is-error
        (error "%s" result)
      result)))

(defun emagent-tools--run-process-async (callback program &rest args)
  "Run PROGRAM with ARGS; call CALLBACK with (output is-error) from a sentinel."
  (let* ((buf (generate-new-buffer " *emagent-proc*"))
         (timeout-secs (emagent-tools--subprocess-timeout))
         (done nil)
         (timer nil)
         (proc nil)
         (finish
          (lambda (output is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (when (and proc (process-live-p proc))
                (delete-process proc))
              (when (buffer-live-p buf)
                (kill-buffer buf))
              (funcall callback output is-error)))))
    (condition-case start-err
        (progn
          (setq proc (apply #'start-process "emagent-proc" buf program args))
          (setq timer
                (run-with-timer
                 timeout-secs nil
                 (lambda ()
                   (when (and proc (process-live-p proc))
                     (delete-process proc))
                   (funcall finish
                            (emagent-tools--timeout-message timeout-secs)
                            t))))
          (set-process-sentinel
           proc
           (lambda (p _event)
             (when (memq (process-status p) '(signal exit))
               (let* ((output (with-current-buffer buf (buffer-string)))
                      (status (process-exit-status p))
                      (is-error (or (eq status 'signal)
                                    (and (numberp status)
                                         (not (zerop status))))))
                 (funcall finish output is-error))))))
      (error (funcall finish (error-message-string start-err) t)))))

(defun emagent-tools--run-process-input-async (callback input program &rest args)
  "Pipe INPUT to PROGRAM with ARGS; call CALLBACK with (output is-error)."
  (let* ((buf (generate-new-buffer " *emagent-proc*"))
         (timeout-secs (emagent-tools--subprocess-timeout))
         (done nil)
         (timer nil)
         (proc nil)
         (finish
          (lambda (output is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (when (and proc (process-live-p proc))
                (delete-process proc))
              (when (buffer-live-p buf)
                (kill-buffer buf))
              (funcall callback output is-error)))))
    (condition-case start-err
        (progn
          (setq proc (apply #'make-process
                            `(:name "emagent-proc"
                              :buffer ,buf
                              :command (,program . ,args)
                              :connection-type pipe
                              :noquery t
                              :sentinel
                              ,(lambda (p _event)
                                 (when (memq (process-status p)
                                             '(signal exit))
                                   (let* ((output
                                           (with-current-buffer buf
                                             (buffer-string)))
                                          (status (process-exit-status p))
                                          (is-error
                                           (or (eq status 'signal)
                                               (and (numberp status)
                                                    (not (zerop status))))))
                                     (funcall finish output is-error)))))))
          (process-send-string proc input)
          (process-send-eof proc)
          (setq timer
                (run-with-timer
                 timeout-secs nil
                 (lambda ()
                   (when (and proc (process-live-p proc))
                     (delete-process proc))
                   (funcall finish
                            (emagent-tools--timeout-message timeout-secs)
                            t)))))
      (error (funcall finish (error-message-string start-err) t)))))

(defun emagent-tools--run-shell-async (callback command directory)
  "Run shell COMMAND in DIRECTORY; call CALLBACK with (output is-error)."
  (let* ((default-directory (emagent-tools--root-directory directory))
         (buf (generate-new-buffer " *emagent-shell*"))
         (timeout-secs (emagent-tools--subprocess-timeout))
         (limit emagent-tools--shell-output-limit)
         (done nil)
         (timer nil)
         (proc nil)
         (finish
          (lambda (output is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (when (and proc (process-live-p proc))
                (delete-process proc))
              (when (buffer-live-p buf)
                (kill-buffer buf))
              (funcall callback output is-error)))))
    (condition-case start-err
        (progn
          (setq proc (start-process-shell-command "emagent-shell" buf command))
          (setq timer
                (run-with-timer
                 timeout-secs nil
                 (lambda ()
                   (when (and proc (process-live-p proc))
                     (delete-process proc))
                   (funcall finish
                            (emagent-tools--timeout-message timeout-secs t)
                            t))))
          (set-process-sentinel
           proc
           (lambda (p _event)
             (when (memq (process-status p) '(signal exit))
               (let* ((output (with-current-buffer buf (buffer-string)))
                      (status (process-exit-status p))
                      (is-error (or (eq status 'signal)
                                    (and (numberp status)
                                         (not (zerop status))))))
                 (when (and (not is-error) (> (length output) limit))
                   (setq output (concat (substring output 0 limit)
                                        "\n… (output truncated)")))
                 (funcall finish output is-error))))))
      (error (funcall finish (error-message-string start-err) t)))))

(defun emagent-tools--run-process-to-string (program &rest args)
  "Run PROGRAM with ARGS and return stdout (sync wrapper for the test suite)."
  (emagent-tools--run-async-sync
   (lambda (callback)
     (apply #'emagent-tools--run-process-async callback program args))))

(defconst emagent-tools--grep-max-results 50
  "Maximum matching lines returned by grep tools.")

(defun emagent-tools--grep-emacs (regexp root max)
  "Search REGEXP under ROOT in Emacs, returning at most MAX lines."
  (let ((lines nil)
        (matches 0))
    (dolist (file (directory-files-recursively root "[^.].*" nil t))
      (when (< matches max)
        (unless (string-match-p "/\\.git/" file)
          (with-temp-buffer
            (condition-case nil
                (progn
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (while (and (< matches max)
                              (re-search-forward regexp nil t))
                    (push (format "%s:%s:%s"
                                  (file-relative-name file root)
                                  (line-number-at-pos)
                                  (string-trim
                                   (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
                          lines)
                    (setq matches (1+ matches))))
              (file-missing nil))))))
    (if lines
        (string-join (nreverse lines) "\n")
      "No matches")))

(defun emagent-tool-grep-async (callback pattern &optional path)
  "Search for PATTERN under PATH; call CALLBACK with (output is-error)."
  (let* ((root (emagent-tools--root-directory path))
         (regexp (if (stringp pattern) pattern (format "%s" pattern))))
    (if (and (boundp 'emagent-acp-prefer-emacs) emagent-acp-prefer-emacs)
        (funcall callback
                 (emagent-tools--grep-emacs
                  regexp root emagent-tools--grep-max-results)
                 nil)
      (if (executable-find "rg")
          (let ((default-directory root))
            (emagent-tools--run-process-async
             (lambda (output is-error)
               (funcall callback output is-error))
             "rg" "--no-heading" "--line-number"
             "--max-count" (number-to-string emagent-tools--grep-max-results)
             "--hidden" "--glob" "!/.git/*"
             regexp "."))
        (funcall callback
                 (emagent-tools--grep-emacs
                  regexp root emagent-tools--grep-max-results)
                 nil)))))

(defun emagent-tool-grep (pattern &optional path)
  "Search for PATTERN under PATH and return matching lines as a string.
Uses pure Emacs search when `emagent-acp-prefer-emacs' is non-nil."
  (emagent-tools--run-async-sync #'emagent-tool-grep-async pattern path))

(defconst emagent-tools--list-files-ignored-dirs
  '(".git" ".build" ".venv" ".cache" ".elpaca" "node_modules" "__pycache__"
    "dist" "target" "out")
  "Directory names `emagent-tool-list-files' skips outside git repos.")

(defun emagent-tools--list-files-walk (root)
  "List files under ROOT recursively, skipping well-known artifact dirs."
  (string-join
   (mapcar (lambda (file) (file-relative-name file root))
           (directory-files-recursively
            root "[^.].*" nil
            (lambda (dir)
              (not (member (file-name-nondirectory (directory-file-name dir))
                           emagent-tools--list-files-ignored-dirs)))))
   "\n"))

(defun emagent-tool-list-files (&optional path)
  "List project files under PATH relative to PATH, one per line.

Inside a git repository this is what git considers the project:
tracked plus untracked-but-not-ignored files (`git ls-files'), so
build artifacts and other gitignored trees don't flood the result.
Elsewhere it walks the tree, skipping
`emagent-tools--list-files-ignored-dirs'."
  (let* ((root (emagent-tools--root-directory path))
         (default-directory root))
    (or (when (and (executable-find "git")
                   (locate-dominating-file root ".git"))
          (with-temp-buffer
            (when (zerop (call-process "git" nil t nil "ls-files"
                                       "--cached" "--others"
                                       "--exclude-standard"))
              (string-trim-right (buffer-string)))))
        (emagent-tools--list-files-walk root))))

(defun emagent-tools--glob-to-regexp (glob)
  "Convert a simple shell GLOB to a regexp."
  (let ((parts nil)
        (i 0)
        (len (length glob)))
    (while (< i len)
      (cond
       ((and (< (1+ i) len)
             (eq (aref glob i) ?*)
             (eq (aref glob (1+ i)) ?*))
        (push ".*" parts)
        (setq i (+ i 2)))
       ((eq (aref glob i) ?*)
        (push "[^/]*" parts)
        (setq i (1+ i)))
       ((eq (aref glob i) ??)
        (push "." parts)
        (setq i (1+ i)))
       (t
        (let ((start i))
          (while (and (< i len)
                      (not (memq (aref glob i) '(?* ??))))
            (setq i (1+ i)))
          (push (regexp-quote (substring glob start i)) parts)))))
    (concat (file-name-as-directory "") (string-join (nreverse parts) ""))))

(defun emagent-tool-find-files (glob &optional path)
  "List files under PATH matching shell GLOB, one relative path per line.

A GLOB with no `/' matches against each file's name; a GLOB with `/' matches
against the file's path relative to the search root.  The glob regexp is
`./'-prefixed, so candidates are compared as `./NAME' / `./REL-PATH'."
  (let* ((root (emagent-tools--root-directory path))
         (has-slash (string-match-p "/" glob))
         (regexp (concat "\\`" (emagent-tools--glob-to-regexp glob) "\\'"))
         (files nil))
    (dolist (file (directory-files-recursively root "[^.].*" nil t))
      (unless (string-match-p "/\\.git/" file)
        (let* ((rel (file-relative-name file root))
               (candidate (concat "./" (if has-slash rel
                                         (file-name-nondirectory rel)))))
          (when (string-match-p regexp candidate)
            (push rel files)))))
    (if files
        (string-join (sort files #'string<) "\n")
      "No matches")))

(cl-defun emagent-tools--run-git-async (callback &rest args)
  "Run git ARGS asynchronously; call CALLBACK with (output is-error).
Passes `--no-pager' so pager-using subcommands (log, diff, show) never
launch a pager that blocks on input when stdout is a pipe (which then
hangs the tool)."
  (unless (executable-find "git")
    (funcall callback "git not found on PATH" t)
    (cl-return-from emagent-tools--run-git-async))
  (let ((default-directory (emagent-tools--root-directory nil)))
    (apply #'emagent-tools--run-process-async
           callback "git" "--no-pager" args)))

(defun emagent-tools--run-git (&rest args)
  "Run git ARGS in the session project directory and return stdout."
  (emagent-tools--run-async-sync
   (lambda (callback)
     (apply #'emagent-tools--run-git-async callback args))))

(defun emagent-tool-git-status-async (callback)
  "Return git status asynchronously.

Arguments: CALLBACK."
  (emagent-tools--run-git-async
   (lambda (output is-error)
     (funcall callback (string-trim output) is-error))
   "status" "--short" "--branch"))

(defun emagent-tool-git-status ()
  "Return git status for the session project directory."
  (emagent-tools--run-async-sync #'emagent-tool-git-status-async))

(defun emagent-tool-git-diff-async (callback &optional args)
  "Return git diff output asynchronously.

Arguments: CALLBACK, ARGS."
  (if (and args (not (string-empty-p args)))
      (apply #'emagent-tools--run-git-async
             (lambda (output is-error)
               (funcall callback (string-trim output) is-error))
             "diff" (split-string-shell-command args))
    (emagent-tools--run-git-async
     (lambda (output is-error)
       (funcall callback (string-trim output) is-error))
     "diff")))

(defun emagent-tool-git-diff (&optional args)
  "Return git diff output.  Optional ARGS is extra git diff arguments."
  (emagent-tools--run-async-sync #'emagent-tool-git-diff-async args))

(defun emagent-tool-git-log-async (callback &optional args)
  "Return git log output asynchronously.

Arguments: CALLBACK, ARGS."
  (if (and args (not (string-empty-p args)))
      (apply #'emagent-tools--run-git-async
             (lambda (output is-error)
               (funcall callback (string-trim output) is-error))
             "log" (split-string-shell-command args))
    (emagent-tools--run-git-async
     (lambda (output is-error)
       (funcall callback (string-trim output) is-error))
     "log" "--oneline" "-n" "20")))

(defun emagent-tool-git-log (&optional args)
  "Return git log output.  Optional ARGS is extra git log arguments."
  (emagent-tools--run-async-sync #'emagent-tool-git-log-async args))

(defconst emagent-tools--fetch-url-limit 100000
  "Maximum response body size returned by `emagent-tool-fetch-url'.")

(defconst emagent-tools--fetch-url-timeout 30
  "Seconds to wait for `url-retrieve-synchronously' in fetch-url.")

(defun emagent-tool-undo-file (path &optional steps)
  "Save PATH after undoing edits.
STEPS is the undo depth.  Use to revert `emagent-tool-write-file' changes."
  (let* ((resolved (emagent-tools--root-directory path))
      (steps (max 1 (or steps 1)))
      (buffer (emagent-tools--file-buffer path))
      (done 0))
    (unless buffer
      (user-error "No buffer for %s" resolved))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (catch 'exhausted
          (dotimes (i steps)
            ;; `undo' only continues the previous undo chain when
            ;; `last-command' is `undo'; inside this loop it is not, so bind it
            ;; for every step after the first — otherwise repeated calls
            ;; oscillate (undo then redo) instead of undoing further.
            (condition-case _
              (let ((last-command (if (> i 0) 'undo last-command)))
                (undo)
                (setq done (1+ done)))
              (user-error (throw 'exhausted nil)))))
        (when (buffer-file-name)
          (basic-save-buffer))))
    (format "Undid %d change(s) in %s" done resolved)))

(defun emagent-tool-delete-file (path)
  "Delete PATH after user confirmation."
  (let ((resolved (emagent-tools--root-directory path)))
    (delete-file resolved t)
    (format "Deleted %s" resolved)))

(defun emagent-tool-delete-directory (path &optional recursive)
  "Delete directory PATH after user confirmation.
When RECURSIVE is non-nil, delete contents as well."
  (let ((resolved (emagent-tools--root-directory path)))
    (delete-directory resolved recursive)
    (format "Deleted %s" resolved)))

(defun emagent-tool-fetch-url-async (callback url &optional max-bytes)
  "Fetch URL asynchronously; call CALLBACK with (body is-error).

Arguments: MAX-BYTES."
  (if (not (and (stringp url) (string-match-p "\\`https?://" url)))
    (funcall callback "fetch_url requires an http:// or https:// URL" t)
    (require 'url)
    (let* ((limit (or max-bytes emagent-tools--fetch-url-limit))
        (timeout-secs (if emagent-tools--timeout-override
            (emagent-tools--clamp-timeout
              emagent-tools--timeout-override)
            emagent-tools--fetch-url-timeout))
        (done nil)
        (timer nil)
        (finish
          (lambda (body is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (funcall callback body is-error)))))
      (url-retrieve
        url
        (lambda (_status)
          (let ((buf (current-buffer)))
            (unwind-protect
              (condition-case err
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward "\n\n" nil t)
                    (let ((body (buffer-substring-no-properties (point) (point-max))))
                      (funcall finish
                        (if (> (length body) limit)
                          (concat (substring body 0 limit)
                            "\n… (output truncated)")
                          body)
                        nil))
                    (funcall finish (format "No HTTP body in response from %s" url) t)))
                (error (funcall finish (error-message-string err) t)))
              (when (buffer-live-p buf)
                (kill-buffer buf)))))
        nil t)
      (setq timer
        (run-with-timer
          timeout-secs nil
          (lambda ()
            (funcall finish
              (emagent-tools--timeout-message timeout-secs)
              t)))))))

(defun emagent-tool-fetch-url (url &optional max-bytes)
  "Fetch URL over HTTP/HTTPS and return the response body as a string.
Runs in Emacs (not the agent sandbox), so network access works when the
agent's built-in WebSearch and shell tools are blocked.

Arguments: MAX-BYTES."
  (emagent-tools--run-async-sync #'emagent-tool-fetch-url-async url max-bytes))

(defun emagent-tool-run-shell-command (command &optional directory)
  "Run COMMAND in DIRECTORY through Emacs, not an agent terminal."
  (require 'emagent-shell)
  (when (fboundp 'emagent-shell-run-command)
    (emagent-shell-run-command command directory)))

(defun emagent-tool-run-shell-command-async (command directory callback)
  "Like `emagent-tool-run-shell-command' for COMMAND via CALLBACK.
CALLBACK receives \(OUTPUT IS-ERROR); for long-running commands Emacs
stays responsive because no polling loop is used.

Arguments: DIRECTORY."
  (require 'emagent-shell)
  (when (fboundp 'emagent-shell-run-command-async)
    (emagent-shell-run-command-async command directory callback)))

(defun emagent-tool-org-move-subtree-to-parent ()
  "Move org subtree at point to its parent section after confirmation."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (org-cut-subtree)
  (org-up-element)
  (org-paste-subtree)
  "Moved subtree to parent section")

(defun emagent-tool-compile-async (callback command &optional directory)
  "Run COMMAND via `compilation-mode'; call CALLBACK with output.
Arguments: DIRECTORY."
  (require 'compile)
  (require 'ansi-color)
  (let* ((default-directory (expand-file-name
          (or directory
            emagent-tools--project-directory
            default-directory)))
      (timeout-secs (emagent-tools--subprocess-timeout))
      (limit emagent-tools--shell-output-limit)
      (done nil)
      (timer nil)
      (proc nil)
      (buf nil)
      (finish
        (lambda (text is-error)
          (unless done
            (setq done t)
            (when timer (cancel-timer timer))
            (when (and proc (process-live-p proc))
              (delete-process proc))
            (funcall callback text is-error)))))
    (condition-case err
      (progn
        (setq buf (let ((display-buffer-overriding-action
                (unless emagent-tools-display-compile-buffer
                  (list #'display-buffer-no-window
                    '(allow-no-window . t)))))
            (compilation-start command 'compilation-mode
              (lambda (_) "*emagent-compile*"))))
        (setq proc (get-buffer-process buf))
        (with-current-buffer buf
          (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil t))
        (when proc
          (setq timer
            (run-with-timer
              timeout-secs nil
              (lambda ()
                (when (process-live-p proc)
                  (delete-process proc))
                (funcall finish
                  (emagent-tools--timeout-message timeout-secs t)
                  t))))
          (set-process-sentinel
            proc
            (lambda (_p _event)
              (with-current-buffer buf
                (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                  (if (> (length text) limit)
                    (setq text (concat (substring text 0 limit)
                        "\n… (output truncated)")))
                  (funcall finish text nil))))))
        (unless proc
          (with-current-buffer buf
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (funcall finish text nil)))))
      (error (funcall finish (error-message-string err) t)))))

(defun emagent-tool-compile (command &optional directory)
  "Run COMMAND via `compilation-mode' and return its output as text.

Unlike `run_shell_command', errors appear in a persistent
`*emagent-compile*' buffer navigable with `next-error' / \\[next-error].
The buffer fills in the background; set
`emagent-tools-display-compile-buffer' to show it when a build starts.

Arguments: DIRECTORY."
  (emagent-tools--run-async-sync #'emagent-tool-compile-async command directory))

(defun emagent-tool-buffer-list ()
  "Return paths of open Emacs buffers inside the session project, one per line.
Modified buffers are marked with (modified).  Only files within the session
root (`emagent-tools--project-directory') are included."
  (let ((root (and emagent-tools--project-directory
          (file-name-as-directory
            (expand-file-name emagent-tools--project-directory)))))
    (string-join
      (delq nil
        (mapcar (lambda (buf)
            (when-let ((file (buffer-file-name buf)))
              (let ((expanded (expand-file-name file)))
                (when (or (null root)
                    (string-prefix-p root expanded))
                  (format "%s%s"
                    (if root
                      (file-relative-name expanded root)
                      (abbreviate-file-name expanded))
                    (if (buffer-modified-p buf)
                      " (modified)"
                      ""))))))
          (buffer-list)))
      "\n")))

(defun emagent-tools--imenu-subalist-p (item)
  "Return non-nil when ITEM is an imenu nested alist entry."
  (and (consp (cdr item))
    (listp (cadr item))
    (not (numberp (cadr item)))))

(defun emagent-tool-imenu-index (&optional file)
  "Return a structural outline (functions, classes, sections) for FILE.
When FILE is omitted, uses the current buffer.  Works for any language
that has imenu support configured (Java, Python, Elisp, JS, org, etc.)."
  (require 'imenu)
  (let* ((resolved (when file (emagent-tools--root-directory file)))
      (buf (if resolved
          (or (find-buffer-visiting resolved)
            (find-file-noselect resolved))
          (current-buffer))))
    (with-current-buffer buf
      (let* ((index (condition-case nil
              (when (functionp imenu-create-index-function)
                (save-excursion
                  (funcall imenu-create-index-function)))
              (error nil)))
          (lines nil))
        (cl-labels ((flatten (alist prefix)
              (dolist (entry alist)
                (if (emagent-tools--imenu-subalist-p entry)
                  (flatten (cdr entry)
                    (concat prefix (car entry) "/"))
                  (push (concat prefix (car entry)) lines)))))
          (when index (flatten index "")))
        (if lines
          (string-join (nreverse lines) "\n")
          "No imenu index available for this buffer")))))

(provide 'emagent-tools-shell)
;;; emagent-tools-shell.el ends here
