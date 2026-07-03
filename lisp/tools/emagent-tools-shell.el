;;; emagent-tools-shell.el --- Shell and grep tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'emagent-log)
(require 'org)

(declare-function emagent-tools--root-directory "emagent-tools")
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--subalist-p "imenu")

(defconst emagent-tools--grep-max-results 50)

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

(defvar emagent-tools--timeout-override nil
  "When non-nil, the per-call subprocess timeout requested by the agent.
Bound dynamically around a tool call and read synchronously when a runner
starts, so it is captured before any process wait.")

(defun emagent-tools--clamp-timeout (secs)
  "Clamp SECS to [1, `emagent-tools-subprocess-timeout-max']."
  (max 1 (min secs emagent-tools-subprocess-timeout-max)))

(defun emagent-tools--subprocess-timeout ()
  "Return the effective agent subprocess timeout in seconds.
Honors `emagent-tools--timeout-override' when set, clamped to the max."
  (emagent-tools--clamp-timeout
   (or emagent-tools--timeout-override emagent-tools-subprocess-timeout)))

(defun emagent-tools--timeout-message (secs &optional shell)
  "Return a timeout error string for a SECS-second limit.
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
             (when (memq (process-status p) '(signal exited))
               (let* ((output (with-current-buffer buf (buffer-string)))
                      (status (process-exit-status p))
                      (is-error (or (eq status 'signal)
                                    (and (numberp status) (not (zerop status))))))
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
                                 (when (memq (process-status p) '(signal exited))
                                   (let* ((output (with-current-buffer buf (buffer-string)))
                                          (status (process-exit-status p))
                                          (is-error (or (eq status 'signal)
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
             (when (memq (process-status p) '(signal exited))
               (let* ((output (with-current-buffer buf (buffer-string)))
                      (status (process-exit-status p))
                      (is-error (or (eq status 'signal)
                                    (and (numberp status) (not (zerop status))))))
                 (when (and (not is-error) (> (length output) limit))
                   (setq output (concat (substring output 0 limit)
                                        "\n… (output truncated)")))
                 (funcall finish output is-error))))))
      (error (funcall finish (error-message-string start-err) t)))))

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
                                  (string-trim (buffer-substring-no-properties
                                                (line-beginning-position)
                                                (line-end-position))))
                          lines)
                    (setq matches (1+ matches))))
              (file-missing nil))))))
    (if lines
        (string-join (nreverse lines) "\n")
      "No matches")))

(defun emagent-tools--run-process-to-string (program &rest args)
  "Run PROGRAM with ARGS and return stdout (sync wrapper for tests)."
  (emagent-tools--run-async-sync
   (lambda (callback)
     (apply #'emagent-tools--run-process-async callback program args))))

(defconst emagent-tools--shell-output-limit 100000)

(defconst emagent-tools--fetch-url-limit 100000
  "Maximum response body size returned by `emagent-tool-fetch-url'.")

(defconst emagent-tools--fetch-url-timeout 30
  "Seconds to wait for `url-retrieve-synchronously' in `emagent-tool-fetch-url'.")

(defun emagent-tool-undo-file (path &optional steps)
  "Undo STEPS edits in PATH and save.
Use to revert `emagent-tool-write-file' changes."
  (let* ((resolved (emagent-tools--root-directory path))
         (steps (max 1 (or steps 1)))
         (buffer (emagent-tools--file-buffer path))
         (done 0))
    (unless buffer
      (user-error "No buffer for %s" resolved))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (dotimes (_ steps)
          (condition-case _
              (progn (undo) (setq done (1+ done)))
            (user-error
             (user-error "Only %d undo step(s) available in %s" done resolved))))
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
  "Fetch URL asynchronously; call CALLBACK with (body is-error)."
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
agent's built-in WebSearch and shell tools are blocked."
  (emagent-tools--run-async-sync #'emagent-tool-fetch-url-async url max-bytes))

(declare-function emagent-shell-run-command "emagent-shell")
(declare-function emagent-shell-run-command-async "emagent-shell")

(defun emagent-tool-run-shell-command (command &optional directory)
  "Run COMMAND in DIRECTORY through Emacs, not an agent terminal."
  (require 'emagent-shell)
  (emagent-shell-run-command command directory))

(defun emagent-tool-run-shell-command-async (command directory callback)
  "Like `emagent-tool-run-shell-command' but call CALLBACK asynchronously.
CALLBACK receives (OUTPUT IS-ERROR); for long-running commands Emacs
stays responsive because no polling loop is used."
  (require 'emagent-shell)
  (emagent-shell-run-command-async command directory callback))

(defun emagent-tool-grep-async (callback pattern &optional path)
  "Search for PATTERN under PATH; call CALLBACK with (output is-error)."
  (let* ((root (emagent-tools--root-directory path))
         (regexp (if (stringp pattern) pattern (format "%s" pattern))))
    (if (and (boundp 'emagent-acp-prefer-emacs) emagent-acp-prefer-emacs)
        (funcall callback
                 (emagent-tools--grep-emacs regexp root emagent-tools--grep-max-results)
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
                 (emagent-tools--grep-emacs regexp root emagent-tools--grep-max-results)
                 nil)))))

(defun emagent-tool-grep (pattern &optional path)
  "Search for PATTERN under PATH and return matching lines as a string.
Uses pure Emacs search when `emagent-acp-prefer-emacs' is non-nil."
  (emagent-tools--run-async-sync #'emagent-tool-grep-async pattern path))

(defun emagent-tool-list-files (&optional path)
  "List files under PATH relative to PATH, one per line."
  (let ((root (emagent-tools--root-directory path)))
    (string-join
     (mapcar (lambda (file)
               (file-relative-name file root))
             (seq-filter
              (lambda (file)
                (not (string-match-p "/\\.git/" file)))
              (directory-files-recursively root "[^.].*" nil t)))
     "\n")))

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
  "List files under PATH matching shell GLOB, one relative path per line."
  (let* ((root (emagent-tools--root-directory path))
         (regexp (if (string-match-p "/" glob)
                     (emagent-tools--glob-to-regexp glob)
                   (concat ".*" (emagent-tools--glob-to-regexp glob))))
         (files nil))
    (dolist (file (directory-files-recursively root regexp nil t))
      (unless (string-match-p "/\\.git/" file)
        (push (file-relative-name file root) files)))
    (if files
        (string-join (sort files #'string<) "\n")
      "No matches")))

(defun emagent-tools--run-git-async (callback &rest args)
  "Run git ARGS asynchronously; call CALLBACK with (output is-error)."
  (unless (executable-find "git")
    (funcall callback "git not found on PATH" t)
    (cl-return-from emagent-tools--run-git-async))
  (let ((default-directory (emagent-tools--root-directory nil)))
    (apply #'emagent-tools--run-process-async callback "git" args)))

(defun emagent-tools--run-git (&rest args)
  "Run git ARGS in the session project directory and return stdout."
  (emagent-tools--run-async-sync
   (lambda (callback)
     (apply #'emagent-tools--run-git-async callback args))))

(defun emagent-tool-git-status-async (callback)
  "Return git status asynchronously."
  (emagent-tools--run-git-async
   (lambda (output is-error)
     (funcall callback (string-trim output) is-error))
   "status" "--short" "--branch"))

(defun emagent-tool-git-status ()
  "Return git status for the session project directory."
  (emagent-tools--run-async-sync #'emagent-tool-git-status-async))

(defun emagent-tool-git-diff-async (callback &optional args)
  "Return git diff output asynchronously."
  (if (and args (not (string-empty-p args)))
      (apply #'emagent-tools--run-git-async
             (lambda (output is-error)
               (funcall callback (string-trim output) is-error))
             "diff" (split-string args "[[:space:]]+" t))
    (emagent-tools--run-git-async
     (lambda (output is-error)
       (funcall callback (string-trim output) is-error))
     "diff")))

(defun emagent-tool-git-diff (&optional args)
  "Return git diff output.  Optional ARGS is extra git diff arguments."
  (emagent-tools--run-async-sync #'emagent-tool-git-diff-async args))

(defun emagent-tool-git-log-async (callback &optional args)
  "Return git log output asynchronously."
  (if (and args (not (string-empty-p args)))
      (apply #'emagent-tools--run-git-async
             (lambda (output is-error)
               (funcall callback (string-trim output) is-error))
             "log" (split-string args "[[:space:]]+" t))
    (emagent-tools--run-git-async
     (lambda (output is-error)
       (funcall callback (string-trim output) is-error))
     "log" "--oneline" "-n" "20")))

(defun emagent-tool-git-log (&optional args)
  "Return git log output.  Optional ARGS is extra git log arguments."
  (emagent-tools--run-async-sync #'emagent-tool-git-log-async args))

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
  "Run COMMAND via compilation-mode; call CALLBACK with (output is-error)."
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
          (setq buf (compilation-start command 'compilation-mode
                                       (lambda (_) "*emagent-compile*")))
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
The buffer is shown to the user while the build runs."
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
                        (imenu--make-index-alist t)
                      (error nil)))
             (lines nil))
        (cl-labels ((flatten (alist prefix)
                      (dolist (entry alist)
                        (if (imenu--subalist-p entry)
                            (flatten (cdr entry)
                                     (concat prefix (car entry) "/"))
                          (push (concat prefix (car entry)) lines)))))
          (when index (flatten index "")))
        (if lines
            (string-join (nreverse lines) "\n")
          "No imenu index available for this buffer")))))

(provide 'emagent-tools-shell)
;;; emagent-tools-shell.el ends here
