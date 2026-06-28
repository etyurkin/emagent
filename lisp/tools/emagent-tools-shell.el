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
  "Run PROGRAM with ARGS and return stdout, yielding to the Emacs event loop."
  (let ((buf (generate-new-buffer " *emagent-proc*"))
        done)
    (unwind-protect
        (progn
          (let ((proc (apply #'start-process "emagent-proc" buf program args)))
            (set-process-sentinel proc (lambda (_p _e) (setq done t))))
          (while (not done)
            (accept-process-output nil 0.05))
          (with-current-buffer buf
            (buffer-string)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

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

(defun emagent-tool-fetch-url (url &optional max-bytes)
  "Fetch URL over HTTP/HTTPS and return the response body as a string.
Runs in Emacs (not the agent sandbox), so network access works when the
agent's built-in WebSearch and shell tools are blocked."
  (unless (and (stringp url) (string-match-p "\\`https?://" url))
    (user-error "fetch_url requires an http:// or https:// URL"))
  (require 'url)
  (let* ((limit (or max-bytes emagent-tools--fetch-url-limit))
         (result-buf nil)
         (done nil))
    (url-retrieve url
                  (lambda (_status)
                    (setq result-buf (current-buffer)
                          done t))
                  nil t)
    (let ((deadline (+ (float-time) emagent-tools--fetch-url-timeout)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.1)))
    (unless done
      (user-error "Failed to fetch %s (timeout)" url))
    (unwind-protect
        (with-current-buffer result-buf
          (goto-char (point-min))
          (unless (re-search-forward "\n\n" nil t)
            (user-error "No HTTP body in response from %s" url))
          (let ((body (buffer-substring-no-properties (point) (point-max))))
            (if (> (length body) limit)
                (concat (substring body 0 limit) "\n… (output truncated)")
              body)))
      (when (and result-buf (buffer-live-p result-buf))
        (kill-buffer result-buf)))))

(declare-function emagent-shell-run-command "emagent-shell")

(defun emagent-tool-run-shell-command (command &optional directory)
  "Run COMMAND in DIRECTORY through Emacs, not an agent terminal."
  (require 'emagent-shell)
  (emagent-shell-run-command command directory))

(defun emagent-tool-grep (pattern &optional path)
  "Search for PATTERN under PATH and return matching lines as a string.
Uses pure Emacs search when `emagent-acp-prefer-emacs' is non-nil."
  (let* ((root (emagent-tools--root-directory path))
         (regexp (if (stringp pattern) pattern (format "%s" pattern))))
    (if (and (boundp 'emagent-acp-prefer-emacs) emagent-acp-prefer-emacs)
        (emagent-tools--grep-emacs regexp root emagent-tools--grep-max-results)
      (if (executable-find "rg")
          (let ((default-directory root))
            (emagent-tools--run-process-to-string
             "rg" "--no-heading" "--line-number"
             "--max-count" (number-to-string emagent-tools--grep-max-results)
             "--hidden" "--glob" "!/.git/*"
             regexp "."))
        (emagent-tools--grep-emacs regexp root emagent-tools--grep-max-results)))))

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

(defun emagent-tools--run-git (&rest args)
  "Run git ARGS in the session project directory and return stdout."
  (unless (executable-find "git")
    (user-error "git not found on PATH"))
  (let ((default-directory (emagent-tools--root-directory nil)))
    (apply #'emagent-tools--run-process-to-string "git" args)))

(defun emagent-tool-git-status ()
  "Return git status for the session project directory."
  (string-trim (emagent-tools--run-git "status" "--short" "--branch")))

(defun emagent-tool-git-diff (&optional args)
  "Return git diff output.  Optional ARGS is extra git diff arguments."
  (string-trim
   (if (and args (not (string-empty-p args)))
       (apply #'emagent-tools--run-git "diff" (split-string args "[[:space:]]+" t))
     (emagent-tools--run-git "diff"))))

(defun emagent-tool-git-log (&optional args)
  "Return git log output.  Optional ARGS is extra git log arguments."
  (string-trim
   (if (and args (not (string-empty-p args)))
       (apply #'emagent-tools--run-git "log" (split-string args "[[:space:]]+" t))
     (emagent-tools--run-git "log" "--oneline" "-n" "20"))))

(defun emagent-tool-org-move-subtree-to-parent ()
  "Move org subtree at point to its parent section after confirmation."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (org-cut-subtree)
  (org-up-element)
  (org-paste-subtree)
  "Moved subtree to parent section")

(defun emagent-tool-compile (command &optional directory)
  "Run COMMAND via `compilation-mode' and return its output as text.

Unlike `run_shell_command', errors appear in a persistent
`*emagent-compile*' buffer navigable with `next-error' / \\[next-error].
The buffer is shown to the user while the build runs."
  (require 'compile)
  (require 'ansi-color)
  (let* ((default-directory (expand-file-name
                             (or directory
                                 emagent-tools--project-directory
                                 default-directory)))
         (buf (compilation-start command 'compilation-mode
                                  (lambda (_) "*emagent-compile*")))
         (proc (get-buffer-process buf)))
    (with-current-buffer buf
      (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil t))
    (when proc
      (while (process-live-p proc)
        (accept-process-output proc 0.05)))
    (with-current-buffer buf
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (if (> (length text) emagent-tools--shell-output-limit)
            (concat (substring text 0 emagent-tools--shell-output-limit)
                    "\n… (output truncated)")
          text)))))

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
