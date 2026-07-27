;;; emagent-shell-redirect.el --- Shell redirects for emagent -*- lexical-binding: t; -*-

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
;; Redirect simple shell commands to Emacs-native emagent tools.

;;; Code:

(require 'emagent-shell-guard)
(require 'emagent-tools-file)
(require 'emagent-tools-shell)

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

(provide 'emagent-shell-redirect)
;;; emagent-shell-redirect.el ends here
