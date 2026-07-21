;;; emagent-tools-search.el --- Grep and file search tools  -*- lexical-binding: t; -*-

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

;; Grep, list-files, find-files, and glob helpers for agent tools.

;;; Code:

(require 'emagent-tools-core)
(require 'emagent-tools-process)

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

(provide 'emagent-tools-search)
;;; emagent-tools-search.el ends here
