;;; emagent-tools-git.el --- Git status/diff/log tools  -*- lexical-binding: t; -*-

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

;; Git status, diff, and log helpers for agent tools.

;;; Code:

(require 'cl-lib)
(require 'emagent-tools-core)
(require 'emagent-tools-process)

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

(provide 'emagent-tools-git)
;;; emagent-tools-git.el ends here
