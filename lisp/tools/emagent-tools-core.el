;;; emagent-tools-core.el --- Core tool path/eval helpers  -*- lexical-binding: t; -*-

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

;; Session-root path resolution and eval guardrails, split out of
;; `emagent-tools' so leaf modules can use them without requiring the
;; facade back.

;;; Code:

(require 'subr-x)
(require 'emagent-elisp)
(require 'emagent-policy)
(require 'emagent-tools-file)

;; Bound by the MCP dispatcher and by `emagent-tools-set-project-directory'
;; (both in `emagent-tools', which requires this file); forward-declared
;; here rather than required back to avoid a cycle.
(defvar emagent-tools--project-directory)
(defvar emagent-tools--root-boundary)

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

(provide 'emagent-tools-core)
;;; emagent-tools-core.el ends here
