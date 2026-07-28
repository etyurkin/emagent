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
;;
;; Shared tool helpers and Emacs Lisp validation utilities.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
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

(provide 'emagent-tools-core)
;;; emagent-tools-core.el ends here
