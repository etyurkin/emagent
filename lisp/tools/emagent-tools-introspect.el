;;; emagent-tools-introspect.el --- Elisp introspection UI tools  -*- lexical-binding: t; -*-

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

;; Emacs Lisp introspection and eval tool handlers.

;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)
(require 'emagent-elisp)
(require 'emagent-policy)
(require 'emagent-policy-match)
(require 'emagent-tools-core)
(require 'emagent-tools-confirm)

(defun emagent-tools--with-stdout (thunk)
  "Call THUNK after an introspection command and return `help-buffer' text.

THUNK should populate *Help* (e.g. via `describe-function').  Returns
whatever THUNK returns; call sites typically read `help-buffer'."
  (funcall thunk))

(defvar emagent-acp-elisp-guide nil "Forward declaration for the ACP Elisp guide string.")

(defun emagent-tools--symbols-in-form (form symbols)
  "Return symbols from SYMBOLS found anywhere in FORM."
  (emagent-policy-match--symbols-in-form form symbols))

(defun emagent-tools--eval-form-dangerous-allowed-p (form-str dangerous)
  "Return non-nil when evaluating FORM-STR is approved with DANGEROUS symbols.
When `emagent-tools--acp-session-p' is set, return t — ACP handles permission."
  (or emagent-tools--acp-session-p
      (let* ((ops (mapconcat #'symbol-name dangerous ", "))
             (preview (truncate-string-to-width form-str 400 nil nil "…"))
             (preamble (format "\n#+begin_src elisp\n%s\n#+end_src" preview))
             (prompt (format "Eval contains: *%s*" ops)))
        (if (and emagent-tools--chat-buffer
                 (buffer-live-p emagent-tools--chat-buffer))
            (let (result)
              (emagent-tools--buttons-prompt
               prompt '(("Allow" . yes) ("Deny" . no))
               emagent-tools--chat-buffer
               (lambda (v) (setq result v))
               preamble)
              (eq 'yes result))
          (y-or-n-p (format "Eval contains %s — allow? " ops))))))

(defun emagent-tools--eval-form-check (form-str)
  "Return nil when FORM-STR may run; otherwise a permission plist.
`:deny' blocks execution; `:confirm' needs user approval at the ACP gate."
  (emagent-policy-check-elisp form-str))

(defun emagent-tools--eval-form-execute (form-str)
  "Evaluate FORM-STR after guardrails; return nil on success or an error string."
  (condition-case err
      (progn (eval (emagent-tools--eval-form-read form-str)) nil)
    (error (error-message-string err))))

(defun emagent-tool-describe-symbol (symbol)
  "Return documentation for SYMBOL as a string."
  (let ((symbol (if (stringp symbol) (intern symbol) symbol)))
    (cond
     ((fboundp symbol)
      (emagent-tools--with-stdout
       (lambda ()
         (describe-function symbol)
         (with-current-buffer (help-buffer)
           (buffer-string)))))
     ((boundp symbol)
      (emagent-tools--with-stdout
       (lambda ()
         (describe-variable symbol)
         (with-current-buffer (help-buffer)
           (buffer-string)))))
     (t
      (format "No function or variable named %s" symbol)))))

(defun emagent-tool-where-is (command)
  "Return key bindings for COMMAND as a string."
  (let ((command (if (stringp command) (intern-soft command) command)))
    (if (commandp command)
        (emagent-tools--with-stdout
         (lambda ()
           (where-is command)
           (with-current-buffer (help-buffer)
             (buffer-string))))
      (format "Unknown command: %s" command))))

(defconst emagent-tools--apropos-max-results 100 "Max matches returned by apropos tools.")

(defun emagent-tool-apropos (pattern)
  "Return Emacs symbols matching PATTERN, one per line.
Searches symbol names.  Use to discover functions and variables before
calling them."
  (let* ((regexp (if (stringp pattern) pattern (format "%s" pattern)))
         (matches (apropos-internal regexp)))
    (if matches
        (string-join
         (mapcar #'symbol-name
                 (seq-take (sort matches #'string-lessp)
                           emagent-tools--apropos-max-results))
         "\n")
      "No matches")))

(defun emagent-tool-apropos-doc (pattern)
  "Return Emacs symbols whose docstring matches PATTERN, one per line.
Use when you know what a function does but not its name — e.g. apropos_doc
\"split string by delimiter\" to find `split-string'.
Slower than apropos (scans all docstrings) but finds symbols by meaning."
  (let* ((regexp (if (stringp pattern) pattern (format "%s" pattern)))
         (results nil)
         (limit emagent-tools--apropos-max-results))
    (mapatoms
     (lambda (sym)
       (when (< (length results) limit)
         (ignore-errors
           (let* ((fdoc (and (fboundp sym) (documentation sym t)))
                  (vdoc (and (boundp sym)
                             (documentation-property sym 'variable-documentation t)))
                  (doc (or fdoc vdoc)))
             (when (and doc (string-match-p regexp doc))
               (push (format "%s — %s"
                             sym
                             (truncate-string-to-width
                              (car (split-string doc "\n"))
                              80 nil nil "…"))
                     results)))))))
    (if results
        (string-join (nreverse results) "\n")
      "No matches")))

(defun emagent-tool-find-function (symbol)
  "Return the source location of SYMBOL as a string."
  (let ((symbol (if (stringp symbol) (intern-soft symbol) symbol)))
    (if (and symbol (fboundp symbol))
        (emagent-tools--with-stdout
         (lambda ()
           (find-function symbol)
           (with-current-buffer (help-buffer)
             (buffer-string))))
      (format "No function named %s" symbol))))

(defun emagent-tool-elisp-guide ()
  "Return the emagent Emacs Lisp reference guide.
Covers validation, structural editing, core patterns, string/list/buffer/file/
JSON/`org-mode' operations, error handling, common pitfalls, and code templates.
Call this before writing non-trivial Elisp."
  (require 'emagent-prompts)
  emagent-acp-elisp-guide)

(defun emagent-tool-check-elisp (form)
  "Check FORM for Emacs Lisp syntax errors without executing it.
Returns \"OK\" when the form parses cleanly, or an error description.
Always call this before eval for any form longer than 3 lines."
  (emagent-elisp-check-form (if (stringp form) form (prin1-to-string form))))

(defun emagent-tool-eval (form)
  "Evaluate Emacs Lisp FORM and return the result as a string.
Use this for small utilities and text processing — not Python or shell.
Blocked symbols must go through dedicated emagent-tool-* helpers.
For forms longer than 3 lines, call check_elisp first."
  (interactive)
  (emagent-tools--eval-form-safely
   (if (stringp form) form (prin1-to-string form))))

(defun emagent-tool-org-element ()
  "Return structured org element at point as a string."
  (if (derived-mode-p 'org-mode)
      (let* ((element (org-element-at-point))
             (type (org-element-type element))
             (props (cond
                     ((eq type 'headline)
                      `((type . headline)
                        (title . ,(org-element-property :raw-value element))
                        (level . ,(org-element-property :level element))
                        (tags . ,(org-element-property :tags element))))
                     ((eq type 'paragraph)
                      `((type . paragraph)
                        (contents . ,(org-element-contents element))))
                     (t
                      `((type . ,type)
                        (properties . ,element))))))
        (prin1-to-string props))
    "Not in org-mode"))

(provide 'emagent-tools-introspect)
;;; emagent-tools-introspect.el ends here
