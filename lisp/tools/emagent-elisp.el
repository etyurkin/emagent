;;; emagent-elisp.el --- Elisp validation helpers for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 1.1.0

;;; Commentary:
;;
;; Validation and security helpers for Emacs Lisp.  Structural editing (tree,
;; bounds, replace, insert) is delegated to the lisp-sitter CLI proxy in
;; emagent-struct.el.  This file only keeps what needs the Emacs reader:
;; byte-compilation, paren-balance checks, and the check_elisp MCP tool.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'subr-x)

(defgroup emagent-elisp nil
  "Elisp validation helpers for emagent."
  :group 'emagent-tools)

(defcustom emagent-elisp-validate-on-write t
  "When non-nil, reject writes to .el files that fail Elisp validation."
  :type 'boolean
  :group 'emagent-elisp)

(defcustom emagent-elisp-byte-compile-on-check t
  "When non-nil, run `byte-compile-file' during .el file validation."
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
  "Like `emagent-elisp--validate-content' but also byte-compile when enabled."
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
  "Validate Elisp file CONTENT.  Return \"OK\" or an error description."
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
  "Return non-nil when PATH looks like an Emacs Lisp file."
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

(provide 'emagent-elisp)
;;; emagent-elisp.el ends here
