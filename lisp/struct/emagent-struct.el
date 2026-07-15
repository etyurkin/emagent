;;; emagent-struct.el --- Structural file editing via lisp-sitter CLI -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6
;; SPDX-License-Identifier: MIT
;; Version: 1.2.3

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
;; Thin proxy over the lisp-sitter CLI (https://github.com/etyurkin/lisp-sitter).
;; All structural Lisp operations pipe buffer content via stdin and read the
;; result from stdout -- no temp files, full buffer-awareness.
;;
;; When `emagent-struct-lisp-sitter-bin' is nil, no structural tools are
;; registered.  The agent falls back to write_file + check_elisp for basic
;; Elisp editing (non-structural).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup emagent-struct nil
  "Structural file editing via lisp-sitter CLI."
  :group 'emagent-tools)

(defcustom emagent-struct-lisp-sitter-bin
  (executable-find "lisp-sitter")
  "Path to the lisp-sitter binary.
When nil, structural Lisp tools are unavailable and the agent
falls back to write_file + check_elisp."
  :type '(choice (string :tag "Path to lisp-sitter binary")
                 (const :tag "Not installed" nil))
  :group 'emagent-struct)

(defcustom emagent-struct-eval-after-structural-edit t
  "When non-nil, eval the changed form after a structural write."
  :type 'boolean
  :group 'emagent-struct)

(defcustom emagent-struct-require-for-lisp-files t
  "When non-nil and lisp-sitter is installed, refuse write_file on Lisp files.

Agents must use structural_* MCP tools for .el, .lisp, .cl, and .scm files."
  :type 'boolean
  :group 'emagent-struct)

;; ── Language detection ────────────────────────────────────────────

(defun emagent-struct--lang-for (path)
  "Return language id string for PATH based on extension."
  (cond
   ((string-match-p "\\.el\\'" path) "elisp")
   ((string-match-p "\\.lisp\\'" path) "commonlisp")
   ((string-match-p "\\.cl\\'" path) "commonlisp")
   ((string-match-p "\\.scm\\'" path) "scheme")
   ((string-match-p "\\.ss\\'" path) "scheme")
   ((string-match-p "\\.sld\\'" path) "scheme")
   (t "elisp")))

(defun emagent-struct--lisp-file-p (path)
  "Return non-nil when PATH is a supported Lisp file."
  (and (stringp path)
       (string-match-p
        "\\.\\(el\\|lisp\\|cl\\|scm\\|ss\\|sld\\)\\'" path)))

;; ── CLI invocation ────────────────────────────────────────────────

(declare-function emagent-tools--run-async-sync "emagent-tools-shell")
(declare-function emagent-tools--run-process-async "emagent-tools-shell")
(declare-function emagent-tools--run-process-input-async "emagent-tools-shell")

(defun emagent-struct--lisp-sitter-error (output)
  "Format non-zero lisp-sitter OUTPUT as an error string."
  (truncate-string-to-width
   (car (split-string output "\n" t)) 80 nil nil "…"))

(defun emagent-struct--call-async (callback content &rest args)
  "Pipe CONTENT to lisp-sitter ARGS; call CALLBACK with (output is-error)."
  (emagent-struct--ensure)
  (apply #'emagent-tools--run-process-input-async
         (lambda (output is-error)
           (if is-error
               (funcall callback
                        (format "lisp-sitter exited: %s"
                                (emagent-struct--lisp-sitter-error output))
                        t)
             (funcall callback (string-trim output) nil)))
         content emagent-struct-lisp-sitter-bin args))

(defun emagent-struct--call (content &rest args)
  "Pipe CONTENT as stdin to lisp-sitter ARGS, return trimmed stdout.
Signal an error when lisp-sitter exits non-zero."
  (emagent-struct--ensure)
  (with-temp-buffer
    (let ((out (current-buffer))
          exit)
      (with-temp-buffer
        (insert content)
        (setq exit (apply #'call-process-region (point-min) (point-max)
                          emagent-struct-lisp-sitter-bin nil out nil args)))
      (if (= exit 0)
          (string-trim (buffer-string))
        (error "Lisp-sitter exited %d: %s" exit
               (emagent-struct--lisp-sitter-error (buffer-string)))))))

(defun emagent-struct--call-path (&rest args)
  "Run lisp-sitter ARGS against a file path; return trimmed stdout."
  (emagent-struct--ensure)
  (with-temp-buffer
    (let ((exit (apply #'call-process emagent-struct-lisp-sitter-bin nil
                      (current-buffer) nil args)))
      (if (= exit 0)
          (string-trim (buffer-string))
        (error "Lisp-sitter exited %d: %s" exit
               (emagent-struct--lisp-sitter-error (buffer-string)))))))

(defun emagent-struct--call-path-async (callback &rest args)
  "Run lisp-sitter ARGS against a file path; call CALLBACK with (output is-error)."
  (emagent-struct--ensure)
  (apply #'emagent-tools--run-process-async
         (lambda (output is-error)
           (if is-error
               (funcall callback
                        (format "lisp-sitter exited: %s"
                                (emagent-struct--lisp-sitter-error output))
                        t)
             (funcall callback (string-trim output) nil)))
         emagent-struct-lisp-sitter-bin args))

(define-error 'emagent-struct-unavailable
  "lisp-sitter is not installed; install it with `make install` in the lisp-sitter repo"
  'error)

(defun emagent-struct--ensure ()
  "Signal an error when lisp-sitter is unavailable."
  (unless (and emagent-struct-lisp-sitter-bin
               (file-executable-p emagent-struct-lisp-sitter-bin))
    (signal 'emagent-struct-unavailable
            (list "lisp-sitter binary not found on exec-path"))))

;; ── Public API ────────────────────────────────────────────────────

(defun emagent-struct-available-p ()
  "Return non-nil when lisp-sitter is installed and executable."
  (and emagent-struct-lisp-sitter-bin
       (file-executable-p emagent-struct-lisp-sitter-bin)))

(defun emagent-struct-tree (content path &optional depth)
  "Return JSON structural outline of CONTENT for PATH's language.

Arguments: DEPTH."
  (emagent-struct--ensure)
  (let ((args (list "tree" "-" "--json" "--lang" (emagent-struct--lang-for path))))
    (when (and depth (> depth 1))
      (setq args (append args (list "--depth" (number-to-string depth)))))
    (apply #'emagent-struct--call content args)))

(defun emagent-struct-bounds (content path symbol)
  "Return START:END string for SYMBOL in CONTENT for PATH's language."
  (emagent-struct--ensure)
  (emagent-struct--call content "bounds" "-" symbol
                        "--lang" (emagent-struct--lang-for path)))

(defun emagent-struct-replace (content path symbol new-body)
  "Replace SYMBOL's form in CONTENT with NEW-BODY.
Return the updated file content.  CONTENT is PATH's current content."
  (emagent-struct--ensure)
  (emagent-struct--call content "replace" "-" symbol
                        "--body" new-body
                        "--lang" (emagent-struct--lang-for path)))

(defun emagent-struct-insert (content path after-symbol node)
  "Insert NODE after AFTER-SYMBOL in CONTENT for PATH's language.
Return the updated file content."
  (emagent-struct--ensure)
  (emagent-struct--call content "insert" "-" after-symbol
                        "--node" node
                        "--lang" (emagent-struct--lang-for path)))

(defun emagent-struct-check (content path)
  "Validate CONTENT for PATH's language.  Return \"OK\" or error text."
  (emagent-struct--ensure)
  (let ((out (emagent-struct--call content "check" "-"
                                   "--lang" (emagent-struct--lang-for path))))
    (if (string-match "^[^:]+: \\(.*\\)$" out)
        (match-string 1 out)
      out)))

(defun emagent-struct-check-node (content lang)
  "Validate a single complete top-level CONTENT for LANG.
Returns \"OK\" or error text."
  (emagent-struct--ensure)
  (emagent-struct--call content "check-node"
                        "--lang" lang "--body-file" "-"))

(defun emagent-struct-get (content path symbol)
  "Return the full text of SYMBOL's form from CONTENT for PATH's language."
  (emagent-struct--ensure)
  (emagent-struct--call content "get" "-" symbol
                        "--lang" (emagent-struct--lang-for path)))

(defun emagent-struct-complete (lang body)
  "Complete missing closing parens in BODY for LANG."
  (emagent-struct--ensure)
  (emagent-struct--call body "complete" "--lang" lang "--body-file" "-"))

(defun emagent-struct-find-errors (path)
  "Return tree-sitter error report for file at absolute PATH."
  (emagent-struct--call-path "find-errors" path))

(defun emagent-struct-context (path)
  "Return structural context (outline + forms) for file at PATH."
  (emagent-struct--call-path "context" path))

(defun emagent-struct-format-file (path &optional write)
  "Re-indent file at PATH; when WRITE is non-nil, save the result."
  (let ((args (list "fmt" path)))
    (when write (setq args (append args '("--write"))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-rename-file (path old new &optional refs no-refs)
  "Rename form OLD to NEW in file at PATH; return updated file text.

Arguments: REFS, NO-REFS."
  (let ((args (list "rename" path old new)))
    (when refs (setq args (append args '("--refs"))))
    (when no-refs (setq args (append args '("--no-refs"))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-wrap-file (path symbol wrap &optional bindings condition)
  "Wrap SYMBOL's body in WRAP construct in file at PATH.

Arguments: BINDINGS, CONDITION."
  (let ((args (list "wrap" path symbol "--in" wrap)))
    (when bindings (setq args (append args (list "--bindings" bindings))))
    (when condition (setq args (append args (list "--condition" condition))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-remove-file (path symbol &optional keep-calls)
  "Remove top-level SYMBOL from file at PATH.

Arguments: KEEP-CALLS."
  (let ((args (list "remove" path symbol)))
    (when keep-calls (setq args (append args '("--keep-calls"))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-move-file (path symbol after)
  "Move SYMBOL after AFTER in file at PATH."
  (emagent-struct--call-path "move" path symbol after))

(defun emagent-struct-substitute-file (path symbol pattern replacement)
  "Substitute PATTERN with REPLACEMENT inside SYMBOL in file at PATH."
  (emagent-struct--call-path "substitute" path symbol
                             "--pattern" pattern "--replacement" replacement))

(defun emagent-struct-extract-file (path symbol pattern name &optional params)
  "Extract PATTERN into new function NAME inside SYMBOL in file at PATH.

Arguments: PARAMS."
  (let ((args (list "extract" path symbol "--pattern" pattern "--name" name)))
    (when (and params (not (string-empty-p params)))
      (setq args (append args (list "--params" params))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-callers-file (path symbol)
  "Return callers of SYMBOL in file at PATH."
  (emagent-struct--call-path "callers" path symbol))

(defun emagent-struct-instrument-file (path symbol &optional with at wrap)
  "Instrument SYMBOL in file at PATH.

Arguments: WITH, WRAP."
  (let ((args (list "instrument" path symbol)))
    (when with (setq args (append args (list "--with" with))))
    (when at (setq args (append args (list "--at" at))))
    (when wrap (setq args (append args (list "--wrap" wrap))))
    (apply #'emagent-struct--call-path args)))

(defun emagent-struct-flatten-file (path symbol)
  "Inline SYMBOL's body at call sites in file at PATH."
  (emagent-struct--call-path "flatten" path symbol))

(defun emagent-struct-convert-let-file (path symbol to)
  "Convert let/let* for SYMBOL to TO in file at PATH."
  (emagent-struct--call-path "convert-let" path symbol "--to" to))

(defun emagent-struct-splice-file (path symbol pattern)
  "Splice PATTERN inside SYMBOL in file at PATH."
  (emagent-struct--call-path "splice" path symbol "--pattern" pattern))

(defun emagent-struct-raise-file (path symbol pattern)
  "Raise PATTERN inside SYMBOL in file at PATH."
  (emagent-struct--call-path "raise" path symbol "--pattern" pattern))

(defun emagent-struct-write-required-p (path)
  "Return non-nil when PATH must be edited with structural tools."
  (and emagent-struct-require-for-lisp-files
       (emagent-struct-available-p)
       (emagent-struct--lisp-file-p path)))

(provide 'emagent-struct)
;;; emagent-struct.el ends here
