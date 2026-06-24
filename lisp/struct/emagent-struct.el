;;; emagent-struct.el --- Structural file editing via lisp-sitter CLI -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

(defun emagent-struct--call (content &rest args)
  "Pipe CONTENT as stdin to lisp-sitter ARGS, return trimmed stdout.
Signal an error when lisp-sitter exits non-zero."
  (with-temp-buffer
    (let ((out (current-buffer))
          exit)
      (with-temp-buffer
        (insert content)
        (setq exit (apply #'call-process-region (point-min) (point-max)
                          emagent-struct-lisp-sitter-bin nil out nil args)))
      (if (= exit 0)
          (string-trim (buffer-string))
        (error "lisp-sitter exited %d: %s" exit
               (truncate-string-to-width
                (car (split-string (buffer-string) "\n" t)) 80 nil nil "…"))))))

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

(defun emagent-struct-tree (content path)
  "Return JSON structural outline of CONTENT for PATH's language."
  (emagent-struct--ensure)
  (emagent-struct--call content "tree" "-" "--json"
                        "--lang" (emagent-struct--lang-for path)))

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

(provide 'emagent-struct)
;;; emagent-struct.el ends here
