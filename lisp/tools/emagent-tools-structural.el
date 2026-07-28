;;; emagent-tools-structural.el --- Structural editing tool handlers  -*- lexical-binding: t; -*-

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

;; lisp-sitter structural editing tool handlers (sync and async).

;;; Code:

(require 'cl-lib)
(require 'emagent-tools-core)
(require 'emagent-struct)
(require 'emagent-tools-file)

(defun emagent-tools--structural-sync-path (file)
  "Sync FILE buffer content to disk; return absolute path."
  (let ((content (emagent-tools--read-structural-file-content file)))
    (emagent-tools--write-file-content file content)
    (emagent-tools--root-directory file)))

(defun emagent-tools--structural-apply-file-result (file result)
  "Write RESULT to FILE when it is updated content, not a status line."
  (if (or (string-prefix-p "Wrote " result) (string-empty-p result))
      result
    (progn
      (emagent-tools--write-file-content file result)
      (format "Wrote %s" (emagent-tools--root-directory file)))))

(defun emagent-tool-check-structural-file (file)
  "Validate FILE with lisp-sitter (when available)."
  (if (emagent-struct-available-p)
      (emagent-struct-check (emagent-tools--read-structural-file-content file) file)
    (emagent-elisp-check-file-content
     (emagent-tools--read-structural-file-content file) file)))

(defun emagent-tool-check-structural-node (file node)
  "Validate NODE text with lisp-sitter for FILE's language."
  (if (emagent-struct-available-p)
      (emagent-struct-check-node node (emagent-struct--lang-for file))
    (if (string-match-p "\\.el\\'" file)
        (emagent-elisp-check-form node)
      (format "No checker for %s (install lisp-sitter)" file))))

(defun emagent-tool-structural-tree (file &optional depth)
  "Return a structural outline of FILE using lisp-sitter.

Arguments: DEPTH."
  (if (emagent-struct-available-p)
      (emagent-struct-tree (emagent-tools--read-structural-file-content file) file depth)
    (let ((err (emagent-tools--read-structural-file-content file)))
      (if (string-empty-p err)
          ""
        (format "install lisp-sitter to see structural outline of %s" file)))))

(defun emagent-tool-structural-get (file symbol)
  "Return full text of top-level SYMBOL in FILE."
  (emagent-struct-get (emagent-tools--read-structural-file-content file) file symbol))

(defun emagent-tool-structural-find-errors (file)
  "Return tree-sitter MISSING/ERROR nodes for FILE."
  (emagent-struct-find-errors (emagent-tools--structural-sync-path file)))

(defun emagent-tool-structural-context (file)
  "Return outline and full text of each top-level form in FILE."
  (emagent-struct-context (emagent-tools--structural-sync-path file)))

(defun emagent-tool-structural-complete (lang body)
  "Complete missing closing parens in BODY for LANG."
  (emagent-struct-complete lang body))

(defun emagent-tool-structural-format (file &optional write)
  "Re-indent FILE with lisp-sitter.

Arguments: WRITE."
  (let ((path (emagent-tools--structural-sync-path file)))
    (if write
        (progn
          (emagent-struct-format-file path t)
          (format "Wrote %s" path))
      (emagent-struct-format-file path nil))))

(defun emagent-tool-structural-rename (file old new &optional refs no-refs)
  "Rename top-level form OLD to NEW in FILE.

Arguments: REFS, NO-REFS."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-rename-file (emagent-tools--structural-sync-path file)
                               old new refs no-refs)))

(defun emagent-tool-structural-wrap (file symbol wrap &optional bindings condition)
  "Wrap SYMBOL's body in WRAP in FILE.

Arguments: BINDINGS, CONDITION."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-wrap-file (emagent-tools--structural-sync-path file)
                             symbol wrap bindings condition)))

(defun emagent-tool-structural-remove (file symbol &optional keep-calls)
  "Remove top-level SYMBOL from FILE.

Arguments: KEEP-CALLS."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-remove-file (emagent-tools--structural-sync-path file)
                               symbol keep-calls)))

(defun emagent-tool-structural-move (file symbol after)
  "Move top-level SYMBOL after AFTER in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-move-file (emagent-tools--structural-sync-path file)
                             symbol after)))

(defun emagent-tool-structural-substitute (file symbol pattern replacement)
  "Replace PATTERN with REPLACEMENT inside SYMBOL in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-substitute-file (emagent-tools--structural-sync-path file)
                                   symbol pattern replacement)))

(defun emagent-tool-structural-extract (file symbol pattern name &optional params)
  "Extract PATTERN into new function NAME inside SYMBOL in FILE.

Arguments: PARAMS."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-extract-file (emagent-tools--structural-sync-path file)
                                symbol pattern name params)))

(defun emagent-tool-structural-callers (file symbol)
  "Return callers of SYMBOL in FILE."
  (emagent-struct-callers-file (emagent-tools--structural-sync-path file) symbol))

(defun emagent-tool-structural-instrument (file symbol &optional with at wrap)
  "Instrument SYMBOL in FILE with tracing.

Arguments: AT, WRAP."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-instrument-file (emagent-tools--structural-sync-path file)
                                   symbol with at wrap)))

(defun emagent-tool-structural-flatten (file symbol)
  "Inline SYMBOL at call sites in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-flatten-file (emagent-tools--structural-sync-path file) symbol)))

(defun emagent-tool-structural-convert-let (file symbol to)
  "Convert let/let* for SYMBOL to TO in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-convert-let-file (emagent-tools--structural-sync-path file)
                                    symbol to)))

(defun emagent-tool-structural-splice (file symbol pattern)
  "Splice PATTERN inside SYMBOL in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-splice-file (emagent-tools--structural-sync-path file)
                                symbol pattern)))

(defun emagent-tool-structural-raise (file symbol pattern)
  "Raise PATTERN inside SYMBOL in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-raise-file (emagent-tools--structural-sync-path file)
                              symbol pattern)))

(defun emagent-tool-structural-bounds (file symbol)
  "Return START:END byte positions for SYMBOL in FILE."
  (emagent-struct-bounds (emagent-tools--read-structural-file-content file)
                         file symbol))

(defun emagent-tool-structural-replace (file symbol new-body)
  "Replace top-level node SYMBOL in FILE with complete NEW-BODY text."
  (when-let ((err (emagent-tools--eval-form-guard new-body)))
    (user-error "%s" err))
  (let* ((content (emagent-tools--read-structural-file-content file))
         (updated (emagent-struct-replace content file symbol new-body)))
    (emagent-tools--write-file-content file updated)
    (when emagent-struct-eval-after-structural-edit
      (ignore-errors (eval (read new-body))))
    (format "Wrote %s" (expand-file-name file))))

(defun emagent-tool-structural-insert (file after-symbol node)
  "Insert complete top-level NODE after AFTER-SYMBOL in FILE."
  (when-let ((err (emagent-tools--eval-form-guard node)))
    (user-error "%s" err))
  (let* ((content (emagent-tools--read-structural-file-content file))
         (updated (emagent-struct-insert content file after-symbol node)))
    (emagent-tools--write-file-content file updated)
    (when emagent-struct-eval-after-structural-edit
      (ignore-errors (eval (read node))))
    (format "Wrote %s" (expand-file-name file))))

(defun emagent-tools--structural-apply-async (callback file args)
  "Run lisp-sitter ARGS on synced FILE; write result and call CALLBACK."
  (apply #'emagent-struct--call-path-async
         (lambda (result is-error)
           (if is-error
               (funcall callback result t)
             (funcall callback
                      (emagent-tools--structural-apply-file-result file result)
                      nil)))
         args))

(defun emagent-tool-check-structural-file-async (callback file)
  "Validate FILE with lisp-sitter asynchronously.

Arguments: CALLBACK."
  (if (emagent-struct-available-p)
      (let ((content (emagent-tools--read-structural-file-content file)))
        (apply #'emagent-struct--call-async
               (lambda (out is-error)
                 (if is-error
                     (funcall callback out t)
                   (funcall callback
                            (if (string-match "^[^:]+: \\(.*\\)$" out)
                                (match-string 1 out)
                              out)
                            nil)))
               content "check" "-" "--lang" (emagent-struct--lang-for file)))
    (funcall callback
             (emagent-elisp-check-file-content
              (emagent-tools--read-structural-file-content file) file)
             nil)))

(defun emagent-tool-check-structural-node-async (callback file node)
  "Validate NODE text with lisp-sitter for FILE's language asynchronously.

Arguments: CALLBACK."
  (if (emagent-struct-available-p)
      (apply #'emagent-struct--call-async callback node "check-node"
             "--lang" (emagent-struct--lang-for file) "--body-file" "-")
    (funcall callback
             (if (string-match-p "\\.el\\'" file)
                 (emagent-elisp-check-form node)
               (format "No checker for %s (install lisp-sitter)" file))
             nil)))

(defun emagent-tool-structural-tree-async (callback file &optional depth)
  "Return a structural outline of FILE asynchronously.

Arguments: CALLBACK, DEPTH."
  (if (emagent-struct-available-p)
      (let* ((content (emagent-tools--read-structural-file-content file))
             (args (list "tree" "-" "--json" "--lang"
                         (emagent-struct--lang-for file))))
        (when (and depth (> depth 1))
          (setq args (append args (list "--depth" (number-to-string depth)))))
        (apply #'emagent-struct--call-async callback content args))
    (let ((content (emagent-tools--read-structural-file-content file)))
      (funcall callback
               (if (string-empty-p content)
                   ""
                 (format "install lisp-sitter to see structural outline of %s" file))
               nil))))

(defun emagent-tool-structural-get-async (callback file symbol)
  "Return full text of top-level SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (let ((content (emagent-tools--read-structural-file-content file)))
    (apply #'emagent-struct--call-async callback content "get" "-" symbol
           "--lang" (emagent-struct--lang-for file))))

(defun emagent-tool-structural-find-errors-async (callback file)
  "Return tree-sitter MISSING/ERROR nodes for FILE asynchronously.

Arguments: CALLBACK."
  (emagent-struct--call-path-async
   callback "find-errors" (emagent-tools--structural-sync-path file)))

(defun emagent-tool-structural-context-async (callback file)
  "Return outline and full text of each top-level form in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-struct--call-path-async
   callback "context" (emagent-tools--structural-sync-path file)))

(defun emagent-tool-structural-complete-async (callback lang body)
  "Complete missing closing parens in BODY for LANG asynchronously.

Arguments: CALLBACK."
  (apply #'emagent-struct--call-async callback body "complete"
         "--lang" lang "--body-file" "-"))

(defun emagent-tool-structural-format-async (callback file &optional write)
  "Re-indent FILE with lisp-sitter asynchronously.

Arguments: CALLBACK, WRITE."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "fmt" path)))
    (when write (setq args (append args '("--write"))))
    (if write
        (apply #'emagent-struct--call-path-async
               (lambda (result is-error)
                 (if is-error
                     (funcall callback result t)
                   (funcall callback (format "Wrote %s" path) nil)))
               args)
      (apply #'emagent-struct--call-path-async callback args))))

(defun emagent-tool-structural-rename-async (callback file old new &optional refs no-refs)
  "Rename top-level form OLD to NEW in FILE asynchronously.

Arguments: CALLBACK, REFS, NO-REFS."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "rename" path old new)))
    (when refs (setq args (append args '("--refs"))))
    (when no-refs (setq args (append args '("--no-refs"))))
    (emagent-tools--structural-apply-async callback file args)))

(defun emagent-tool-structural-wrap-async (callback file symbol wrap
                                                   &optional bindings condition)
  "Wrap SYMBOL's body in WRAP in FILE asynchronously.

Arguments: CALLBACK, BINDINGS, CONDITION."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "wrap" path symbol "--in" wrap)))
    (when bindings (setq args (append args (list "--bindings" bindings))))
    (when condition (setq args (append args (list "--condition" condition))))
    (emagent-tools--structural-apply-async callback file args)))

(defun emagent-tool-structural-remove-async (callback file symbol &optional keep-calls)
  "Remove top-level SYMBOL from FILE asynchronously.

Arguments: CALLBACK, KEEP-CALLS."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "remove" path symbol)))
    (when keep-calls (setq args (append args '("--keep-calls"))))
    (emagent-tools--structural-apply-async callback file args)))

(defun emagent-tool-structural-move-async (callback file symbol after)
  "Move top-level SYMBOL after AFTER in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "move" (emagent-tools--structural-sync-path file) symbol after)))

(defun emagent-tool-structural-substitute-async (callback file symbol pattern replacement)
  "Replace PATTERN with REPLACEMENT inside SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "substitute" (emagent-tools--structural-sync-path file) symbol
         "--pattern" pattern "--replacement" replacement)))

(defun emagent-tool-structural-extract-async (callback file symbol pattern name
                                                      &optional params)
  "Extract PATTERN into new function NAME inside SYMBOL in FILE asynchronously.

Arguments: CALLBACK, PARAMS."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "extract" path symbol "--pattern" pattern "--name" name)))
    (when (and params (not (string-empty-p params)))
      (setq args (append args (list "--params" params))))
    (emagent-tools--structural-apply-async callback file args)))

(defun emagent-tool-structural-callers-async (callback file symbol)
  "Return callers of SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-struct--call-path-async
   callback "callers" (emagent-tools--structural-sync-path file) symbol))

(defun emagent-tool-structural-instrument-async (callback file symbol
                                                         &optional with at wrap)
  "Instrument SYMBOL in FILE with tracing asynchronously.

Arguments: CALLBACK, AT, WRAP."
  (let* ((path (emagent-tools--structural-sync-path file))
         (args (list "instrument" path symbol)))
    (when with (setq args (append args (list "--with" with))))
    (when at (setq args (append args (list "--at" at))))
    (when wrap (setq args (append args (list "--wrap" wrap))))
    (emagent-tools--structural-apply-async callback file args)))

(defun emagent-tool-structural-flatten-async (callback file symbol)
  "Inline SYMBOL at call sites in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "flatten" (emagent-tools--structural-sync-path file) symbol)))

(defun emagent-tool-structural-convert-let-async (callback file symbol to)
  "Convert let/let* for SYMBOL to TO in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "convert-let" (emagent-tools--structural-sync-path file) symbol "--to" to)))

(defun emagent-tool-structural-splice-async (callback file symbol pattern)
  "Splice PATTERN inside SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "splice" (emagent-tools--structural-sync-path file) symbol "--pattern" pattern)))

(defun emagent-tool-structural-raise-async (callback file symbol pattern)
  "Raise PATTERN inside SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (emagent-tools--structural-apply-async
   callback file
   (list "raise" (emagent-tools--structural-sync-path file) symbol "--pattern" pattern)))

(defun emagent-tool-structural-bounds-async (callback file symbol)
  "Return START:END byte positions for SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (let ((content (emagent-tools--read-structural-file-content file)))
    (apply #'emagent-struct--call-async callback content "bounds" "-" symbol
           "--lang" (emagent-struct--lang-for file))))

(cl-defun emagent-tool-structural-replace-async (callback file symbol new-body)
  "Replace top-level node SYMBOL in FILE with NEW-BODY asynchronously.

Arguments: CALLBACK."
  (condition-case err
      (when-let ((guard (emagent-tools--eval-form-guard new-body)))
        (user-error "%s" guard))
    (error (funcall callback (error-message-string err) t)
           (cl-return-from emagent-tool-structural-replace-async)))
  (let* ((content (emagent-tools--read-structural-file-content file))
         (lang (emagent-struct--lang-for file)))
    (apply #'emagent-struct--call-async
           (lambda (updated is-error)
             (if is-error
                 (funcall callback updated t)
               (emagent-tools--write-file-content file updated)
               (when emagent-struct-eval-after-structural-edit
                 (ignore-errors (eval (read new-body))))
               (funcall callback (format "Wrote %s" (expand-file-name file)) nil)))
           content "replace" "-" symbol "--body" new-body "--lang" lang)))

(cl-defun emagent-tool-structural-insert-async (callback file after-symbol node)
  "Insert complete top-level NODE after AFTER-SYMBOL in FILE asynchronously.

Arguments: CALLBACK."
  (condition-case err
      (when-let ((guard (emagent-tools--eval-form-guard node)))
        (user-error "%s" guard))
    (error (funcall callback (error-message-string err) t)
           (cl-return-from emagent-tool-structural-insert-async)))
  (let* ((content (emagent-tools--read-structural-file-content file))
         (lang (emagent-struct--lang-for file)))
    (apply #'emagent-struct--call-async
           (lambda (updated is-error)
             (if is-error
                 (funcall callback updated t)
               (emagent-tools--write-file-content file updated)
               (when emagent-struct-eval-after-structural-edit
                 (ignore-errors (eval (read node))))
               (funcall callback (format "Wrote %s" (expand-file-name file)) nil)))
           content "insert" "-" after-symbol "--node" node "--lang" lang)))

(provide 'emagent-tools-structural)
;;; emagent-tools-structural.el ends here
