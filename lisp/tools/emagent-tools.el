;;; emagent-tools.el --- Emacs tool handlers for emagent -*- lexical-binding: t; -*-

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
;;
;; Tool registry, file/structural tools, confirm prompts, and introspection.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-element)
(require 'emagent-chat-ui)
(require 'emagent-log)
(require 'emagent-policy)
(require 'emagent-struct)
(require 'emagent-tools-shell)

(defvar auto-insert)

(defvar emagent-tools-show-written-buffer)

(defun emagent-tools--protected-fs-path-p (path)
  "Return non-nil when PATH must not be accessed via Emacs on macOS."
  (emagent-tools--protected-truename-p (file-truename (expand-file-name path))))

(defun emagent-tools--file-buffer (path)
  "Return a buffer visiting PATH, visiting it if the file exists."
  (let ((resolved (emagent-tools--root-directory path)))
    (or (find-buffer-visiting resolved)
        (when (file-exists-p resolved)
          (find-file-noselect resolved)))))

(defun emagent-tools--extract-buffer-text (buffer &optional line limit)
  "Return text from BUFFER starting at LINE for LIMIT lines."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (and line (> line 1))
          (forward-line (1- line)))
        (let ((start (point)))
          (if limit
              (forward-line limit)
            (goto-char (point-max)))
          (buffer-substring-no-properties start (point)))))))

(defun emagent-tools--read-file-content (path &optional line limit)
  "Read PATH through Emacs, including unsaved buffer contents.

Arguments: LINE, LIMIT."
  (let* ((resolved (emagent-tools--root-directory path))
         (buffer (find-buffer-visiting resolved)))
    (if buffer
        (emagent-tools--extract-buffer-text buffer line limit)
      (with-temp-buffer
        (insert-file-contents resolved)
        (emagent-tools--extract-buffer-text (current-buffer) line limit)))))

(defun emagent-tools--read-elisp-file-content (path)
  "Like `emagent-tools--read-file-content' but return \"\" when PATH is missing."
  (condition-case-unless-debug nil
      (emagent-tools--read-file-content path)
    (file-missing "")))

(defun emagent-tool-read-file (path &optional line limit)
  "Return contents of PATH as a string.

Arguments: LINE, LIMIT."
  (when (emagent-tools--protected-fs-path-p path)
    (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                (emagent-tools--root-directory path)))
  (emagent-tools--read-file-content path line limit))

(defun emagent-tools--read-structural-file-content (path)
  "Like `emagent-tools--read-file-content' but return \"\" when PATH is missing."
  (condition-case-unless-debug nil
      (emagent-tools--read-file-content path)
    (file-missing "")))

(defun emagent-tools--write-file-content (path content)
  "Write CONTENT to PATH through an Emacs buffer.
Each call is recorded as a single undoable change in the target buffer."
  (let* ((resolved (emagent-tools--root-directory path))
         (dir (file-name-directory resolved))
         (buffer (or (find-buffer-visiting resolved)
                     (let ((auto-insert nil))
                       (find-file-noselect resolved)))))
    (when (and emagent-elisp-validate-on-write
               (emagent-elisp-elisp-file-p resolved))
      (when-let ((err (emagent-elisp--validate-content-strict content resolved)))
        (user-error "Validation failed for %s: %s" resolved err)))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t))
    (with-temp-buffer
      (insert content)
      (let ((content-buffer (current-buffer))
            (inhibit-read-only t))
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (undo-boundary)
            (replace-buffer-contents content-buffer 1.0)
            (undo-boundary))
          (basic-save-buffer))))
    ;; Showing the result is best-effort: the file is already saved, so a
    ;; display failure must not surface as a write_file tool error.
    (condition-case-unless-debug err
        (pcase emagent-tools-show-written-buffer
          ('magit-diff
           (with-current-buffer buffer
             ;; `magit-diff-buffer-file' is autoloaded, so `fboundp' alone
             ;; doesn't prove magit is loaded; `magit-toplevel' has no
             ;; autoload cookie and would be void.
             (if (and (fboundp 'magit-diff-buffer-file)
                      (fboundp 'magit-toplevel)
                      (magit-toplevel))
                 (magit-diff-buffer-file)
               (display-buffer buffer))))
          ((pred identity)
           (display-buffer buffer)))
      (error
       (emagent-log "write_file: showing %s failed: %s"
                    resolved (error-message-string err))))
    resolved))

(cl-defun emagent-tools--unified-diff-async (callback old new label)
  "Return unified diff between OLD and NEW for LABEL via CALLBACK."
  (if (string= old new)
      (funcall callback "" nil)
    (let ((old-file (make-temp-file "emagent-old"))
          (new-file (make-temp-file "emagent-new")))
      (write-region old nil old-file nil 'silent)
      (write-region new nil new-file nil 'silent)
      (unless (executable-find "diff")
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))
        (funcall callback "" nil)
        (cl-return-from emagent-tools--unified-diff-async))
      (emagent-tools--run-process-async
       (lambda (output is-error)
         (ignore-errors (delete-file old-file))
         (ignore-errors (delete-file new-file))
         (funcall callback output is-error))
       "diff" "-u"
       "--label" (concat "a/" label)
       "--label" (concat "b/" label)
       old-file new-file))))

(defun emagent-tools--unified-diff (old new label)
  "Return a unified diff string between OLD and NEW content for LABEL."
  (if (string= old new)
      ""
    (let ((old-file (make-temp-file "emagent-old"))
          (new-file (make-temp-file "emagent-new")))
      (unwind-protect
          (progn
            (write-region old nil old-file nil 'silent)
            (write-region new nil new-file nil 'silent)
            (with-temp-buffer
              (call-process "diff" nil t nil "-u"
                            "--label" (concat "a/" label)
                            "--label" (concat "b/" label)
                            old-file new-file)
              (buffer-string)))
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))))))

(cl-defun emagent-tool-write-file-async (callback path content)
  "Write CONTENT to PATH; call CALLBACK with (result is-error)."
  (condition-case err
      (when (emagent-struct-write-required-p path)
        (user-error
         "Refusing write_file on %s: lisp-sitter is installed — use structural_* tools"
         (emagent-tools--root-directory path)))
    (error (funcall callback (error-message-string err) t)
           (cl-return-from emagent-tool-write-file-async)))
  (let* ((resolved (emagent-tools--root-directory path))
         (label (file-name-nondirectory resolved))
         (old (emagent-tools--read-elisp-file-content path)))
    (condition-case err
        (progn
          (when (emagent-tools--protected-fs-path-p path)
            (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                        resolved))
          (emagent-tools--write-file-content path content)
          (emagent-tools--unified-diff-async
           (lambda (diff is-error)
             (if is-error
                 (funcall callback diff t)
               (funcall callback
                        (if (string-empty-p diff)
                            (format "Wrote %s (no changes)" resolved)
                          diff)
                        nil)))
           old content label))
      (error (funcall callback (error-message-string err) t)))))

(defun emagent-tool-write-file (path content)
  "Write CONTENT to PATH through Emacs after user confirmation."
  (when (emagent-struct-write-required-p path)
    (user-error
     "Refusing write_file on %s: lisp-sitter is installed — use structural_* tools"
     (emagent-tools--root-directory path)))
  (let* ((resolved (emagent-tools--root-directory path))
         (label (file-name-nondirectory resolved))
         (old (emagent-tools--read-elisp-file-content path)))
    (when (emagent-tools--protected-fs-path-p path)
      (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                  resolved))
    (emagent-tools--write-file-content path content)
    (let ((diff (emagent-tools--unified-diff old content label)))
      (if (string-empty-p diff)
          (format "Wrote %s (no changes)" resolved)
        diff))))

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

(defcustom emagent-allowed-tools '(emagent-tool-fetch-url)
  "Symbols naming tools that may run without confirmation."
  :type '(repeat symbol)
  :group 'emagent-tools)

(defvar emagent-tools--session-allowed-tools nil
  "Tools allowed without confirmation for the current session only.

Bound by the MCP dispatcher from the chat buffer's persisted allow-list so a
per-document choice (see `emagent-tools-allow-all-function') is honoured on
the next call without touching the global `emagent-allowed-tools'.")

(defvar emagent-tools-allow-all-function nil
  "Function of one tool symbol, called when the user chooses \"allow all\".

Bound by the MCP dispatcher to persist the choice per project directory under
`emagent-permissions-directory'.  Nil means the choice only lasts for the
current call.")

(defvar emagent-tools--chat-buffer nil
  "The emagent chat buffer for the active session.
When non-nil, permission prompts are shown as inline buttons there instead
of in the minibuffer.  Bound per MCP dispatch by `emagent-mcp--run-tool'.")

(defvar emagent-tools--acp-session-p nil
  "When non-nil, skip Emacs-side tool confirmation for this call.
ACP chat sessions use `session/request_permission' instead; a second MCP
prompt would not block the agent and is ignored.")

(defun emagent-tools--remember-allowed-tool (tool-name)
  "Record TOOL-NAME as allowed for this session and persist it when possible."
  (unless (memq tool-name emagent-tools--session-allowed-tools)
    (push tool-name emagent-tools--session-allowed-tools))
  (when (functionp emagent-tools-allow-all-function)
    (funcall emagent-tools-allow-all-function tool-name)))

(defun emagent-tools--allowed-p (tool-name)
  "Return non-nil when TOOL-NAME is allowed without confirmation."
  (or (memq tool-name emagent-allowed-tools)
      (memq tool-name emagent-tools--session-allowed-tools)))

(cl-defun emagent-tools--write-diff-string-async (callback resolved new-content)
  "Compare RESOLVED with NEW-CONTENT; call CALLBACK with (diff is-error)."
  (unless (executable-find "diff")
    (funcall callback nil nil)
    (cl-return-from emagent-tools--write-diff-string-async))
  (let ((old-file (make-temp-file "emagent-old-"))
        (new-file (make-temp-file "emagent-new-")))
    (if (file-exists-p resolved)
        (copy-file resolved old-file t)
      (write-region "" nil old-file nil 'quiet))
    (write-region new-content nil new-file nil 'quiet)
    (emagent-tools--run-process-async
     (lambda (output is-error)
       (ignore-errors (delete-file old-file))
       (ignore-errors (delete-file new-file))
       ;; diff exits 1 when the files differ — that is the success case
       ;; here, not an error.  Distinguish it from real trouble (exit 2)
       ;; by the unified-diff header.
       (if (or (string-empty-p output)
               (and is-error (not (string-prefix-p "---" output))))
           (funcall callback nil nil)
         (funcall callback output nil)))
     "diff" "-u"
     "--label" (concat (file-name-nondirectory resolved) " (current)")
     "--label" (concat (file-name-nondirectory resolved) " (proposed)")
     old-file new-file)))

(defun emagent-tools--diff-strings (name old-content new-content)
  "Return a unified diff between OLD-CONTENT and NEW-CONTENT strings, or nil.
NAME labels the sides as `NAME (current)' / `NAME (proposed)'.  Returns nil
when the contents are identical or the diff binary is unavailable."
  (when (executable-find "diff")
    (let ((old-file (make-temp-file "emagent-old-"))
          (new-file (make-temp-file "emagent-new-")))
      (unwind-protect
          (progn
            (write-region old-content nil old-file nil 'quiet)
            (write-region new-content nil new-file nil 'quiet)
            (with-temp-buffer
              (call-process "diff" nil t nil "-u"
                            "--label" (concat name " (current)")
                            "--label" (concat name " (proposed)")
                            old-file new-file)
              (unless (= (point-min) (point-max))
                (buffer-string))))
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))))))

(defun emagent-tools--write-diff-string (resolved new-content)
  "Return a unified diff string comparing RESOLVED with NEW-CONTENT, or nil."
  (emagent-tools--diff-strings
   (file-name-nondirectory resolved)
   (if (file-exists-p resolved)
       (with-temp-buffer
         (insert-file-contents resolved)
         (buffer-string))
     "")
   new-content))

(defun emagent-tools--confirm-write (tool-name resolved new-content &optional chat-buffer)
  "Show diff of NEW-CONTENT vs RESOLVED in CHAT-BUFFER with inline buttons.
Inserts a #+begin_src diff block (when changes exist) followed by Allow /
Allow all / Deny buttons; the whole block is removed after the decision.
Falls back to a minibuffer prompt when CHAT-BUFFER is unavailable.
Returns non-nil when the write is approved.

When `emagent-tools--acp-session-p' is set, return t — ACP handles permission.

Arguments: TOOL-NAME."
  (if (or emagent-tools--acp-session-p (emagent-tools--allowed-p tool-name))
      t
    (let* ((diff (emagent-tools--write-diff-string resolved new-content))
           (preamble (when diff (format "\n#+begin_src diff\n%s#+end_src" diff)))
           (choice nil))
      (emagent-tools--buttons-prompt
       (format "Write %s?" (file-name-nondirectory resolved))
       '(("Allow" . yes) ("Allow all" . all) ("Deny" . no))
       chat-buffer
       (lambda (v) (setq choice v))
       preamble)
      (pcase choice
        ('all (emagent-tools--remember-allowed-tool tool-name) t)
        ('yes t)
        (_ nil)))))

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

(defvaralias 'emagent-tools-eval-blocked-symbols 'emagent-policy-elisp-blocked-symbols)

(defvaralias 'emagent-tools-eval-dangerous-symbols 'emagent-policy-elisp-dangerous-symbols)

(defgroup emagent-tools nil
  "Emacs tool handlers for emagent."
  :group 'emagent)

(defvar emagent-tools--project-directory nil
  "Project directory for the active emagent session.")

(defvar emagent-tools--root-boundary nil
  "When non-nil, the absolute directory emagent file tools must stay within.

Bound per session by the emagent MCP dispatcher so a tool call cannot reach
outside the session's project root.  Nil disables the check (the historical
behaviour for non-MCP call sites).")

(defun emagent-tools-set-project-directory (directory)
  "Set project DIRECTORY used by emagent-tool-* when PATH is omitted."
  (setq emagent-tools--project-directory
        (and directory (expand-file-name directory))))

(defun emagent-tool-project-directory ()
  "Return the emagent session project directory as a string."
  (emagent-tools--root-directory nil))

(defcustom emagent-tools-show-written-buffer nil
  "How to reveal a file after emagent writes it.

nil        — do nothing (default; agent writes never touch the window layout)
t          — display the buffer
magit-diff — run `magit-diff-buffer-file' (falls back to `display-buffer'
             when magit is unavailable or the file is outside a git repo)"
  :type '(choice (const :tag "Don't show" nil)
                 (const :tag "Display buffer" t)
                 (const :tag "Magit diff" magit-diff))
  :group 'emagent-tools)

(provide 'emagent-tools)
;;; emagent-tools.el ends here
