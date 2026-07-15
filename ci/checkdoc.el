;;; checkdoc.el --- Fail CI on any checkdoc warning -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Batch checkdoc over Melpa recipe sources (emagent.el + lisp/**/*.el).
;; Exits non-zero when any warning remains.
;;
;;   emacs -Q --batch -l ci/checkdoc.el -f emagent-checkdoc-batch

;;; Code:

(require 'checkdoc)
(require 'cl-lib)

(defvar emagent-checkdoc--root
  (file-name-as-directory
   (or (getenv "EMAGENT_ROOT")
       (expand-file-name ".." (file-name-directory load-file-name)))))

(defvar emagent-checkdoc--count 0)

(defun emagent-checkdoc--create-error (text start end &optional unfixable)
  "Record TEXT at START..END for CI; never auto-fix.
UNFIXABLE is accepted for API compatibility and ignored."
  (ignore unfixable)
  (setq emagent-checkdoc--count (1+ emagent-checkdoc--count))
  (let ((line (save-excursion
                (goto-char (or start (point)))
                (line-number-at-pos))))
    (message "%s:%d: %s"
             (or buffer-file-name "<unknown>")
             line
             text))
  ;; Return a checkdoc error structure so callers continue scanning.
  (checkdoc--create-error-for-checkdoc text start end unfixable))

(defun emagent-checkdoc--files ()
  "Return Melpa recipe Elisp files under `emagent-checkdoc--root'."
  (cons (expand-file-name "emagent.el" emagent-checkdoc--root)
        (directory-files-recursively
         (expand-file-name "lisp" emagent-checkdoc--root)
         "\\.el\\'")))

(defun emagent-checkdoc-batch ()
  "Run checkdoc on recipe files; kill Emacs with status 1 on warnings."
  (setq checkdoc-autofix-flag 'never
        checkdoc-create-error-function #'emagent-checkdoc--create-error
        emagent-checkdoc--count 0)
  (dolist (file (emagent-checkdoc--files))
    (with-current-buffer (find-file-noselect file)
      (checkdoc-current-buffer t)
      (kill-buffer (current-buffer))))
  (if (zerop emagent-checkdoc--count)
      (progn
        (message "checkdoc: clean (%d files)"
                 (length (emagent-checkdoc--files)))
        (kill-emacs 0))
    (message "checkdoc: %d warning(s)" emagent-checkdoc--count)
    (kill-emacs 1)))

(provide 'emagent-ci-checkdoc)

;;; checkdoc.el ends here
