;;; check-declare.el --- Fail CI on check-declare warnings -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Batch `check-declare' over Melpa recipe sources (emagent.el + lisp/**/*.el).
;; Exits non-zero when any declaration is wrong or the defining file is missing.
;;
;;   emacs -Q --batch -l ci/check-declare.el -f emagent-check-declare-batch

;;; Code:

(require 'check-declare)
(require 'cl-lib)

(defvar emagent-check-declare--root
  (file-name-as-directory
   (or (getenv "EMAGENT_ROOT")
       (expand-file-name ".." (file-name-directory load-file-name)))))

(defun emagent-check-declare--files ()
  "Return Melpa recipe Elisp files under `emagent-check-declare--root'."
  (cons (expand-file-name "emagent.el" emagent-check-declare--root)
        (directory-files-recursively
         (expand-file-name "lisp" emagent-check-declare--root)
         "\\.el\\'")))

(defun emagent-check-declare--flatten (errors)
  "Flatten ERRORS from `check-declare-file' into (DEF-FILE SRC FN REASON) lists."
  (let (out)
    (dolist (group errors)
      (let ((def-file (car group))
            (entries (cdr group)))
        (dolist (entry entries)
          (push (list def-file (nth 0 entry) (nth 1 entry) (nth 2 entry))
                out))))
    (nreverse out)))

(defun emagent-check-declare-batch ()
  "Run check-declare on recipe files; kill Emacs with status 1 on errors."
  (let* ((root emagent-check-declare--root)
         (default-directory root)
         (load-path (append
                     (list root)
                     (directory-files
                      (expand-file-name "lisp" root) t "\\`[^.]")
                     load-path))
         (files (emagent-check-declare--files))
         (errors (emagent-check-declare--flatten
                  (cl-loop for file in files
                           append (check-declare-file file)))))
    (if (null errors)
        (progn
          (message "check-declare: clean (%d files)" (length files))
          (kill-emacs 0))
      (dolist (err errors)
        (message "%s: %s: %s" (nth 1 err) (nth 2 err) (nth 3 err)))
      (message "check-declare: %d error(s)" (length errors))
      (kill-emacs 1))))

(provide 'emagent-ci-check-declare)

;;; check-declare.el ends here
