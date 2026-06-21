;;; emagent-load-path.el --- Register emagent `lisp/' subdirs on load-path -*- lexical-binding: t; -*-

;;; Code:

(defconst emagent-elpaca-files
  '(:defaults ("lisp" . ("lisp/*/*.el")))
  "Elpaca :files recipe for grouped `lisp/' modules.")

(defun emagent--register-load-path (root)
  "Add subdirectories of ROOT/lisp/ to `load-path'."
  (let ((lisp (expand-file-name "lisp" root)))
    (when (file-directory-p lisp)
      (dolist (name (directory-files lisp nil "^[^.]"))
        (let ((path (expand-file-name name lisp)))
          (when (file-directory-p path)
            (add-to-list 'load-path path)))))))

(defun emagent--elpaca-recipe (recipe)
  "Merge `emagent-elpaca-files' into Elpaca RECIPE for package emagent."
  (when (equal (plist-get recipe :package) "emagent")
    (list :files emagent-elpaca-files)))

(defun emagent--register-elpaca-recipe ()
  (when (fboundp 'elpaca-recipe-functions)
    (add-hook 'elpaca-recipe-functions #'emagent--elpaca-recipe nil t)))

(provide 'emagent-load-path)

;;; emagent-load-path.el ends here
