;;; package-lint.el --- Batch package-lint for emagent -*- lexical-binding: t; -*-

;; Run via: make package-lint
;; Installs package-lint from MELPA when missing, then lints emagent.el.

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'package-lint)
  (package-refresh-contents)
  (package-install 'package-lint))

(require 'package-lint)

(defun emagent-package-lint-batch ()
  "Lint `emagent.el' with package-lint and exit non-zero on findings."
  (let* ((root (or (getenv "EMAGENT_ROOT") default-directory))
         (file (expand-file-name "emagent.el" root))
         (package-lint-main-file file)
         (package-lint-batch-fail-on-warnings t))
    (unless (file-readable-p file)
      (error "Cannot read %s" file))
    (unless (package-lint-batch-and-exit-1 (list file))
      (kill-emacs 1))
    (kill-emacs 0)))

(provide 'emagent-ci-package-lint)
;;; package-lint.el ends here
