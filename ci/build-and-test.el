;;; build-and-test.el --- Build emagent with Elpaca and run ERT -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; CI entry point: mirrors the Elpaca recipe from README.org (link, autoloads,
;; byte-compile, activate), then runs the ERT suite against the built artifacts.

;;; Code:

(require 'cl-lib)

(defvar ci--root
  (file-name-as-directory
   (or (getenv "EMAGENT_ROOT")
       (getenv "GITHUB_WORKSPACE")
       (expand-file-name ".." load-file-name))))

(defvar ci--elpaca-dir
  (file-name-as-directory
   (or (getenv "ELPACA_DIR")
       (expand-file-name ".elpaca-vendor/elpaca/" ci--root))))

(defvar ci--store
  (expand-file-name ".elpaca-ci/" ci--root))

(defun ci--require-elpaca ()
  (unless (featurep 'elpaca)
    (add-to-list 'load-path ci--elpaca-dir)
    (load (expand-file-name "elpaca.el" ci--elpaca-dir) nil t))
  (require 'elpaca-git)
  (setq elpaca-menu-functions nil
        elpaca-order-functions '((lambda (_) '(:inherit nil :type git)))))

(defun ci--elpaca-order ()
  (list 'emagent
        :type 'git
        :repo ci--root
        :build '(:not elpaca-source elpaca-queue-dependencies elpaca-check-version
                 elpaca-build-autoloads elpaca-build-docs)
        :files (list :defaults (cons "lisp" '("lisp/*/*.el")))))

(defun ci--elpaca-build ()
  (setq default-directory ci--root
        elpaca-directory ci--store
        elpaca-sources-directory (expand-file-name "sources/" ci--store)
        elpaca-builds-directory (expand-file-name "builds/" ci--store))
  (make-directory elpaca-directory t)
  (ci--require-elpaca)
  (catch 'elpaca-abort
    (elpaca--expand-declaration (ci--elpaca-order) nil))
  (elpaca-wait)
  (let ((e (elpaca-get 'emagent)))
    (unless (and e (eq (elpaca<-status e) 'finished))
      (error "Elpaca build failed (status %S)" (and e (elpaca<-status e))))
    (elpaca<-build-dir e)))

(defun ci--register-load-path (root)
  (setq load-path (delq nil load-path))
  (add-to-list 'load-path root)
  (let ((lisp (expand-file-name "lisp" root)))
    (when (file-directory-p lisp)
      (dolist (name (directory-files lisp nil "^[^.]"))
        (let ((path (expand-file-name name lisp)))
          (when (file-directory-p path)
            (add-to-list 'load-path path)))))))

(defun ci--run-tests (build-dir)
  (ci--register-load-path build-dir)
  (let ((tests (expand-file-name "tests/" ci--root)))
    (load (expand-file-name "emagent-test-utils.el" tests) nil t)
    (dolist (file (sort (directory-files tests t "^emagent-.*-test\\.el$") #'string<))
      (unless (string-match-p "emagent-test-utils\\.el$" file)
        (load file nil t)))
    (ert-run-tests-batch-and-exit)))

(let ((build-dir (ci--elpaca-build)))
  (ci--run-tests build-dir))

;;; build-and-test.el ends here
