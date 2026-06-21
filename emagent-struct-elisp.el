;;; emagent-struct-elisp.el --- Elisp structural editing plugin for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'emagent-struct)
(require 'emagent-elisp)

(declare-function emagent-tools--eval-form-guard "emagent-tools")
(declare-function emagent-tools--eval-form-execute "emagent-tools")

(defun emagent-struct-elisp--before-save (node _path)
  "Return error string when NODE must not be saved/eval'd yet."
  (when emagent-elisp-eval-after-structural-edit
    (emagent-tools--eval-form-guard node)))

(defun emagent-struct-elisp--after-save (node _path)
  "Eval NODE after structural save when configured."
  (when emagent-elisp-eval-after-structural-edit
    (emagent-tools--eval-form-execute node)))

(emagent-struct-register-plugin
 `(:id elisp
   :node-label "form"
   :file-p ,#'emagent-elisp-elisp-file-p
   :treesit-available-p ,#'emagent-elisp-treesit-available-p
   :validate-on-write-p (lambda () emagent-elisp-validate-on-write)
   :validate-content ,#'emagent-elisp--validate-content-strict
   :check-file ,#'emagent-elisp-check-file-content
   :check-node (lambda (node _path)
                 (emagent-elisp-check-form node))
   :outline ,#'emagent-elisp-sexp-tree
   :node-bounds ,#'emagent-elisp-defun-bounds
   :replace-node ,#'emagent-elisp-replace-defun
   :insert-after ,#'emagent-elisp-insert-after-form
   :before-save ,#'emagent-struct-elisp--before-save
   :after-save ,#'emagent-struct-elisp--after-save))

(provide 'emagent-struct-elisp)

;;; emagent-struct-elisp.el ends here
