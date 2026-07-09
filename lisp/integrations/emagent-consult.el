;;; emagent-consult.el --- Optional consult integration for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.0

;;; Commentary:
;;
;; Adds an emagent section to `consult-buffer' so active emagent session
;; buffers appear in their own group alongside files, recentf, etc.
;;
;; Activated automatically when consult is loaded; no configuration needed.

;;; Code:

(declare-function consult--buffer-state "consult")
(defvar consult-buffer-sources)

(defvar emagent-consult--source
  (list :name "Emagent"
        :narrow ?e
        :category 'buffer
        :face 'consult-buffer
        :history 'buffer-name-history
        :state (lambda () (consult--buffer-state))
        :default t
        :items (lambda ()
                 (mapcar #'buffer-name
                         (seq-filter (lambda (b)
                                       (eq (buffer-local-value 'major-mode b)
                                           'emagent-mode))
                                     (buffer-list)))))
  "Consult source listing active emagent session buffers.")

(defun emagent-consult--maybe-register ()
  "Add the emagent source to `consult-buffer-sources' when consult is loaded."
  (when (boundp 'consult-buffer-sources)
    (add-to-list 'consult-buffer-sources 'emagent-consult--source 'append)))

;; Register on first emagent buffer activation; by then consult is loaded
;; if the user uses it.  Avoids configuring consult at load time.
(add-hook 'emagent-mode-hook #'emagent-consult--maybe-register)

(provide 'emagent-consult)

;;; emagent-consult.el ends here
