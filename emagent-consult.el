;;; emagent-consult.el --- Optional consult integration for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

(with-eval-after-load 'consult
  (add-to-list 'consult-buffer-sources 'emagent-consult--source 'append))

(provide 'emagent-consult)

;;; emagent-consult.el ends here
