;;; emagent-consult.el --- Optional consult integration for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6
;; SPDX-License-Identifier: MIT
;; Version: 1.2.5

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
