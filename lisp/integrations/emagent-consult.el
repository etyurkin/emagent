;;; emagent-consult.el --- Optional consult integration for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.7
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
;; Uses public `:action' and `:state' source fields only (no consult
;; private APIs).

;;; Code:

(defvar consult-buffer-sources)

(defun emagent-consult--buffer-action (cand)
  "Switch to emagent buffer candidate CAND."
  (when-let ((buf (and cand (get-buffer cand))))
    (switch-to-buffer buf)))

(defun emagent-consult--buffer-state ()
  "Return a consult `:state' function for emagent buffer preview.

Implements the public consult multi-source state contract without
calling consult private helpers such as `consult--buffer-state'."
  (let* ((orig-win (selected-window))
         (orig-buf (window-buffer orig-win)))
    (lambda (action cand)
      (pcase action
        ('preview
         (when-let ((buf (and cand (get-buffer cand))))
           (when (and (window-live-p orig-win) (buffer-live-p buf))
             (with-selected-window orig-win
               (switch-to-buffer buf 'norecord)))))
        ((or 'exit 'return)
         (when (and (window-live-p orig-win) (buffer-live-p orig-buf))
           (with-selected-window orig-win
             (switch-to-buffer orig-buf 'norecord))))))))

(defun emagent-consult--items ()
  "Return names of live emagent session buffers."
  (mapcar #'buffer-name
          (seq-filter (lambda (b)
                        (eq (buffer-local-value 'major-mode b)
                            'emagent-mode))
                      (buffer-list))))

(defvar emagent-consult--source
  (list :name "Emagent"
        :narrow ?e
        :category 'buffer
        :face 'consult-buffer
        :history 'buffer-name-history
        :action #'emagent-consult--buffer-action
        :state #'emagent-consult--buffer-state
        :default t
        :items #'emagent-consult--items)
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
