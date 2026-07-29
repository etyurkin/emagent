;;; emagent-embark.el --- Optional embark integration for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.3.0
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
;; Adds an embark target type `emagent-src-block' for #+BEGIN_SRC blocks
;; inside emagent response regions.  When you call `embark-act' with point
;; on a src block in an emagent buffer, you get actions to copy the code,
;; insert it into another buffer, or execute it with org-babel.
;;
;; Activated automatically when embark is loaded; no configuration needed.

;;; Code:

(defvar embark-target-finders)
(defvar embark-keymap-alist)

(defun emagent-embark--src-block-target ()
  "Embark target for a #+BEGIN_SRC block in an emagent response."
  (when (and (derived-mode-p 'emagent-mode)
             (org-in-src-block-p))
    (let* ((element (org-element-at-point))
           (value (org-element-property :value element)))
      (when value
        (let ((code (string-trim value)))
          (cons 'emagent-src-block code))))))

(defun emagent-embark-copy-src-block (code)
  "Copy CODE to the kill ring."
  (interactive "sCode: ")
  (kill-new code)
  (message "emagent: copied src block (%d chars)" (length code)))

(defun emagent-embark-insert-src-block (code)
  "Insert CODE at point in the selected buffer."
  (interactive "sCode: ")
  (let* ((others (seq-filter (lambda (b) (not (eq b (current-buffer))))
                             (buffer-list)))
         (target (get-buffer
                  (completing-read "Insert into buffer: "
                                   (mapcar #'buffer-name others) nil t))))
    (with-current-buffer target
      (insert code))
    (message "emagent: inserted src block into %s" (buffer-name target))))

(defvar emagent-embark-src-block-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'emagent-embark-copy-src-block)
    (define-key map "i" #'emagent-embark-insert-src-block)
    (define-key map "e" #'org-babel-execute-src-block)
    map)
  "Embark keymap for emagent src blocks.
Actions: c=copy, i=insert into buffer, e=execute with org-babel.")

(defun emagent-embark--maybe-register ()
  "Register the emagent embark target and actions when embark is loaded."
  (when (boundp 'embark-keymap-alist)
    (when (boundp 'embark-general-map)
      (set-keymap-parent emagent-embark-src-block-map embark-general-map))
    (add-to-list 'embark-target-finders #'emagent-embark--src-block-target)
    (add-to-list 'embark-keymap-alist
                 '(emagent-src-block . emagent-embark-src-block-map))))

;; Register on first emagent buffer activation; by then embark is loaded
;; if the user uses it.  Avoids configuring embark at load time.
(add-hook 'emagent-mode-hook #'emagent-embark--maybe-register)

(provide 'emagent-embark)

;;; emagent-embark.el ends here
