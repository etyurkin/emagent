;;; emagent-load-path.el --- Register emagent `lisp/' subdirs on load-path -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6

;; SPDX-License-Identifier: MIT

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

;; Register emagent lisp/ subdirectories on load-path.

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
  
  "Internal helper."
  (when (fboundp 'elpaca-recipe-functions)
    (add-hook 'elpaca-recipe-functions #'emagent--elpaca-recipe nil t)))

(provide 'emagent-load-path)

;;; emagent-load-path.el ends here
