;;; emagent-trust-cursor.el --- Cursor CLI workspace trust -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.8
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
;; Reads and writes =~/.cursor/projects/<slug>/.workspace-trusted= the same way
;; Cursor associates a trusted workspace path with the CLI.

;;; Code:

(require 'emagent-trust)

(defcustom emagent-trust-cursor-config-dir
  (expand-file-name "~/.cursor")
  "Cursor configuration directory (contains `projects/' trust markers)."
  :type 'directory
  :group 'emagent-trust)

(defun emagent-trust-cursor--project-slug (directory)
  "Return Cursor project slug for DIRECTORY (under projects/ in config dir)."
  (let ((abs (emagent-trust--normalize-dir directory)))
    (if (string= abs "/")
        ""
      (replace-regexp-in-string "/" "-" (string-remove-prefix "/" abs)))))

(defun emagent-trust-cursor--trust-file (directory)
  "Return the path to Cursor's `.workspace-trusted' marker for DIRECTORY."
  (expand-file-name
   (format "projects/%s/.workspace-trusted"
           (emagent-trust-cursor--project-slug directory))
   emagent-trust-cursor-config-dir))

(defun emagent-trust-cursor-trusted-p (directory)
  "Return non-nil when Cursor's trust marker exists and matches DIRECTORY."
  (let ((tf (emagent-trust-cursor--trust-file directory)))
    (and (file-readable-p tf)
         (condition-case nil
             (let* ((data (emagent-trust--json-read-file tf))
                    (wp (alist-get "workspacePath" data nil nil #'equal)))
               (and (stringp wp)
                    (string= (emagent-trust--normalize-dir wp)
                             (emagent-trust--normalize-dir directory))))
           (json-error nil)))))

(defun emagent-trust-cursor-record-trust (directory)
  "Write Cursor's `.workspace-trusted' marker for DIRECTORY."
  (let* ((dir (emagent-trust--normalize-dir directory))
         (tf (emagent-trust-cursor--trust-file directory))
         (obj (list (cons "trustedAt" (emagent-trust--iso8601-utc-now))
                    (cons "workspacePath" dir))))
    (emagent-trust--json-write-file obj tf)))

(provide 'emagent-trust-cursor)

;;; emagent-trust-cursor.el ends here
