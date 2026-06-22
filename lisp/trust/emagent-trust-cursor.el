;;; emagent-trust-cursor.el --- Cursor CLI workspace trust -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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
