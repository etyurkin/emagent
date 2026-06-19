;;; emagent-trust-cursor.el --- Cursor CLI workspace trust -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Reads and writes =~/.cursor/projects/<slug>/.workspace-trusted= the same way
;; Cursor associates a trusted workspace path with the CLI.

;;; Code:

(defvar emagent-cursor-acp-extra-args)

(require 'emagent-trust)

(defcustom emagent-trust-cursor-config-dir
  (expand-file-name "~/.cursor")
  "Cursor configuration directory (contains `projects/' trust markers)."
  :type 'directory
  :group 'emagent-trust)

(defcustom emagent-trust-cursor-restricted-extra-args
  '("--approve-mcps" "--sandbox" "enabled")
  "Extra `cursor-agent acp' arguments when the user declines workspace trust.

These replace `emagent-cursor-acp-extra-args' for that session only (see
`emagent-chat-cursor-acp-extra-args').  The default drops `--force' and enables
the agent sandbox compared to emagent's usual `disabled' preset."
  :type '(repeat string)
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

(defun emagent-trust-cursor-extra-args-after-no ()
  "Return `cursor-agent acp' extra args when the user declined trust."
  (copy-sequence emagent-trust-cursor-restricted-extra-args))

(defun emagent-trust-cursor-extra-args-after-yes ()
  "Return `cursor-agent acp' extra args after recording trust (append `--trust')."
  (require 'emagent-cursor)
  (let ((base (copy-sequence emagent-cursor-acp-extra-args)))
    (unless (member "--trust" base)
      (setq base (append base '("--trust"))))
    base))

(provide 'emagent-trust-cursor)

;;; emagent-trust-cursor.el ends here
