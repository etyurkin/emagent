;;; emagent-trust-claude.el --- Claude Code workspace trust -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.0

;;; Commentary:
;;
;; Reads and updates =~/.claude.json= `projects' entries the same way Claude Code
;; does for `hasTrustDialogAccepted' (including walking parent directories when
;; checking trust).
;;
;; A directory is *untrusted* when there is no `projects' entry, when
;; `hasTrustDialogAccepted' is missing or JSON false, or when it is explicitly
;; false.  Only JSON true counts as trusted at that path.

;;; Code:

(require 'emagent-trust)

(defcustom emagent-trust-claude-json-file
  (expand-file-name "~/.claude.json")
  "Claude Code user state file (`projects' map per absolute directory).

Each project value may include `hasTrustDialogAccepted'.  Trust applies only
when that field is JSON true; a missing entry, a missing field, or JSON false
means not trusted at that path (parent directories are still checked, as in
the `claude' CLI)."
  :type 'file
  :group 'emagent-trust)

(defun emagent-trust-claude--has-trust-accepted-p (cell)
  "Return non-nil when project CELL records explicit trust.

In the JSON file the flag is the boolean true; after `json-read' it is the
symbol `t'.  Absent key, JSON false (`:json-false'), or any other value is not
accepted at this path."
  (and cell
       (json-alist-p cell)
       (eq (alist-get "hasTrustDialogAccepted" cell nil nil #'equal) t)))

(defun emagent-trust-claude--projects-table (path)
  "Return (ROOT . PROJECTS) for PATH, creating `projects' if missing."
  (let ((root (emagent-trust--json-read-file path)))
    (unless (or (null root) (json-alist-p root))
      (error "Expected JSON object at top level: %s" path))
    (unless root (setq root '()))
    (let* ((entry (assoc "projects" root #'equal))
           (projects nil))
      (unless entry
        (setq entry (cons "projects" '()))
        (setq root (cons entry root)))
      (setq projects (cdr entry))
      (when (and projects (not (json-alist-p projects)))
        (user-error
         "Key `projects' must be a JSON object in %s (repair the file, then retry)"
         path))
      (cons root projects))))

(defun emagent-trust-claude-trusted-p (directory)
  "Return non-nil when DIRECTORY is trusted per Claude Code.

Walks upward like the CLI; see `emagent-trust-claude--has-trust-accepted-p'."
  (let* ((dir (emagent-trust--normalize-dir directory))
         (path emagent-trust-claude-json-file)
         (projects (cdr (emagent-trust-claude--projects-table path)))
         (p dir)
         (trusted nil))
    (while (and p (not trusted))
      (when-let ((cell (alist-get p projects nil nil #'equal)))
        (when (emagent-trust-claude--has-trust-accepted-p cell)
          (setq trusted t)))
      (unless trusted
        (setq p (unless (string= p "/")
                  (file-name-directory (directory-file-name p))))
        (when p
          (setq p (directory-file-name (expand-file-name p))))))
    trusted))

(defun emagent-trust-claude-record-trust (directory)
  "Set `hasTrustDialogAccepted' true for DIRECTORY in the Claude JSON file.

Preserves other keys on the same `projects' entry (e.g. fixes explicit false)."
  (let* ((dir (emagent-trust--normalize-dir directory))
         (path emagent-trust-claude-json-file)
         (pair (emagent-trust-claude--projects-table path))
         (root (car pair))
         (entry (assoc "projects" root #'equal))
         (projects (cdr entry)))
    (let ((cell (alist-get dir projects nil nil #'equal)))
      (cond
       ((and cell (json-alist-p cell))
        (setf (alist-get "hasTrustDialogAccepted" cell nil nil #'equal) t))
       (t
        (setf (alist-get dir projects nil nil #'equal)
              (list (cons "hasTrustDialogAccepted" t))))))
    ;; setf on an empty `projects' alist can replace the local list without
    ;; updating the cons on ROOT; sync the entry before writing.
    (setcdr entry projects)
    (emagent-trust--json-write-file root path)))

(provide 'emagent-trust-claude)

;;; emagent-trust-claude.el ends here
