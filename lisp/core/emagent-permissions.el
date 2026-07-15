;;; emagent-permissions.el --- ~/.emagent permission store -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; SPDX-License-Identifier: MIT

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6

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

;; Persists global, session, and project permission choices under
;; `emagent-permissions-directory' (default =~/.emagent=).

;;; Code:

(require 'cl-lib)
(require 'emagent-trust)

(defgroup emagent-permissions nil
  "Persistent permission storage for emagent."
  :group 'emagent
  :prefix "emagent-permissions-")

(defcustom emagent-permissions-directory
  (expand-file-name ".emagent" "~")
  "Directory for emagent permission files.

Layout:
- global.json — fingerprints from \"Allow always\"
- sessions/SESSION.json — per-ACP-session fingerprints and allow-all flag
- projects/HASH.json — per-project MCP tools and permission fingerprints"
  :type 'directory
  :group 'emagent-permissions)

(defvar emagent-permissions--cache (make-hash-table :test 'equal)
  "Cache of (FILE-MTIME . DATA) keyed by absolute file path.")

(defun emagent-permissions--ensure-directory (subdir)
  "Return SUBDIR under `emagent-permissions-directory', creating it."
  (let ((dir (expand-file-name subdir emagent-permissions-directory)))
    (make-directory dir t)
    dir))

(defun emagent-permissions--safe-filename (name)
  "Return a filesystem-safe form of NAME."
  (replace-regexp-in-string "[^a-zA-Z0-9._-]+" "_" name))

(defun emagent-permissions--global-file ()
  
  "Internal helper."
  (expand-file-name "global.json" emagent-permissions-directory))

(defun emagent-permissions--session-file (session-id)
  
  "Internal helper for SESSION-ID."
  (expand-file-name
   (format "%s.json" (emagent-permissions--safe-filename session-id))
   (emagent-permissions--ensure-directory "sessions")))

(defun emagent-permissions--project-file (directory)
  
  "Internal helper for DIRECTORY."
  (when directory
    (let ((norm (emagent-trust--normalize-dir directory)))
      (expand-file-name
       (format "%s.json" (secure-hash 'sha1 norm))
       (emagent-permissions--ensure-directory "projects")))))

(defun emagent-permissions--invalidate (path)
  
  "Internal helper for PATH."
  (when path (remhash path emagent-permissions--cache)))

(defun emagent-permissions--file-mtime (path)
  
  "Internal helper for PATH."
  (when (file-readable-p path)
    (file-attribute-modification-time (file-attributes path))))

(defun emagent-permissions--read-json (path)
  
  "Internal helper for PATH."
  (or (emagent-trust--json-read-file path) nil))

(defun emagent-permissions--write-json (path object)
  
  "Internal helper for PATH and OBJECT."
  (make-directory emagent-permissions-directory t)
  (emagent-trust--json-write-file object path)
  (emagent-permissions--invalidate path))

(defun emagent-permissions--cached (path reader)
  
  "Internal helper for PATH and READER."
  (let ((mtime (emagent-permissions--file-mtime path)))
    (if-let ((entry (gethash path emagent-permissions--cache)))
        (if (equal (car entry) mtime)
            (cdr entry)
          (let ((data (funcall reader path)))
            (puthash path (cons mtime data) emagent-permissions--cache)
            data))
      (let ((data (funcall reader path)))
        (puthash path (cons mtime data) emagent-permissions--cache)
        data))))

(defun emagent-permissions--vector-to-list (value)
  
  "Internal helper for VALUE."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun emagent-permissions--fingerprints (data)
  
  "Internal helper for DATA."
  (emagent-permissions--vector-to-list (cdr (assoc "fingerprints" data))))

(defun emagent-permissions--tools (data)
  
  "Internal helper for DATA."
  (mapcar #'intern (emagent-permissions--vector-to-list (cdr (assoc "tools" data)))))

(defun emagent-permissions--auto-approve-p (data)
  
  "Internal helper for DATA."
  (let ((value (cdr (assoc "autoApprove" data))))
    (and value (not (eq value :json-false)) (not (eq value :false)))))

(defun emagent-permissions--maybe-migrate-legacy-global ()
  
  "Internal helper."
  (let* ((legacy (expand-file-name "allowed-permissions" emagent-permissions-directory))
         (global (emagent-permissions--global-file)))
    (when (and (file-readable-p legacy) (not (file-readable-p global)))
      (let (fingerprints)
        (with-temp-buffer
          (insert-file-contents legacy)
          (dolist (line (split-string (buffer-string) "\n" t))
            (setq fingerprints
                  (append fingerprints (split-string line "[ \t]+" t)))))
        (emagent-permissions--write-json
         global `((fingerprints . ,(vconcat (delete-dups fingerprints)))))))))

(defun emagent-permissions--read-global-data (path)
  
  "Internal helper for PATH."
  (emagent-permissions--maybe-migrate-legacy-global)
  (or (emagent-permissions--read-json path)
      '((fingerprints . []))))

(defun emagent-permissions--read-session-data (session-id path)
  
  "Internal helper for SESSION-ID and PATH."
  (or (emagent-permissions--read-json path)
      `((sessionId . ,session-id)
        (fingerprints . [])
        (autoApprove . :json-false))))

(defun emagent-permissions--read-project-data (directory path)
  
  "Internal helper for DIRECTORY and PATH."
  (or (emagent-permissions--read-json path)
      `((directory . ,(emagent-trust--normalize-dir directory))
        (fingerprints . [])
        (tools . []))))

(defun emagent-permissions-global-fingerprints ()
  "Return globally allowed ACP permission fingerprints."
  (emagent-permissions--fingerprints
   (emagent-permissions--cached (emagent-permissions--global-file)
                                #'emagent-permissions--read-global-data)))

(defun emagent-permissions-add-global-fingerprint (fingerprint)
  "Persist FINGERPRINT as globally allowed."
  (when fingerprint
    (let* ((path (emagent-permissions--global-file))
           (data (emagent-permissions--read-global-data path))
           (merged (append (emagent-permissions--fingerprints data)
                           (list fingerprint))))
      (unless (member fingerprint (emagent-permissions--fingerprints data))
        (emagent-permissions--write-json
         path `((fingerprints . ,(vconcat (delete-dups merged)))))))))

(defun emagent-permissions-session-fingerprints (session-id)
  "Return session-scoped fingerprints for SESSION-ID."
  (when (and session-id (not (string-empty-p session-id)))
    (emagent-permissions--fingerprints
     (emagent-permissions--cached (emagent-permissions--session-file session-id)
                                  (lambda (path)
                                    (emagent-permissions--read-session-data session-id path))))))

(defun emagent-permissions-add-session-fingerprint (session-id fingerprint)
  "Persist FINGERPRINT for SESSION-ID."
  (when (and session-id fingerprint (not (string-empty-p session-id)))
    (let* ((path (emagent-permissions--session-file session-id))
           (data (emagent-permissions--read-session-data session-id path))
           (merged (append (emagent-permissions--fingerprints data)
                           (list fingerprint))))
      (unless (member fingerprint (emagent-permissions--fingerprints data))
        (emagent-permissions--write-json
         path `((sessionId . ,session-id)
                (fingerprints . ,(vconcat (delete-dups merged)))
                (autoApprove . ,(if (emagent-permissions--auto-approve-p data) t :json-false))))))))

(defun emagent-permissions-session-auto-approve-p (session-id)
  "Return non-nil when SESSION-ID has allow-all enabled."
  (when (and session-id (not (string-empty-p session-id)))
    (emagent-permissions--auto-approve-p
     (emagent-permissions--cached (emagent-permissions--session-file session-id)
                                  (lambda (path)
                                    (emagent-permissions--read-session-data session-id path))))))

(defun emagent-permissions-set-session-auto-approve (session-id)
  "Enable allow-all for SESSION-ID."
  (when (and session-id (not (string-empty-p session-id)))
    (let* ((path (emagent-permissions--session-file session-id))
           (data (emagent-permissions--read-session-data session-id path)))
      (unless (emagent-permissions--auto-approve-p data)
        (emagent-permissions--write-json
         path `((sessionId . ,session-id)
                (fingerprints . ,(vconcat (emagent-permissions--fingerprints data)))
                (autoApprove . t)))))))

(defun emagent-permissions-project-fingerprints (directory)
  "Return project-scoped fingerprints for DIRECTORY."
  (when-let ((path (emagent-permissions--project-file directory)))
    (emagent-permissions--fingerprints
     (emagent-permissions--cached path
                                  (lambda (_path)
                                    (emagent-permissions--read-project-data directory path))))))

(defun emagent-permissions-project-tools (directory)
  "Return project-allowed MCP tool symbols for DIRECTORY."
  (when-let ((path (emagent-permissions--project-file directory)))
    (emagent-permissions--tools
     (emagent-permissions--cached path
                                  (lambda (_path)
                                    (emagent-permissions--read-project-data directory path))))))

(defun emagent-permissions-add-project-tool (directory tool)
  "Persist TOOL as allowed for DIRECTORY."
  (when (and directory tool)
    (let* ((sym (if (stringp tool) (intern tool) tool))
           (path (emagent-permissions--project-file directory))
           (data (emagent-permissions--read-project-data directory path))
           (merged (append (emagent-permissions--tools data) (list sym))))
      (unless (memq sym (emagent-permissions--tools data))
        (emagent-permissions--write-json
         path `((directory . ,(emagent-trust--normalize-dir directory))
                (fingerprints . ,(vconcat (emagent-permissions--fingerprints data)))
                (tools . ,(vconcat (mapcar #'symbol-name (delete-dups merged))))))))))

(defun emagent-permissions-reset-global ()
  "Clear all globally allowed permission fingerprints."
  (let ((path (emagent-permissions--global-file)))
    (emagent-permissions--write-json path '((fingerprints . [])))))

(defun emagent-permissions-reset-session (session-id)
  "Clear all permissions stored for SESSION-ID."
  (when (and session-id (not (string-empty-p session-id)))
    (let ((path (emagent-permissions--session-file session-id)))
      (emagent-permissions--write-json
       path `((sessionId . ,session-id)
               (fingerprints . [])
               (autoApprove . :json-false))))))

(defun emagent-permissions-reset-project (directory)
  "Clear all permissions stored for DIRECTORY."
  (when-let ((path (emagent-permissions--project-file directory)))
    (emagent-permissions--write-json
     path `((directory . ,(emagent-trust--normalize-dir directory))
             (fingerprints . [])
             (tools . [])))))

(provide 'emagent-permissions)
;;; emagent-permissions.el ends here
