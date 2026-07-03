;;; emagent-tools-file.el --- File operation tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'emagent-log)
(require 'emagent-elisp)

(defvar auto-insert)
(defvar emagent-tools-show-written-buffer)

(declare-function emagent-tools--root-directory "emagent-tools")
(declare-function emagent-tools--buffer-mode "emagent-tools")
(declare-function emagent-struct-write-required-p "emagent-struct")
(declare-function magit-toplevel "magit-git")
(declare-function emagent-tools--run-async-sync "emagent-tools-shell")
(declare-function emagent-tools--run-process-async "emagent-tools-shell")

(defconst emagent-tools--icloud-dir
  (expand-file-name "~/Library/Mobile Documents/"))

(defconst emagent-tools--containers-dir
  (expand-file-name "~/Library/Containers/"))

(defconst emagent-tools--group-containers-dir
  (expand-file-name "~/Library/Group Containers/"))

(defun emagent-tools--protected-fs-path-p (path)
  "Return non-nil when PATH must not be accessed via Emacs on macOS."
  (let ((resolved (file-truename (emagent-tools--root-directory path))))
    (or (string-prefix-p emagent-tools--icloud-dir resolved)
        (string-prefix-p emagent-tools--containers-dir resolved)
        (string-prefix-p emagent-tools--group-containers-dir resolved))))

(defun emagent-tools--file-buffer (path)
  "Return a buffer visiting PATH, visiting it if the file exists."
  (let ((resolved (emagent-tools--root-directory path)))
    (or (find-buffer-visiting resolved)
        (when (file-exists-p resolved)
          (find-file-noselect resolved)))))

(defun emagent-tools--extract-buffer-text (buffer &optional line limit)
  "Return text from BUFFER starting at LINE for LIMIT lines."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (and line (> line 1))
          (forward-line (1- line)))
        (let ((start (point)))
          (if limit
              (forward-line limit)
            (goto-char (point-max)))
          (buffer-substring-no-properties start (point)))))))

(defun emagent-tools--read-file-content (path &optional line limit)
  "Read PATH through Emacs, including unsaved buffer contents."
  (let* ((resolved (emagent-tools--root-directory path))
         (buffer (find-buffer-visiting resolved)))
    (if buffer
        (emagent-tools--extract-buffer-text buffer line limit)
      (with-temp-buffer
        (insert-file-contents resolved)
        (emagent-tools--extract-buffer-text (current-buffer) line limit)))))

(defun emagent-tools--read-elisp-file-content (path)
  "Like `emagent-tools--read-file-content' but return \"\" when PATH is missing."
  (condition-case-unless-debug nil
      (emagent-tools--read-file-content path)
    (file-missing "")))

(defun emagent-tool-read-file (path &optional line limit)
  "Return contents of PATH as a string."
  (when (emagent-tools--protected-fs-path-p path)
    (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                (emagent-tools--root-directory path)))
  (emagent-tools--read-file-content path line limit))

(defun emagent-tools--read-structural-file-content (path)
  "Like `emagent-tools--read-file-content' but return \"\" when PATH is missing."
  (condition-case-unless-debug nil
      (emagent-tools--read-file-content path)
    (file-missing "")))

(defun emagent-tools--write-file-content (path content)
  "Write CONTENT to PATH through an Emacs buffer.
Each call is recorded as a single undoable change in the target buffer."
  (let* ((resolved (emagent-tools--root-directory path))
         (dir (file-name-directory resolved))
         (buffer (or (find-buffer-visiting resolved)
                     (let ((auto-insert nil))
                       (find-file-noselect resolved)))))
    (when (and emagent-elisp-validate-on-write
               (emagent-elisp-elisp-file-p resolved))
      (when-let ((err (emagent-elisp--validate-content-strict content resolved)))
        (user-error "Validation failed for %s: %s" resolved err)))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t))
    (with-temp-buffer
      (insert content)
      (let ((content-buffer (current-buffer))
            (inhibit-read-only t))
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (undo-boundary)
            (replace-buffer-contents content-buffer 1.0)
            (undo-boundary))
          (basic-save-buffer))))
    (pcase emagent-tools-show-written-buffer
      ('magit-diff
       (with-current-buffer buffer
         (if (and (fboundp 'magit-diff-buffer-file) (magit-toplevel))
             (magit-diff-buffer-file)
           (display-buffer buffer))))
      ((pred identity)
       (display-buffer buffer)))
    resolved))

(defun emagent-tools--unified-diff-async (callback old new label)
  "Return unified diff between OLD and NEW for LABEL via CALLBACK."
  (if (string= old new)
      (funcall callback "" nil)
    (let ((old-file (make-temp-file "emagent-old"))
          (new-file (make-temp-file "emagent-new")))
      (write-region old nil old-file nil 'silent)
      (write-region new nil new-file nil 'silent)
      (unless (executable-find "diff")
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))
        (funcall callback "" nil)
        (cl-return-from emagent-tools--unified-diff-async))
      (emagent-tools--run-process-async
       (lambda (output is-error)
         (ignore-errors (delete-file old-file))
         (ignore-errors (delete-file new-file))
         (funcall callback output is-error))
       "diff" "-u"
       "--label" (concat "a/" label)
       "--label" (concat "b/" label)
       old-file new-file))))

(defun emagent-tools--unified-diff (old new label)
  "Return a unified diff string between OLD and NEW content for LABEL."
  (if (string= old new)
      ""
    (let ((old-file (make-temp-file "emagent-old"))
          (new-file (make-temp-file "emagent-new")))
      (unwind-protect
          (progn
            (write-region old nil old-file nil 'silent)
            (write-region new nil new-file nil 'silent)
            (with-temp-buffer
              (call-process "diff" nil t nil "-u"
                            "--label" (concat "a/" label)
                            "--label" (concat "b/" label)
                            old-file new-file)
              (buffer-string)))
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))))))

(defun emagent-tool-write-file-async (callback path content)
  "Write CONTENT to PATH; call CALLBACK with (result is-error)."
  (condition-case err
      (when (emagent-struct-write-required-p path)
        (user-error
         "Refusing write_file on %s: lisp-sitter is installed — use structural_* tools"
         (emagent-tools--root-directory path)))
    (error (funcall callback (error-message-string err) t)
           (cl-return-from emagent-tool-write-file-async)))
  (let* ((resolved (emagent-tools--root-directory path))
         (label (file-name-nondirectory resolved))
         (old (emagent-tools--read-elisp-file-content path)))
    (condition-case err
        (progn
          (when (emagent-tools--protected-fs-path-p path)
            (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                        resolved))
          (emagent-tools--write-file-content path content)
          (emagent-tools--unified-diff-async
           (lambda (diff is-error)
             (if is-error
                 (funcall callback diff t)
               (funcall callback
                        (if (string-empty-p diff)
                            (format "Wrote %s (no changes)" resolved)
                          diff)
                        nil)))
           old content label))
      (error (funcall callback (error-message-string err) t)))))

(defun emagent-tool-write-file (path content)
  "Write CONTENT to PATH through Emacs after user confirmation."
  (when (emagent-struct-write-required-p path)
    (user-error
     "Refusing write_file on %s: lisp-sitter is installed — use structural_* tools"
     (emagent-tools--root-directory path)))
  (let* ((resolved (emagent-tools--root-directory path))
         (label (file-name-nondirectory resolved))
         (old (emagent-tools--read-elisp-file-content path)))
    (when (emagent-tools--protected-fs-path-p path)
      (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                  resolved))
    (emagent-tools--write-file-content path content)
    (let ((diff (emagent-tools--unified-diff old content label)))
      (if (string-empty-p diff)
          (format "Wrote %s (no changes)" resolved)
        diff))))

(provide 'emagent-tools-file)
;;; emagent-tools-file.el ends here
