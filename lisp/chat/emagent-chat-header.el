;;; emagent-chat-header.el --- Buffer metadata header R/W for emagent  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Read and write #+EMAGENT_* and #+STARTUP header properties in
;; emagent chat buffers.  The buffer header is the region before the
;; first non-comment, non-property line -- a narrow zone at the top.

;;; Code:

(require 'cl-lib)
(require 'map)

(declare-function project-root "project")

;;;###autoload
(defun emagent-chat--read-top-property (name)
  "Return the value of #+NAME at the top of the buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)" name) nil t)
      (string-trim (match-string 1)))))

;;;###autoload
(defun emagent-chat--metadata-end ()
  "Return point after emagent comment and metadata header lines."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (or (looking-at "#\\+")
                    (looking-at "# ")
                    (looking-at "#$")))
      (forward-line 1))
    (point)))

;;;###autoload
(defun emagent-chat--write-top-property (name value)
  "Insert or update #+NAME in the emagent metadata header."
  (let* ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (line (format "#+%s: %s" name value))
         (pattern (format "^#\\+%s:[ \t]*.*\n?" name)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward pattern nil t)
          (delete-region (match-beginning 0) (match-end 0)))
        (goto-char (emagent-chat--metadata-end))
        (unless (bolp) (insert "\n"))
        (insert line "\n")))))

;;;###autoload
(defun emagent-chat--delete-top-property (name)
  "Delete #+NAME from the top of the buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "^#\\+%s:.*\n?" name) nil t)
        (replace-match "")))))

;;;###autoload
(defun emagent-chat--read-project-property ()
  "Return the #+EMAGENT_PROJECT value at the top of the buffer."
  (emagent-chat--read-top-property "EMAGENT_PROJECT"))

;;;###autoload
(defun emagent-chat--read-model-property ()
  "Return the #+EMAGENT_MODEL value at the top of the buffer."
  (emagent-chat--read-top-property "EMAGENT_MODEL"))

;;;###autoload
(defun emagent-chat--normalize-model-id (model)
  "Return user-facing model id, mapping Cursor default[] to auto.
Strips key=value annotations (e.g. [thinking=true]) and empty brackets ([]).
Preserves identifier-only annotations (e.g. [1m]) that are part of the model ID."
  (when model
    (let ((stripped (replace-regexp-in-string
                     "\\[\\([^]]*=[^]]*\\)?\\]" "" model)))
      (if (string= stripped "default") "auto" stripped))))

;;;###autoload
(defun emagent-chat--read-session-property ()
  "Return the #+EMAGENT_SESSION value at the top of the buffer."
  (emagent-chat--read-top-property "EMAGENT_SESSION"))

;;;###autoload
(defun emagent-chat--read-agent-property ()
  "Return the #+EMAGENT_AGENT value at the top of the buffer."
  (emagent-chat--read-top-property "EMAGENT_AGENT"))

(defconst emagent-chat--allowed-tools-property "EMAGENT_ALLOWED_TOOLS")
(defconst emagent-chat--allowed-permissions-property "EMAGENT_ALLOWED_PERMISSIONS")

;;;###autoload
(defun emagent-chat--read-allowed-tools-property ()
  "Return the #+EMAGENT_ALLOWED_TOOLS value as a list of tool symbols."
  (when-let* ((value (emagent-chat--read-top-property
                      emagent-chat--allowed-tools-property))
              ((not (string-empty-p value))))
    (mapcar #'intern (split-string value "[ ,]+" t))))

;;;###autoload
(defun emagent-chat--read-allowed-permissions-property ()
  "Return #+EMAGENT_ALLOWED_PERMISSIONS as a list of permission fingerprints."
  (when-let* ((value (emagent-chat--read-top-property
                      emagent-chat--allowed-permissions-property))
              ((not (string-empty-p value))))
    (split-string value "[ ,]+" t)))

;;;###autoload
(defun emagent-chat--session-directory ()
  "Return the ACP working directory for the current emagent buffer.
Reads #+EMAGENT_PROJECT from the buffer header if set, falling back to
buffer-file-name, project-current or user-emacs-directory."
  (expand-file-name
   (or (emagent-chat--read-project-property)
       (and buffer-file-name (file-name-directory buffer-file-name))
       (if (boundp 'emagent-default-directory) emagent-default-directory)
       (and (fboundp 'project-current)
            (when-let ((proj (project-current nil default-directory)))
              (project-root proj)))
       user-emacs-directory)))

(provide 'emagent-chat-header)
;;; emagent-chat-header.el ends here
