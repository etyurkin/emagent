;;; emagent-chat-header.el --- Buffer metadata header R/W for emagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Read and write #+EMAGENT_* and #+STARTUP header properties in
;; emagent chat buffers.  The buffer header is the region before the
;; first non-comment, non-property line -- a narrow zone at the top.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-model)

(declare-function project-root "project")

(defun emagent-chat--read-top-property (name)
  "Return the value of #+NAME at the top of the buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)" name) nil t)
      (string-trim (match-string 1)))))

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

(defun emagent-chat--delete-top-property (name)
  "Delete #+NAME from the top of the buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "^#\\+%s:.*\n?" name) nil t)
        (replace-match "")))))

(defun emagent-chat--read-project-property ()
  "Return the #+EMAGENT_PROJECT value at the top of the buffer."
  (emagent-chat--read-top-property "EMAGENT_PROJECT"))

(defun emagent-chat--read-model-property ()
  "Return the #+EMAGENT_MODEL value at the top of the buffer."
  (emagent-chat--read-top-property "EMAGENT_MODEL"))

;; Model-id helpers moved to the `emagent-model' leaf.  These aliases keep the
;; historical `emagent-chat--*' names working for existing callers.
(defalias 'emagent-chat--canonical-model-id #'emagent-model-canonical-id)
(defalias 'emagent-chat--normalize-model-id #'emagent-model-normalize-id)
(defalias 'emagent-chat--model-choice-label-parts #'emagent-model-choice-label-parts)
(defalias 'emagent-chat--model-choice-label #'emagent-model-choice-label)
(defalias 'emagent-chat--model-choice-label-display #'emagent-model-choice-label-display)

(defun emagent-chat--read-session-property ()
  "Return the #+EMAGENT_SESSION value at the top of the buffer."
  (emagent-chat--read-top-property "EMAGENT_SESSION"))

(defun emagent-chat--read-agent-property ()
  "Return the #+EMAGENT_AGENT value at the top of the buffer."
  (emagent-chat--read-top-property "EMAGENT_AGENT"))

(defconst emagent-chat--allowed-tools-property "EMAGENT_ALLOWED_TOOLS")
(defconst emagent-chat--allowed-permissions-property "EMAGENT_ALLOWED_PERMISSIONS")

(defun emagent-chat--read-allowed-tools-property ()
  "Return the #+EMAGENT_ALLOWED_TOOLS value as a list of tool symbols."
  (when-let* ((value (emagent-chat--read-top-property
                      emagent-chat--allowed-tools-property))
              ((not (string-empty-p value))))
    (mapcar #'intern (split-string value "[ ,]+" t))))

(defun emagent-chat--read-allowed-permissions-property ()
  "Return #+EMAGENT_ALLOWED_PERMISSIONS as a list of permission fingerprints."
  (when-let* ((value (emagent-chat--read-top-property
                      emagent-chat--allowed-permissions-property))
              ((not (string-empty-p value))))
    (split-string value "[ ,]+" t)))

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
