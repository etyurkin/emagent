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

(defun emagent-chat--canonical-model-id (model)
  "Return MODEL id in the form Cursor ACP expects (keep bracket suffixes)."
  (when model
    (if (member model '("auto" "default"))
        "default[]"
      model)))

(defun emagent-chat--normalize-model-id (model)
  "Return a short user-facing model label for display (mode line, prompts).
Strips key=value annotations (e.g. [thinking=true]) and empty brackets ([]).
Maps Cursor default[] to auto."
  (when model
    (let ((stripped (replace-regexp-in-string
                     "\\[\\([^]]*=[^]]*\\)?\\]" "" model)))
      (if (member stripped '("default" "auto")) "auto" stripped))))

(defun emagent-chat--model-choice-label-parts (id &optional name)
  "Return (PRIMARY . SUFFIX) for model ID.
PRIMARY is the base id without bracket annotations; SUFFIX is brackets
plus an optional parenthetical alias when NAME differs from the normalized id."
  (when id
    (let* ((bracket (and (string-match "\\[" id) (match-beginning 0)))
           (base (if bracket (substring id 0 bracket) id))
           (brackets (if bracket (substring id bracket) ""))
           (short-name (and name
                            (not (string= name (emagent-chat--normalize-model-id id)))
                            name)))
      (cons base (concat brackets (if short-name (format " (%s)" short-name) ""))))))

(defun emagent-chat--model-choice-label (id &optional name)
  "Return a completing-read label for model ID, showing the full canonical id.
When NAME differs from the normalized ID (e.g. Auto vs default[]), append it."
  (let ((parts (emagent-chat--model-choice-label-parts id name)))
    (when parts (concat (car parts) (cdr parts)))))

(defun emagent-chat--model-choice-label-display (id &optional name)
  "Like `emagent-chat--model-choice-label', with theme faces for model and details."
  (let ((parts (emagent-chat--model-choice-label-parts id name)))
    (when parts
      (concat (propertize (car parts) 'face 'emagent-model-choice-model)
              (if (string-empty-p (cdr parts))
                  ""
                (propertize (cdr parts) 'face 'emagent-model-choice-detail))))))

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
