;;; emagent-chat-header.el --- Buffer metadata header R/W for emagent  -*- lexical-binding: t; -*-

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
  "Insert or update #+NAME in the emagent metadata header.
No-op when #+NAME already holds VALUE, so re-running `emagent-mode' (e.g. on
desktop restore) does not mark the session buffer modified."
  (let* ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (value (format "%s" value))
         (line (format "#+%s: %s" name value))
         (pattern (format "^#\\+%s:[ \t]*.*\n?" name)))
    (unless (equal (emagent-chat--read-top-property name) value)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (delete-region (match-beginning 0) (match-end 0)))
          (goto-char (emagent-chat--metadata-end))
          (unless (bolp) (insert "\n"))
          (insert line "\n"))))))

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

(defun emagent-chat--display-path (path &optional project-dir)
  "Return PATH formatted for display in the chat UI.
Under the session project root: ./projectname/relative/path.
Under user home but outside the project: ~/….
Otherwise: the absolute PATH.

Relative paths resolve against the project directory, not
`default-directory' — saving the session file moves `default-directory'
to the session file's directory, which is unrelated to the project.

Arguments: PROJECT-DIR."
  (let* ((project (when-let ((raw (or project-dir
                                      (and (boundp 'emagent-chat-project-directory)
                                           emagent-chat-project-directory)
                                      (emagent-chat--read-project-property))))
                    (file-truename
                     (file-name-as-directory (expand-file-name raw)))))
         (expanded (file-truename (expand-file-name path project)))
         (home (file-truename (expand-file-name "~")))
         (home-prefix (concat home "/")))
    (cond
     ((and project
           (string-prefix-p project expanded)
           (not (string= expanded (directory-file-name project))))
      (concat "./"
              (file-name-nondirectory (directory-file-name project))
              "/"
              (file-relative-name expanded project)))
     ((string-prefix-p home-prefix expanded)
      (abbreviate-file-name expanded))
     ((string= expanded home)
      "~")
     (t expanded))))

(defun emagent-chat--display-project-directory (directory)
  "Return DIRECTORY as written in #+EMAGENT_PROJECT."
  (file-name-as-directory
   (abbreviate-file-name (expand-file-name directory))))

(defun emagent-chat--session-directory ()
  "Return the ACP working directory for the current emagent buffer.
Reads #+EMAGENT_PROJECT from the buffer header if set, falling back to
variable `buffer-file-name', `project-current' or `user-emacs-directory'."
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
