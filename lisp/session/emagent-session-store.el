;;; emagent-session-store.el --- Org header persistence for emagent sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

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

;; Read and write the #+EMAGENT_* org header properties that back
;; `emagent-session''s persisted fields (project, model, ACP session id,
;; provider, allowed tools/permissions).  This is a leaf module — no
;; dependency on `emagent-chat' or any other UI code — so `emagent-session'
;; can depend downward on it instead of reaching up into `emagent-chat-header'.
;;
;; `emagent-chat-header' remains the home for header helpers that are about
;; chat display (`emagent-chat--display-path', `emagent-chat--session-directory')
;; rather than session persistence; it now consumes this module's readers
;; instead of owning them.

;;; Code:

(defun emagent-session-store-read-top-property (name)
  "Return the value of #+NAME at the top of the buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)" name) nil t)
      (string-trim (match-string 1)))))

(defun emagent-session-store-metadata-end ()
  "Return point after emagent comment and metadata header lines."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (or (looking-at "#\\+")
                    (looking-at "# ")
                    (looking-at "#$")))
      (forward-line 1))
    (point)))

(defun emagent-session-store-write-top-property (name value)
  "Insert or update #+NAME in the emagent metadata header.
No-op when #+NAME already holds VALUE, so re-running `emagent-mode' (e.g. on
desktop restore) does not mark the session buffer modified."
  (let* ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (value (format "%s" value))
         (line (format "#+%s: %s" name value))
         (pattern (format "^#\\+%s:[ \t]*.*\n?" name)))
    (unless (equal (emagent-session-store-read-top-property name) value)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (delete-region (match-beginning 0) (match-end 0)))
          (goto-char (emagent-session-store-metadata-end))
          (unless (bolp) (insert "\n"))
          (insert line "\n"))))))

(defun emagent-session-store-delete-top-property (name)
  "Delete #+NAME from the top of the buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "^#\\+%s:.*\n?" name) nil t)
        (replace-match "")))))

(defun emagent-session-store-read-project-property ()
  "Return the #+EMAGENT_PROJECT value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_PROJECT"))

(defun emagent-session-store-read-model-property ()
  "Return the #+EMAGENT_MODEL value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_MODEL"))

(defun emagent-session-store-read-session-property ()
  "Return the #+EMAGENT_SESSION value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_SESSION"))

(defun emagent-session-store-read-agent-property ()
  "Return the #+EMAGENT_AGENT value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_AGENT"))

(defconst emagent-session-store--allowed-tools-property "EMAGENT_ALLOWED_TOOLS")
(defconst emagent-session-store--allowed-permissions-property "EMAGENT_ALLOWED_PERMISSIONS")

(defun emagent-session-store-read-allowed-tools-property ()
  "Return the #+EMAGENT_ALLOWED_TOOLS value as a list of tool symbols."
  (when-let* ((value (emagent-session-store-read-top-property
                      emagent-session-store--allowed-tools-property))
              ((not (string-empty-p value))))
    (mapcar #'intern (split-string value "[ ,]+" t))))

(defun emagent-session-store-read-allowed-permissions-property ()
  "Return #+EMAGENT_ALLOWED_PERMISSIONS as a list of permission fingerprints."
  (when-let* ((value (emagent-session-store-read-top-property
                      emagent-session-store--allowed-permissions-property))
              ((not (string-empty-p value))))
    (split-string value "[ ,]+" t)))

(defun emagent-session-store-display-project-directory (directory)
  "Return DIRECTORY as written in #+EMAGENT_PROJECT."
  (file-name-as-directory
   (abbreviate-file-name (expand-file-name directory))))

(provide 'emagent-session-store)
;;; emagent-session-store.el ends here
