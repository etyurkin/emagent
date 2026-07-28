;;; emagent-session.el --- Per-buffer emagent session identity  -*- lexical-binding: t; -*-

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
;;
;; Session metadata, persistence, and project/agent state.
;;
;;; Code:

(require 'cl-lib)
(require 'emagent-model)
(require 'emagent-permissions)

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

;; Names retain the `emagent-chat-' prefix: they are referenced widely and are
;; effectively the persisted-session field names.  Their home is here.

(defvar-local emagent-chat-project-directory nil
  "Project directory for the current emagent buffer.")

(defvar-local emagent-chat-model nil
  "ACP model id for the current emagent buffer.")

(defvar-local emagent-chat-session-id nil
  "ACP session id for the current emagent buffer.")

(defvar-local emagent-chat-provider nil
  "ACP provider symbol (`cursor' or `claude') for the current emagent buffer.")

(defvar-local emagent-chat-cursor-acp-extra-args nil
  "When non-nil, replaces `emagent-cursor-acp-extra-args' for this buffer only.")

(defvar-local emagent-chat-allowed-tools nil
  "Extra MCP tools allowed without confirmation for this buffer session.

Project-wide choices persist under `emagent-permissions-directory'.")

(defvar-local emagent-chat-allowed-permissions nil
  "Legacy buffer-local permission fingerprints from #+EMAGENT_ALLOWED_PERMISSIONS.

New choices persist under `emagent-permissions-directory'.")

(defun emagent-session-id ()
  "Return the persisted ACP session id for the current buffer."
  (or emagent-chat-session-id (emagent-session-store-read-session-property)))

(defun emagent-session-set-id (session-id)
  "Store ACP SESSION-ID in the current buffer."
  (unless (equal emagent-chat-session-id session-id)
    (setq emagent-chat-session-id session-id)
    (emagent-session-store-write-top-property "EMAGENT_SESSION" session-id)))

(defun emagent-session-clear-id ()
  "Remove the ACP session id from the current buffer."
  (setq emagent-chat-session-id nil)
  (emagent-session-store-delete-top-property "EMAGENT_SESSION"))

(defun emagent-session-set-project-directory (directory)
  "Store DIRECTORY as the project directory in the current buffer."
  (let ((dir (expand-file-name directory)))
    (setq emagent-chat-project-directory dir)
    (setq-local default-directory dir)
    (emagent-session-store-write-top-property
     "EMAGENT_PROJECT" (emagent-session-store-display-project-directory dir))))

(defun emagent-session-project-directory ()
  "Return the project directory for the current emagent buffer."
  (or emagent-chat-project-directory (emagent-session-store-read-project-property)))

(defun emagent-session-set-model (model)
  "Store ACP MODEL id in the current buffer.
No UI side effects — callers that need a mode-line refresh add it themselves."
  (setq model (emagent-model-canonical-id model))
  (unless (equal emagent-chat-model model)
    (setq emagent-chat-model model)
    (emagent-session-store-write-top-property "EMAGENT_MODEL" model))
  (setq emagent-chat-model (or emagent-chat-model model)))

(defun emagent-session-model ()
  "Return the ACP model id for the current emagent buffer."
  (emagent-model-canonical-id
   (or emagent-chat-model (emagent-session-store-read-model-property))))

(defun emagent-session-model-display (&optional model)
  "Return MODEL as a short label for the mode line."
  (emagent-model-normalize-id
   (or model (emagent-session-model))))

(defun emagent-session-set-agent (agent)
  "Store the ACP provider AGENT symbol in the current buffer."
  (when agent
    (setq emagent-chat-provider agent)
    (emagent-session-store-write-top-property "EMAGENT_AGENT" (symbol-name agent))))

(defun emagent-session-agent ()
  "Return the ACP provider symbol for the current emagent buffer, or nil."
  (or emagent-chat-provider
      (when-let* ((value (emagent-session-store-read-agent-property))
                  ((not (string-empty-p value))))
        (intern value))))

(defun emagent-session-allowed-tools ()
  "Return MCP tools allowed without confirmation for this buffer's project."
  (let* ((legacy (or emagent-chat-allowed-tools
                     (emagent-session-store-read-allowed-tools-property)))
         (stored (when-let ((dir (emagent-session-project-directory)))
                   (emagent-permissions-project-tools dir))))
    (cl-delete-duplicates (append legacy stored))))

(defun emagent-session-add-allowed-tool (tool)
  "Allow TOOL for this project without confirmation and persist it."
  (let* ((sym (if (stringp tool) (intern tool) tool))
         (dir (emagent-session-project-directory)))
    (unless (memq sym (emagent-session-allowed-tools))
      (setq emagent-chat-allowed-tools (append (or emagent-chat-allowed-tools nil)
                                               (list sym)))
      (when dir
        (emagent-permissions-add-project-tool dir sym)))))

(defun emagent-session-allowed-permissions ()
  "Return legacy buffer permission fingerprints still honored at the gate."
  (or emagent-chat-allowed-permissions
      (emagent-session-store-read-allowed-permissions-property)))

(defun emagent-session-add-allowed-permission (fingerprint)
  "Persist FINGERPRINT as globally allowed for ACP permission requests."
  (emagent-permissions-add-global-fingerprint fingerprint))

(defun emagent-session-allowed-permissions-for (session-id)
  "Return session-scoped permission fingerprints for SESSION-ID."
  (emagent-permissions-session-fingerprints session-id))

(defun emagent-session-add-session-permission (session-id fingerprint)
  "Record FINGERPRINT as session-scoped for SESSION-ID."
  (emagent-permissions-add-session-fingerprint session-id fingerprint))

(defun emagent-session-auto-approve-p (session-id)
  "Return non-nil when SESSION-ID has allow-all enabled."
  (emagent-permissions-session-auto-approve-p session-id))

(defun emagent-session-set-auto-approve (session-id)
  "Enable allow-all for SESSION-ID."
  (emagent-permissions-set-session-auto-approve session-id))

(provide 'emagent-session)
;;; emagent-session.el ends here
