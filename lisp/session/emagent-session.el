;;; emagent-session.el --- Per-buffer emagent session identity  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session identity for an emagent chat buffer: project root, model, ACP
;; session id, provider, and the buffer's allowed-tools/permissions.  These
;; are the fields lower layers (ACP runtime, MCP server) need to read, so they
;; live here — below the chat UI — rather than in `emagent-chat', which would
;; force those layers to depend upward on the whole UI module.
;;
;; Persistence (org top-property read/write) and model-id normalization live in
;; the leaf `emagent-chat-header'; permission stores live in
;; `emagent-permissions'.  This module carries no UI side effects — callers that
;; also need a mode-line refresh (e.g. on a model change) wrap these accessors.

;;; Code:

(require 'cl-lib)
(require 'emagent-chat-header)
(require 'emagent-permissions)

;;;; Buffer-local session fields
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

;;;; Accessors

(defun emagent-session-id ()
  "Return the persisted ACP session id for the current buffer."
  (or emagent-chat-session-id (emagent-chat--read-session-property)))

(defun emagent-session-set-id (session-id)
  "Store ACP SESSION-ID in the current buffer."
  (unless (equal emagent-chat-session-id session-id)
    (setq emagent-chat-session-id session-id)
    (emagent-chat--write-top-property "EMAGENT_SESSION" session-id)))

(defun emagent-session-clear-id ()
  "Remove the ACP session id from the current buffer."
  (setq emagent-chat-session-id nil)
  (emagent-chat--delete-top-property "EMAGENT_SESSION"))

(defun emagent-session-set-project-directory (directory)
  "Store DIRECTORY as the project directory in the current buffer."
  (let ((dir (expand-file-name directory)))
    (setq emagent-chat-project-directory dir)
    (setq-local default-directory dir)
    (emagent-chat--write-top-property "EMAGENT_PROJECT" dir)))

(defun emagent-session-project-directory ()
  "Return the project directory for the current emagent buffer."
  (or emagent-chat-project-directory (emagent-chat--read-project-property)))

(defun emagent-session-set-model (model)
  "Store ACP MODEL id in the current buffer.
No UI side effects — callers that need a mode-line refresh add it themselves."
  (setq model (emagent-chat--canonical-model-id model))
  (unless (equal emagent-chat-model model)
    (setq emagent-chat-model model)
    (emagent-chat--write-top-property "EMAGENT_MODEL" model))
  (setq emagent-chat-model (or emagent-chat-model model)))

(defun emagent-session-model ()
  "Return the ACP model id for the current emagent buffer."
  (emagent-chat--canonical-model-id
   (or emagent-chat-model (emagent-chat--read-model-property))))

(defun emagent-session-model-display (&optional model)
  "Return MODEL as a short label for the mode line."
  (emagent-chat--normalize-model-id
   (or model (emagent-session-model))))

(defun emagent-session-set-agent (agent)
  "Store the ACP provider AGENT symbol in the current buffer."
  (when agent
    (setq emagent-chat-provider agent)
    (emagent-chat--write-top-property "EMAGENT_AGENT" (symbol-name agent))))

(defun emagent-session-agent ()
  "Return the ACP provider symbol for the current emagent buffer, or nil."
  (or emagent-chat-provider
      (when-let* ((value (emagent-chat--read-agent-property))
                  ((not (string-empty-p value))))
        (intern value))))

(defun emagent-session-allowed-tools ()
  "Return MCP tools allowed without confirmation for this buffer's project."
  (let* ((legacy (or emagent-chat-allowed-tools
                     (emagent-chat--read-allowed-tools-property)))
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
      (emagent-chat--read-allowed-permissions-property)))

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
