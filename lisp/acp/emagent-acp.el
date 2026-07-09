;;; emagent-acp.el --- ACP wire-up for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.1

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)

;; Register grouped lisp/ subdirectories on load-path so that
;; cross-directory requires (emagent-log from lisp/core/ etc.)
;; work during byte-compilation by Elpaca or other build tools.
;; Uses `byte-compile-current-file' when set (Elpaca compile).
(eval-and-compile
  (when-let ((file (or load-file-name
                       (and (boundp 'byte-compile-current-file)
                            byte-compile-current-file)))
             (lisp (expand-file-name ".." (file-name-directory file))))
    (when (file-directory-p lisp)
      (dolist (dir (directory-files lisp nil "^[^.]"))
        (let ((path (expand-file-name dir lisp)))
          (when (file-directory-p path)
            (add-to-list 'load-path path)))))))

(require 'emagent-acp-protocol)
(require 'emagent-log)
(require 'emagent-chat)
(require 'emagent-context)
(require 'emagent-mcp)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-gate)
(require 'emagent-acp-usage)
(require 'emagent-acp-model)
(require 'emagent-acp-file)
(require 'emagent-acp-provider)
(require 'emagent-acp-cursor)
(require 'emagent-acp-claude)
(require 'emagent-acp-permit)
(require 'emagent-acp-tool-call)
(require 'emagent-acp-request)
(require 'emagent-acp-prompt)
(require 'emagent-acp-notify)
(require 'emagent-acp-lifecycle)
(require 'emagent-acp-send)
(require 'emagent-prompts)

(declare-function emagent-prompts--prefer-emacs-prompt "emagent-prompts")
(declare-function emagent-prompts--structural-policy "emagent-prompts")

(defun emagent-acp--system-prompt ()
  "Return the system prompt for new ACP sessions."
  (concat emagent-acp-system-prompt
          (emagent-mcp-gateway-system-prompt)
          (when emagent-acp-prefer-emacs
            (emagent-prompts--prefer-emacs-prompt))
          (when emagent-acp-prefer-emacs
            (emagent-prompts--structural-policy))))

(defun emagent-acp--session-system-prompt (&optional compressed-context)
  "Return the system prompt for session/new, optionally with COMPRESSED-CONTEXT."
  (let ((summary (string-trim (or compressed-context ""))))
    (if (string-empty-p summary)
        (emagent-acp--system-prompt)
      (concat (emagent-acp--system-prompt)
              (format "\n\n[Compressed prior conversation context]\n%s"
                      summary)))))

(declare-function emagent-chat-clear-slash-commands "emagent-chat-slash")
(declare-function emagent-chat-seed-cursor-slash-commands "emagent-chat-slash")
(declare-function emagent-chat--bare-slash-command-p "emagent-chat-compress")
(declare-function emagent-chat--compress-command-p "emagent-chat-compress")
(declare-function emagent-chat--conversation-history-text "emagent-chat-compress")
(declare-function emagent-chat--compress-prompt-text "emagent-chat-compress")
(declare-function emagent-chat--open-response-p "emagent-chat")
(declare-function emagent-chat--refresh-mode-line-soon "emagent-chat-mode-line")
(declare-function emagent-chat--spinner-start "emagent-chat-mode-line")

(defun emagent-acp-prefer-emacs-p ()
  "Return non-nil when emagent instructs the agent to prefer Emacs tools."
  emagent-acp-prefer-emacs)

;;;; Public session state accessors (for use by emagent-chat.el)






(declare-function emagent-acp--read-labeled-choice "emagent-acp-model")
(declare-function emagent-permissions-reset-global "emagent-permissions")
(declare-function emagent-permissions-reset-session "emagent-permissions")
(declare-function emagent-permissions-reset-project "emagent-permissions")

(defun emagent-reset-permissions ()
  "Reset stored emagent permissions via a minibuffer menu.

Choices:
  project: all      — clears fingerprints and allowed tools for the current
                      project directory
  project: session  — clears fingerprints and auto-approve for the current
                      ACP session
  global: all       — clears all globally approved fingerprints"
  (interactive)
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Not in an emagent buffer"))
  (let* ((session-id (emagent-session-id))
         (project-dir (emagent-session-project-directory))
         (choices
          (delq nil
                (list
                 (when project-dir  "project: all")
                 (when session-id   "project: session")
                 "global: all")))
         (choice (completing-read "Reset permissions: " choices nil t)))
    (pcase choice
      ("project: all"
       (unless project-dir (user-error "No project directory for this buffer"))
       (emagent-permissions-reset-project project-dir)
       (message "emagent: cleared project permissions for %s" project-dir))
      ("project: session"
       (unless session-id (user-error "No active session for this buffer"))
       (emagent-permissions-reset-session session-id)
       (message "emagent: cleared session permissions for session %s" session-id))
      ("global: all"
       (emagent-permissions-reset-global)
       (message "emagent: cleared all global permissions")))))

(defun emagent-acp-current-model-id ()
  "Return the ACP session model id for the current buffer, or nil."
  (when-let ((state (emagent-acp--session)))
    (emagent-acp--current-model-id state nil)))

(defun emagent-acp-set-model-transient (model-id on-done)
  "Switch this buffer's ACP session model to MODEL-ID without persisting it.
The buffer model (`emagent-chat-model') is left unchanged, so this is a
per-turn override.  ON-DONE is called once the switch resolves (success or
failure) so the caller can proceed to send the prompt."
  (let ((state (emagent-acp--session)))
    (if state
        (emagent-acp--config-option-set-model-id
         :state state
         :session-id (emagent-acp-state-session-id state)
         :model-id model-id
         :persist nil
         :on-success on-done
         :on-failure (lambda (&rest _) (when on-done (funcall on-done))))
      (when on-done (funcall on-done)))))

(defun emagent-set-model ()
  "Set the ACP model for the current emagent session."
  (interactive)
  (let* ((state (emagent-acp--session))
         (session-id (emagent-acp-state-session-id state))
         (choices (emagent-acp--model-choices state nil))
         (labels (mapcar #'car choices))
         (selection (emagent-acp--read-labeled-choice
                     "Set emagent model: "
                     labels))
         (model-id (cdr (assoc-string selection choices))))
    (unless session-id
      (user-error "No active session"))
    (unless choices
      (user-error "No models available"))
    (unless model-id
      (user-error "Unknown model: %s" selection))
    (when-let ((current (emagent-acp--current-model-id state nil)))
      (when (string= model-id current)
        (user-error "Model already %s"
                    (emagent-acp--model-display-name state nil model-id))))
    (emagent-acp--config-option-set-model-id
     :state state
     :session-id session-id
     :model-id model-id)))

(provide 'emagent-acp)
;;; emagent-acp.el ends here
