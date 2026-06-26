;;; emagent-acp.el --- ACP wire-up for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

(declare-function emagent-chat-clear-slash-commands "emagent-chat-slash")
(declare-function emagent-chat-seed-cursor-slash-commands "emagent-chat-slash")
(declare-function emagent-chat--bare-slash-command-p "emagent-chat-compress")
(declare-function emagent-chat--compress-command-p "emagent-chat-compress")
(declare-function emagent-chat--conversation-history-text "emagent-chat-compress")
(declare-function emagent-chat--compress-prompt-text "emagent-chat-compress")
(declare-function emagent-chat-apply-compression "emagent-chat-compress")
(declare-function emagent-chat-show-tool-call "emagent-chat")
(declare-function emagent-chat-permission-prompt "emagent-chat")
(declare-function emagent-chat--open-response-p "emagent-chat")
(declare-function emagent-chat--refresh-mode-line-soon "emagent-chat-mode-line")
(declare-function emagent-chat--spinner-start "emagent-chat-mode-line")
(declare-function emagent-cursor-enrich-tool-call-update "emagent-cursor")
(declare-function emagent-cursor-normalize-slash-prompt "emagent-cursor")

(defun emagent-acp-prefer-emacs-p ()
  "Return non-nil when emagent instructs the agent to prefer Emacs tools."
  emagent-acp-prefer-emacs)

;;;; Public session state accessors (for use by emagent-chat.el)






(defun emagent-set-model ()
  "Set the ACP model for the current emagent session."
  (interactive)
  (let* ((state (emagent-acp--session))
         (session-id (map-elt state :session-id))
         (choices (emagent-acp--model-choices state nil))
         (current (emagent-acp--current-model-id state nil))
         (default-name (and current
                            (emagent-acp--model-display-name state nil current)))
         (selection (completing-read
                     "Set emagent model: "
                     (mapcar #'car choices)
                     nil t nil nil
                     (and default-name
                          (car (seq-find (lambda (choice)
                                           (string-prefix-p default-name (car choice)))
                                         choices)))))
         (model-id (cdr (assoc-string selection choices))))
    (unless session-id
      (user-error "No active session"))
    (unless choices
      (user-error "No models available"))
    (unless model-id
      (user-error "Unknown model: %s" selection))
    (when (and current (string= model-id current))
      (user-error "Model already %s"
                  (emagent-acp--model-display-name state nil model-id)))
    (emagent-acp--config-option-set-model-id
     :state state
     :session-id session-id
     :model-id model-id)))

(provide 'emagent-acp)
;;; emagent-acp.el ends here
