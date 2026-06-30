;;; emagent-chat-mode.el --- mode module  -*- lexical-binding: t; -*-

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
(require 'org)
(require 'map)
(require 'emagent-log)
(require 'emagent-chat-header)
(require 'emagent-context)
(require 'emagent-chat-mode-line)
(require 'emagent-chat-actions)

(defvar emagent--force-activation)

(defun emagent-chat--ensure-org-startup ()
  "Ensure the buffer requests Org block folding on startup."
  (unless (save-excursion
            (goto-char (point-min))
            ;; Accept both modern #+STARTUP: and legacy STARTUP: (without #+).
            (re-search-forward "^\\(?:#\\+\\)?STARTUP:.*\\bhideblocks\\b" nil t))
    (emagent-chat--write-top-property "STARTUP" "hideblocks")))

(defun emagent-chat--disable-incompatible-org-minor-modes ()
  "Turn off org minor modes that break on emagent chat buffer content."
  (when (fboundp 'org-appear-mode)
    (org-appear-mode -1))
  (setq-local org-element-use-cache nil))

(defun emagent-chat--setup-buffer-display ()
  "Configure prose wrapping and table scrolling for emagent buffers.

Prose uses `visual-line-mode'.  Wide org tables scroll horizontally via
`org-phscroll-mode', which applies only inside table regions — not buffer-wide.
`truncate-lines' must stay nil; phscroll does not work when it is t.

Runs late on `org-mode-hook' so it overrides user hooks (e.g. org-modern
`kwarks/org--table-buffer-setup') that disable wrapping globally."
  (when (derived-mode-p 'emagent-mode)
    (setq-local org-startup-truncated nil
                truncate-lines nil)
    (when (boundp 'word-wrap)
      (setq-local word-wrap t))
    (visual-line-mode 1)
    (when (fboundp 'org-phscroll-mode)
      (org-phscroll-mode 1))))

(defun emagent-chat--setup-faces ()
  "Configure org highlighting, line wrap, and block folding for emagent buffers."
  (emagent-chat--disable-incompatible-org-minor-modes)
  (setq-local org-src-fontify-natively t
              org-ellipsis "…"
              org-fontify-quote-and-verse-blocks t
              org-cycle-hide-block-startup t)
  (emagent-chat--setup-buffer-display))

(defun emagent-chat--setup-faces-deferred ()
  "Re-apply `emagent-chat--setup-faces' after org startup hooks finish."
  (when (derived-mode-p 'emagent-mode)
    (emagent-chat--setup-faces)))

;;;###autoload
(define-derived-mode emagent-mode org-mode "Emagent"
  "Major mode for emagent chat scratch buffers.

Derived from `org-mode'.  Type naturally, then \\[emagent-chat-send] to send
the line at point.  Select a region first to send multiline text.
On a slash-command line (plugin skills such as /workflow:dev), \\[emagent-chat-tab]
completes available commands.  Agent responses are inserted between
# --- emagent --- delimiter lines (TAB on that line folds the response).

Run \\[emagent-mode] to reconnect a saved session."
  (require 'emagent)
  (setq-local buffer-read-only nil)
  (emagent-chat--writable)
  (setq emagent-chat-project-directory
        (or emagent-chat-project-directory (emagent-chat--read-project-property))
        emagent-chat-session-id (or emagent-chat-session-id
                                    (emagent-chat--read-session-property))
        emagent-chat-model (or emagent-chat-model (emagent-chat--read-model-property))
        emagent-chat-provider (emagent-chat-agent)
        emagent-chat-allowed-tools (or emagent-chat-allowed-tools
                                       (emagent-chat--read-allowed-tools-property))
        emagent-chat-allowed-permissions (or emagent-chat-allowed-permissions
                                            (emagent-chat--read-allowed-permissions-property))
        emagent-chat--font-lock-deferred-p nil
        emagent-chat--table-align-deferred-p nil)
  (when-let ((model (or emagent-chat-model (emagent-chat--read-model-property))))
    (setq emagent-chat-model (emagent-chat--canonical-model-id model)))
  (setq-local default-directory (emagent-chat--session-directory))
  (if (bound-and-true-p doom-modeline-mode)
      (emagent-chat--setup-doom-modeline)
    (setq-local mode-line-format (list "" 'emagent-mode-line "")))
  (org-indent-mode -1)
  (emagent-chat--disable-incompatible-org-minor-modes)
  (when-let ((dir (emagent-chat-project-directory)))
    (rename-buffer (emagent-chat--buffer-name-for-label
                    (emagent-chat--short-cwd-label dir))
                   t))
  (emagent-chat--insert-initial-comment)
  (emagent-chat--sync-user-zone-marker)
  (add-hook 'completion-at-point-functions
            #'emagent-chat-slash-command-completion-at-point -90 t)
  (setq-local imenu-create-index-function #'emagent-chat--imenu-create-index)
  (remove-hook 'font-lock-after-fontify-region-hook
               #'emagent-chat--after-fontify-repair-tool-lines t)
  (setq-local bookmark-make-record-function #'emagent-chat--bookmark-make-record)
  (emagent-chat--setup-faces)
  (emagent-chat--mode-line-recompute)
  (run-with-idle-timer 0 nil #'emagent-chat--setup-faces-deferred))

(defun emagent-mode--safe-org-init (orig-fn &rest args)
  "Run ORIG-FN with Org init vars that misbehave on huge sessions disabled.

`emagent-mode' derives from `org-mode'.  `define-derived-mode' calls the
parent before the derived body runs, so any `setq-local' inside
`emagent-mode' lands too late to influence `org-mode' initialization.
`org-mode' calls `org-display-inline-images' when
`org-startup-with-inline-images' is non-nil; on large chat buffers that
parse can trip an \"Invalid search bound (wrong side of point)\" error
inside `org-element-context'.  It also warms `org-element--cache' before
the derived body disables it.  Bind both to nil for the whole init."
  (let ((org-startup-with-inline-images nil)
        (org-element-use-cache nil))
    (apply orig-fn args)))

(defun emagent-chat--session-buffer-p ()
  "Return non-nil when current buffer looks like an emagent session file."
  (and (derived-mode-p 'org-mode)
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (let ((limit (min (+ (point-min) 4096) (point-max))))
             (or (looking-at-p "# -*- mode: emagent -*-")
                 (re-search-forward
                  "^#\\+EMAGENT_SESSION:[ \t]*\\S-"
                  limit t)))))))

(defun emagent-chat--suppress-inline-images-in-session-buffers (orig-fn &rest args)
  "Skip Org startup inline image rendering for emagent session files.

This avoids Org parser errors during desktop restore of large emagent session
buffers when `org-startup-with-inline-images' is enabled globally."
  (if (emagent-chat--session-buffer-p)
      nil
    (apply orig-fn args)))

(advice-remove 'emagent-mode #'emagent-mode--safe-org-init)

(advice-add 'emagent-mode :around #'emagent-mode--safe-org-init)

(cl-defun emagent-chat-open (&key project-dir)
  "Open or create an emagent buffer for PROJECT-DIR.

Buffer names look like *emagent .emacs.d* from a short cwd label.
PROJECT-DIR is stored as #+EMAGENT_PROJECT and passed to the ACP agent as cwd."
  (unless project-dir
    (user-error "PROJECT-DIR is required"))
  (let* ((dir (expand-file-name project-dir))
         (label (emagent-chat--short-cwd-label dir))
         (slug (emagent-chat--sanitize-slug label))
         (buffer-name (emagent-chat--buffer-name-for-label label))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'emagent-mode)
        (let ((emagent--force-activation t))
          (emagent-mode)))
      (rename-buffer buffer-name t)
      (setq emagent-chat-slug slug
            emagent-chat-session-id (or emagent-chat-session-id
                                        (emagent-chat--read-session-property)))
      (emagent-chat-set-project-directory dir))
    buffer))

(advice-remove 'emagent-mode #'emagent-mode--safe-org-init)

;;;; Context-sensitive C-c C-c

(defun emagent-chat-send-or-babel ()
  "Send the prompt at point, or execute a src block when point is inside one.

On a `#+BEGIN_SRC ... #+END_SRC' block, delegates to
`org-babel-execute-src-block' so code blocks in agent responses are
executable without leaving `emagent-mode'.  Otherwise calls `emagent-chat-send'."
  (interactive)
  (if (org-in-src-block-p)
      (call-interactively #'org-babel-execute-src-block)
    (call-interactively #'emagent-chat-send)))

;;;; Imenu

(defun emagent-dispatch ()
  "Show the emagent command palette."
  (interactive)
  (if (fboundp 'transient-define-prefix)
      (progn
        (unless (fboundp 'emagent--transient-menu)
          (eval
           '(transient-define-prefix emagent--transient-menu ()
              "Emagent commands."
              ["Send & navigate"
               ("SPC" "Send / execute src block" emagent-chat-send-or-babel)
               ("u" "New prompt heading" emagent-chat-new-prompt)
               ("g" "Interrupt agent (C-g C-g)" emagent-chat-interrupt)]
              ["Attach"
               ("a" "Attach buffer context" emagent-chat-attach-buffer)
               ("b" "Send btw side note to agent" emagent-btw)
               ("d" "Attach project files" emagent-chat-attach-files)
               ("e" "Attach error context" emagent-chat-attach-error-context)
               ("i" "Attach image" emagent-chat-attach-image)]
              ["Extract response"
               ("r" "Insert last response into buffer" emagent-chat-insert-last-response)
               ("s" "Insert src block into buffer" emagent-chat-insert-src-block)]
              ["Session"
               ("m" "Set model" emagent-set-model)
               ("p" "Change project directory" emagent-set-project-directory)
               ("t" "Trust workspace on disk" emagent-trust-workspace)
               ("R" "Claude: new session (trust)" emagent-trust-claude-reconnect)
               ("l" "View log" emagent-log-view)])
           t))
        (call-interactively 'emagent--transient-menu))
    (message "emagent: SPC=send, p=prompt, g=interrupt, a=attach, i=image, m=model, t=trust, R=reconnect, l=log")))

(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'emagent-mode)
      (remove-hook 'font-lock-after-fontify-region-hook
                   #'emagent-chat--after-fontify-repair-tool-lines t))))

(unless (advice-member-p #'emagent-chat--suppress-inline-images-in-session-buffers
                          'org-display-inline-images)
  (advice-add 'org-display-inline-images :around
              #'emagent-chat--suppress-inline-images-in-session-buffers))

(add-hook 'org-mode-hook #'emagent-chat--setup-buffer-display 110 t)
(add-hook 'emagent-mode-hook #'emagent-chat--setup-faces 100 t)
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'emagent-mode)
      (emagent-chat--setup-faces))))

(provide 'emagent-chat-mode)
;;; emagent-chat-mode.el ends here
