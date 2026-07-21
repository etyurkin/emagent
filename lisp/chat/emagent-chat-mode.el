;;; emagent-chat-mode.el --- mode module  -*- lexical-binding: t; -*-

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

;; Facade for `emagent-mode': keymap, `define-derived-mode', public
;; wrapper, open/send/dispatch.  Faces and activation live in
;; `emagent-chat-mode-faces' and `emagent-chat-mode-activate'.
;;
;; DAG: mode-faces → mode-activate → mode facade.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'emagent-chat-mode-activate)
(require 'emagent-chat-header)
(require 'emagent-session-store)
(require 'emagent-chat-actions)
(require 'emagent-chat-attach)
(require 'emagent-chat-input)

(defvar emagent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'emagent-chat-send-or-babel)
    (define-key map (kbd "ESC ESC") #'emagent-chat-interrupt)
    (define-key map (kbd "C-c ?") #'emagent-dispatch)
    (define-key map (kbd "TAB") #'emagent-chat-tab)
    (define-key map (kbd "C-a") #'emagent-chat-beginning-of-line)
    (define-key map (kbd "C-y") #'emagent-chat-yank)
    (define-key map (kbd "C-p") #'emagent-chat-history-previous-or-previous-line)
    (define-key map (kbd "C-n") #'emagent-chat-history-next-or-next-line)
    map)
  "Keymap for `emagent-mode'.")

;; The public `defun emagent-mode' further down this file redefines this
;; symbol with a different (optional-arg) calling convention; suppress the
;; resulting compiler warning about the redefinition, on both definitions.
(with-suppressed-warnings ((callargs emagent-mode) (redefine emagent-mode))
  (define-derived-mode emagent-mode org-mode "Emagent"
    "Major mode for emagent chat scratch buffers.

Derived from `org-mode'.  Type after the `* user>' stub, then
\\[emagent-chat-send] to send the prompt at point (its heading line
plus any body lines).
On a slash-command line (plugin skills such as /workflow:dev), \\[emagent-chat-tab]
completes available commands.  Agent responses are inserted between
`** Thinking' / `** Response' subsections (TAB folds them as Org headlines).

Run \\[emagent-mode] to reconnect a saved session."
    (require 'emagent)
    (setq-local buffer-read-only nil)
    (setq-local emagent-chat--tool-call-lines (make-hash-table :test 'equal))
    (emagent-chat--writable)
    (when-let* ((raw (or emagent-chat-project-directory
                         (emagent-session-store-read-project-property)))
                (dir (expand-file-name raw)))
      (setq emagent-chat-project-directory dir)
      ;; Normalize the header form without dirtying a just-opened file.
      ;; Trailing-slash / abbreviate differences used to mark every visit
      ;; modified even when the project path was unchanged.
      (let* ((display (emagent-session-store-display-project-directory dir))
             (current (emagent-session-store-read-project-property))
             (current-norm
              (and current
                   (emagent-session-store-display-project-directory current)))
             (was-modified (buffer-modified-p)))
        (unless (equal current-norm display)
          (emagent-session-store-write-top-property "EMAGENT_PROJECT" display)
          (set-buffer-modified-p was-modified))))
    (setq emagent-chat-session-id (or emagent-chat-session-id
                                      (emagent-session-store-read-session-property))
          emagent-chat-model (or emagent-chat-model (emagent-session-store-read-model-property))
          emagent-chat-provider (emagent-session-agent)
          emagent-chat-allowed-tools (or emagent-chat-allowed-tools
                                         (emagent-session-store-read-allowed-tools-property))
          emagent-chat-allowed-permissions (or emagent-chat-allowed-permissions
                                              (emagent-session-store-read-allowed-permissions-property))
          emagent-chat--font-lock-deferred-p nil)
    (emagent-chat--cancel-scheduled-table-align)
    (when-let ((model (or emagent-chat-model (emagent-session-store-read-model-property))))
      (setq emagent-chat-model (emagent-model-canonical-id model)))
    (setq-local default-directory (emagent-chat--session-directory))
    (if (bound-and-true-p doom-modeline-mode)
        (emagent-chat--setup-doom-modeline)
      (setq-local mode-line-format
                    (append (default-value 'mode-line-format)
                            '(" " (:eval (emagent-mode-line))))))
    (org-indent-mode -1)
    (emagent-chat--disable-incompatible-org-minor-modes)
    (when-let ((dir (emagent-session-project-directory)))
      (rename-buffer (emagent-chat--buffer-name-for-label
                      (emagent-chat--short-cwd-label dir))
                     t))
    (emagent-chat--insert-initial-comment)
    (emagent-chat--sync-user-zone-marker)
    (add-hook 'completion-at-point-functions
              #'emagent-chat-slash-command-completion-at-point -90 t)
    (setq-local imenu-create-index-function #'emagent-chat--imenu-create-index)
    (font-lock-add-keywords nil emagent-chat--tool-line-font-lock-keywords 'append)
    (setq-local bookmark-make-record-function #'emagent-chat--bookmark-make-record)
    (emagent-chat--setup-faces)
    (emagent-chat--mode-line-recompute)
    (emagent-chat--register-live-buffer)
    (add-hook 'kill-buffer-hook #'emagent-chat--unregister-live-buffer nil t)
    (run-with-idle-timer 0 nil #'emagent-chat--setup-faces-deferred)))

(defalias 'emagent--derived-mode (symbol-function 'emagent-mode)
  "Bare `define-derived-mode' implementation of `emagent-mode'.

Captured immediately after `define-derived-mode' so the public
`emagent-mode' defun below can wrap display deferral without `advice-add'.
Installed into `emagent--derived-mode-function' for
`emagent-chat-mode-activate' (which must not require this facade back).")

(setq emagent--derived-mode-function #'emagent--derived-mode)


;;;###autoload
(with-suppressed-warnings ((redefine emagent-mode))
  (defun emagent-mode (&optional arg)
    "Major mode for emagent chat scratch buffers.

ARG is accepted for major-mode compatibility; pass `force' (or use
`emagent-mode-force') to bypass display deferral.

Derived from `org-mode'.  Type after the `* user>' stub, then
\\[emagent-chat-send] to send the prompt at point (its heading line
plus any body lines).
On a slash-command line (plugin skills such as /workflow:dev),
\\[emagent-chat-tab] completes available commands.  Agent responses are
inserted between `** Thinking' / `** Response' subsections (TAB folds
them as Org headlines).

When `emagent-activate-on-display' is non-nil, opening an undisplayed
session file defers full activation until first display.

Run \\[emagent-mode] to reconnect a saved session."
    (interactive "P")
    (emagent-mode-entry arg)))


(cl-defun emagent-chat-open (&key project-dir)
  "Open or create an emagent buffer for PROJECT-DIR.

Buffer names look like *emagent myproj* from a short cwd label.
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
        (emagent-mode-force))
      (rename-buffer buffer-name t)
      (setq emagent-chat-slug slug
            emagent-chat-session-id (or emagent-chat-session-id
                                        (emagent-session-store-read-session-property)))
      (emagent-session-set-project-directory dir))
    buffer))

;;;; Context-sensitive C-c C-c

(defun emagent-chat-send-or-babel ()
  "Execute the src block at point, send the prompt, or defer to org.

Precedence: a `#+BEGIN_SRC ... #+END_SRC' block executes via
`org-babel-execute-src-block' (even inside a prompt); on or inside a
`* user>' prompt (old prompts are re-evaluable) `emagent-chat-send'
sends it; anywhere else falls through to `org-ctrl-c-ctrl-c', so
tables realign, checkboxes toggle, and the rest of org ctrl-c ctrl-c keeps
working inside session buffers."
  (interactive)
  (cond
   ((org-in-src-block-p)
    (call-interactively #'org-babel-execute-src-block))
   ((emagent-chat--send-bounds)
    (call-interactively #'emagent-chat-send))
   (t
    (call-interactively #'org-ctrl-c-ctrl-c))))

;;;; Imenu

(defun emagent-dispatch ()
  "Show the emagent command palette."
  (interactive)
  (if (fboundp 'transient-define-prefix)
      (progn
        ;; Redefine each time so palette changes apply after package reload
        ;; without requiring a full Emacs restart.
        (eval
         '(transient-define-prefix emagent--transient-menu ()
            "Emagent commands."
            ["Send & navigate"
             ("SPC" "Send / execute src block" emagent-chat-send-or-babel)
             ("u" "New prompt heading" emagent-chat-new-prompt)
             ("g" "Interrupt agent (ESC ESC)" emagent-chat-interrupt)]
            ["Attach"
             ("a" "Attach buffer context" emagent-chat-attach-buffer)
             ("b" "Send btw side note to agent" emagent-btw)
             ("d" "Attach project files" emagent-chat-attach-files)
             ("e" "Attach error context" emagent-chat-attach-error-context)
             ("i" "Attach image" emagent-chat-attach-image)]
            ["Extract response"
             ("r" "Insert last response into buffer" emagent-chat-insert-last-response)
             ("s" "Insert src block into buffer" emagent-chat-insert-src-block)]
            ;; org's own `C-c ?' command, shadowed by this palette; shown
            ;; only when point is in a table, where it is meaningful.
            ["Table" :if org-at-table-p
             ("f" "Field info (org's C-c ?)" org-table-field-info)]
            ["Session"
             ("c" "Connect / reconnect agent" emagent-connect)
             ("m" "Set session model (/model = one turn)" emagent-set-model)
             ("p" "Change project directory" emagent-set-project-directory)
             ("P" "Reset permissions" emagent-reset-permissions)
             ("t" "Trust workspace on disk" emagent-trust-workspace)
             ("R" "Claude: new session (trust)" emagent-trust-claude-reconnect)
             ("l" "View log" emagent-log-view)])
         t)
        (call-interactively 'emagent--transient-menu))
    (message "emagent: SPC=send, c=connect, g=interrupt, a=attach, i=image, m=model, t=trust, R=reconnect, l=log")))

(provide 'emagent-chat-mode)
;;; emagent-chat-mode.el ends here
