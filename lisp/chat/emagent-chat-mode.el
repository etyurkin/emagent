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

;; Major mode setup and keymaps for emagent chat buffers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'map)
(require 'emagent-log)
(require 'emagent-chat-markup)
(require 'emagent-chat-header)
(require 'emagent-context)
(require 'emagent-chat-mode-line)
(require 'emagent-chat-actions)
(require 'emagent-chat-attach)
(require 'emagent-chat-slash)

(declare-function emagent-chat--register-live-buffer "emagent-chat")
(declare-function emagent-chat--unregister-live-buffer "emagent-chat")

(declare-function org-appear-mode "ext:org-appear")
(declare-function emagent-chat--cancel-scheduled-table-align "emagent-chat")

;; ACP connect/send composition lives above this file (`emagent-acp-connect'
;; requires `emagent-chat'); declared here rather than required to avoid a
;; load cycle.
(declare-function emagent-acp-send "emagent-acp-connect")
(declare-function emagent-acp-attach-context "emagent-acp-send")
(declare-function emagent-acp-shutdown-buffer "emagent-acp-send")

(defvar emagent-default-provider)

(defun emagent-chat--wire-buffer ()
  "Attach per-buffer emagent callbacks."
  (setq emagent-chat--on-send #'emagent-acp-send
        emagent-chat--on-attach #'emagent-acp-attach-context
        emagent-chat--on-quit #'emagent-acp-shutdown-buffer
        emagent-chat-provider (or (emagent-session-agent) emagent-default-provider)))

(defun emagent-chat--on-mode-enable ()
  "Wire callbacks when enabling `emagent-mode'.

Do not auto-connect on mode activation; delayed ACP reconnect callbacks can
rewrite session metadata and mark restored buffers modified.  Connection still
happens on first send via `emagent-acp-send', or explicitly via
`emagent-connect'.

Cursor built-in slash commands are seeded locally so TAB works before the
first prompt without spawning the agent.  Claude agent slash commands require
`emagent-connect' (or any send) so the agent can publish them."
  (emagent-chat--wire-buffer)
  (emagent-chat--setup-faces)
  (add-hook 'kill-buffer-hook #'emagent-acp-shutdown-buffer nil t)
  (emagent-chat-seed-cursor-slash-commands))

(add-hook 'emagent-mode-hook #'emagent-chat--on-mode-enable)

(defun emagent-chat--ensure-org-startup ()
  "Ensure the buffer requests Org block folding on startup."
  (unless (save-excursion
            (goto-char (point-min))
            ;; Accept both modern #+STARTUP: and legacy STARTUP: (without #+).
            (re-search-forward "^\\(?:#\\+\\)?STARTUP:.*\\bhideblocks\\b" nil t))
    (emagent-chat--write-top-property "STARTUP" "hideblocks")))

(defun emagent-chat--disable-incompatible-org-minor-modes ()
  "Turn off org minor modes that break on emagent chat buffer content."
  (setq-local org-element-use-cache nil)
  ;; Toggling org-appear off runs org-element parsing on the element at point.
  ;; During desktop restore point can sit mid-buffer with the org cache in an
  ;; inconsistent state, which signals \"Invalid search bound (wrong side of
  ;; point)\".  Disabling org-appear must never abort emagent-mode setup.
  (when (bound-and-true-p org-appear-mode)
    (condition-case nil
        (org-appear-mode -1)
      (error nil))))

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

(defvar-local emagent-chat--safe-src-fontify-p nil
  "Non-nil when this buffer uses buffer-local safe src fontification.")

(defvar-local emagent-chat--safe-fontify-installed nil
  "Non-nil when `emagent-chat--safe-fontify-region' is installed locally.")

(defconst emagent-chat--org-src-fontify-fn
  (symbol-function 'org-src-font-lock-fontify-block)
  "Original `org-src-font-lock-fontify-block' function cell.")

(defun emagent-chat--setup-faces ()
  "Configure org highlighting, line wrap, and block folding for emagent buffers."
  (emagent-chat--disable-incompatible-org-minor-modes)
  (emagent-chat--enable-safe-src-fontify)
  (setq-local org-src-fontify-natively t
              org-ellipsis "…"
              org-fontify-quote-and-verse-blocks t
              org-cycle-hide-block-startup t
              ;; Render `[[link][text]]' as just TEXT regardless of the
              ;; user's global setting — the `/model' marker and any links
              ;; in agent output should read as links, not raw markup.
              org-link-descriptive t)
  (when font-lock-mode
    (font-lock-flush))
  (emagent-chat--setup-buffer-display))

(defun emagent-chat--setup-faces-deferred ()
  "Re-apply `emagent-chat--setup-faces' after org startup hooks finish."
  (when (derived-mode-p 'emagent-mode)
    (emagent-chat--setup-faces)))

(defun emagent-chat--fragile-shell-src-p (lang start end)
  "Return non-nil when LANG src between START and END would break `sh-mode'.

`sh-mode' signals `end-of-buffer' while font-locking a command substitution
that wraps a heredoc (`$(cat <<'EOF'...)').  Detect that cheaply and skip
native fontification instead of paying for the failing pass.

Arguments: LANG, START, END."
  (and (member (downcase (or lang "")) '("sh" "bash" "shell" "zsh"))
       (< start end)
       (save-excursion
         (save-restriction
           (narrow-to-region start end)
           (goto-char start)
           (and (search-forward "$(" end t)
                (search-forward "<<" end t))))))

(defun emagent-chat--plain-src-block-face (start end)
  "Mark START..END as a plain `org-block' without native lang fontify."
  (add-text-properties
   start end
   '(face org-block src-block t
     font-lock-fontified t fontified t font-lock-multiline t)))

(defun emagent-chat--safe-src-fontify-block (lang start end)
  "Safe `org-src-font-lock-fontify-block' for emagent session buffers.

Skip native fontification for fragile shell heredoc patterns and catch
other lang font-lock errors so large session buffers stay quiet.

Arguments: LANG, START, END."
  (if (emagent-chat--fragile-shell-src-p lang start end)
      (emagent-chat--plain-src-block-face start end)
    (condition-case nil
        (funcall emagent-chat--org-src-fontify-fn lang start end)
      (error
       (emagent-chat--plain-src-block-face start end)
       nil))))

(defun emagent-chat--safe-fontify-region (orig beg end &optional verbose)
  "Around buffer-local `font-lock-fontify-region-function' for sessions.

Rebinds `org-src-font-lock-fontify-block' only while fontifying this
buffer — no global `advice-add' on Org.

Arguments: ORIG, BEG, END, VERBOSE."
  (if (not emagent-chat--safe-src-fontify-p)
      (funcall orig beg end verbose)
    (cl-letf (((symbol-function 'org-src-font-lock-fontify-block)
               #'emagent-chat--safe-src-fontify-block))
      (funcall orig beg end verbose))))

(defun emagent-chat--enable-safe-src-fontify ()
  "Install buffer-local safe src fontification for the current buffer."
  (setq-local emagent-chat--safe-src-fontify-p t)
  (unless emagent-chat--safe-fontify-installed
    (add-function :around (local 'font-lock-fontify-region-function)
                  #'emagent-chat--safe-fontify-region)
    (setq-local emagent-chat--safe-fontify-installed t)))

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
                         (emagent-chat--read-project-property)))
                (dir (expand-file-name raw)))
      (setq emagent-chat-project-directory dir)
      (emagent-chat--write-top-property "EMAGENT_PROJECT"
                                        (emagent-chat--display-project-directory dir)))
    (setq emagent-chat-session-id (or emagent-chat-session-id
                                      (emagent-chat--read-session-property))
          emagent-chat-model (or emagent-chat-model (emagent-chat--read-model-property))
          emagent-chat-provider (emagent-session-agent)
          emagent-chat-allowed-tools (or emagent-chat-allowed-tools
                                         (emagent-chat--read-allowed-tools-property))
          emagent-chat-allowed-permissions (or emagent-chat-allowed-permissions
                                              (emagent-chat--read-allowed-permissions-property))
          emagent-chat--font-lock-deferred-p nil)
    (emagent-chat--cancel-scheduled-table-align)
    (when-let ((model (or emagent-chat-model (emagent-chat--read-model-property))))
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
`define-derived-mode' overwrites the `emagent-mode' function cell; the public
`defun emagent-mode' further down this file overwrites it again with the
deferral wrapper, so reloading this file always ends with the wrapper
installed.")

(defun emagent--run-derived-mode ()
  "Run derived `emagent-mode' with Org startup inline images disabled.

Only bind `org-startup-with-inline-images' here.  Do not let-bind
`org-element-use-cache': the mode body and
`emagent-chat--disable-incompatible-org-minor-modes' set it buffer-local,
and let-binding it makes `setq-local' fail on Emacs 29."
  (let ((org-startup-with-inline-images nil))
    (emagent--derived-mode)))

(defun emagent--session-buffer-p ()
  "Return non-nil when current `org-mode' buffer is an emagent session."
  (and (derived-mode-p 'org-mode)
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (let ((limit (min (+ (point-min) 4096) (point-max))))
             (or (looking-at-p "#[ \t]*-\\*-.*\\bmode:[ \t]*emagent\\b.*-\\*-")
                 (re-search-forward "^#\\+EMAGENT_SESSION:[ \t]*\\S-" limit t)))))))

(defun emagent-chat--ensure-mode-cookie ()
  "Insert `# -*- mode: emagent -*-' at point-min when missing.

Session files always carry the cookie so `set-auto-mode' routes through
`emagent-mode' (and its safe Org init bindings) instead of bare
`org-mode'."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unless (looking-at-p "[ \t]*# -*- mode: emagent -*-")
        (let ((inhibit-read-only t))
          (insert "# -*- mode: emagent -*-\n"))))))

(defcustom emagent-activate-on-display t
  "When non-nil, defer emagent session activation until the buffer is shown.

Restored session org files (opened via `find-file', desktop, or the
`# -*- mode: emagent -*-' file cookie) stay in plain `org-mode' until the user
first switches to them.  Activation — and therefore any agent connection — only
happens on first display, so restoring many saved sessions at startup spawns
nothing until each one is actually visited.

When nil, session files activate `emagent-mode' immediately on open."
  :type 'boolean
  :group 'emagent)

(defvar emagent--pending-buffers nil
  "Session buffers awaiting first-display activation of `emagent-mode'.")

(defvar-local emagent--session-pending nil
  "Non-nil when this buffer is a session deferred until first display.")

(defun emagent--mark-session-pending ()
  "Leave the current buffer in `org-mode' and queue activation on first display."
  (unless (derived-mode-p 'org-mode)
    (let ((org-startup-with-inline-images nil))
      (org-mode)))
  ;; Deferred sessions stay in plain `org-mode' until first display.  Apply the
  ;; same org-appear / org-element safeguards as `emagent-mode' so desktop
  ;; restore and find-file do not trip \"Invalid search bound\" on large logs.
  (emagent-chat--disable-incompatible-org-minor-modes)
  (emagent-chat--ensure-mode-cookie)
  (emagent-chat--enable-safe-src-fontify)
  (setq-local emagent--session-pending t)
  (cl-pushnew (current-buffer) emagent--pending-buffers)
  (emagent-log "session deferred until displayed: %s" (buffer-name)))

(defun emagent--activate-session-now ()
  "Activate `emagent-mode' in the current buffer immediately."
  (setq emagent--pending-buffers (delq (current-buffer) emagent--pending-buffers))
  (kill-local-variable 'emagent--session-pending)
  (unless (derived-mode-p 'emagent-mode)
    (condition-case err
        (emagent-mode-force)
      (error
       (emagent-log "could not enable emagent-mode in %s: %s"
                    (or (buffer-name) "<dead-buffer>")
                    (error-message-string err))))))

(defun emagent-mode--defer-p (&optional force)
  "Return non-nil when `emagent-mode' should defer activation until display.

Defers for undisplayed file-visiting buffers when
`emagent-activate-on-display' is on and FORCE is nil.  Does not defer
when already in `emagent-mode' so toggle-off works."
  (and emagent-activate-on-display
       (not force)
       (not (eq major-mode 'emagent-mode))
       buffer-file-name
       (not (emagent-chat--buffer-displayed-p))))

(defun emagent-mode-entry (&optional arg)
  "Public entry for `emagent-mode' with display deferral.

ARG is accepted for major-mode compatibility.  Pass `force' (or non-nil
interactively via `emagent-mode-force') to bypass display deferral.
See `emagent-mode' for the user-facing docstring."
  (interactive "P")
  (let ((force (memq arg '(force t))))
    (if (emagent-mode--defer-p force)
        (emagent--mark-session-pending)
      (emagent--run-derived-mode))))

(defun emagent-mode-force ()
  "Activate `emagent-mode', bypassing display deferral.

Use for explicit opens (`emagent-chat-open') and first-display
activation of deferred session buffers."
  (interactive)
  (emagent-mode-entry 'force))

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

(defun emagent--maybe-register-session ()
  "On opening a session file, mark it pending or activate it now.

Used for session files without the `mode: emagent' cookie (only the
`#+EMAGENT_SESSION' property), which `set-auto-mode' opens in `org-mode'."
  (when (and (not (derived-mode-p 'emagent-mode))
             (not emagent--session-pending)
             (emagent--session-buffer-p))
    (if (and emagent-activate-on-display
             (not (emagent-chat--buffer-displayed-p)))
        (emagent--mark-session-pending)
      (emagent--activate-session-now))))

(defun emagent--activate-displayed-pending (&rest _)
  "Activate any pending session buffers that have become displayed."
  (dolist (buf (copy-sequence emagent--pending-buffers))
    (if (buffer-live-p buf)
        (when (emagent-chat--buffer-displayed-p buf)
          (with-current-buffer buf
            (emagent--activate-session-now)))
      (setq emagent--pending-buffers (delq buf emagent--pending-buffers)))))

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
                                        (emagent-chat--read-session-property)))
      (emagent-session-set-project-directory dir))
    buffer))

(add-hook 'find-file-hook #'emagent--maybe-register-session)
(add-hook 'window-buffer-change-functions #'emagent--activate-displayed-pending)

;; When this file loads after session org files were already opened, register
;; them: activate the ones currently displayed, defer the rest.
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (and (not (derived-mode-p 'emagent-mode))
               (emagent--session-buffer-p))
      (if emagent--session-pending
          (cl-pushnew buf emagent--pending-buffers)
        (if (and emagent-activate-on-display
                 (not (emagent-chat--buffer-displayed-p)))
            (emagent--mark-session-pending)
          (emagent--activate-session-now))))))

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

(add-hook 'org-mode-hook #'emagent-chat--setup-buffer-display 110 t)
(add-hook 'emagent-mode-hook #'emagent-chat--setup-faces 100 t)
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'emagent-mode)
      (emagent-chat--setup-faces))))

(provide 'emagent-chat-mode)
;;; emagent-chat-mode.el ends here
