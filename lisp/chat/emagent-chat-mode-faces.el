;;; emagent-chat-mode-faces.el --- Provider, Org display, safe src faces  -*- lexical-binding: t; -*-

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

;; Provider wiring, Org startup/display, safe src fontify, and faces setup
;; for emagent chat buffers.
;;
;; DAG: mode-line + tools-fontify/markup → mode-faces → mode-activate →
;; mode facade.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'emagent-session)
(require 'emagent-session-store)
(require 'emagent-chat-mode-line)
(require 'emagent-chat-tools-fontify)
(require 'emagent-chat-markup)
(require 'emagent-chat-slash)

(defvar emagent-default-provider)

(defun emagent-chat--set-provider ()
  "Set the buffer's provider from the saved session or the default.

The ACP send/attach/quit callbacks are wired separately by
`emagent-acp-connect' (which requires `emagent-chat'; this file cannot
require it back) via its own `emagent-mode-hook' function."
  (setq emagent-chat-provider (or (emagent-session-agent) emagent-default-provider)))

(defun emagent-chat--on-mode-enable ()
  "Wire callbacks when enabling `emagent-mode'.

Do not auto-connect on mode activation; delayed ACP reconnect callbacks can
rewrite session metadata and mark restored buffers modified.  Connection still
happens on first send via `emagent-acp-send', or explicitly via
`emagent-connect'.

Cursor built-in slash commands are seeded locally so TAB works before the
first prompt without spawning the agent.  Claude agent slash commands require
`emagent-connect' (or any send) so the agent can publish them."
  (emagent-chat--set-provider)
  (emagent-chat--setup-faces)
  (emagent-chat-seed-cursor-slash-commands))

(add-hook 'emagent-mode-hook #'emagent-chat--on-mode-enable)

(defun emagent-chat--ensure-org-startup ()
  "Ensure the buffer requests Org block folding on startup."
  (unless (save-excursion
            (goto-char (point-min))
            ;; Accept both modern #+STARTUP: and legacy STARTUP: (without #+).
            (re-search-forward "^\\(?:#\\+\\)?STARTUP:.*\\bhideblocks\\b" nil t))
    (emagent-session-store-write-top-property "STARTUP" "hideblocks")))

(defun emagent-chat--disable-incompatible-org-minor-modes ()
  "Turn off org minor modes that break on emagent chat buffer content."
  (setq-local org-element-use-cache nil)
  ;; Toggling org-appear off runs org-element parsing on the element at point.
  ;; During desktop restore point can sit mid-buffer with the org cache in an
  ;; inconsistent state, which signals \"Invalid search bound (wrong side of
  ;; point)\".  Disabling org-appear must never abort emagent-mode setup.
  (when (and (fboundp 'org-appear-mode) (bound-and-true-p org-appear-mode))
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

(add-hook 'org-mode-hook #'emagent-chat--setup-buffer-display 110 t)
(add-hook 'emagent-mode-hook #'emagent-chat--setup-faces 100 t)
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'emagent-mode)
      (emagent-chat--setup-faces))))

(provide 'emagent-chat-mode-faces)
;;; emagent-chat-mode-faces.el ends here
