;;; emagent-chat-mode-activate.el --- Session deferral and mode entry  -*- lexical-binding: t; -*-

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

;; Session pending/deferral, `emagent-mode-entry' / `emagent-mode-force', and
;; first-display activation helpers.
;;
;; `define-derived-mode' stays in the facade `emagent-chat-mode' so the public
;; `emagent-mode' wrapper can overwrite the function cell after capture.
;; This file runs the captured body via `emagent--derived-mode-function',
;; installed by the facade immediately after `define-derived-mode'.
;;
;; DAG: mode-faces → mode-activate → mode facade.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'emagent-log)
(require 'emagent-chat-buffer)
(require 'emagent-chat-mode-faces)

(defvar emagent--derived-mode-function #'ignore
  "Bare `define-derived-mode' body; set by `emagent-chat-mode' after capture.")

(defun emagent--run-derived-mode ()
  "Run derived `emagent-mode' with Org startup inline images disabled.

Only bind `org-startup-with-inline-images' here.  Do not let-bind
`org-element-use-cache': the mode body and
`emagent-chat--disable-incompatible-org-minor-modes' set it buffer-local,
and let-binding it makes `setq-local' fail on Emacs 29."
  (let ((org-startup-with-inline-images nil))
    (funcall emagent--derived-mode-function)))

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

(provide 'emagent-chat-mode-activate)
;;; emagent-chat-mode-activate.el ends here
