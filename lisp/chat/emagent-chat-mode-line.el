;;; emagent-chat-mode-line.el --- Mode-line status for emagent chat  -*- lexical-binding: t; -*-

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

;; Mode-line status string assembly, context-usage display, and doom-modeline
;; integration for emagent chat buffers.  Busy spinner lives in
;; `emagent-chat-spinner' (DAG: buffer → spinner → mode-line).

;;; Code:

(require 'emagent-chat-buffer)
(require 'emagent-chat-spinner)
(require 'emagent-chat-response-state)
(require 'emagent-chat-send-state)
(require 'emagent-session)

;; Owned by the facade `emagent-chat' (which requires this file); forward
;; declared here so this file never requires it back.
(defvar emagent-chat--turn-model)

;;; -------------------------------------------------------------------------
;;; Mode-line cache and refresh
;;; -------------------------------------------------------------------------

(defvar-local emagent-chat--mode-line-head nil
  "Cached mode-line status prefix for the current emagent buffer.")

(defvar-local emagent-chat--mode-line-tail nil
  "Cached mode-line metadata suffix for the current emagent buffer.")

(defvar-local emagent-chat--mode-line-cache nil
  "Cached full mode-line string for `emagent-mode-line'.")

(defvar-local emagent-chat--mode-line-stale-p nil
  "When non-nil, recompute the mode line when this buffer becomes active.")

(defvar-local emagent-chat--status nil
  "Plist snapshot of ACP session status, pushed by the ACP layer via :cb-status.

Keys: :busy :waiting-permission :ready :prompt-finishing :tool :tool-kind :rss
:model-id :ctx-usage (a (USED . SIZE) cons or nil) :ctx-unavailable.  The mode
line renders from this snapshot so the UI never calls up into the ACP runtime.")

(defun emagent-chat--stat (key)
  "Return status field KEY from the pushed ACP snapshot."
  (plist-get emagent-chat--status key))

(defun emagent-chat-model-display ()
  "Return a short model label for the mode line.
Prefer the pending `/model' target while preparing a send, then the live ACP
session model pushed via `emagent-chat-set-status' (including transient
per-turn switches), otherwise the buffer's saved #+EMAGENT_MODEL."
  (let ((id (cond
              ((and emagent-chat--send-pending emagent-chat--turn-model)
               emagent-chat--turn-model)
              ((emagent-chat--stat :ready)
               (emagent-chat--stat :model-id))
              (t (emagent-session-model)))))
    (when id (emagent-session-model-display id))))

(defun emagent-chat-set-model (model)
  "Store ACP MODEL id in the current buffer and refresh the mode line."
  (emagent-session-set-model model)
  (emagent-chat--refresh-mode-line))

(defun emagent-chat-set-status (status)
  "Store the ACP STATUS snapshot for this buffer and refresh the mode line.
This is the ACP layer's downward entry point (wired as :cb-status); it replaces
the mode line pulling session state back out of the ACP layer."
  (setq emagent-chat--status status)
  (when (emagent-chat--stat :busy)
    (emagent-chat--spinner-ensure-running))
  (if (emagent-chat--stat :busy)
      (emagent-chat--refresh-mode-line-soon)
    (emagent-chat--refresh-mode-line)))

(defun emagent-chat--mode-line-recompute ()
  "Rebuild cached mode-line strings for the current emagent buffer."
  (let ((parts (emagent-chat--mode-line-strings)))
    (setq emagent-chat--mode-line-head (car parts)
          emagent-chat--mode-line-tail (cdr parts)
          emagent-chat--mode-line-cache (concat (car parts) (cdr parts))
          emagent-chat--mode-line-stale-p nil)))

(defvar-local emagent-chat--mode-line-refresh-timer nil
  "One-shot idle timer that coalesces mode-line recomputes for this buffer.")

(defun emagent-chat--refresh-mode-line ()
  "Recompute and invalidate the mode line in the current buffer immediately."
  (when emagent-chat--mode-line-refresh-timer
    (cancel-timer emagent-chat--mode-line-refresh-timer)
    (setq emagent-chat--mode-line-refresh-timer nil))
  (emagent-chat--mode-line-recompute)
  (emagent-chat--maybe-force-mode-line-update))

(defun emagent-chat--refresh-mode-line-soon ()
  "Queue a single mode-line recompute for the current buffer."
  (let ((buf (current-buffer)))
    (if (emagent-chat--buffer-active-p)
        (progn
          (when emagent-chat--mode-line-refresh-timer
            (cancel-timer emagent-chat--mode-line-refresh-timer))
          (let ((refresh
                 (lambda ()
                   (setq emagent-chat--mode-line-refresh-timer nil)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (emagent-chat--mode-line-recompute)
                       (emagent-chat--maybe-force-mode-line-update))))))
            (setq emagent-chat--mode-line-refresh-timer
                  (run-with-idle-timer 0 nil refresh))))
      (progn
        (setq emagent-chat--mode-line-stale-p t)
        (when emagent-chat--mode-line-refresh-timer
          (cancel-timer emagent-chat--mode-line-refresh-timer)
          (setq emagent-chat--mode-line-refresh-timer nil))))))

(defun emagent-chat--refresh-mode-line-on-focus ()
  "Recompute a stale or busy mode line after this buffer becomes active."
  (when (emagent-chat--buffer-active-p)
    (when (or emagent-chat--mode-line-stale-p
              (emagent-chat--stat :busy))
      (emagent-chat--mode-line-recompute)
      (force-mode-line-update))))

;;; -------------------------------------------------------------------------
;;; Mode-line string assembly
;;; -------------------------------------------------------------------------

(defun emagent-chat--mode-line-strings ()
  "Return (HEAD . TAIL) strings for the emagent mode line."
  (let* ((busy  (emagent-chat--stat :busy))
         (waiting-permission (emagent-chat--stat :waiting-permission))
         (ready (emagent-chat--stat :ready))
         (tool  (emagent-chat--stat :tool))
         (kind  (emagent-chat--stat :tool-kind))
         (rss   (emagent-chat--stat :rss))
         (connected (or busy ready))
         (spinner (when (emagent-chat--spinner-animate-p)
                    (emagent-chat--mode-line-spinner-suffix)))
         (busy-face '(bold mode-line-emphasis))
         (head (cond
                (waiting-permission
                 (propertize "emagent:Allow?" 'face 'warning))
                ((and (not busy) ready
                      (emagent-chat--open-response-p)
                      (not (emagent-chat--stat :prompt-finishing)))
                 (propertize "emagent:stalled" 'face 'warning))
                ((and busy tool (member kind '("write" "execute")))
                 (concat (propertize "Executing" 'face busy-face)
                         spinner))
                (emagent-chat--send-pending
                 (concat (propertize
                          (if emagent-chat--turn-model "Switching" "Preparing")
                          'face busy-face)
                         spinner))
                (busy
                 (concat (propertize "Thinking" 'face busy-face)
                         spinner))
                (ready (propertize "emagent:Idle" 'face 'success))
                (connected (propertize "emagent:connecting" 'face 'warning))
                (t "emagent")))
         (model (emagent-chat-model-display))
         (sep (propertize " | " 'face 'shadow))
         (model-str (when (and model (not (string-empty-p model)))
                      (propertize model 'face 'shadow)))
         (context (emagent-chat--mode-line-context-usage))
         (rss-str (when rss
                    (propertize (format "mem:%dMB" rss)
                                'face (cond ((>= rss 1000) 'error)
                                            ((>= rss 500)  'warning)
                                            (t             'success)))))
         (tail (concat (when model-str (concat sep model-str))
                       (when context   (concat sep (string-trim-left context)))
                       (when rss-str   (concat sep rss-str)))))
    (cons head tail)))

(defun emagent-chat--mode-line-context-usage ()
  "Return a propertized context fill string, or nil.
Shows a percentage when the provider reports context usage, `ctx:n/a' when a
connected provider (cursor) cannot report it, and nil otherwise."
  (if-let* ((pair (emagent-chat--stat :ctx-usage))
            (used (car pair))
            (size (cdr pair))
            ((and (numberp used) (numberp size) (> size 0))))
      (let ((pct (* 100.0 (/ (float used) size))))
        (propertize (format " ctx:%.0f%%" pct)
                    'face (cond
                           ((>= pct 80) 'error)
                           ((>= pct 50) 'warning)
                           (t           'success))))
    (when (emagent-chat--stat :ctx-unavailable)
      (propertize " ctx:n/a" 'face 'shadow))))

;;;###autoload
(defun emagent-mode-line ()
  "Return cached emagent status text for the mode line."
  (or emagent-chat--mode-line-cache
      (progn (emagent-chat--mode-line-recompute)
             emagent-chat--mode-line-cache)))

;;; -------------------------------------------------------------------------
;;; Doom-modeline integration
;;; -------------------------------------------------------------------------

(defvar emagent-chat--doom-modeline-registered-p nil)

(defun emagent-chat--register-doom-modeline ()
  "Register emagent segment and modeline layout with doom-modeline."
  ;; doom-modeline-def-* are macros; eval quoted forms at runtime so
  ;; byte-compilation of emagent-chat.el does not expand them early.
  (eval
   '(progn
      (doom-modeline-def-segment emagent-ml
        "Emagent session status."
        (when (derived-mode-p 'emagent-mode)
          (when emagent-chat--mode-line-head
            (concat (doom-modeline-spc)
                    emagent-chat--mode-line-head
                    (doom-modeline-display-text emagent-chat--mode-line-tail)))))
      (unless emagent-chat--doom-modeline-registered-p
        (setq emagent-chat--doom-modeline-registered-p t)
        (unless (assoc 'emagent-mode doom-modeline-mode-alist)
          (add-to-list 'doom-modeline-mode-alist
                       (cons 'emagent-mode 'emagent-chat)))
        (unless (fboundp 'doom-modeline-format--emagent-chat)
          (doom-modeline-def-modeline 'emagent-chat
            '(matches buffer-info remote-host parrot)
            '(buffer-position selection-info minor-modes process emagent-ml vcs
                              input-method buffer-encoding battery misc-info
                              major-mode)))))
   t))

(defvar doom-modeline-mode)

(defun emagent-chat--setup-doom-modeline ()
  "Use the emagent doom-modeline layout when doom-modeline is active."
  (when (featurep 'doom-modeline)
    (emagent-chat--register-doom-modeline)
    (when (and doom-modeline-mode (fboundp 'doom-modeline-set-modeline))
      (doom-modeline-set-modeline 'emagent-chat))))

;; Apply the emagent doom-modeline layout whenever an emagent buffer is
;; activated.  `emagent-chat--setup-doom-modeline' registers the format and
;; no-ops unless doom-modeline is loaded, so no `with-eval-after-load' hook
;; on the optional dependency is needed.
(add-hook 'emagent-mode-hook #'emagent-chat--setup-doom-modeline)

(provide 'emagent-chat-mode-line)
;;; emagent-chat-mode-line.el ends here
