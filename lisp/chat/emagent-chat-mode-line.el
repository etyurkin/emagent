;;; emagent-chat-mode-line.el --- Mode-line status and spinner for emagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6

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

;; Busy spinner animation, mode-line status string assembly, context-usage
;; display, and doom-modeline integration for emagent chat buffers.

;;; Code:

(require 'cl-lib)
(require 'map)

(declare-function emagent-chat--open-response-p "emagent-chat")
(declare-function emagent-chat-model-display "emagent-chat")
(declare-function doom-modeline-set-modeline "ext:doom-modeline")

;;; -------------------------------------------------------------------------
;;; Spinner appearance
;;; -------------------------------------------------------------------------

(defgroup emagent-chat nil
  "Emagent chat UI."
  :group 'emagent)

(defvar emagent-chat--spinner-timer nil
  "Repeating timer that advances the spinner while any session is busy.")

(defun emagent-chat--spinner-after-custom-set (sym val)
  "Set SYM to VAL and refresh emagent mode lines."
  (set-default sym val)
  (set sym val)
  (when (and (eq sym 'emagent-chat-spinner-interval)
             emagent-chat--spinner-timer)
    (cancel-timer emagent-chat--spinner-timer)
    (setq emagent-chat--spinner-timer
          (run-with-timer 0 val #'emagent-chat--spinner-tick)))
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'emagent-mode)
          (emagent-chat--mode-line-recompute)
          (emagent-chat--maybe-force-mode-line-update)))))
  nil)

(defcustom emagent-chat-spinner-interval 0.4
  "Seconds between spinner animation frames."
  :type 'number
  :group 'emagent-chat
  :set #'emagent-chat--spinner-after-custom-set)

(defcustom emagent-chat-spinner-height 1.15
  "Scale factor for spinner dots or the braille glyph (`height' face property).
When nil, the spinner inherits the mode-line height."
  :type '(choice (const :tag "inherit" nil) number)
  :group 'emagent-chat
  :set #'emagent-chat--spinner-after-custom-set)

(defcustom emagent-chat-spinner-style 'dots
  "How to render the busy spinner in the mode line.
`braille' is one Unicode braille character; `dots' is three horizontal dots."
  :type '(choice (const :tag "Braille glyph" braille)
                 (const :tag "Dot grid" dots))
  :group 'emagent-chat
  :set #'emagent-chat--spinner-after-custom-set)

(defcustom emagent-chat-spinner-dot-on "●"
  "Character for a lit spinner dot when `emagent-chat-spinner-style' is `dots'."
  :type 'string
  :group 'emagent-chat
  :set #'emagent-chat--spinner-after-custom-set)

(defcustom emagent-chat-spinner-dot-off "○"
  "Character for an unlit spinner dot when `emagent-chat-spinner-style' is `dots'."
  :type 'string
  :group 'emagent-chat
  :set #'emagent-chat--spinner-after-custom-set)

(defface emagent-chat-spinner
  '((t (:inherit (bold mode-line-emphasis))))
  "Face for the mode-line busy spinner glyph."
  :group 'emagent-chat)

;;; -------------------------------------------------------------------------
;;; Frame data and rendering
;;; -------------------------------------------------------------------------

(defconst emagent-chat--spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Braille spinner frames shown while the agent is busy.")

(defvar emagent-chat--spinner-frame 0
  "Current spinner frame index into `emagent-chat--spinner-frames'.")

(defvar emagent-chat--spinner-start-time nil
  "Epoch time when the busy spinner animation started, or nil when idle.")

(defconst emagent-chat--spinner-dot-frames '((t nil nil) (nil t nil) (nil nil t) (nil t nil))
  "Four-frame chase: @00, 0@0, 00@, 0@0, and so on.")

(defun emagent-chat--spinner-frame-count ()
  "Return the number of spinner frames for the active style."
  (pcase emagent-chat-spinner-style
    ('dots (length emagent-chat--spinner-dot-frames))
    (_ (length emagent-chat--spinner-frames))))

(defun emagent-chat--spinner-dot-face (lit)
  "Return the face for spinner dot LIT state."
  (let ((height emagent-chat-spinner-height))
    (if lit
        (if height
            `(:inherit emagent-chat-spinner :height ,height)
          'emagent-chat-spinner)
      (if height
          `(:inherit shadow :height ,height)
        'shadow))))

(defun emagent-chat--spinner-dot-char (lit)
  "Return a propertized on/off dot character.

Arguments: LIT."
  (propertize (if lit emagent-chat-spinner-dot-on emagent-chat-spinner-dot-off)
              'face (emagent-chat--spinner-dot-face lit)))

(defun emagent-chat--spinner-dot-grid ()
  "Return three horizontal dots for the current spinner frame."
  (let ((pattern (nth emagent-chat--spinner-frame emagent-chat--spinner-dot-frames)))
    (concat (emagent-chat--spinner-dot-char (nth 0 pattern))
            (emagent-chat--spinner-dot-char (nth 1 pattern))
            (emagent-chat--spinner-dot-char (nth 2 pattern)))))

(defun emagent-chat--spinner-braille ()
  "Return the current frame as one braille character."
  (let ((face (if emagent-chat-spinner-height
                  `(:inherit emagent-chat-spinner
                            :height ,emagent-chat-spinner-height)
                'emagent-chat-spinner)))
    (propertize (aref emagent-chat--spinner-frames emagent-chat--spinner-frame)
                'face face)))

(defun emagent-chat--spinner-sync-frame ()
  "Update `emagent-chat--spinner-frame' from elapsed time since spinner start."
  (when emagent-chat--spinner-start-time
    (let* ((count (emagent-chat--spinner-frame-count))
           (interval (max 0.05 emagent-chat-spinner-interval)))
      (setq emagent-chat--spinner-frame
            (% (floor (/ (- (float-time) emagent-chat--spinner-start-time)
                         interval))
               count))
      t)))

(defun emagent-chat--spinner-string ()
  "Return the current spinner rendering for the mode line."
  (emagent-chat--spinner-sync-frame)
  (pcase emagent-chat-spinner-style
    ('dots (emagent-chat--spinner-dot-grid))
    (_ (emagent-chat--spinner-braille))))

(defun emagent-chat--mode-line-spinner-suffix ()
  "Return the propertized busy spinner suffix for the mode line."
  (concat " " (emagent-chat--spinner-string)))

(defun emagent-chat--spinner-active-p ()
  "Return non-nil when the mode-line thinking spinner should animate."
  (and (or (emagent-chat--stat :busy) emagent-chat--send-pending)
       (not (emagent-chat--stat :waiting-permission))))

(defun emagent-chat--spinner-animate-p (&optional buffer)
  "Return non-nil when BUFFER is displayed and should animate the spinner.

The spinner keeps animating whenever the buffer is shown in any visible
window, including an unselected window (two emagent buffers side by side) or
while Emacs is unfocused.  It stops only when no visible frame displays the
buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and (emagent-chat--spinner-active-p)
         (emagent-chat--buffer-displayed-p (current-buffer)))))

(defun emagent-chat--any-spinner-active-p ()
  "Return non-nil when any active emagent buffer needs spinner animation."
  (cl-loop for buf in (buffer-list)
           thereis (and (buffer-live-p buf)
                        (with-current-buffer buf
                          (and (derived-mode-p 'emagent-mode)
                               (emagent-chat--spinner-animate-p buf))))))

(declare-function emagent-chat--buffer-active-p "emagent-chat")
(declare-function emagent-chat--buffer-displayed-p "emagent-chat")
(declare-function emagent-chat--maybe-force-mode-line-update "emagent-chat")

(defun emagent-chat--spinner-stop ()
  "Cancel the spinner timer when no session needs animation."
  (when emagent-chat--spinner-timer
    (cancel-timer emagent-chat--spinner-timer))
  (setq emagent-chat--spinner-timer nil
        emagent-chat--spinner-start-time nil))

(defun emagent-chat--spinner-ensure-running ()
  "Start or stop the spinner timer based on active busy emagent buffers."
  (if (emagent-chat--any-spinner-active-p)
      (unless emagent-chat--spinner-timer
        (setq emagent-chat--spinner-start-time (float-time)
              emagent-chat--spinner-frame 0)
        (emagent-chat--spinner-restart-timer))
    (emagent-chat--spinner-stop)))


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
:ctx-usage (a (USED . SIZE) cons or nil) :ctx-unavailable.  The mode line
renders from this snapshot so the UI never calls up into the ACP runtime.")

(defun emagent-chat--stat (key)
  "Return status field KEY from the pushed ACP snapshot."
  (plist-get emagent-chat--status key))

(defun emagent-chat-set-status (status)
  "Store the ACP STATUS snapshot for this buffer and refresh the mode line.
This is the ACP layer's downward entry point (wired as :cb-status); it replaces
the mode line pulling session state back out of the ACP layer."
  (setq emagent-chat--status status)
  (when (and (emagent-chat--stat :busy)
             (fboundp 'emagent-chat--spinner-ensure-running))
    (emagent-chat--spinner-ensure-running))
  (if (emagent-chat--stat :busy)
      (emagent-chat--refresh-mode-line-soon)
    (emagent-chat--refresh-mode-line)))

(defun emagent-chat--spinner-refresh-buffer (buffer)
  "Refresh BUFFER's mode line when it is the active busy emagent buffer."
  (with-current-buffer buffer
    (when (emagent-chat--spinner-animate-p buffer)
      (emagent-chat--mode-line-recompute)
      (emagent-chat--maybe-force-mode-line-update)
      t)))

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

;;; -------------------------------------------------------------------------
;;; Spinner lifecycle
;;; -------------------------------------------------------------------------

(defun emagent-chat--spinner-refresh-idle ()
  "Apply the current spinner frame to the active busy emagent buffer.

Uses `force-mode-line-update' only — never `redisplay'.  A forced redisplay
from this timer re-entered `window-configuration-change-hook' and could run
deferred org table alignment on the chat buffer until Emacs pegged CPU."
  (let (active-refreshed)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (emagent-chat--spinner-refresh-buffer buf)
            (setq active-refreshed t)))))
    (when active-refreshed
      (force-mode-line-update t))
    (emagent-chat--spinner-ensure-running)))

(defun emagent-chat--spinner-tick ()
  "Refresh visible busy mode lines for the current spinner frame."
  (emagent-chat--spinner-sync-frame)
  (emagent-chat--spinner-refresh-idle))

(defun emagent-chat--spinner-restart-timer ()
  "Restart the spinner timer using `emagent-chat-spinner-interval'."
  (when emagent-chat--spinner-timer
    (cancel-timer emagent-chat--spinner-timer))
  (setq emagent-chat--spinner-timer
        (run-with-timer 0 emagent-chat-spinner-interval
                        #'emagent-chat--spinner-tick)))

(defun emagent-chat--spinner-start ()
  "Start the spinner timer if not already running."
  (emagent-chat--spinner-ensure-running)
  (when (derived-mode-p 'emagent-mode)
    (emagent-chat--mode-line-recompute)
    (emagent-chat--maybe-force-mode-line-update)))

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
    (when doom-modeline-mode
      (doom-modeline-set-modeline 'emagent-chat))))

;; Apply the emagent doom-modeline layout whenever an emagent buffer is
;; activated.  `emagent-chat--setup-doom-modeline' registers the format and
;; no-ops unless doom-modeline is loaded, so no `with-eval-after-load' hook
;; on the optional dependency is needed.
(add-hook 'emagent-mode-hook #'emagent-chat--setup-doom-modeline)

(provide 'emagent-chat-mode-line)
;;; emagent-chat-mode-line.el ends here
