;;; emagent-chat-spinner.el --- Busy spinner for emagent chat mode line  -*- lexical-binding: t; -*-

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

;; Busy spinner customs, faces, frames, and timer lifecycle for emagent chat.
;; Requires `emagent-chat-buffer' for visibility/registry helpers.  Mode-line
;; recompute is optional (`fboundp'-guarded) so this leaf never requires
;; `emagent-chat-mode-line' back (DAG: buffer → spinner → mode-line).

;;; Code:

(require 'emagent-chat-buffer)
(require 'emagent-chat-send-state)

;; Owned by `emagent-chat-mode-line'; read here without requiring it back.
(defvar emagent-chat--status)

(defgroup emagent-chat nil
  "Emagent chat UI."
  :group 'emagent)

(defvar emagent-chat--spinner-timer nil
  "Repeating timer that advances the spinner while any session is busy.")

(defun emagent-chat--maybe-force-mode-line-update ()
  "Refresh this buffer's mode line in every window that displays it.

Updates whenever the buffer is shown in a visible window, not only the selected
one, so the thinking spinner keeps animating in a side-by-side emagent window
after focus moves elsewhere."
  (when (emagent-chat--buffer-displayed-p)
    (force-mode-line-update)))

(defun emagent-chat--spinner-after-custom-set (sym val)
  "Set SYM to VAL and refresh emagent mode lines."
  (set-default sym val)
  (set sym val)
  (when (and (eq sym 'emagent-chat-spinner-interval)
             emagent-chat--spinner-timer)
    (cancel-timer emagent-chat--spinner-timer)
    (setq emagent-chat--spinner-timer
          (run-with-timer 0 val #'emagent-chat--spinner-tick)))
  (emagent-chat--map-live-buffers
   (lambda (buf)
     (with-current-buffer buf
       (when (fboundp 'emagent-chat--mode-line-recompute)
         (emagent-chat--mode-line-recompute))
       (emagent-chat--maybe-force-mode-line-update))))
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
  (and (or (plist-get emagent-chat--status :busy) emagent-chat--send-pending)
       (not (plist-get emagent-chat--status :waiting-permission))))

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
  (catch 'found
    (emagent-chat--map-live-buffers
     (lambda (buf)
       (when (with-current-buffer buf
               (emagent-chat--spinner-animate-p buf))
         (throw 'found t))))
    nil))

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

(defun emagent-chat--spinner-refresh-buffer (buffer)
  "Refresh BUFFER's mode line when it is the active busy emagent buffer."
  (with-current-buffer buffer
    (when (emagent-chat--spinner-animate-p buffer)
      (when (fboundp 'emagent-chat--mode-line-recompute)
        (emagent-chat--mode-line-recompute))
      (emagent-chat--maybe-force-mode-line-update)
      t)))

(defun emagent-chat--spinner-refresh-idle ()
  "Apply the current spinner frame to the active busy emagent buffer.

Uses `force-mode-line-update' only — never `redisplay'.  A forced redisplay
from this timer re-entered `window-configuration-change-hook' and could run
deferred org table alignment on the chat buffer until Emacs pegged CPU."
  (let (active-refreshed)
    (emagent-chat--map-live-buffers
     (lambda (buf)
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
    (when (fboundp 'emagent-chat--mode-line-recompute)
      (emagent-chat--mode-line-recompute))
    (emagent-chat--maybe-force-mode-line-update)))

(provide 'emagent-chat-spinner)
;;; emagent-chat-spinner.el ends here
