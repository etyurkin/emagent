;;; emagent-chat-mode-line.el --- Mode-line status and spinner for emagent  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Busy spinner animation, mode-line status string assembly, context-usage
;; display, and doom-modeline integration for emagent chat buffers.

;;; Code:

(require 'cl-lib)
(require 'map)

(declare-function emagent-chat--open-response-p "emagent-chat")
(declare-function emagent-chat-model "emagent-chat")
(declare-function emagent-acp-busy-p "emagent-acp")
(declare-function emagent-acp-waiting-permission-p "emagent-acp")
(declare-function emagent-acp-ready-p "emagent-acp")
(declare-function emagent-acp-current-tool "emagent-acp")
(declare-function emagent-acp-current-tool-kind "emagent-acp")
(declare-function emagent-acp-agent-rss "emagent-acp")
(declare-function emagent-acp-prompt-finishing-p "emagent-acp")
(declare-function emagent-acp-context-usage "emagent-acp")
(declare-function doom-modeline-set-modeline "ext:doom-modeline")

;;; -------------------------------------------------------------------------
;;; Spinner appearance
;;; -------------------------------------------------------------------------

(defgroup emagent-chat nil
  "Emagent chat UI."
  :group 'emagent)

(defvar emagent-chat--spinner-timer nil
  "Repeating timer that advances the spinner while any session is busy.")

(defvar emagent-chat--spinner-refresh-timer nil
  "One-shot idle timer that applies spinner frame updates to mode lines.")

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
          (force-mode-line-update t)))))
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

(defconst emagent-chat--spinner-dot-frames '((t nil nil) (nil t nil) (nil nil t) (nil t nil))
  "Four-frame chase: @00, 0@0, 00@, 0@0, …")

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
  "Return a propertized on/off dot character."
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

(defun emagent-chat--spinner-string ()
  "Return the current spinner rendering for the mode line."
  (pcase emagent-chat-spinner-style
    ('dots (emagent-chat--spinner-dot-grid))
    (_ (emagent-chat--spinner-braille))))

(defun emagent-chat--mode-line-spinner-suffix ()
  "Return the propertized busy spinner suffix for the mode line."
  (concat " " (emagent-chat--spinner-string)))

;;; -------------------------------------------------------------------------
;;; Mode-line cache and refresh
;;; -------------------------------------------------------------------------

(defvar-local emagent-chat--mode-line-head nil
  "Cached mode-line status prefix for the current emagent buffer.")

(defvar-local emagent-chat--mode-line-tail nil
  "Cached mode-line metadata suffix for the current emagent buffer.")

(defvar-local emagent-chat--mode-line-cache nil
  "Cached full mode-line string for `emagent-mode-line'.")

(defun emagent-chat--spinner-refresh-buffer (buffer)
  "Refresh BUFFER's cached mode line while its session is busy."
  (with-current-buffer buffer
    (when (and emagent-chat--mode-line-tail
               (fboundp 'emagent-acp-busy-p)
               (emagent-acp-busy-p))
      (emagent-chat--mode-line-recompute)
      (when (get-buffer-window buffer 'visible)
        (force-mode-line-update t)))))

(defun emagent-chat--mode-line-recompute ()
  "Rebuild cached mode-line strings for the current emagent buffer."
  (let ((parts (emagent-chat--mode-line-strings)))
    (setq emagent-chat--mode-line-head (car parts)
          emagent-chat--mode-line-tail (cdr parts)
          emagent-chat--mode-line-cache (concat (car parts) (cdr parts)))))

(defvar-local emagent-chat--mode-line-refresh-timer nil
  "One-shot idle timer that coalesces mode-line recomputes for this buffer.")

;;;###autoload
(defun emagent-chat--refresh-mode-line ()
  "Recompute and invalidate the mode line in the current buffer immediately."
  (when emagent-chat--mode-line-refresh-timer
    (cancel-timer emagent-chat--mode-line-refresh-timer)
    (setq emagent-chat--mode-line-refresh-timer nil))
  (emagent-chat--mode-line-recompute)
  (force-mode-line-update t)
  ;; Force immediate redisplay when the session is busy so the
  ;; mode-line animation starts without waiting for user input.
  (when (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
    (redisplay t)))

;;;###autoload
(defun emagent-chat--refresh-mode-line-soon ()
  "Queue a single idle mode-line recompute for the current buffer."
  (let ((buf (current-buffer)))
    (when emagent-chat--mode-line-refresh-timer
      (cancel-timer emagent-chat--mode-line-refresh-timer))
    (setq emagent-chat--mode-line-refresh-timer
          (run-with-idle-timer
           0 nil
           (lambda ()
             (setq emagent-chat--mode-line-refresh-timer nil)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (emagent-chat--mode-line-recompute)
                 (force-mode-line-update t))))))))

;;; -------------------------------------------------------------------------
;;; Mode-line string assembly
;;; -------------------------------------------------------------------------

(defun emagent-chat--mode-line-strings ()
  "Return (HEAD . TAIL) strings for the emagent mode line."
  (let* ((busy  (and (fboundp 'emagent-acp-busy-p)  (emagent-acp-busy-p)))
         (waiting-permission (and (fboundp 'emagent-acp-waiting-permission-p)
                                  (emagent-acp-waiting-permission-p)))
         (ready (and (fboundp 'emagent-acp-ready-p) (emagent-acp-ready-p)))
         (tool  (and (fboundp 'emagent-acp-current-tool) (emagent-acp-current-tool)))
         (kind  (and (fboundp 'emagent-acp-current-tool-kind) (emagent-acp-current-tool-kind)))
         (rss   (and (fboundp 'emagent-acp-agent-rss) (emagent-acp-agent-rss)))
         (connected (or busy ready))
         (spinner (when (and busy (not waiting-permission))
                    (emagent-chat--mode-line-spinner-suffix)))
         (busy-face '(bold mode-line-emphasis))
         (head (cond
                (waiting-permission
                 (propertize "emagent:Allow?" 'face 'warning))
                ((and (not busy) ready
                      (emagent-chat--open-response-p)
                      (not (and (fboundp 'emagent-acp-prompt-finishing-p)
                                (emagent-acp-prompt-finishing-p))))
                 (propertize "emagent:stalled" 'face 'warning))
                ((and busy tool (member kind '("write" "execute")))
                 (concat (propertize "Executing" 'face busy-face)
                         spinner))
                (busy
                 (concat (propertize "Thinking" 'face busy-face)
                         spinner))
                (ready (propertize "emagent:Idle" 'face 'success))
                (connected (propertize "emagent:connecting" 'face 'warning))
                (t "emagent")))
         (model (emagent-chat-model))
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
                       (when rss-str   (concat sep rss-str))
                       (when emagent-chat--pending-prompt
                         (concat sep (propertize "⏳btw" 'face 'warning))))))
    (cons head tail)))

;;; -------------------------------------------------------------------------
;;; Spinner lifecycle
;;; -------------------------------------------------------------------------

(defun emagent-chat--spinner-refresh-idle ()
  "Apply the current spinner frame to busy emagent buffers; stop when idle."
  (setq emagent-chat--spinner-refresh-timer nil)
  (let ((any-busy nil))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (derived-mode-p 'emagent-mode)
                     (fboundp 'emagent-acp-busy-p)
                     (emagent-acp-busy-p))
            (setq any-busy t)
            (emagent-chat--spinner-refresh-buffer buf)))))
    (unless any-busy
      (when emagent-chat--spinner-timer
        (cancel-timer emagent-chat--spinner-timer))
      (setq emagent-chat--spinner-timer nil))))

(defun emagent-chat--spinner-schedule-refresh ()
  "Schedule a lightweight idle refresh of visible busy mode lines."
  (when emagent-chat--spinner-refresh-timer
    (cancel-timer emagent-chat--spinner-refresh-timer))
  (setq emagent-chat--spinner-refresh-timer
        (run-at-time 0 nil #'emagent-chat--spinner-refresh-idle)))

(defun emagent-chat--spinner-tick ()
  "Advance spinner one frame and queue an idle mode-line refresh."
  (setq emagent-chat--spinner-frame
        (% (1+ emagent-chat--spinner-frame)
           (emagent-chat--spinner-frame-count)))
  (emagent-chat--spinner-schedule-refresh))

;;;###autoload
(defun emagent-chat--spinner-restart-timer ()
  "Restart the spinner timer using `emagent-chat-spinner-interval'."
  (when emagent-chat--spinner-timer
    (cancel-timer emagent-chat--spinner-timer))
  (when emagent-chat--spinner-refresh-timer
    (cancel-timer emagent-chat--spinner-refresh-timer)
    (setq emagent-chat--spinner-refresh-timer nil))
  (setq emagent-chat--spinner-timer
        (run-with-timer 0 emagent-chat-spinner-interval
                        #'emagent-chat--spinner-tick)))

;;;###autoload
(defun emagent-chat--spinner-start ()
  "Start the spinner timer if not already running."
  (unless emagent-chat--spinner-timer
    (emagent-chat--spinner-restart-timer)))

;;;###autoload
(defun emagent-chat--mode-line-context-usage ()
  "Return a propertized context fill percentage string, or nil."
  (when-let* ((pair (and (fboundp 'emagent-acp-context-usage)
                         (emagent-acp-context-usage)))
              (used (car pair))
              (size (cdr pair))
              ((and (numberp used) (numberp size) (> size 0))))
    (let ((pct (* 100.0 (/ (float used) size))))
      (propertize (format " ctx:%.0f%%" pct)
                  'face (cond
                         ((>= pct 80) 'error)
                         ((>= pct 50) 'warning)
                         (t           'success))))))

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

(with-eval-after-load 'doom-modeline
  (emagent-chat--register-doom-modeline)
  (add-hook 'emagent-mode-hook #'emagent-chat--setup-doom-modeline)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'emagent-mode)
        (emagent-chat--setup-doom-modeline)))))

(when (featurep 'doom-modeline)
  (emagent-chat--register-doom-modeline))

(provide 'emagent-chat-mode-line)
;;; emagent-chat-mode-line.el ends here
