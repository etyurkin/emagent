;;; emagent-log.el --- Emagent status log buffer -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6
;; SPDX-License-Identifier: MIT
;; Version: 1.2.4

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
;;
;; Emagent session progress, tool activity, OAuth, and agent stderr are
;; appended to `emagent-log-buffer-name' instead of *Messages*.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)

(defconst emagent-log-buffer-name "*Emagent Log*"
  "Name of the buffer that collects emagent status lines.")

(defgroup emagent-log nil
  "Emagent status log buffer."
  :group 'emagent
  :prefix "emagent-log-")

(defcustom emagent-log-max-lines 2000
  "Maximum lines to keep in `emagent-log-buffer-name'.

Set to nil to disable trimming."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'emagent-log)

(defcustom emagent-log-echo-minibuffer nil
  "When non-nil, also mirror emagent log lines to the echo area.

By default emagent writes only to `emagent-log-buffer-name'."
  :type 'boolean
  :group 'emagent-log)

(defface emagent-log-timestamp
  '((t (:inherit font-lock-comment-face)))
  "Face for timestamps in the emagent log."
  :group 'emagent-log)

(defface emagent-log-error
  '((t (:inherit error)))
  "Face for error keywords in the emagent log."
  :group 'emagent-log)

(defface emagent-log-warning
  '((t (:inherit warning)))
  "Face for warning keywords in the emagent log."
  :group 'emagent-log)

(defface emagent-log-success
  '((t (:inherit success)))
  "Face for success keywords in the emagent log."
  :group 'emagent-log)

(defvar emagent-log-font-lock-keywords
  `(("^\\(\\[[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\]\\)"
     (1 'emagent-log-timestamp))
    ("\\b\\(error\\|failed\\|denied\\|refused\\|stalled\\|aborted?\\)\\b"
     (1 'emagent-log-error))
    ("\\b\\(skipping\\|hint\\)\\b"
     (1 'emagent-log-warning))
    ("\\b\\(ok\\|auto-approve\\)\\b"
     (1 'emagent-log-success)))
  "Font-lock keywords for `emagent-log-mode'.

Kept small and keywords-only so append-heavy logging stays cheap.")

(defvar emagent-log--mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "g") #'emagent-log-refresh)
    map)
  "Keymap for `emagent-log-mode'.")

(defun emagent-log--get-buffer ()
  "Return the emagent log buffer, creating it when needed."
  (let ((buffer (get-buffer-create emagent-log-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'emagent-log-mode)
        (emagent-log-mode)))
    buffer))

(defvar-local emagent-log--line-count 0
  "Cached count of lines in the emagent log buffer.")

(defun emagent-log--truncate (buffer lines-added)
  "Drop oldest lines in BUFFER when over `emagent-log-max-lines'.
LINES-ADDED is the number of lines the latest entry inserted.  Uses a
buffer-local counter instead of `count-lines' to avoid O(n) scanning."
  (when (and emagent-log-max-lines (> emagent-log-max-lines 0))
    (with-current-buffer buffer
      (cl-incf emagent-log--line-count lines-added)
      (when (> emagent-log--line-count emagent-log-max-lines)
        (let ((drop (- emagent-log--line-count emagent-log-max-lines)))
          (save-excursion
            (goto-char (point-min))
            (forward-line drop)
            (delete-region (point-min) (point)))
          (cl-decf emagent-log--line-count drop))))))

(defun emagent-log-truncate-line (string width &optional keep-tail)
  "Truncate STRING for logging to display WIDTH.

KEEP-TAIL non-nil keeps the end of STRING visible."
  (setq string (string-trim string))
  (if (<= (string-width string) width)
      string
    (if keep-tail
        (let* ((ellipsis "…")
               (chars (max 1 (- width (string-width ellipsis)))))
          (concat ellipsis
                  (substring string (max 0 (- (length string) chars)))))
      (truncate-string-to-width string width nil nil "…"))))

(defun emagent-log (format-string &rest args)
  "Append a timestamped line to `emagent-log-buffer-name'.

Arguments: FORMAT-STRING, ARGS."
  (let* ((text (apply #'format format-string args))
         (buffer (emagent-log--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert (format-time-string "[%H:%M:%S] ") text)
        ;; A single entry's TEXT may span multiple lines (e.g. the model
        ;; list); count them all so the buffer trims at the real limit.  Count
        ;; content lines, not a spurious extra for a trailing newline.
        (emagent-log--truncate
         buffer (1+ (cl-count ?\n (string-trim-right text "\n")))))
      (when-let ((window (get-buffer-window buffer t)))
        (with-selected-window window
          (goto-char (point-max))
          (recenter -1))))
    (when emagent-log-echo-minibuffer
      (message "%s" text))))

;;;###autoload
(defun emagent-log-view ()
  "Display the emagent status log buffer."
  (interactive)
  (pop-to-buffer (emagent-log--get-buffer))
  (goto-char (point-max)))

(defun emagent-log-refresh ()
  "Go to the end of the emagent log buffer."
  (interactive)
  (goto-char (point-max))
  (recenter -1))

(define-derived-mode emagent-log-mode special-mode "Emagent-Log"
  "Major mode for the emagent status log."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local revert-buffer-function #'emagent-log-refresh)
  ;; Keywords-only (no syntactic pass); jit-lock fonts only visible text.
  (setq font-lock-defaults '(emagent-log-font-lock-keywords t t)))

(provide 'emagent-log)

;;; emagent-log.el ends here
