;;; emagent-chat-attach.el --- File and image attachment for emagent  -*- lexical-binding: t; -*-

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

;; Buffer context, clipboard image, project file, and error-context
;; attachment for emagent chat prompts.

;;; Code:

(require 'cl-lib)
(require 'emagent-log)
(require 'emagent-context)
(require 'emagent-session)

(eval-when-compile (require 'flymake))

(defvar emagent-chat--on-attach)

;;;###autoload

(defun emagent-chat-attach-buffer ()
  "Attach a buffer summary to the next prompt."
  (interactive)
  (let ((text (emagent-context-buffer-summary)))
    (emagent-log "attached buffer summary to next prompt")
    (when emagent-chat--on-attach
      (funcall emagent-chat--on-attach text))))

;;;###autoload
(defun emagent-chat-yank (&optional arg)
  "Yank text or paste a clipboard image.

If the clipboard contains an image, saves it to a temp file under
`emagent-chat--image-dir' and inserts a [[file:...]] org link at point.
Otherwise behaves exactly like `yank' (ARG is forwarded)."
  (interactive "*P")
  (let ((clip (emagent-chat--clipboard-image)))
    (if clip
        (let ((file (emagent-chat--save-clipboard-image (car clip) (cdr clip))))
          (insert (format "[[file:%s]]" file))
          (message "emagent: clipboard image → %s" (file-name-nondirectory file)))
      (yank arg))))

(defvar emagent-chat--image-dir
  (expand-file-name "emagent/images" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory where clipboard images pasted into emagent buffers are saved.")

(defun emagent-chat--ensure-image-dir ()
  "Ensure `emagent-chat--image-dir' exists and return its path."
  (unless (file-directory-p emagent-chat--image-dir)
    (make-directory emagent-chat--image-dir t))
  emagent-chat--image-dir)

(defun emagent-chat--clipboard-image ()
  "Return (MIME-TYPE-STRING . RAW-BYTES) for a clipboard image, or nil.

Tries PNG, JPEG, GIF, WebP in order and returns the first available type."
  (when (fboundp 'gui-get-selection)
    (let ((targets (ignore-errors (gui-get-selection 'CLIPBOARD 'TARGETS))))
      (when targets
        (let ((target-list (cond ((vectorp targets) (append targets nil))
                                 ((listp targets)   targets)
                                 (t                 nil))))
          (cl-some
           (lambda (mime)
             (when (memq (intern mime) target-list)
               (let ((data (ignore-errors (gui-get-selection 'CLIPBOARD (intern mime)))))
                 (when (and data (not (equal data "")))
                   (cons mime data)))))
           '("image/png" "image/jpeg" "image/gif" "image/webp")))))))

(defun emagent-chat--save-clipboard-image (mime data)
  "Write clipboard image DATA (raw bytes) of MIME type to a temp file.
Returns the file path."
  (let* ((ext (pcase mime
                ("image/jpeg" "jpg")
                ("image/gif"  "gif")
                ("image/webp" "webp")
                (_            "png")))
         (file (expand-file-name
                (format "img-%s.%s" (format-time-string "%Y%m%d-%H%M%S") ext)
                (emagent-chat--ensure-image-dir))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (write-region (point-min) (point-max) file nil 'silent))
    file))

;;;###autoload
(defun emagent-chat-attach-image ()
  "Insert an image link at point for the next prompt (C-c C-e i).

If the clipboard contains an image, saves it to a temp file under
`emagent-chat--image-dir' and inserts a [[file:...]] org link at point.
Otherwise opens a file picker.

On send, emagent finds all [[file:...]] image links in the heading,
base64-encodes them, and sends them as multimodal content blocks alongside
the prompt text."
  (interactive)
  (let ((clip (emagent-chat--clipboard-image)))
    (if clip
        (let ((file (emagent-chat--save-clipboard-image (car clip) (cdr clip))))
          (insert (format "[[file:%s]]" file))
          (message "emagent: clipboard image → %s (C-c C-c to send)"
                   (file-name-nondirectory file)))
      (let ((file (expand-file-name
                   (read-file-name "Attach image: " nil nil t))))
        (insert (format "[[file:%s]]" file))
        (message "emagent: %s attached (C-c C-c to send)"
                 (file-name-nondirectory file))))))

(defun emagent-chat--compilation-error-lines ()
  "Return error lines from *compilation* buffer using text properties, or nil."
  (when-let ((buf (get-buffer "*compilation*")))
    (with-current-buffer buf
      (let (lines)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (get-text-property (point) 'compilation-message)
              (let ((text (string-trim
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))
                (unless (string-empty-p text)
                  (push text lines))))
            (forward-line 1)))
        (nreverse lines)))))

(defun emagent-chat--flymake-error-lines ()
  "Return flymake diagnostic lines from all open file-visiting buffers."
  (when (and (require 'flymake nil t) (fboundp 'flymake-diagnostics))
    (let (lines)
      (dolist (buf (buffer-list))
        (when (and (buffer-file-name buf)
                   (buffer-local-value 'flymake-mode buf))
          (let ((diags (with-current-buffer buf (flymake-diagnostics))))
            (dolist (d diags)
              (push (format "%s:%s [%s] %s"
                            (abbreviate-file-name (buffer-file-name buf))
                            (with-current-buffer buf
                              (line-number-at-pos
                               (flymake-diagnostic-beg d)))
                            (flymake-diagnostic-type d)
                            (flymake-diagnostic-text d))
                    lines)))))
      (nreverse lines))))

;;;###autoload
(defun emagent-chat-attach-error-context ()
  "Attach compilation errors and flymake diagnostics to the next prompt.

Scans `*compilation*' for error lines and all open file buffers for
active flymake diagnostics.  Attaches a combined error context block."
  (interactive)
  (let* ((compile-lines (emagent-chat--compilation-error-lines))
         (flymake-lines (emagent-chat--flymake-error-lines))
         (all (append compile-lines flymake-lines)))
    (if all
        (let ((text (concat "[Error context]\n" (string-join all "\n"))))
          (emagent-log "attached %d error(s) to next prompt" (length all))
          (when emagent-chat--on-attach
            (funcall emagent-chat--on-attach text)))
      (message "emagent: no errors found in compilation buffer or flymake"))))

;;;###autoload
(defun emagent-chat-attach-files ()
  "Pick project files and attach summaries to the next prompt.

Presents `completing-read-multiple' over files under
`emagent-session-project-directory'.  For each chosen file includes its
relative path, size in lines, and a short content preview."
  (interactive)
  (let* ((root (or (emagent-session-project-directory)
                   default-directory))
         (all-files (directory-files-recursively root "[^.].*" nil t))
         (rel-files (seq-filter
                     (lambda (f)
                       (not (string-match-p "/\\.git/" f)))
                     (mapcar (lambda (f) (file-relative-name f root))
                             all-files)))
         (chosen (completing-read-multiple
                  "Attach files (comma-separated): " rel-files nil t))
         (blocks
          (mapcar (lambda (rel)
                    (let* ((abs (expand-file-name rel root))
                           (size (and (file-exists-p abs)
                                      (with-temp-buffer
                                        (insert-file-contents abs nil 0 4096)
                                        (count-lines (point-min) (point-max))))))
                      (format "[File: %s (%s lines)]\n%s"
                              rel (or size "?")
                              (condition-case nil
                                  (with-temp-buffer
                                    (insert-file-contents abs nil 0 2048)
                                    (buffer-string))
                                (error "(unreadable)")))))
                  chosen)))
    (if blocks
        (let ((text (string-join blocks "\n\n")))
          (emagent-log "attached %d file(s) to next prompt" (length blocks))
          (when emagent-chat--on-attach
            (funcall emagent-chat--on-attach text)))
      (message "emagent: no files selected"))))

(provide 'emagent-chat-attach)
;;; emagent-chat-attach.el ends here
