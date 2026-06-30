;;; emagent-chat-extract.el --- Response extraction, imenu, and bookmarks  -*- lexical-binding: t; -*-

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

;; Imenu index, bookmark support, and response/source-block extraction
;; for emagent chat buffers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'map)
(require 'bookmark)
(require 'emagent-log)

(declare-function emagent-chat-model "emagent-chat")
(declare-function emagent-chat-set-model "emagent-chat")
(declare-function emagent-chat-project-directory "emagent-chat")
(declare-function emagent-chat-set-agent "emagent-chat")
(declare-function emagent-chat-set-session-id "emagent-chat")
(declare-function emagent-chat-open "emagent-chat")

(defvar emagent-chat-provider)
(defvar emagent-chat--response-headline-re)

;;; -------------------------------------------------------------------------
;;; Imenu
;;; -------------------------------------------------------------------------

(defun emagent-chat--imenu-create-index ()
  "Return an imenu index of user turns for an emagent buffer."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(.*\\)$" nil t)
        (let ((heading (match-string 1)))
          (unless (string-match-p
                   "\\`emagent>\\|\\`\\(?:[/#]\\)\\|\\`\\(?:Thinking\\|Response\\|Request permissions\\)\\'"
                   heading)
            (push (cons heading (match-beginning 0)) index))))
      (nreverse index))))

;;; -------------------------------------------------------------------------
;;; Bookmark support
;;; -------------------------------------------------------------------------

(defun emagent-chat--bookmark-make-record ()
  "Return a bookmark record for the current emagent buffer."
  (let* ((session-id (emagent-chat-session-id))
         (project-dir (emagent-chat-project-directory))
         (model (emagent-chat-model))
         (provider (when emagent-chat-provider (symbol-name emagent-chat-provider))))
    `(,(buffer-name)
      (handler . emagent-chat--bookmark-jump)
      (session-id . ,session-id)
      (project-dir . ,project-dir)
      (model . ,model)
      (provider . ,provider)
      (position . ,(point)))))

;;;###autoload
(defun emagent-chat--bookmark-jump (bookmark)
  "Jump to an emagent BOOKMARK, reopening or reconnecting the session."
  (let* ((session-id (bookmark-prop-get bookmark 'session-id))
         (project-dir (bookmark-prop-get bookmark 'project-dir))
         (model (bookmark-prop-get bookmark 'model))
         (provider (when-let ((p (bookmark-prop-get bookmark 'provider)))
                     (intern p)))
         (pos (bookmark-prop-get bookmark 'position))
         (buffer (when project-dir
                   (emagent-chat-open :project-dir project-dir))))
    (when buffer
      (with-current-buffer buffer
        (when model (emagent-chat-set-model model))
        (when provider (emagent-chat-set-agent provider))
        (when session-id (emagent-chat-set-session-id session-id)))
      (pop-to-buffer buffer)
      (when pos (goto-char pos)))))

;;; -------------------------------------------------------------------------
;;; Response extraction
;;; -------------------------------------------------------------------------

;;;###autoload
(defun emagent-chat--last-response-bounds ()
  "Return (BEG . END) for the last completed `** Response' body, or nil."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward emagent-chat--response-headline-re nil t)
      (forward-line 1)
      (skip-chars-forward "\n")
      (let ((beg (point))
            (end (if (re-search-forward "^\\* " nil t)
                     (line-beginning-position)
                   (point-max))))
        (cons beg end)))))

;;;###autoload
(defun emagent-chat--collect-src-blocks (beg end)
  "Return list of (LANG . CODE) for each src block between BEG and END."
  (let (blocks)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^#\\+BEGIN_SRC \\(.*\\)\n" end t)
        (let* ((lang (string-trim (match-string 1)))
               (start (point))
               (block-end (and (re-search-forward "^#\\+END_SRC\\s-*$" end t)
                               (match-beginning 0))))
          (when block-end
            (push (cons lang (buffer-substring-no-properties start block-end))
                  blocks)))))
    (nreverse blocks)))

;;;###autoload
(defun emagent-chat-insert-last-response ()
  "Insert the last completed agent response into another buffer.

Prompts for a target buffer with `completing-read'."
  (interactive)
  (if-let* ((bounds (emagent-chat--last-response-bounds))
            (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (let* ((others (seq-filter (lambda (b) (not (eq b (current-buffer))))
                                 (buffer-list)))
             (choice (completing-read "Insert response into buffer: "
                                      (mapcar #'buffer-name others) nil t))
             (target (get-buffer choice)))
        (with-current-buffer target
          (insert text))
        (message "emagent: inserted response into %s" choice))
    (message "emagent: no completed response found")))

;;;###autoload
(defun emagent-chat-insert-src-block ()
  "Pick a src block from the last response and insert it into another buffer."
  (interactive)
  (if-let* ((bounds (emagent-chat--last-response-bounds))
            (blocks (emagent-chat--collect-src-blocks (car bounds) (cdr bounds))))
      (let* ((choices
              (cl-loop for (lang . code) in blocks
                       for i from 1
                       collect
                       (cons (format "%d [%s] %s" i lang
                                     (truncate-string-to-width
                                      (car (split-string code "\n")) 60 nil nil "…"))
                             code)))
             (pick (completing-read "Insert src block: "
                                    (mapcar #'car choices) nil t))
             (code (cdr (assoc pick choices)))
             (others (seq-filter (lambda (b) (not (eq b (current-buffer))))
                                 (buffer-list)))
             (target (get-buffer
                      (completing-read "Into buffer: "
                                       (mapcar #'buffer-name others) nil t))))
        (with-current-buffer target
          (insert code))
        (message "emagent: inserted src block into %s" (buffer-name target)))
    (message "emagent: no src blocks found in last response")))

(provide 'emagent-chat-extract)
;;; emagent-chat-extract.el ends here
