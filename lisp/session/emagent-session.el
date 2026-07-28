;;; emagent-session.el --- Per-buffer emagent session identity  -*- lexical-binding: t; -*-

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
;;
;; Session state, model helpers, and buffer/context capture.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'emagent-trust)

(defun emagent-model-canonical-id (model)
  "Return MODEL id in the form Cursor ACP expects (keep bracket suffixes)."
  (when model
    (if (member model '("auto" "default"))
        "default[]"
      model)))

(defun emagent-model-normalize-id (model)
  "Return a short user-facing label for MODEL.
Strips key=value annotations (e.g. [thinking=true]) and empty brackets ([]).
Maps Cursor default[] to auto."
  (when model
    (let ((stripped (replace-regexp-in-string
                     "\\[\\([^]]*=[^]]*\\)?\\]" "" model)))
      (if (member stripped '("default" "auto")) "auto" stripped))))

(defun emagent-model-choice-label-parts (id &optional name)
  "Return (PRIMARY . SUFFIX) for model ID.
PRIMARY is the base id without bracket annotations; SUFFIX is brackets
plus an optional parenthetical alias when NAME differs from the normalized id."
  (when id
    (let* ((bracket (and (string-match "\\[" id) (match-beginning 0)))
           (base (if bracket (substring id 0 bracket) id))
           (brackets (if bracket (substring id bracket) ""))
           (short-name (and name
                            (not (string= name (emagent-model-normalize-id id)))
                            name)))
      (cons base (concat brackets (if short-name (format " (%s)" short-name) ""))))))

(defun emagent-model-choice-label (id &optional name)
  "Return a `completing-read' label for model ID with the full canonical id.
When NAME differs from the normalized ID (e.g. Auto vs default[]), append it."
  (let ((parts (emagent-model-choice-label-parts id name)))
    (when parts (concat (car parts) (cdr parts)))))

(defun emagent-model-choice-label-display (id &optional name)
  "Like `emagent-model-choice-label', with theme faces for model and details.
The faces `emagent-model-choice-model'/`emagent-model-choice-detail' are
referenced by symbol and resolved at render time, so this stays a leaf.

Arguments: ID, NAME."
  (let ((parts (emagent-model-choice-label-parts id name)))
    (when parts
      (concat (propertize (car parts) 'face 'emagent-model-choice-model)
              (if (string-empty-p (cdr parts))
                  ""
                (propertize (cdr parts) 'face 'emagent-model-choice-detail))))))

(eval-when-compile
  (require 'cl-lib))

(eval-when-compile
  (require 'flymake)
  (require 'which-func))

(defun emagent-context--point-info ()
  "Return point line and column as an alist."
  (save-excursion
    (list (cons :line (line-number-at-pos))
          (cons :column (current-column)))))

(defun emagent-context--region-info ()
  "Return region bounds and text when active."
  (when (region-active-p)
    (list (cons :begin (region-beginning))
          (cons :end (region-end))
          (cons :text (buffer-substring-no-properties (region-beginning) (region-end))))))

(defun emagent-context--org-info ()
  "Return org headline info at point when in `org-mode'.
Skipped in `emagent-mode' buffers: `org-element-at-point' on large session
files is slow and the chat structure is not a normal org document."
  (when (and (derived-mode-p 'org-mode)
             (not (derived-mode-p 'emagent-mode)))
    (ignore-errors
      (let ((element (org-element-at-point)))
        (when (eq (org-element-type element) 'headline)
          (list (cons :title (org-element-property :raw-value element))
                (cons :level (org-element-property :level element))
                (cons :tags (org-element-property :tags element))))))))

(defun emagent-context--which-function ()
  "Return the enclosing function/method name at point, or nil."
  (when (fboundp 'which-function)
    (require 'which-func)
    (ignore-errors (which-function))))

(defun emagent-context--treesit-node ()
  "Return the treesit node type at point, or nil."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p)
             (fboundp 'treesit-node-at))
    (ignore-errors
      (treesit-node-type (treesit-node-at (point))))))

(defun emagent-context--flymake-diagnostics ()
  "Return flymake diagnostics at point as a list of (TYPE . TEXT) pairs."
  (when (and (bound-and-true-p flymake-mode)
             (require 'flymake nil t)
             (fboundp 'flymake-diagnostics))
    (ignore-errors
      (mapcar (lambda (d)
                (cons (format "%s" (flymake-diagnostic-type d))
                      (flymake-diagnostic-text d)))
              (flymake-diagnostics (point))))))

(defun emagent-context-auto ()
  "Build automatic Emacs context for the current buffer."
  (list (cons :buffer (buffer-name))
        (cons :file (or (buffer-file-name) nil))
        (cons :major-mode (symbol-name major-mode))
        (cons :default-directory default-directory)
        (cons :point (emagent-context--point-info))
        (cons :enclosing-function (emagent-context--which-function))
        (cons :treesit-node (emagent-context--treesit-node))
        (cons :region (emagent-context--region-info))
        (cons :flymake (emagent-context--flymake-diagnostics))
        (cons :org (emagent-context--org-info))))

(defun emagent-context-format (context)
  "Format CONTEXT alist as a readable string block."
  (let ((lines
         (list "[Emacs context]"
               (format "buffer: %s" (map-elt context :buffer))
               (format "file: %s" (or (map-elt context :file) "<none>"))
               (format "major-mode: %s" (map-elt context :major-mode))
               (format "default-directory: %s" (map-elt context :default-directory)))))
    (when-let* ((point (map-elt context :point)))
      (setq lines (append lines
                          (list (format "point: line %s, column %s"
                                        (map-elt point :line)
                                        (map-elt point :column))))))
    (when-let* ((fn (map-elt context :enclosing-function)))
      (setq lines (append lines (list (format "enclosing-function: %s" fn)))))
    (when-let* ((node (map-elt context :treesit-node)))
      (setq lines (append lines (list (format "treesit-node: %s" node)))))
    (when-let* ((region (map-elt context :region)))
      (setq lines (append lines
                          (list (format "region: %s-%s"
                                        (map-elt region :begin)
                                        (map-elt region :end))
                                (format "region-text:\n%s" (map-elt region :text))))))
    (when-let* ((diags (map-elt context :flymake)))
      (setq lines (append lines
                          (list (format "flymake-diagnostics: %s"
                                        (mapconcat (lambda (d)
                                                     (format "[%s] %s" (car d) (cdr d)))
                                                   diags "; "))))))
    (when-let* ((org (map-elt context :org)))
      (setq lines (append lines
                          (list (format "org-headline: level %s, title %s"
                                        (map-elt org :level)
                                        (map-elt org :title))))))
    (string-join lines "\n")))

(defun emagent-context-buffer-summary ()
  "Return a short summary of the current buffer."
  (let ((lines (count-lines (point-min) (point-max)))
        (chars (- (point-max) (point-min))))
    (format "[Buffer summary]\nname: %s\nlines: %s\nchars: %s\nmode: %s"
            (buffer-name) lines chars (symbol-name major-mode))))

(defun emagent-context-region ()
  "Return the active region text or signal an error."
  (unless (region-active-p)
    (user-error "No active region"))
  (format "[Region]\n%s"
          (buffer-substring-no-properties (region-beginning) (region-end))))

(defun emagent-context-build-prompt (user-text &optional extra-blocks)
  "Combine USER-TEXT with auto context and EXTRA-BLOCKS."
  (let* ((auto (emagent-context-format (emagent-context-auto)))
         (blocks (cons auto (or extra-blocks nil))))
    (string-join (cons user-text blocks) "\n\n")))

(defun emagent-session-store-read-top-property (name)
  "Return the value of #+NAME at the top of the buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)" name) nil t)
      (string-trim (match-string 1)))))

(defun emagent-session-store-metadata-end ()
  "Return point after emagent comment and metadata header lines."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (or (looking-at "#\\+")
                    (looking-at "# ")
                    (looking-at "#$")))
      (forward-line 1))
    (point)))

(defun emagent-session-store-write-top-property (name value)
  "Insert or update #+NAME in the emagent metadata header.
No-op when #+NAME already holds VALUE, so re-running `emagent-mode' (e.g. on
desktop restore) does not mark the session buffer modified."
  (let* ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (value (format "%s" value))
         (line (format "#+%s: %s" name value))
         (pattern (format "^#\\+%s:[ \t]*.*\n?" name)))
    (unless (equal (emagent-session-store-read-top-property name) value)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (delete-region (match-beginning 0) (match-end 0)))
          (goto-char (emagent-session-store-metadata-end))
          (unless (bolp) (insert "\n"))
          (insert line "\n"))))))

(defun emagent-session-store-delete-top-property (name)
  "Delete #+NAME from the top of the buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "^#\\+%s:.*\n?" name) nil t)
        (replace-match "")))))

(defun emagent-session-store-read-project-property ()
  "Return the #+EMAGENT_PROJECT value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_PROJECT"))

(defun emagent-session-store-read-model-property ()
  "Return the #+EMAGENT_MODEL value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_MODEL"))

(defun emagent-session-store-read-session-property ()
  "Return the #+EMAGENT_SESSION value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_SESSION"))

(defun emagent-session-store-read-agent-property ()
  "Return the #+EMAGENT_AGENT value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_AGENT"))

(defconst emagent-session-store--allowed-tools-property "EMAGENT_ALLOWED_TOOLS")

(defconst emagent-session-store--allowed-permissions-property "EMAGENT_ALLOWED_PERMISSIONS")

(defun emagent-session-store-read-allowed-tools-property ()
  "Return the #+EMAGENT_ALLOWED_TOOLS value as a list of tool symbols."
  (when-let* ((value (emagent-session-store-read-top-property
                      emagent-session-store--allowed-tools-property))
              ((not (string-empty-p value))))
    (mapcar #'intern (split-string value "[ ,]+" t))))

(defun emagent-session-store-read-allowed-permissions-property ()
  "Return #+EMAGENT_ALLOWED_PERMISSIONS as a list of permission fingerprints."
  (when-let* ((value (emagent-session-store-read-top-property
                      emagent-session-store--allowed-permissions-property))
              ((not (string-empty-p value))))
    (split-string value "[ ,]+" t)))

(defun emagent-session-store-display-project-directory (directory)
  "Return DIRECTORY as written in #+EMAGENT_PROJECT."
  (file-name-as-directory
   (abbreviate-file-name (expand-file-name directory))))

;; Names retain the `emagent-chat-' prefix: they are referenced widely and are
;; effectively the persisted-session field names.  Their home is here.

(defvar-local emagent-chat-project-directory nil
  "Project directory for the current emagent buffer.")

(defvar-local emagent-chat-model nil
  "ACP model id for the current emagent buffer.")

(defvar-local emagent-chat-session-id nil
  "ACP session id for the current emagent buffer.")

(defvar-local emagent-chat-provider nil
  "ACP provider symbol (`cursor' or `claude') for the current emagent buffer.")

(defvar-local emagent-chat-cursor-acp-extra-args nil
  "When non-nil, replaces `emagent-cursor-acp-extra-args' for this buffer only.")

(defvar-local emagent-chat-allowed-tools nil
  "Extra MCP tools allowed without confirmation for this buffer session.

Project-wide choices persist under `emagent-permissions-directory'.")

(defvar-local emagent-chat-allowed-permissions nil
  "Legacy buffer-local permission fingerprints from #+EMAGENT_ALLOWED_PERMISSIONS.

New choices persist under `emagent-permissions-directory'.")

(defun emagent-session-id ()
  "Return the persisted ACP session id for the current buffer."
  (or emagent-chat-session-id (emagent-session-store-read-session-property)))

(defun emagent-session-set-id (session-id)
  "Store ACP SESSION-ID in the current buffer."
  (unless (equal emagent-chat-session-id session-id)
    (setq emagent-chat-session-id session-id)
    (emagent-session-store-write-top-property "EMAGENT_SESSION" session-id)))

(defun emagent-session-clear-id ()
  "Remove the ACP session id from the current buffer."
  (setq emagent-chat-session-id nil)
  (emagent-session-store-delete-top-property "EMAGENT_SESSION"))

(defun emagent-session-set-project-directory (directory)
  "Store DIRECTORY as the project directory in the current buffer."
  (let ((dir (expand-file-name directory)))
    (setq emagent-chat-project-directory dir)
    (setq-local default-directory dir)
    (emagent-session-store-write-top-property
     "EMAGENT_PROJECT" (emagent-session-store-display-project-directory dir))))

(defun emagent-session-project-directory ()
  "Return the project directory for the current emagent buffer."
  (or emagent-chat-project-directory (emagent-session-store-read-project-property)))

(defun emagent-session-set-model (model)
  "Store ACP MODEL id in the current buffer.
No UI side effects — callers that need a mode-line refresh add it themselves."
  (setq model (emagent-model-canonical-id model))
  (unless (equal emagent-chat-model model)
    (setq emagent-chat-model model)
    (emagent-session-store-write-top-property "EMAGENT_MODEL" model))
  (setq emagent-chat-model (or emagent-chat-model model)))

(defun emagent-session-model ()
  "Return the ACP model id for the current emagent buffer."
  (emagent-model-canonical-id
   (or emagent-chat-model (emagent-session-store-read-model-property))))

(defun emagent-session-model-display (&optional model)
  "Return MODEL as a short label for the mode line."
  (emagent-model-normalize-id
   (or model (emagent-session-model))))

(defun emagent-session-set-agent (agent)
  "Store the ACP provider AGENT symbol in the current buffer."
  (when agent
    (setq emagent-chat-provider agent)
    (emagent-session-store-write-top-property "EMAGENT_AGENT" (symbol-name agent))))

(defun emagent-session-agent ()
  "Return the ACP provider symbol for the current emagent buffer, or nil."
  (or emagent-chat-provider
      (when-let* ((value (emagent-session-store-read-agent-property))
                  ((not (string-empty-p value))))
        (intern value))))

(defun emagent-session-allowed-tools ()
  "Return MCP tools allowed without confirmation for this buffer's project."
  (let* ((legacy (or emagent-chat-allowed-tools
                     (emagent-session-store-read-allowed-tools-property)))
         (stored (when-let ((dir (emagent-session-project-directory)))
                   (emagent-permissions-project-tools dir))))
    (cl-delete-duplicates (append legacy stored))))

(defun emagent-session-add-allowed-tool (tool)
  "Allow TOOL for this project without confirmation and persist it."
  (let* ((sym (if (stringp tool) (intern tool) tool))
         (dir (emagent-session-project-directory)))
    (unless (memq sym (emagent-session-allowed-tools))
      (setq emagent-chat-allowed-tools (append (or emagent-chat-allowed-tools nil)
                                               (list sym)))
      (when dir
        (emagent-permissions-add-project-tool dir sym)))))

(defun emagent-session-allowed-permissions ()
  "Return legacy buffer permission fingerprints still honored at the gate."
  (or emagent-chat-allowed-permissions
      (emagent-session-store-read-allowed-permissions-property)))

(defun emagent-session-add-allowed-permission (fingerprint)
  "Persist FINGERPRINT as globally allowed for ACP permission requests."
  (emagent-permissions-add-global-fingerprint fingerprint))

(defun emagent-session-allowed-permissions-for (session-id)
  "Return session-scoped permission fingerprints for SESSION-ID."
  (emagent-permissions-session-fingerprints session-id))

(defun emagent-session-add-session-permission (session-id fingerprint)
  "Record FINGERPRINT as session-scoped for SESSION-ID."
  (emagent-permissions-add-session-fingerprint session-id fingerprint))

(defun emagent-session-auto-approve-p (session-id)
  "Return non-nil when SESSION-ID has allow-all enabled."
  (emagent-permissions-session-auto-approve-p session-id))

(defun emagent-session-set-auto-approve (session-id)
  "Enable allow-all for SESSION-ID."
  (emagent-permissions-set-session-auto-approve session-id))

(provide 'emagent-session)
;;; emagent-session.el ends here
