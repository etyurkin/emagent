;;; emagent-tools-confirm.el --- Tool confirmation and allow-lists  -*- lexical-binding: t; -*-

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

;; Tool allow-lists, session permission state, and write confirmation UI.

;;; Code:

(require 'cl-lib)
(require 'emagent-chat-ui)
(require 'emagent-tools-shell)

(defcustom emagent-allowed-tools '(emagent-tool-fetch-url)
  "Symbols naming tools that may run without confirmation."
  :type '(repeat symbol)
  :group 'emagent-tools)

(defvar emagent-tools--session-allowed-tools nil
  "Tools allowed without confirmation for the current session only.

Bound by the MCP dispatcher from the chat buffer's persisted allow-list so a
per-document choice (see `emagent-tools-allow-all-function') is honoured on
the next call without touching the global `emagent-allowed-tools'.")

(defvar emagent-tools-allow-all-function nil
  "Function of one tool symbol, called when the user chooses \"allow all\".

Bound by the MCP dispatcher to persist the choice per project directory under
`emagent-permissions-directory'.  Nil means the choice only lasts for the
current call.")

(defvar emagent-tools--chat-buffer nil
  "The emagent chat buffer for the active session.
When non-nil, permission prompts are shown as inline buttons there instead
of in the minibuffer.  Bound per MCP dispatch by `emagent-mcp--run-tool'.")

(defvar emagent-tools--acp-session-p nil
  "When non-nil, skip Emacs-side tool confirmation for this call.
ACP chat sessions use `session/request_permission' instead; a second MCP
prompt would not block the agent and is ignored.")

(defun emagent-tools--remember-allowed-tool (tool-name)
  "Record TOOL-NAME as allowed for this session and persist it when possible."
  (unless (memq tool-name emagent-tools--session-allowed-tools)
    (push tool-name emagent-tools--session-allowed-tools))
  (when (functionp emagent-tools-allow-all-function)
    (funcall emagent-tools-allow-all-function tool-name)))

(defun emagent-tools--allowed-p (tool-name)
  "Return non-nil when TOOL-NAME is allowed without confirmation."
  (or (memq tool-name emagent-allowed-tools)
      (memq tool-name emagent-tools--session-allowed-tools)))

(cl-defun emagent-tools--write-diff-string-async (callback resolved new-content)
  "Compare RESOLVED with NEW-CONTENT; call CALLBACK with (diff is-error)."
  (unless (executable-find "diff")
    (funcall callback nil nil)
    (cl-return-from emagent-tools--write-diff-string-async))
  (let ((old-file (make-temp-file "emagent-old-"))
        (new-file (make-temp-file "emagent-new-")))
    (if (file-exists-p resolved)
        (copy-file resolved old-file t)
      (write-region "" nil old-file nil 'quiet))
    (write-region new-content nil new-file nil 'quiet)
    (emagent-tools--run-process-async
     (lambda (output is-error)
       (ignore-errors (delete-file old-file))
       (ignore-errors (delete-file new-file))
       ;; diff exits 1 when the files differ — that is the success case
       ;; here, not an error.  Distinguish it from real trouble (exit 2)
       ;; by the unified-diff header.
       (if (or (string-empty-p output)
               (and is-error (not (string-prefix-p "---" output))))
           (funcall callback nil nil)
         (funcall callback output nil)))
     "diff" "-u"
     "--label" (concat (file-name-nondirectory resolved) " (current)")
     "--label" (concat (file-name-nondirectory resolved) " (proposed)")
     old-file new-file)))

(defun emagent-tools--diff-strings (name old-content new-content)
  "Return a unified diff between OLD-CONTENT and NEW-CONTENT strings, or nil.
NAME labels the sides as `NAME (current)' / `NAME (proposed)'.  Returns nil
when the contents are identical or the diff binary is unavailable."
  (when (executable-find "diff")
    (let ((old-file (make-temp-file "emagent-old-"))
          (new-file (make-temp-file "emagent-new-")))
      (unwind-protect
          (progn
            (write-region old-content nil old-file nil 'quiet)
            (write-region new-content nil new-file nil 'quiet)
            (with-temp-buffer
              (call-process "diff" nil t nil "-u"
                            "--label" (concat name " (current)")
                            "--label" (concat name " (proposed)")
                            old-file new-file)
              (unless (= (point-min) (point-max))
                (buffer-string))))
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))))))

(defun emagent-tools--write-diff-string (resolved new-content)
  "Return a unified diff string comparing RESOLVED with NEW-CONTENT, or nil."
  (emagent-tools--diff-strings
   (file-name-nondirectory resolved)
   (if (file-exists-p resolved)
       (with-temp-buffer
         (insert-file-contents resolved)
         (buffer-string))
     "")
   new-content))

(defun emagent-tools--confirm-write (tool-name resolved new-content &optional chat-buffer)
  "Show diff of NEW-CONTENT vs RESOLVED in CHAT-BUFFER with inline buttons.
Inserts a #+begin_src diff block (when changes exist) followed by Allow /
Allow all / Deny buttons; the whole block is removed after the decision.
Falls back to a minibuffer prompt when CHAT-BUFFER is unavailable.
Returns non-nil when the write is approved.

When `emagent-tools--acp-session-p' is set, return t — ACP handles permission.

Arguments: TOOL-NAME."
  (if (or emagent-tools--acp-session-p (emagent-tools--allowed-p tool-name))
      t
    (let* ((diff (emagent-tools--write-diff-string resolved new-content))
           (preamble (when diff (format "\n#+begin_src diff\n%s#+end_src" diff)))
           (choice nil))
      (emagent-tools--buttons-prompt
       (format "Write %s?" (file-name-nondirectory resolved))
       '(("Allow" . yes) ("Allow all" . all) ("Deny" . no))
       chat-buffer
       (lambda (v) (setq choice v))
       preamble)
      (pcase choice
        ('all (emagent-tools--remember-allowed-tool tool-name) t)
        ('yes t)
        (_ nil)))))

(provide 'emagent-tools-confirm)
;;; emagent-tools-confirm.el ends here
