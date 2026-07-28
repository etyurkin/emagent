;;; emagent-tools.el --- Emacs tool handlers for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.8
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
;; Tool registry, confirm prompts, and introspection helpers.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-element)
(require 'emagent-chat-ui)
(require 'emagent-policy)
(require 'emagent-tools-core)
(require 'emagent-tools-file)
(require 'emagent-tools-shell)
(require 'emagent-tools-structural)

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

(defun emagent-tools--with-stdout (thunk)
  "Call THUNK after an introspection command and return `help-buffer' text.

THUNK should populate *Help* (e.g. via `describe-function').  Returns
whatever THUNK returns; call sites typically read `help-buffer'."
  (funcall thunk))

(defvar emagent-acp-elisp-guide nil "Forward declaration for the ACP Elisp guide string.")

(defun emagent-tools--symbols-in-form (form symbols)
  "Return symbols from SYMBOLS found anywhere in FORM."
  (emagent-policy-match--symbols-in-form form symbols))

(defun emagent-tools--eval-form-dangerous-allowed-p (form-str dangerous)
  "Return non-nil when evaluating FORM-STR is approved with DANGEROUS symbols.
When `emagent-tools--acp-session-p' is set, return t — ACP handles permission."
  (or emagent-tools--acp-session-p
      (let* ((ops (mapconcat #'symbol-name dangerous ", "))
             (preview (truncate-string-to-width form-str 400 nil nil "…"))
             (preamble (format "\n#+begin_src elisp\n%s\n#+end_src" preview))
             (prompt (format "Eval contains: *%s*" ops)))
        (if (and emagent-tools--chat-buffer
                 (buffer-live-p emagent-tools--chat-buffer))
            (let (result)
              (emagent-tools--buttons-prompt
               prompt '(("Allow" . yes) ("Deny" . no))
               emagent-tools--chat-buffer
               (lambda (v) (setq result v))
               preamble)
              (eq 'yes result))
          (y-or-n-p (format "Eval contains %s — allow? " ops))))))

(defun emagent-tools--eval-form-check (form-str)
  "Return nil when FORM-STR may run; otherwise a permission plist.
`:deny' blocks execution; `:confirm' needs user approval at the ACP gate."
  (emagent-policy-check-elisp form-str))

(defun emagent-tools--eval-form-execute (form-str)
  "Evaluate FORM-STR after guardrails; return nil on success or an error string."
  (condition-case err
      (progn (eval (emagent-tools--eval-form-read form-str)) nil)
    (error (error-message-string err))))

(defun emagent-tool-describe-symbol (symbol)
  "Return documentation for SYMBOL as a string."
  (let ((symbol (if (stringp symbol) (intern symbol) symbol)))
    (cond
     ((fboundp symbol)
      (emagent-tools--with-stdout
       (lambda ()
         (describe-function symbol)
         (with-current-buffer (help-buffer)
           (buffer-string)))))
     ((boundp symbol)
      (emagent-tools--with-stdout
       (lambda ()
         (describe-variable symbol)
         (with-current-buffer (help-buffer)
           (buffer-string)))))
     (t
      (format "No function or variable named %s" symbol)))))

(defun emagent-tool-where-is (command)
  "Return key bindings for COMMAND as a string."
  (let ((command (if (stringp command) (intern-soft command) command)))
    (if (commandp command)
        (emagent-tools--with-stdout
         (lambda ()
           (where-is command)
           (with-current-buffer (help-buffer)
             (buffer-string))))
      (format "Unknown command: %s" command))))

(defconst emagent-tools--apropos-max-results 100 "Max matches returned by apropos tools.")

(defun emagent-tool-apropos (pattern)
  "Return Emacs symbols matching PATTERN, one per line.
Searches symbol names.  Use to discover functions and variables before
calling them."
  (let* ((regexp (if (stringp pattern) pattern (format "%s" pattern)))
         (matches (apropos-internal regexp)))
    (if matches
        (string-join
         (mapcar #'symbol-name
                 (seq-take (sort matches #'string-lessp)
                           emagent-tools--apropos-max-results))
         "\n")
      "No matches")))

(defun emagent-tool-apropos-doc (pattern)
  "Return Emacs symbols whose docstring matches PATTERN, one per line.
Use when you know what a function does but not its name — e.g. apropos_doc
\"split string by delimiter\" to find `split-string'.
Slower than apropos (scans all docstrings) but finds symbols by meaning."
  (let* ((regexp (if (stringp pattern) pattern (format "%s" pattern)))
         (results nil)
         (limit emagent-tools--apropos-max-results))
    (mapatoms
     (lambda (sym)
       (when (< (length results) limit)
         (ignore-errors
           (let* ((fdoc (and (fboundp sym) (documentation sym t)))
                  (vdoc (and (boundp sym)
                             (documentation-property sym 'variable-documentation t)))
                  (doc (or fdoc vdoc)))
             (when (and doc (string-match-p regexp doc))
               (push (format "%s — %s"
                             sym
                             (truncate-string-to-width
                              (car (split-string doc "\n"))
                              80 nil nil "…"))
                     results)))))))
    (if results
        (string-join (nreverse results) "\n")
      "No matches")))

(defun emagent-tool-find-function (symbol)
  "Return the source location of SYMBOL as a string."
  (let ((symbol (if (stringp symbol) (intern-soft symbol) symbol)))
    (if (and symbol (fboundp symbol))
        (emagent-tools--with-stdout
         (lambda ()
           (find-function symbol)
           (with-current-buffer (help-buffer)
             (buffer-string))))
      (format "No function named %s" symbol))))

(defun emagent-tool-elisp-guide ()
  "Return the emagent Emacs Lisp reference guide.
Covers validation, structural editing, core patterns, string/list/buffer/file/
JSON/`org-mode' operations, error handling, common pitfalls, and code templates.
Call this before writing non-trivial Elisp."
  (require 'emagent-prompts)
  emagent-acp-elisp-guide)

(defun emagent-tool-check-elisp (form)
  "Check FORM for Emacs Lisp syntax errors without executing it.
Returns \"OK\" when the form parses cleanly, or an error description.
Always call this before eval for any form longer than 3 lines."
  (emagent-elisp-check-form (if (stringp form) form (prin1-to-string form))))

(defun emagent-tool-eval (form)
  "Evaluate Emacs Lisp FORM and return the result as a string.
Use this for small utilities and text processing — not Python or shell.
Blocked symbols must go through dedicated emagent-tool-* helpers.
For forms longer than 3 lines, call check_elisp first."
  (interactive)
  (emagent-tools--eval-form-safely
   (if (stringp form) form (prin1-to-string form))))

(defun emagent-tool-org-element ()
  "Return structured org element at point as a string."
  (if (derived-mode-p 'org-mode)
      (let* ((element (org-element-at-point))
             (type (org-element-type element))
             (props (cond
                     ((eq type 'headline)
                      `((type . headline)
                        (title . ,(org-element-property :raw-value element))
                        (level . ,(org-element-property :level element))
                        (tags . ,(org-element-property :tags element))))
                     ((eq type 'paragraph)
                      `((type . paragraph)
                        (contents . ,(org-element-contents element))))
                     (t
                      `((type . ,type)
                        (properties . ,element))))))
        (prin1-to-string props))
    "Not in org-mode"))

(defvaralias 'emagent-tools-eval-blocked-symbols 'emagent-policy-elisp-blocked-symbols)

(defvaralias 'emagent-tools-eval-dangerous-symbols 'emagent-policy-elisp-dangerous-symbols)

(defgroup emagent-tools nil
  "Emacs tool handlers for emagent."
  :group 'emagent)

(defvar emagent-tools--project-directory nil
  "Project directory for the active emagent session.")

(defvar emagent-tools--root-boundary nil
  "When non-nil, the absolute directory emagent file tools must stay within.

Bound per session by the emagent MCP dispatcher so a tool call cannot reach
outside the session's project root.  Nil disables the check (the historical
behaviour for non-MCP call sites).")

(defun emagent-tools-set-project-directory (directory)
  "Set project DIRECTORY used by emagent-tool-* when PATH is omitted."
  (setq emagent-tools--project-directory
        (and directory (expand-file-name directory))))

(defun emagent-tool-project-directory ()
  "Return the emagent session project directory as a string."
  (emagent-tools--root-directory nil))

(defcustom emagent-tools-show-written-buffer nil
  "How to reveal a file after emagent writes it.

nil        — do nothing (default; agent writes never touch the window layout)
t          — display the buffer
magit-diff — run `magit-diff-buffer-file' (falls back to `display-buffer'
             when magit is unavailable or the file is outside a git repo)"
  :type '(choice (const :tag "Don't show" nil)
                 (const :tag "Display buffer" t)
                 (const :tag "Magit diff" magit-diff))
  :group 'emagent-tools)

(provide 'emagent-tools)
;;; emagent-tools.el ends here
