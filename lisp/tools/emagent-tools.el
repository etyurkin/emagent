;;; emagent-tools.el --- Emacs tool handlers for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'org)
(require 'org-element)
(require 'emagent-struct)
(require 'emagent-elisp)
(require 'emagent-tools-file)
(require 'emagent-tools-intro)
(require 'emagent-tools-shell)

;; Declared special so binding it to suppress auto-insert is a dynamic binding.
(defvar auto-insert)

(declare-function magit-diff-buffer-file "magit-diff")
(declare-function magit-toplevel "magit-git")
(declare-function emagent-tools--with-stdout "emagent-tools-intro")
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--subalist-p "imenu")

(defvar emagent-acp-elisp-guide)

(defgroup emagent-tools nil
  "Emacs tool handlers for emagent."
  :group 'emagent)

(defcustom emagent-allowed-tools '(emagent-tool-fetch-url)
  "Symbols naming tools that may run without confirmation."
  :type '(repeat symbol)
  :group 'emagent-tools)

(defcustom emagent-tools-eval-blocked-symbols
  '(kill-emacs pause-emacs)
  "Symbols hard-blocked in `emagent-tool-eval'; cannot run under any circumstances.
These are too dangerous to allow even with confirmation:
- kill-emacs / pause-emacs — would terminate or freeze the Emacs process"
  :type '(repeat symbol)
  :group 'emagent-tools)

(defcustom emagent-tools-eval-dangerous-symbols
  '(delete-file delete-directory
    rename-file rename-directory
    copy-file copy-directory
    write-region write-file
    insert-file-contents
    load load-file load-library
    shell-command shell-command-to-string
    call-process start-process start-file-process process-file
    kill-buffer kill-buffer-and-save)
  "Symbols in `emagent-tool-eval' that require explicit user confirmation.
The user sees the full code and must approve before execution.
`load-file' / `load-library' require confirmation when used in eval — not a
shortcut for .el edits; use structural tools for project .el files."
  :type '(repeat symbol)
  :group 'emagent-tools)

(defvar emagent-tools--project-directory nil
  "Project directory for the active emagent session.")

(defvar emagent-tools--root-boundary nil
  "When non-nil, the absolute directory emagent file tools must stay within.

Bound per session by the emagent MCP dispatcher so a tool call cannot reach
outside the session's project root.  Nil disables the check (the historical
behaviour for non-MCP call sites).")

(defun emagent-tools-set-project-directory (directory)
  "Set the project directory used by emagent-tool-* when PATH is omitted."
  (setq emagent-tools--project-directory
        (and directory (expand-file-name directory))))

(defun emagent-tools--within-boundary-p (resolved)
  "Return non-nil when RESOLVED is inside `emagent-tools--root-boundary'."
  (or (null emagent-tools--root-boundary)
      (let ((root (file-name-as-directory
                   (expand-file-name emagent-tools--root-boundary))))
        (or (string-prefix-p root (file-name-as-directory resolved))
            (string= (directory-file-name resolved)
                     (directory-file-name root))))))

(defun emagent-tools--root-directory (path)
  "Return PATH resolved against the active emagent session project directory.

A relative PATH is resolved against the session project directory (not the
process `default-directory'), and an omitted PATH yields that directory.
Signal an error when the result escapes `emagent-tools--root-boundary'."
  (let* ((base (or emagent-tools--project-directory default-directory))
         (resolved (expand-file-name (or path base) base)))
    (unless (emagent-tools--within-boundary-p resolved)
      (user-error "Path %s is outside the session root %s"
                  resolved emagent-tools--root-boundary))
    resolved))

(defun emagent-tool-project-directory ()
  "Return the emagent session project directory as a string."
  (emagent-tools--root-directory nil))

(defvar emagent-tools--session-allowed-tools nil
  "Tools allowed without confirmation for the current session only.

Bound by the MCP dispatcher from the chat buffer's persisted allow-list so a
per-document choice (see `emagent-tools-allow-all-function') is honoured on the
next call without touching the global `emagent-allowed-tools'.")

(defvar emagent-tools-allow-all-function nil
  "Function of one tool symbol, called when the user chooses \"allow all\".

Bound by the MCP dispatcher to persist the choice for the session (emagent
writes it to the chat buffer's =#+EMAGENT_ALLOWED_TOOLS= header).  Nil means the
choice only lasts for the current call.")

(defvar emagent-tools--chat-buffer nil
  "The emagent chat buffer for the active session.
When non-nil, permission prompts are shown as inline buttons there instead
of in the minibuffer.  Bound per MCP dispatch by `emagent-mcp--run-tool'.")

(defconst emagent-tools--permission-choices
  '((?y "yes" "Allow this call only")
    (?a "allow all" "Always allow this tool; save it to the document header")
    (?n "no" "Decline this call"))
  "Choices offered when confirming an emagent tool call.")

(defun emagent-tools--read-permission (prompt)
  "Ask the user PROMPT and return `yes', `all', or `no'."
  (pcase (car (read-multiple-choice prompt emagent-tools--permission-choices))
    (?y 'yes)
    (?a 'all)
    (_ 'no)))

(defun emagent-tools--buttons-prompt (prompt choices chat-buffer &optional preamble)
  "Insert optional PREAMBLE, PROMPT, and CHOICES as buttons in CHAT-BUFFER.
CHOICES is a list of (LABEL . VALUE) pairs.  Blocks via `recursive-edit'
until a button is clicked or C-g is pressed; removes the entire inserted
block (preamble + prompt + buttons) afterward.
Returns the VALUE of the clicked button, or nil on C-g.
Falls back to `completing-read' when CHAT-BUFFER is nil or dead."
  (if (not (and chat-buffer (buffer-live-p chat-buffer)))
      (let* ((labels (mapcar #'car choices))
             (label (completing-read (concat prompt " ") labels nil t)))
        (cdr (assoc label choices)))
    (let ((result nil) start-mark end-mark)
      (with-current-buffer chat-buffer
        (let ((inhibit-read-only t))
          (when (fboundp 'emagent-chat--writable)
            (funcall #'emagent-chat--writable))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (setq start-mark (copy-marker (point) nil))
          (when preamble (insert preamble))
          (insert "\n" prompt "\n")
          (dolist (choice choices)
            (insert-button (concat "[" (car choice) "]")
                           'action (let ((v (cdr choice)))
                                     (lambda (_b)
                                       (setq result v)
                                       (exit-recursive-edit)))
                           'follow-link t)
            (insert "  "))
          (insert "\n")
          (setq end-mark (copy-marker (point) nil))))
      (when-let ((win (get-buffer-window chat-buffer)))
        (with-selected-window win
          (when (with-current-buffer chat-buffer
                  (pos-visible-in-window-p (point-max) nil t))
            (goto-char (marker-position end-mark))
            (recenter -3))))
      (unwind-protect
          (condition-case nil
              (recursive-edit)
            (quit nil))
        (when (and start-mark end-mark
                   (marker-buffer start-mark)
                   (marker-buffer end-mark))
          (with-current-buffer chat-buffer
            (let ((inhibit-read-only t))
              (when (fboundp 'emagent-chat--writable)
                (funcall #'emagent-chat--writable))
              (delete-region (marker-position start-mark)
                             (marker-position end-mark))))))
      result)))

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

(defun emagent-tools--confirm (tool-name prompt)
  "Return non-nil when TOOL-NAME may run, prompting with PROMPT if needed.

When `emagent-tools--chat-buffer' is live, shows inline buttons there.
Otherwise falls back to `read-multiple-choice' in the minibuffer.
\"Allow all\" remembers TOOL-NAME for the session via
`emagent-tools-allow-all-function'."
  (or (emagent-tools--allowed-p tool-name)
      (if (and emagent-tools--chat-buffer
               (buffer-live-p emagent-tools--chat-buffer))
          (pcase (emagent-tools--buttons-prompt
                  prompt
                  '(("Allow" . yes) ("Allow all" . all) ("Deny" . no))
                  emagent-tools--chat-buffer)
            ('yes t)
            ('all (emagent-tools--remember-allowed-tool tool-name) t)
            (_ nil))
        (pcase (emagent-tools--read-permission prompt)
          ('yes t)
          ('all (emagent-tools--remember-allowed-tool tool-name) t)
          (_ nil)))))

(defun emagent-tools--confirm-p (tool-name)
  "Return non-nil when TOOL-NAME may run without confirmation."
  (emagent-tools--confirm tool-name (format "Run emagent tool %s? " tool-name)))

(defun emagent-tools--confirm-action-p (tool-name detail)
  "Like `emagent-tools--confirm-p' but include DETAIL in the prompt."
  (emagent-tools--confirm
   tool-name (format "Allow emagent tool %s: %s? " tool-name detail)))

(defun emagent-tools--write-diff-string (resolved new-content)
  "Return a unified diff string comparing RESOLVED with NEW-CONTENT, or nil."
  (when (executable-find "diff")
    (let ((old-file (make-temp-file "emagent-old-"))
          (new-file (make-temp-file "emagent-new-")))
      (unwind-protect
          (progn
            (if (file-exists-p resolved)
                (copy-file resolved old-file t)
              (write-region "" nil old-file nil 'quiet))
            (write-region new-content nil new-file nil 'quiet)
            (with-temp-buffer
              (call-process "diff" nil t nil "-u"
                            "--label" (concat (file-name-nondirectory resolved)
                                              " (current)")
                            "--label" (concat (file-name-nondirectory resolved)
                                              " (proposed)")
                            old-file new-file)
              (unless (= (point-min) (point-max))
                (buffer-string))))
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))))))

(defun emagent-tools--confirm-write (tool-name resolved new-content &optional chat-buffer)
  "Show diff of NEW-CONTENT vs RESOLVED in CHAT-BUFFER with inline buttons.
Inserts a #+begin_src diff block (when changes exist) followed by Allow /
Allow all / Deny buttons; the whole block is removed after the decision.
Falls back to a minibuffer prompt when CHAT-BUFFER is unavailable.
Returns non-nil when the write is approved."
  (if (emagent-tools--allowed-p tool-name)
      t
    (let* ((diff (emagent-tools--write-diff-string resolved new-content))
           (preamble (when diff
                       (format "\n#+begin_src diff\n%s#+end_src" diff))))
      (pcase (emagent-tools--buttons-prompt
              (format "Write %s?" (file-name-nondirectory resolved))
              '(("Allow" . yes) ("Allow all" . all) ("Deny" . no))
              chat-buffer
              preamble)
        ('all (emagent-tools--remember-allowed-tool tool-name) t)
        ('yes t)
        (_ nil)))))

(defun emagent-tools--symbols-in-form (form symbols)
  "Return symbols from SYMBOLS found anywhere in FORM."
  (let (found stack)
    (setq stack (list form))
    (while stack
      (let ((sexp (pop stack)))
        (when sexp
          (if (memq sexp symbols)
              (push sexp found)
            (when (consp sexp)
              (push (cdr sexp) stack)
              (push (car sexp) stack))))))
    (delete-dups found)))

(defun emagent-tools--eval-form-read (form-str)
  "Return FORM-STR parsed as `(progn ,@forms)'."
  (read (concat "(progn " (string-trim (or form-str "")) ")")))

(defun emagent-tools--eval-form-dangerous-allowed-p (form-str dangerous)
  "Return non-nil when the user allows evaluating FORM-STR with DANGEROUS symbols."
  (let* ((ops (mapconcat #'symbol-name dangerous ", "))
         (preview (truncate-string-to-width form-str 400 nil nil "…"))
         (preamble (format "\n#+begin_src elisp\n%s\n#+end_src" preview))
         (prompt (format "Eval contains: *%s*" ops)))
    (if (and emagent-tools--chat-buffer
             (buffer-live-p emagent-tools--chat-buffer))
        (eq 'yes
            (emagent-tools--buttons-prompt
             prompt
             '(("Allow" . yes) ("Deny" . no))
             emagent-tools--chat-buffer
             preamble))
      (y-or-n-p (format "Eval contains %s — allow? " ops)))))

(defun emagent-tools--eval-form-guard (form-str)
  "Return nil when FORM-STR passes eval guardrails, else an error string."
  (let* ((form-str (string-trim (or form-str "")))
         (parsed (condition-case parse-err
                     (emagent-tools--eval-form-read form-str)
                   (error
                    (format "Elisp read error: %s"
                            (error-message-string parse-err)))))
         (blocked (emagent-tools--symbols-in-form parsed emagent-tools-eval-blocked-symbols))
         (dangerous (emagent-tools--symbols-in-form parsed emagent-tools-eval-dangerous-symbols)))
    (cond
     (blocked
      (format "Eval blocked (%s). Use the dedicated emagent tools instead."
              (mapconcat #'symbol-name blocked ", ")))
     (dangerous
      (unless (emagent-tools--eval-form-dangerous-allowed-p form-str dangerous)
        (format "Eval cancelled: contains %s"
                (mapconcat #'symbol-name dangerous ", "))))
     (t nil))))

(defun emagent-tools--eval-form-execute (form-str)
  "Evaluate FORM-STR after guardrails; return nil on success or an error string."
  (condition-case err
      (progn (eval (emagent-tools--eval-form-read form-str)) nil)
    (error (error-message-string err))))

(defun emagent-tools--eval-form-safely (form-str)
  "Evaluate FORM-STR with syntax and symbol guardrails; return a result string."
  (let ((check-result (emagent-elisp-check-form form-str)))
    (unless (string= "OK" check-result)
      (user-error "%s" check-result))
    (when-let ((err (emagent-tools--eval-form-guard form-str)))
      (user-error "%s" err))
    (condition-case err
        (let ((result (eval (emagent-tools--eval-form-read form-str))))
          (if (null result) "nil" (prin1-to-string result)))
      (error (format "Eval error: %s" (error-message-string err))))))

(defcustom emagent-tools-show-written-buffer 'magit-diff
  "How to reveal a file after emagent writes it.

nil        — do nothing
t          — display the buffer
magit-diff — run `magit-diff-buffer-file' (falls back to `display-buffer'
             when magit is unavailable or the file is outside a git repo)"
  :type '(choice (const :tag "Don't show" nil)
                 (const :tag "Display buffer" t)
                 (const :tag "Magit diff" magit-diff))
  :group 'emagent-tools)

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

(defconst emagent-tools--apropos-max-results 100)

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
JSON/org-mode operations, error handling, common pitfalls, and code templates.
Call this before writing non-trivial Elisp."
  (require 'emagent-prompts)
  emagent-acp-elisp-guide)

(defun emagent-tools--structural-error-p (result prefixes)
  "Return non-nil when RESULT looks like an error string from PREFIXES."
  (or (string-prefix-p "SYNTAX ERROR" result)
      (seq-some (lambda (p) (string-prefix-p p result)) prefixes)))

(defun emagent-tool-check-structural-file (file)
  "Validate FILE with its structural language plugin."
  (emagent-struct-check-file file (emagent-tools--read-structural-file-content file)))

(defun emagent-tool-check-structural-node (file node)
  "Validate NODE text for FILE's structural language plugin."
  (emagent-struct-check-node file node))

(defun emagent-tool-structural-tree (file &optional depth)
  "Return a shallow structural outline of FILE."
  (emagent-struct-outline file (emagent-tools--read-structural-file-content file)
                          depth))

(defun emagent-tool-structural-bounds (file symbol)
  "Return START:END byte positions for SYMBOL in FILE."
  (emagent-struct-node-bounds file (emagent-tools--read-structural-file-content file)
                              symbol))

(defun emagent-tool-structural-replace (file symbol new-body)
  "Replace top-level node SYMBOL in FILE with complete NEW-BODY text."
  (let* ((content (emagent-tools--read-structural-file-content file))
         (updated (emagent-struct-replace-node file content symbol new-body)))
    (if (emagent-tools--structural-error-p
         updated '("No " "new_body" "node must"))
        updated
      (emagent-tools--structural-write file updated new-body))))

(defun emagent-tool-structural-insert (file after-symbol node)
  "Insert complete top-level NODE after AFTER-SYMBOL in FILE."
  (let* ((content (emagent-tools--read-structural-file-content file))
         (updated (emagent-struct-insert-after file content after-symbol node)))
    (if (emagent-tools--structural-error-p
         updated '("No " "node must" "__start__"))
        updated
      (emagent-tools--structural-write file updated node))))

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

(provide 'emagent-tools)
;;; emagent-tools.el ends here
