;;; emagent-tools.el --- Emacs tool handlers for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'org)
(require 'org-element)

;; Declared special so binding it to suppress auto-insert is a dynamic binding.
(defvar auto-insert)

(declare-function magit-diff-buffer-file "magit-diff")
(declare-function magit-toplevel "magit-git")
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
`load-file' / `load-library' are here (not hard-blocked) to enable the
write-file-then-load-file pattern for complex Elisp: the agent writes code
with write_file (user reviews diff), then loads it — both steps confirmed."
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
    (let ((result nil) start end)
      (with-current-buffer chat-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (setq start (point))
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
          (setq end (point))))
      (when-let ((win (get-buffer-window chat-buffer)))
        (with-selected-window win
          (when (with-current-buffer chat-buffer
                  (pos-visible-in-window-p (point-max) nil t))
            (goto-char end)
            (recenter -3))))
      (unwind-protect
          (condition-case nil
              (recursive-edit)
            (quit nil))
        (with-current-buffer chat-buffer
          (let ((inhibit-read-only t))
            (delete-region start end))))
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

(defconst emagent-tools--icloud-dir
  (expand-file-name "~/Library/Mobile Documents/"))

(defconst emagent-tools--containers-dir
  (expand-file-name "~/Library/Containers/"))

(defconst emagent-tools--group-containers-dir
  (expand-file-name "~/Library/Group Containers/"))

(defun emagent-tools--protected-fs-path-p (path)
  "Return non-nil when PATH must not be accessed via Emacs on macOS."
  (let ((resolved (file-truename (emagent-tools--root-directory path))))
    (or (string-prefix-p emagent-tools--icloud-dir resolved)
        (string-prefix-p emagent-tools--containers-dir resolved)
        (string-prefix-p emagent-tools--group-containers-dir resolved))))

(defun emagent-tools--file-buffer (path)
  "Return a buffer visiting PATH, visiting it if the file exists."
  (let ((resolved (emagent-tools--root-directory path)))
    (or (find-buffer-visiting resolved)
        (when (file-exists-p resolved)
          (find-file-noselect resolved)))))

(defun emagent-tools--extract-buffer-text (buffer &optional line limit)
  "Return text from BUFFER starting at LINE for LIMIT lines."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (and line (> line 1))
          (forward-line (1- line)))
        (let ((start (point)))
          (if limit
              (forward-line limit)
            (goto-char (point-max)))
          (buffer-substring-no-properties start (point)))))))

(defun emagent-tools--read-file-content (path &optional line limit)
  "Read PATH through Emacs, including unsaved buffer contents."
  (let* ((resolved (emagent-tools--root-directory path))
         (buffer (find-buffer-visiting resolved)))
    (if buffer
        (emagent-tools--extract-buffer-text buffer line limit)
      (with-temp-buffer
        (insert-file-contents resolved)
        (emagent-tools--extract-buffer-text (current-buffer) line limit)))))

(defun emagent-tool-read-file (path &optional line limit)
  "Return contents of PATH as a string."
  (when (emagent-tools--protected-fs-path-p path)
    (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                (emagent-tools--root-directory path)))
  (emagent-tools--read-file-content path line limit))

(defun emagent-tools--write-file-content (path content)
  "Write CONTENT to PATH through an Emacs buffer.
Each call is recorded as a single undoable change in the target buffer."
  (let* ((resolved (emagent-tools--root-directory path))
         (dir (file-name-directory resolved))
         (buffer (or (find-buffer-visiting resolved)
                     (let ((auto-insert nil))
                       (find-file-noselect resolved)))))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t))
    (with-temp-buffer
      (insert content)
      (let ((content-buffer (current-buffer))
            (inhibit-read-only t))
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (undo-boundary)
            (replace-buffer-contents content-buffer 1.0)
            (undo-boundary))
          (basic-save-buffer))))
    (pcase emagent-tools-show-written-buffer
      ('magit-diff
       (with-current-buffer buffer
         (if (and (fboundp 'magit-diff-buffer-file) (magit-toplevel))
             (magit-diff-buffer-file)
           (display-buffer buffer))))
      ((pred identity)
       (display-buffer buffer)))
    resolved))

(defun emagent-tool-write-file (path content)
  "Write CONTENT to PATH through Emacs after user confirmation."
  (let ((resolved (emagent-tools--root-directory path)))
    (when (emagent-tools--protected-fs-path-p path)
      (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                  resolved))
    (emagent-tools--write-file-content path content)
    (format "Wrote %s" resolved)))

(defun emagent-tools--with-stdout (fn)
  "Capture stdout from FN and return it as a string."
  (with-output-to-string
    (princ (funcall fn))))

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
Covers core patterns, string/list/buffer/file/JSON/org-mode operations,
error handling, common pitfalls, and ready-to-use code templates.
Call this before writing non-trivial Elisp."
  (require 'emagent-prompts)
  emagent-acp-elisp-guide)

(defun emagent-tools--check-elisp-parens (form-str)
  "Return nil when FORM-STR parses cleanly, or an error string on failure.
Catches unclosed and extra closing parentheses."
  (condition-case err
      (with-temp-buffer
        (insert "(progn " form-str ")")
        ;; scan-sexps throws scan-error for unclosed '('
        (let ((end (scan-sexps (point-min) 1)))
          ;; Check for trailing content — catches extra ')'
          (goto-char end)
          (skip-chars-forward " \t\n")
          (when (< (point) (point-max))
            (error "Extra closing parenthesis (more ')' than '(')"))
          nil))
    (scan-error
     (format "Unclosed parentheses at char %d: %s"
             (max 0 (- (nth 2 err) 7))
             (nth 1 err)))
    (error
     (format "Parse error: %s" (error-message-string err)))))

(defun emagent-tool-check-elisp (form)
  "Check FORM for Emacs Lisp syntax errors without executing it.
Returns \"OK\" when the form parses cleanly, or an error description.
Always call this before eval for any form longer than 3 lines."
  (let* ((form-str (if (stringp form) form (prin1-to-string form)))
         (paren-error (emagent-tools--check-elisp-parens form-str)))
    (if paren-error
        (format "SYNTAX ERROR — %s\n\nFix the form and call check_elisp again before eval."
                paren-error)
      "OK")))

(defun emagent-tool-eval (form)
  "Evaluate Emacs Lisp FORM and return the result as a string.
Use this for small utilities and text processing — not Python or shell.
Blocked symbols must go through dedicated emagent-tool-* helpers.
For forms longer than 3 lines, call check_elisp first to validate parens."
  (interactive)
  (let* ((form-str (if (stringp form) form (prin1-to-string form)))
         (paren-error (emagent-tools--check-elisp-parens form-str)))
    (when paren-error
      (user-error "Elisp paren/syntax error (fix before eval): %s" paren-error))
    ;; Wrap in progn so multiple top-level forms all execute, not just the first.
    (let* ((wrapped-str (concat "(progn " form-str ")"))
           (parsed (condition-case parse-err
                       (read wrapped-str)
                     (error
                      (user-error "Elisp read error: %s"
                                  (error-message-string parse-err)))))
           (blocked (emagent-tools--symbols-in-form parsed emagent-tools-eval-blocked-symbols))
           (dangerous (emagent-tools--symbols-in-form parsed emagent-tools-eval-dangerous-symbols)))
      (when blocked
        (user-error
         "Eval blocked (%s). Use the dedicated emagent tools instead."
         (mapconcat #'symbol-name blocked ", ")))
      (when dangerous
        (let* ((ops (mapconcat #'symbol-name dangerous ", "))
               (preview (truncate-string-to-width form-str 400 nil nil "…"))
               (preamble (format "\n#+begin_src elisp\n%s\n#+end_src" preview))
               (prompt (format "Eval contains: *%s*" ops))
               (allowed
                (if (and emagent-tools--chat-buffer
                         (buffer-live-p emagent-tools--chat-buffer))
                    (eq 'yes
                        (emagent-tools--buttons-prompt
                         prompt
                         '(("Allow" . yes) ("Deny" . no))
                         emagent-tools--chat-buffer
                         preamble))
                  (y-or-n-p (format "Eval contains %s — allow? " ops)))))
          (unless allowed
            (user-error "Eval cancelled: contains %s" ops))))
      (condition-case err
          (let ((result (eval parsed)))
            (if (null result)
                "nil"
              (prin1-to-string result)))
        (error
         (format "Eval error: %s" (error-message-string err)))))))

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

(defconst emagent-tools--grep-max-results 50)

(defconst emagent-tools--buffer-search-default-limit 20
  "Default max matches returned by `emagent-tool-buffer-search'.")

(defconst emagent-tools--buffer-search-max-context 3
  "Maximum context lines per side in `emagent-tool-buffer-search'.")

(defun emagent-tools--pos-at-line (line)
  "Return point at beginning of 1-based LINE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (> line 1)
      (forward-line (1- line)))
    (point)))

(defun emagent-tools--pos-after-line (line)
  "Return point after end of 1-based LINE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (point)))

(defun emagent-tools--buffer-display-name (resolved)
  "Return a display path for RESOLVED relative to the session root."
  (let ((root emagent-tools--project-directory))
    (if root
        (file-relative-name resolved root)
      (abbreviate-file-name resolved))))

(defun emagent-tools--buffer-search-context-block (center-line context)
  "Return a context block string around 1-based CENTER-LINE in current buffer."
  (let* ((start-line (max 1 (- center-line context)))
         (end-line (+ center-line context))
         (lines nil))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start-line))
      (while (and (<= (line-number-at-pos) end-line)
                  (not (eobp)))
        (push (format "  %d| %s"
                      (line-number-at-pos)
                      (string-trim (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
              lines)
        (forward-line 1)))
    (concat "  --- context ---\n"
            (string-join (nreverse lines) "\n")
            "\n  --- /context ---")))

(defun emagent-tools--buffer-search-run (display-name pattern limit context
                                           search-fn ignore-case backward)
  "Search current narrowed buffer; return result string or nil when no matches."
  (save-excursion
    (let ((output nil)
          (count 0)
          (more nil)
          (resume-from-line nil))
      (if backward
          (goto-char (point-max))
        (goto-char (point-min)))
      (let ((case-fold-search ignore-case))
        (while (and (< count limit)
                    (funcall search-fn pattern nil t))
          (let* ((line (line-number-at-pos))
                 (col (1+ (current-column)))
                 (text (string-trim (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))))
            (setq output
                  (append output
                          (list (format "%s:%s:%s:%s"
                                        display-name line col text))
                          (if (> context 0)
                              (list (emagent-tools--buffer-search-context-block
                                     line context))
                            nil))
                  count (1+ count))))
        (when (and (= count limit)
                   (funcall search-fn pattern nil t))
          (setq more t
                resume-from-line (line-number-at-pos))))
      (list output count more resume-from-line))))

(defun emagent-tool-buffer-search (file pattern &optional from-line to-line limit
                                     literal ignore-case backward context)
  "Search FILE for PATTERN in Emacs, including unsaved buffer content.

Returns grep-style lines (path:line:col:text) plus an optional footer with
pagination hints.  PATTERN is an Emacs regexp unless LITERAL is non-nil.
FROM-LINE and TO-LINE bound the search region (1-based, inclusive)."
  (unless (and file (not (string-empty-p file)))
    (user-error "buffer_search requires file"))
  (unless (and pattern (not (string-empty-p pattern)))
    (user-error "buffer_search requires pattern"))
  (when (emagent-tools--protected-fs-path-p file)
    (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                (emagent-tools--root-directory file)))
  (let* ((resolved (emagent-tools--root-directory file))
         (display-name (emagent-tools--buffer-display-name resolved))
         (buf (or (find-buffer-visiting resolved)
                  (find-file-noselect resolved)))
         (from-line (max 1 (or from-line 1)))
         (limit (min (max 1 (or limit emagent-tools--buffer-search-default-limit))
                     emagent-tools--grep-max-results))
         (context (min emagent-tools--buffer-search-max-context
                       (max 0 (or context 0))))
         (mode-label (if literal "literal" "regex"))
         (search-fn (if backward
                        (if literal #'search-backward #'re-search-backward)
                      (if literal #'search-forward #'re-search-forward))))
    (with-current-buffer buf
      (save-restriction
        (widen)
        (let* ((region-end-line (or to-line (line-number-at-pos (point-max))))
               (region-beg (emagent-tools--pos-at-line from-line))
               (region-end (emagent-tools--pos-after-line region-end-line)))
          (when (> region-beg region-end)
            (user-error "from_line (%d) is after to_line (%d)"
                        from-line region-end-line))
          (narrow-to-region region-beg region-end)
          (condition-case err
              (let* ((run (emagent-tools--buffer-search-run
                           display-name pattern limit context search-fn
                           ignore-case backward))
                     (output (nth 0 run))
                     (count (nth 1 run))
                     (more (nth 2 run))
                     (resume-from-line (nth 3 run)))
                (if (zerop count)
                    (format "No matches in %s (%s, lines %d–%d)"
                            display-name mode-label from-line region-end-line)
                  (let ((footer
                         (format (concat "---\n"
                                         "matches: %d\n"
                                         "more: %s\n"
                                         "resume_from_line: %s\n"
                                         "search: %s from_line=%d to_line=%d "
                                         "ignore_case=%s backward=%s")
                                 count
                                 (if more "true" "false")
                                 (or (and more (number-to-string resume-from-line))
                                     "null")
                                 mode-label
                                 from-line
                                 region-end-line
                                 (if ignore-case "true" "false")
                                 (if backward "true" "false"))))
                    (string-join (append output (list footer)) "\n"))))
            (invalid-regexp
             (format "Invalid regexp: %s" (error-message-string err)))))))))

(defun emagent-tools--grep-emacs (regexp root max)
  "Search REGEXP under ROOT in Emacs, returning at most MAX lines."
  (let ((lines nil)
        (matches 0))
    (dolist (file (directory-files-recursively root "[^.].*" nil t))
      (when (< matches max)
        (unless (string-match-p "/\\.git/" file)
          (with-temp-buffer
            (condition-case nil
                (progn
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (while (and (< matches max)
                              (re-search-forward regexp nil t))
                    (push (format "%s:%s:%s"
                                  (file-relative-name file root)
                                  (line-number-at-pos)
                                  (string-trim (buffer-substring-no-properties
                                                (line-beginning-position)
                                                (line-end-position))))
                          lines)
                    (setq matches (1+ matches))))
              (file-missing nil))))))
    (if lines
        (string-join (nreverse lines) "\n")
      "No matches")))

(defconst emagent-tools--shell-output-limit 100000)

(defconst emagent-tools--fetch-url-limit 100000
  "Maximum response body size returned by `emagent-tool-fetch-url'.")

(defconst emagent-tools--fetch-url-timeout 30
  "Seconds to wait for `url-retrieve-synchronously' in `emagent-tool-fetch-url'.")

(defun emagent-tool-undo-file (path &optional steps)
  "Undo STEPS edits in PATH and save.
Use to revert `emagent-tool-write-file' changes."
  (let* ((resolved (emagent-tools--root-directory path))
         (steps (max 1 (or steps 1)))
         (buffer (emagent-tools--file-buffer path))
         (done 0))
    (unless buffer
      (user-error "No buffer for %s" resolved))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (dotimes (_ steps)
          (condition-case _
              (progn (undo) (setq done (1+ done)))
            (user-error
             (user-error "Only %d undo step(s) available in %s" done resolved))))
        (when (buffer-file-name)
          (basic-save-buffer))))
    (format "Undid %d change(s) in %s" done resolved)))

(defun emagent-tool-delete-file (path)
  "Delete PATH after user confirmation."
  (let ((resolved (emagent-tools--root-directory path)))
    (delete-file resolved t)
    (format "Deleted %s" resolved)))

(defun emagent-tool-delete-directory (path &optional recursive)
  "Delete directory PATH after user confirmation.
When RECURSIVE is non-nil, delete contents as well."
  (let ((resolved (emagent-tools--root-directory path)))
    (delete-directory resolved recursive)
    (format "Deleted %s" resolved)))

(defun emagent-tool-fetch-url (url &optional max-bytes)
  "Fetch URL over HTTP/HTTPS and return the response body as a string.
Runs in Emacs (not the agent sandbox), so network access works when the
agent's built-in WebSearch and shell tools are blocked."
  (unless (and (stringp url) (string-match-p "\\`https?://" url))
    (user-error "fetch_url requires an http:// or https:// URL"))
  (require 'url)
  (let* ((limit (or max-bytes emagent-tools--fetch-url-limit))
         (buffer (url-retrieve-synchronously
                  url nil t emagent-tools--fetch-url-timeout)))
    (unwind-protect
        (if (null buffer)
            (user-error "Failed to fetch %s" url)
          (with-current-buffer buffer
            (goto-char (point-min))
            (unless (re-search-forward "\n\n" nil t)
              (user-error "No HTTP body in response from %s" url))
            (let ((body (buffer-substring-no-properties (point) (point-max))))
              (if (> (length body) limit)
                  (concat (substring body 0 limit) "\n… (output truncated)")
                body))))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(declare-function emagent-shell-run-command "emagent-shell")

(defun emagent-tool-run-shell-command (command &optional directory)
  "Run COMMAND in DIRECTORY through Emacs, not an agent terminal."
  (require 'emagent-shell)
  (emagent-shell-run-command command directory))

(defun emagent-tool-grep (pattern &optional path)
  "Search for PATTERN under PATH and return matching lines as a string.
Uses pure Emacs search when `emagent-acp-prefer-emacs' is non-nil."
  (let* ((root (emagent-tools--root-directory path))
         (regexp (if (stringp pattern) pattern (format "%s" pattern))))
    (if (and (boundp 'emagent-acp-prefer-emacs) emagent-acp-prefer-emacs)
        (emagent-tools--grep-emacs regexp root emagent-tools--grep-max-results)
      (if (executable-find "rg")
          (with-temp-buffer
            (let ((default-directory root))
              (call-process
               "rg" nil t nil "--no-heading" "--line-number"
               "--max-count" (number-to-string emagent-tools--grep-max-results)
               "--hidden" "--glob" "!/.git/*"
               regexp "."))
            (buffer-string))
        (emagent-tools--grep-emacs regexp root emagent-tools--grep-max-results)))))

(defun emagent-tool-list-files (&optional path)
  "List files under PATH relative to PATH, one per line."
  (let ((root (emagent-tools--root-directory path)))
    (string-join
     (mapcar (lambda (file)
               (file-relative-name file root))
             (seq-filter
              (lambda (file)
                (not (string-match-p "/\\.git/" file)))
              (directory-files-recursively root "[^.].*" nil t)))
     "\n")))

(defun emagent-tools--glob-to-regexp (glob)
  "Convert a simple shell GLOB to a regexp."
  (let ((parts nil)
        (i 0)
        (len (length glob)))
    (while (< i len)
      (cond
       ((and (< (1+ i) len)
             (eq (aref glob i) ?*)
             (eq (aref glob (1+ i)) ?*))
        (push ".*" parts)
        (setq i (+ i 2)))
       ((eq (aref glob i) ?*)
        (push "[^/]*" parts)
        (setq i (1+ i)))
       ((eq (aref glob i) ??)
        (push "." parts)
        (setq i (1+ i)))
       (t
        (let ((start i))
          (while (and (< i len)
                      (not (memq (aref glob i) '(?* ??))))
            (setq i (1+ i)))
          (push (regexp-quote (substring glob start i)) parts)))))
    (concat (file-name-as-directory "") (string-join (nreverse parts) ""))))

(defun emagent-tool-find-files (glob &optional path)
  "List files under PATH matching shell GLOB, one relative path per line."
  (let* ((root (emagent-tools--root-directory path))
         (regexp (if (string-match-p "/" glob)
                     (emagent-tools--glob-to-regexp glob)
                   (concat ".*" (emagent-tools--glob-to-regexp glob))))
         (files nil))
    (dolist (file (directory-files-recursively root regexp nil t))
      (unless (string-match-p "/\\.git/" file)
        (push (file-relative-name file root) files)))
    (if files
        (string-join (sort files #'string<) "\n")
      "No matches")))

(defun emagent-tools--run-git (&rest args)
  "Run git ARGS in `default-directory' and return stdout."
  (unless (executable-find "git")
    (user-error "git not found on PATH"))
  (with-temp-buffer
    (apply #'call-process "git" nil t nil args)
    (buffer-string)))

(defun emagent-tool-git-status ()
  "Return git status for the session project directory."
  (string-trim (apply #'emagent-tools--run-git "status" "--short" "--branch")))

(defun emagent-tool-git-diff (&optional args)
  "Return git diff output.  Optional ARGS is extra git diff arguments."
  (string-trim
   (if (and args (not (string-empty-p args)))
       (apply #'emagent-tools--run-git "diff" (split-string args "[[:space:]]+" t))
     (apply #'emagent-tools--run-git "diff"))))

(defun emagent-tool-git-log (&optional args)
  "Return git log output.  Optional ARGS is extra git log arguments."
  (string-trim
   (if (and args (not (string-empty-p args)))
       (apply #'emagent-tools--run-git "log" (split-string args "[[:space:]]+" t))
     (apply #'emagent-tools--run-git "log" "--oneline" "-n" "20"))))

(defun emagent-tool-org-move-subtree-to-parent ()
  "Move org subtree at point to its parent section after confirmation."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (unless (emagent-tools--confirm-p 'emagent-tool-org-move-subtree-to-parent)
    (user-error "Move cancelled"))
  (org-cut-subtree)
  (org-up-element)
  (org-paste-subtree)
  "Moved subtree to parent section")

(defun emagent-tool-compile (command &optional directory)
  "Run COMMAND via `compilation-mode' and return its output as text.

Unlike `run_shell_command', errors appear in a persistent
`*emagent-compile*' buffer navigable with `next-error' / \\[next-error].
The buffer is shown to the user while the build runs."
  (require 'compile)
  (let* ((default-directory (expand-file-name
                             (or directory
                                 emagent-tools--project-directory
                                 default-directory)))
         (buf (compilation-start command 'compilation-mode
                                  (lambda (_) "*emagent-compile*")))
         (proc (get-buffer-process buf)))
    (when proc
      (while (process-live-p proc)
        (accept-process-output proc 0.05 nil t)))
    (with-current-buffer buf
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (if (> (length text) emagent-tools--shell-output-limit)
            (concat (substring text 0 emagent-tools--shell-output-limit)
                    "\n… (output truncated)")
          text)))))

(defun emagent-tool-buffer-list ()
  "Return paths of open Emacs buffers inside the session project, one per line.
Modified buffers are marked with (modified).  Only files within the session
root (`emagent-tools--project-directory') are included."
  (let ((root (and emagent-tools--project-directory
                   (file-name-as-directory
                    (expand-file-name emagent-tools--project-directory)))))
    (string-join
     (delq nil
           (mapcar (lambda (buf)
                     (when-let ((file (buffer-file-name buf)))
                       (let ((expanded (expand-file-name file)))
                         (when (or (null root)
                                   (string-prefix-p root expanded))
                           (format "%s%s"
                                   (if root
                                       (file-relative-name expanded root)
                                     (abbreviate-file-name expanded))
                                   (if (buffer-modified-p buf)
                                       " (modified)"
                                     ""))))))
                   (buffer-list)))
     "\n")))

(defun emagent-tool-imenu-index (&optional file)
  "Return a structural outline (functions, classes, sections) for FILE.
When FILE is omitted, uses the current buffer.  Works for any language
that has imenu support configured (Java, Python, Elisp, JS, org, etc.)."
  (require 'imenu)
  (let* ((resolved (when file (emagent-tools--root-directory file)))
         (buf (if resolved
                  (or (find-buffer-visiting resolved)
                      (find-file-noselect resolved))
                (current-buffer))))
    (with-current-buffer buf
      (let* ((index (condition-case nil
                        (imenu--make-index-alist t)
                      (error nil)))
             (lines nil))
        (cl-labels ((flatten (alist prefix)
                      (dolist (entry alist)
                        (if (imenu--subalist-p entry)
                            (flatten (cdr entry)
                                     (concat prefix (car entry) "/"))
                          (push (concat prefix (car entry)) lines)))))
          (when index (flatten index "")))
        (if lines
            (string-join (nreverse lines) "\n")
          "No imenu index available for this buffer")))))

(provide 'emagent-tools)

;;; emagent-tools.el ends here
