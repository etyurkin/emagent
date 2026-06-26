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

(declare-function emagent-struct-write-required-p "emagent-struct")
(declare-function emagent-struct-tree "emagent-struct")
(declare-function emagent-struct-find-errors "emagent-struct")
(declare-function emagent-struct-context "emagent-struct")
(declare-function emagent-struct-complete "emagent-struct")
(declare-function emagent-struct-format-file "emagent-struct")
(declare-function emagent-struct-rename-file "emagent-struct")
(declare-function emagent-struct-wrap-file "emagent-struct")
(declare-function emagent-struct-remove-file "emagent-struct")
(declare-function emagent-struct-move-file "emagent-struct")
(declare-function emagent-struct-substitute-file "emagent-struct")
(declare-function emagent-struct-extract-file "emagent-struct")
(declare-function emagent-struct-callers-file "emagent-struct")
(declare-function emagent-struct-instrument-file "emagent-struct")
(declare-function emagent-struct-flatten-file "emagent-struct")
(declare-function emagent-struct-convert-let-file "emagent-struct")
(declare-function emagent-struct-splice-file "emagent-struct")
(declare-function emagent-struct-raise-file "emagent-struct")
(require 'emagent-elisp)
(require 'emagent-tools-file)
(require 'emagent-tools-intro)
(require 'emagent-tools-shell)
(require 'emagent-policy-match)
(require 'emagent-policy-rules-elisp)
(require 'emagent-policy)

(defvaralias 'emagent-tools-eval-blocked-symbols 'emagent-policy-elisp-blocked-symbols)
(defvaralias 'emagent-tools-eval-dangerous-symbols 'emagent-policy-elisp-dangerous-symbols)
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


(defun emagent-tools--buttons-prompt (prompt choices chat-buffer callback &optional preamble)
  "Insert optional PREAMBLE, PROMPT, and CHOICES as buttons in CHAT-BUFFER.
CHOICES is a list of (LABEL . VALUE) pairs.  Non-blocking: inserts the dialog
and returns immediately.  CALLBACK is called with the VALUE when a button is
clicked.  Falls back to `completing-read' (synchronous) when CHAT-BUFFER is
nil or dead, calling CALLBACK with the chosen value."
  (if (not (and chat-buffer (buffer-live-p chat-buffer)))
      (let* ((labels (mapcar #'car choices))
             (label (completing-read (concat prompt " ") labels nil t)))
        (funcall callback (cdr (assoc label choices))))
    (let (start-mark end-mark btn-start (responded nil))
      (let ((do-respond
             (lambda (v)
               (unless responded
                 (setq responded t)
                 (when (and start-mark end-mark
                            (marker-buffer start-mark)
                            (marker-buffer end-mark))
                   (with-current-buffer chat-buffer
                     (let ((inhibit-read-only t))
                       (when (fboundp 'emagent-chat--writable)
                         (funcall #'emagent-chat--writable))
                       (delete-region (marker-position start-mark)
                                      (marker-position end-mark)))))
                 (funcall callback v)))))
        (with-current-buffer chat-buffer
          (let ((inhibit-read-only t))
            (when (fboundp 'emagent-chat--writable)
              (funcall #'emagent-chat--writable))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (setq start-mark (copy-marker (point) nil))
            (when preamble (insert preamble))
            (insert "\n" prompt "\n")
            ;; Build keymap with all shortcuts BEFORE inserting buttons,
            ;; then pass it to each insert-button so the button's own
            ;; overlay keymap contains our shortcuts (higher priority than
            ;; any external overlay we add afterward).
            (let ((btn-keymap (make-sparse-keymap)))
              (setq btn-start (copy-marker (point) nil))
              (set-keymap-parent btn-keymap button-map)
              ;; First pass: define all shortcuts in btn-keymap
              (dolist (choice choices)
                (let* ((v (cdr choice))
                       (key (cond
                             ((memq v '(yes :allow-once)) "y")
                             ((memq v '(no :deny))        "n")
                             ((eq v :allow-session)       "s")
                             ((eq v :allow-always)        "w")
                             ((memq v '(all :allow-all))  "a")
                             (t nil))))
                  (when key
                    (define-key btn-keymap (kbd key)
                                (let ((vv v))
                                  (lambda () (interactive) (funcall do-respond vv)))))))
              ;; Second pass: insert buttons with btn-keymap as their keymap
              (dolist (choice choices)
                (let ((v (cdr choice)))
                  (insert-button
                   (concat "[" (car choice) "]")
                   'keymap btn-keymap
                   'action (lambda (_b) (funcall do-respond v))
                   'follow-link t)
                  (insert "  ")))
              (insert "\n")
              (setq end-mark (copy-marker (point) nil)))))
        (when-let ((win (get-buffer-window chat-buffer)))
          (with-selected-window win
            (when (with-current-buffer chat-buffer
                    (pos-visible-in-window-p (point-max) nil t))
              (goto-char (marker-position btn-start))
              (recenter -3))))))))

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
Returns non-nil when the write is approved.

When `emagent-tools--acp-session-p' is set, return t — ACP handles permission."
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

(defun emagent-tools--symbols-in-form (form symbols)
  "Return symbols from SYMBOLS found anywhere in FORM."
  (emagent-policy-match--symbols-in-form form symbols))

(defun emagent-tools--eval-form-read (form-str)
  "Return FORM-STR parsed as `(progn ,@forms)'."
  (read (concat "(progn " (string-trim (or form-str "")) ")")))

(defun emagent-tools--eval-form-dangerous-allowed-p (form-str dangerous)
  "Return non-nil when the user allows evaluating FORM-STR with DANGEROUS symbols.
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

(defun emagent-tools--eval-form-guard (form-str)
  "Return nil when FORM-STR passes eval guardrails, else an error string."
  (emagent-policy-enforce-string (emagent-policy-check-elisp form-str) form-str))

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

(defun emagent-tools--structural-sync-path (file)
  "Sync FILE buffer content to disk; return absolute path."
  (let ((content (emagent-tools--read-structural-file-content file)))
    (emagent-tools--write-file-content file content)
    (emagent-tools--root-directory file)))

(defun emagent-tools--structural-apply-file-result (file result)
  "Write RESULT to FILE when it is updated content, not a status line."
  (if (or (string-prefix-p "Wrote " result) (string-empty-p result))
      result
    (progn
      (emagent-tools--write-file-content file result)
      (format "Wrote %s" (emagent-tools--root-directory file)))))

(defun emagent-tool-check-structural-file (file)
  "Validate FILE with lisp-sitter (when available)."
  (if (emagent-struct-available-p)
      (emagent-struct-check (emagent-tools--read-structural-file-content file) file)
    (emagent-elisp-check-file-content
     (emagent-tools--read-structural-file-content file) file)))

(defun emagent-tool-check-structural-node (file node)
  "Validate NODE text with lisp-sitter for FILE's language."
  (if (emagent-struct-available-p)
      (emagent-struct-check-node node (emagent-struct--lang-for file))
    (if (string-match-p "\\.el\\'" file)
        (emagent-elisp-check-form node)
      (format "No checker for %s (install lisp-sitter)" file))))

(defun emagent-tool-structural-tree (file &optional depth)
  "Return a structural outline of FILE using lisp-sitter."
  (if (emagent-struct-available-p)
      (emagent-struct-tree (emagent-tools--read-structural-file-content file) file depth)
    (let ((err (emagent-tools--read-structural-file-content file)))
      (if (string-empty-p err)
          ""
        (format "install lisp-sitter to see structural outline of %s" file)))))

(defun emagent-tool-structural-get (file symbol)
  "Return full text of top-level SYMBOL in FILE."
  (emagent-struct-get (emagent-tools--read-structural-file-content file) file symbol))

(defun emagent-tool-structural-find-errors (file)
  "Return tree-sitter MISSING/ERROR nodes for FILE."
  (emagent-struct-find-errors (emagent-tools--structural-sync-path file)))

(defun emagent-tool-structural-context (file)
  "Return outline and full text of each top-level form in FILE."
  (emagent-struct-context (emagent-tools--structural-sync-path file)))

(defun emagent-tool-structural-complete (lang body)
  "Complete missing closing parens in BODY for LANG."
  (emagent-struct-complete lang body))

(defun emagent-tool-structural-format (file &optional write)
  "Re-indent FILE with lisp-sitter."
  (let ((path (emagent-tools--structural-sync-path file)))
    (if write
        (progn
          (emagent-struct-format-file path t)
          (format "Wrote %s" path))
      (emagent-struct-format-file path nil))))

(defun emagent-tool-structural-rename (file old new &optional refs no-refs)
  "Rename top-level form OLD to NEW in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-rename-file (emagent-tools--structural-sync-path file)
                               old new refs no-refs)))

(defun emagent-tool-structural-wrap (file symbol wrap &optional bindings condition)
  "Wrap SYMBOL's body in WRAP in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-wrap-file (emagent-tools--structural-sync-path file)
                             symbol wrap bindings condition)))

(defun emagent-tool-structural-remove (file symbol &optional keep-calls)
  "Remove top-level SYMBOL from FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-remove-file (emagent-tools--structural-sync-path file)
                               symbol keep-calls)))

(defun emagent-tool-structural-move (file symbol after)
  "Move top-level SYMBOL after AFTER in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-move-file (emagent-tools--structural-sync-path file)
                             symbol after)))

(defun emagent-tool-structural-substitute (file symbol pattern replacement)
  "Replace PATTERN with REPLACEMENT inside SYMBOL in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-substitute-file (emagent-tools--structural-sync-path file)
                                   symbol pattern replacement)))

(defun emagent-tool-structural-extract (file symbol pattern name &optional params)
  "Extract PATTERN into new function NAME inside SYMBOL in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-extract-file (emagent-tools--structural-sync-path file)
                                symbol pattern name params)))

(defun emagent-tool-structural-callers (file symbol)
  "Return callers of SYMBOL in FILE."
  (emagent-struct-callers-file (emagent-tools--structural-sync-path file) symbol))

(defun emagent-tool-structural-instrument (file symbol &optional with at wrap)
  "Instrument SYMBOL in FILE with tracing."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-instrument-file (emagent-tools--structural-sync-path file)
                                   symbol with at wrap)))

(defun emagent-tool-structural-flatten (file symbol)
  "Inline SYMBOL at call sites in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-flatten-file (emagent-tools--structural-sync-path file) symbol)))

(defun emagent-tool-structural-convert-let (file symbol to)
  "Convert let/let* for SYMBOL to TO in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-convert-let-file (emagent-tools--structural-sync-path file)
                                    symbol to)))

(defun emagent-tool-structural-splice (file symbol pattern)
  "Splice PATTERN inside SYMBOL in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-splice-file (emagent-tools--structural-sync-path file)
                                symbol pattern)))

(defun emagent-tool-structural-raise (file symbol pattern)
  "Raise PATTERN inside SYMBOL in FILE."
  (emagent-tools--structural-apply-file-result
   file
   (emagent-struct-raise-file (emagent-tools--structural-sync-path file)
                              symbol pattern)))

(defun emagent-tool-structural-bounds (file symbol)
  "Return START:END byte positions for SYMBOL in FILE."
  (emagent-struct-bounds (emagent-tools--read-structural-file-content file)
                         file symbol))

(defun emagent-tool-structural-replace (file symbol new-body)
  "Replace top-level node SYMBOL in FILE with complete NEW-BODY text."
  (when-let ((err (emagent-tools--eval-form-guard new-body)))
    (user-error "%s" err))
  (let* ((content (emagent-tools--read-structural-file-content file))
         (updated (emagent-struct-replace content file symbol new-body)))
    (emagent-tools--write-file-content file updated)
    (when emagent-struct-eval-after-structural-edit
      (ignore-errors (eval (read new-body))))
    (format "Wrote %s" (expand-file-name file))))

(defun emagent-tool-structural-insert (file after-symbol node)
  "Insert complete top-level NODE after AFTER-SYMBOL in FILE."
  (when-let ((err (emagent-tools--eval-form-guard node)))
    (user-error "%s" err))
  (let* ((content (emagent-tools--read-structural-file-content file))
         (updated (emagent-struct-insert content file after-symbol node)))
    (emagent-tools--write-file-content file updated)
    (when emagent-struct-eval-after-structural-edit
      (ignore-errors (eval (read node))))
    (format "Wrote %s" (expand-file-name file))))

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
