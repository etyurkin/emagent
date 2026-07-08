;;; emagent-policy.el --- Unified security policy for emagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Evgeniy Tyurkin

;;; Commentary:

;; Single entry point for permission and execution-time checks.

;;; Code:

(require 'cl-lib)
(require 'emagent-policy-match)
(require 'emagent-policy-rules-shell)
(require 'emagent-policy-rules-elisp)
(require 'emagent-policy-rules-python)

(declare-function emagent-tools--buttons-prompt "emagent-tools")
(defvar emagent-tools--chat-buffer)
(defvar emagent-tools--acp-session-p)

(defun emagent-policy--verdict-from-merge (verdict)
  "Normalize internal VERDICT cons to a permission plist or nil."
  (pcase (car verdict)
    ('deny `(:deny . ,(cdr verdict)))
    ('confirm `(:confirm . ,(cdr verdict)))
    (_ nil)))

(defun emagent-policy--check-rule-list (rules predicate)
  "Check RULES with PREDICATE; return highest-severity internal verdict or nil."
  (let ((verdict nil))
    (dolist (rule rules)
      (when-let ((reason (funcall predicate rule)))
        (setq verdict (emagent-policy-match--merge-verdict
                        verdict
                        (plist-get rule :severity)
                        reason))))
    (emagent-policy--verdict-from-merge verdict)))

(defun emagent-policy--verdict-rank (verdict)
  "Return precedence rank for a permission VERDICT (:deny > :confirm > nil)."
  (pcase (car-safe verdict) (:deny 2) (:confirm 1) (_ 0)))

(defun emagent-policy--merge-plist-verdict (a b)
  "Return the higher-precedence permission verdict between A and B."
  (if (>= (emagent-policy--verdict-rank a) (emagent-policy--verdict-rank b))
      (or a b)
    b))

(defun emagent-policy--check-one-shell (command)
  "Check a single shell COMMAND string against shell and embedded-python rules."
  (or (emagent-policy--check-rule-list
       (emagent-policy--all-shell-rules)
       (lambda (rule)
         (and (emagent-policy-match--shell-rule-p rule command)
              (plist-get rule :reason))))
      (when-let ((code (emagent-policy-match--python-c-code command)))
        (emagent-policy-check-python code))))

(defun emagent-policy-check-shell (command)
  "Check shell COMMAND against shell and embedded-python rules.
Return nil when ok, (:deny . REASON), or (:confirm . REASON).

Checks the whole command (so rules that span a pipeline, e.g. `curl | sh',
still fire) AND each decomposed leaf command (so a dangerous argv hidden behind
`&&'/`;'/`|' or inside `sh -c'/`sudo' is caught), keeping the worst verdict."
  (when (and (stringp command) (not (string-empty-p (string-trim command))))
    (let ((worst (emagent-policy--check-one-shell command)))
      (dolist (leaf (emagent-policy-shell-commands command))
        (unless (string= leaf command)
          (setq worst (emagent-policy--merge-plist-verdict
                       worst (emagent-policy--check-one-shell leaf)))))
      worst)))

(defun emagent-policy-check-python (code)
  "Check python CODE against `emagent-policy-python-rules'.
Return nil when ok, (:deny . REASON), or (:confirm . REASON)."
  (when (and (stringp code) (not (string-empty-p (string-trim code))))
    (emagent-policy--check-rule-list
     (emagent-policy--all-python-rules)
     (lambda (rule)
       (and (emagent-policy-match--python-rule-p rule code)
            (plist-get rule :reason))))))

(defun emagent-policy--elisp-read (form-str)
  "Parse FORM-STR as progn or return a deny plist on read error."
  (condition-case parse-err
      (read (concat "(progn " (string-trim (or form-str "")) ")"))
    (error
     `(:deny . ,(format "Elisp read error: %s"
                        (error-message-string parse-err))))))

(defun emagent-policy--elisp-rule-reason (rule parsed)
  "Return reason string when RULE matches PARSED elisp form."
  (when (emagent-policy-match--elisp-rule-p rule parsed)
    (let ((symbols (emagent-policy-match--elisp-matched-symbols rule parsed)))
      (when symbols
        (pcase (plist-get rule :reason-kind)
          ('blocked
           (format "Eval blocked (%s). Use the dedicated emagent tools instead."
                   (mapconcat #'symbol-name symbols ", ")))
          ('dangerous
           (format "Eval contains: %s"
                   (mapconcat #'symbol-name symbols ", ")))
          (_ (or (plist-get rule :reason)
                 (format "Eval contains: %s"
                         (mapconcat #'symbol-name symbols ", ")))))))))

(defun emagent-policy-check-elisp (form-str)
  "Check elisp FORM-STR against `emagent-policy--all-elisp-rules'.
Return nil when ok, (:deny . REASON), or (:confirm . REASON)."
  (let* ((form-str (string-trim (or form-str "")))
         (parsed (emagent-policy--elisp-read form-str)))
    (if (and (listp parsed) (eq (car parsed) :deny))
        parsed
      (emagent-policy--check-rule-list
       (emagent-policy--all-elisp-rules)
       (lambda (rule)
         (emagent-policy--elisp-rule-reason rule parsed))))))

(defun emagent-policy-check (kind content)
  "Dispatch policy check for KIND (shell, elisp, python) and CONTENT."
  (pcase kind
    ('shell (emagent-policy-check-shell content))
    ('elisp (emagent-policy-check-elisp content))
    ('python (emagent-policy-check-python content))
    (_ nil)))

(defun emagent-policy-shell-needs-confirm-p (command)
  "Return non-nil when COMMAND needs user confirmation."
  (let ((verdict (emagent-policy-check-shell command)))
    (and verdict (memq (car verdict) '(:deny :confirm)))))

(defun emagent-policy-shell-deny-p (command)
  "Return non-nil when COMMAND is hard-blocked by policy."
  (eq (car (emagent-policy-check-shell command)) :deny))

(defun emagent-policy-rule-id-matches-p (kind id content)
  "Return non-nil when rule ID of KIND matches CONTENT."
  (let* ((rules (pcase kind
                  ('shell (emagent-policy--all-shell-rules))
                  ('elisp (emagent-policy--all-elisp-rules))
                  ('python (emagent-policy--all-python-rules))
                  (_ nil)))
         (rule (cl-find id rules :key (lambda (r) (plist-get r :id)) :test #'equal)))
    (and rule
         (pcase kind
           ('shell (emagent-policy-match--shell-rule-p rule content))
           ('elisp
            (let ((parsed (emagent-policy--elisp-read content)))
              (and (not (and (listp parsed) (eq (car parsed) :deny)))
                   (emagent-policy-match--elisp-rule-p rule parsed))))
           ('python (emagent-policy-match--python-rule-p rule content))
           (_ nil)))))

(defun emagent-policy--skip-runtime-confirm-p ()
  "Return non-nil when execution-time confirm prompts should be skipped."
  (and (boundp 'emagent-tools--acp-session-p)
       emagent-tools--acp-session-p))

(defun emagent-policy--runtime-confirm-p (reason context)
  "Prompt for runtime confirmation; return non-nil when allowed."
  (or (emagent-policy--skip-runtime-confirm-p)
      (let* ((preview (truncate-string-to-width (or context "") 400 nil nil "…"))
             (preamble (when (not (string-empty-p preview))
                         (format "\n#+begin_src %s\n%s\n#+end_src"
                                 (if (string-match-p "\\`\\(?:python\\|python3\\)" context)
                                     "python"
                                   "shell")
                                 preview)))
             (prompt (format "Policy check: *%s*" reason)))
        (if (and (boundp 'emagent-tools--chat-buffer)
                 emagent-tools--chat-buffer
                 (buffer-live-p emagent-tools--chat-buffer))
            (eq 'yes
                (emagent-tools--buttons-prompt
                 prompt
                 '(("Allow" . yes) ("Deny" . no))
                 emagent-tools--chat-buffer
                 preamble))
          (y-or-n-p (format "%s — allow? " prompt))))))

(defun emagent-policy-enforce (verdict &optional context)
  "Apply VERDICT at execution time; signal `user-error' when blocked."
  (pcase verdict
    (`(:deny . ,msg) (user-error "%s" msg))
    (`(:confirm . ,msg)
     (unless (emagent-policy--runtime-confirm-p msg context)
       (user-error "Cancelled: %s" msg)))
    (_ nil)))

(defun emagent-policy-enforce-string (verdict &optional context)
  "Like `emagent-policy-enforce' but return an error string instead of signaling."
  (pcase verdict
    (`(:deny . ,msg) msg)
    (`(:confirm . ,msg)
     (unless (emagent-policy--runtime-confirm-p msg context)
       (format "Cancelled: %s" msg)))
    (_ nil)))

(provide 'emagent-policy)
;;; emagent-policy.el ends here
