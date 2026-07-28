;;; emagent-policy.el --- Unified security policy for emagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; SPDX-License-Identifier: MIT

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

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
;; Policy engine for tool and shell execution: matching, shell/elisp/python
;; rule tables, and enforcement helpers.
;;
;;; Code:

(require 'cl-lib)
(require 'emagent-chat-ui)
(require 'emagent-policy-match)

(defgroup emagent-policy nil
  "Security policy rules for emagent tool execution."
  :group 'emagent
  :prefix "emagent-policy-")

(defcustom emagent-policy-extra-shell-rules nil
  "Extra shell policy rules appended after `emagent-policy-shell-rules'."
  :type '(repeat plist)
  :group 'emagent-policy)

(defconst emagent-policy-shell-rules
  '((:id shell-rm-combined-rf
     :severity confirm
     :reason "rm with combined -rf/-fr flags"
     :match ((regexp . "\\brm[[:space:]]+-[rf]+")))
    (:id shell-rm-recursive
     :severity confirm
     :reason "rm --recursive"
     :match ((argv-first . "rm") (long-flag . "--recursive")))
    (:id shell-rm-force
     :severity confirm
     :reason "rm --force"
     :match ((argv-first . "rm") (long-flag . "--force")))
    (:id shell-dd
     :severity confirm
     :reason "direct disk write (dd)"
     :match ((argv-first . "dd")))
    (:id shell-mkfs
     :severity confirm
     :reason "filesystem format (mkfs)"
     :match ((regexp . "\\bmkfs\\.")))
    (:id shell-mke2fs
     :severity confirm
     :reason "filesystem format (mke2fs)"
     :match ((argv-first . "mke2fs")))
    (:id shell-format
     :severity confirm
     :reason "disk format"
     :match ((argv-first . "format")))
    (:id shell-shutdown
     :severity confirm
     :reason "system shutdown"
     :match ((argv-first . "shutdown")))
    (:id shell-reboot
     :severity confirm
     :reason "system reboot"
     :match ((argv-first . "reboot")))
    (:id shell-init-0
     :severity confirm
     :reason "init 0 (halt)"
     :match ((argv-first . "init") (argv-index . (2 . "0"))))
    (:id shell-sudo-rm
     :severity confirm
     :reason "sudo rm"
     :match ((argv-first . "sudo") (argv-index . (2 . "rm"))))
    (:id shell-curl-pipe-sh
     :severity confirm
     :reason "pipe curl into shell"
     :match ((pipe-to-shell . t)))
    (:id shell-trash
     :severity confirm
     :reason "trash CLI"
     :match ((argv-first . "trash")))
    (:id shell-kill-9
     :severity confirm
     :reason "kill -9"
     :match ((argv-first . "kill") (any-flag . ("-9" "-KILL"))))
    (:id shell-disk-overwrite
     :severity confirm
     :reason "overwrite block device"
     :match ((regexp . ">*/dev/[sh]d[a-z]")))
    (:id shell-chmod-world
     :severity confirm
     :reason "world-writable chmod"
     :match ((argv-first . "chmod") (regexp . "-R[[:space:]]*777"))))
  "Built-in shell policy rules, highest severity wins when several match.")

(defun emagent-policy--all-shell-rules ()
  "Return built-in and user `emagent-policy-extra-shell-rules'."
  (append emagent-policy-shell-rules emagent-policy-extra-shell-rules))

(defcustom emagent-policy-elisp-blocked-symbols
  '(kill-emacs pause-emacs)
  "Symbols hard-blocked in eval; cannot run under any circumstances."
  :type '(repeat symbol)
  :group 'emagent-policy)

(defcustom emagent-policy-elisp-shell-blocked-symbols
  '(shell-command shell-command-to-string
    call-process call-process-shell-command process-file)
  "Synchronous shell functions blocked in eval.
These block Emacs until the subprocess exits; use the run_shell_command
tool instead, which runs the process asynchronously."
  :type '(repeat symbol)
  :group 'emagent-policy)

(defcustom emagent-policy-elisp-dangerous-symbols
  '(delete-file delete-directory
    rename-file rename-directory
    copy-file copy-directory
    write-region write-file
    insert-file-contents
    load load-file load-library
    start-process start-file-process
    kill-buffer kill-buffer-and-save)
  "Symbols in eval that require explicit user confirmation."
  :type '(repeat symbol)
  :group 'emagent-policy)

(defcustom emagent-policy-extra-elisp-rules nil
  "Extra elisp policy rules appended after built-in symbol rules."
  :type '(repeat plist)
  :group 'emagent-policy)

(defun emagent-policy--builtin-elisp-rules ()
  "Return symbol-based elisp rules from `emagent-policy-elisp-*-symbols'."
  (list
   `(:id elisp-blocked
     :severity deny
     :reason-kind blocked
     :match ((any-symbol . ,emagent-policy-elisp-blocked-symbols)))
   `(:id elisp-shell-blocked
     :severity deny
     :reason "Synchronous shell functions block Emacs. Use the run_shell_command tool instead — it runs the process asynchronously."
     :match ((any-symbol . ,emagent-policy-elisp-shell-blocked-symbols)))
   `(:id elisp-dangerous
     :severity confirm
     :reason-kind dangerous
     :match ((any-symbol . ,emagent-policy-elisp-dangerous-symbols)))))

(defun emagent-policy--all-elisp-rules ()
  "Return built-in and user `emagent-policy-extra-elisp-rules'."
  (append (emagent-policy--builtin-elisp-rules)
          emagent-policy-extra-elisp-rules))

(defcustom emagent-policy-extra-python-rules nil
  "Extra python policy rules appended after `emagent-policy-python-rules'."
  :type '(repeat plist)
  :group 'emagent-policy)

(defconst emagent-policy-python-rules
  '((:id python-os-system
     :severity confirm
     :reason "os.system"
     :match ((regexp . "\\<os\\.system[[:space:]]*(")))
    (:id python-os-popen
     :severity confirm
     :reason "os.popen"
     :match ((regexp . "\\<os\\.popen[[:space:]]*(")))
    (:id python-subprocess-import
     :severity confirm
     :reason "subprocess"
     :match ((import-module . "subprocess")))
    (:id python-subprocess-call
     :severity confirm
     :reason "subprocess"
     :match ((regexp . "\\<subprocess\\.")))
    (:id python-shutil-rmtree
     :severity confirm
     :reason "shutil.rmtree"
     :match ((regexp . "\\<shutil\\.rmtree[[:space:]]*(")))
    (:id python-os-remove
     :severity confirm
     :reason "os.remove/unlink"
     :match ((regexp . "\\<os\\.\\(?:remove\\|unlink\\|rmdir\\)[[:space:]]*(")))
    (:id python-exec-eval
     :severity confirm
     :reason "exec/eval/compile"
     :match ((regexp . "\\<\\(?:exec\\|eval\\|compile\\)[[:space:]]*(")))
    (:id python-dunder-import
     :severity confirm
     :reason "__import__"
     :match ((regexp . "\\<__import__[[:space:]]*(")))
    (:id python-ctypes-import
     :severity confirm
     :reason "ctypes"
     :match ((import-module . "ctypes")))
    (:id python-ctypes-call
     :severity confirm
     :reason "ctypes"
     :match ((regexp . "\\<ctypes\\."))))
  "Built-in python policy rules.")

(defun emagent-policy--all-python-rules ()
  "Return built-in and user `emagent-policy-extra-python-rules'."
  (append emagent-policy-python-rules emagent-policy-extra-python-rules))

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
  "Return non-nil when execution-time confirm dialogue should be skipped."
  (and (boundp 'emagent-tools--acp-session-p)
       emagent-tools--acp-session-p))

(defun emagent-policy--runtime-confirm-p (reason context)
  "Prompt for runtime confirmation; return non-nil when allowed.

Arguments: REASON, CONTEXT.
Uses a synchronous `completing-read' dialog.  The inline button prompt is
asynchronous and cannot drive `emagent-policy-enforce', which must return a
boolean before tool execution continues."
  (or (emagent-policy--skip-runtime-confirm-p)
      (let* ((preview (truncate-string-to-width (or context "") 400 nil nil "…"))
             (preamble (when (not (string-empty-p preview))
                         (format "\n#+begin_src %s\n%s\n#+end_src"
                                 (if (string-match-p "\\`\\(?:python\\|python3\\)" context)
                                     "python"
                                   "shell")
                                 preview)))
             (prompt (format "Policy check: *%s*" reason))
             (choice nil))
        (emagent-tools--buttons-prompt
         prompt
         '(("Allow" . yes) ("Deny" . no))
         nil
         (lambda (v) (setq choice v))
         preamble)
        (eq 'yes choice))))

(defun emagent-policy-enforce (verdict &optional context)
  "Apply VERDICT at execution time; signal `user-error' when blocked.

Arguments: CONTEXT."
  (pcase verdict
    (`(:deny . ,msg) (user-error "%s" msg))
    (`(:confirm . ,msg)
     (unless (emagent-policy--runtime-confirm-p msg context)
       (user-error "Cancelled: %s" msg)))
    (_ nil)))

(defun emagent-policy-enforce-string (verdict &optional context)
  "Like `emagent-policy-enforce' but return an error string instead of signaling.

Arguments: VERDICT, CONTEXT."
  (pcase verdict
    (`(:deny . ,msg) msg)
    (`(:confirm . ,msg)
     (unless (emagent-policy--runtime-confirm-p msg context)
       (format "Cancelled: %s" msg)))
    (_ nil)))

(provide 'emagent-policy)
;;; emagent-policy.el ends here
