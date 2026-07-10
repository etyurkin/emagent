;;; emagent-policy-rules-elisp.el --- Elisp policy rule table  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Declarative elisp rules for `emagent-policy-check-elisp'.

;;; Code:

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

(provide 'emagent-policy-rules-elisp)
;;; emagent-policy-rules-elisp.el ends here
