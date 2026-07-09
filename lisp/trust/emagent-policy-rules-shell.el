;;; emagent-policy-rules-shell.el --- Shell policy rule table  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Evgeniy Tyurkin

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Declarative shell rules for `emagent-policy-check-shell'.
;; Each rule is a plist: :id, :severity (deny confirm safe), :reason, :match alist.

;;; Code:

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

(provide 'emagent-policy-rules-shell)
;;; emagent-policy-rules-shell.el ends here
