;;; emagent-guard.el --- Single authorization choke point for agent effects  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; One place that answers "may this agent-initiated effect proceed?" for every
;; effect kind, unifying the file-boundary check and the shell/elisp policy
;; checks behind a single verdict function.
;;
;; File operations resolve through `emagent-tools--root-directory', which
;; canonicalizes the path (resolving symlinks), confines it to the session root
;; when one is bound, and rejects protected macOS trees.  Shell and elisp
;; operations run through the declarative policy in `emagent-policy'.
;;
;; A verdict is one of:
;;   (:allow   . RESOLVED)  ; file ops: the canonical path; shell/eval: t
;;   (:confirm . REASON)    ; needs user confirmation (shell/eval only)
;;   (:deny    . REASON)    ; refused outright

;;; Code:

(require 'cl-lib)
(require 'emagent-tools)
(require 'emagent-policy)

(defun emagent-guard--path-verdict (path)
  "Return an authorization verdict for file PATH.
Resolves and confines PATH via `emagent-tools--root-directory', turning its
boundary/protected-tree signal into a `:deny' verdict."
  (condition-case err
      (cons :allow (emagent-tools--root-directory path))
    (error (cons :deny (error-message-string err)))))

(defun emagent-guard-check (op payload)
  "Return the authorization verdict for OP applied to PAYLOAD.

OP is one of:
  `read' `write' `delete' — PAYLOAD is a file path; a `:allow' verdict carries
                            the resolved canonical path.
  `shell'                 — PAYLOAD is a command string.
  `eval'                  — PAYLOAD is an elisp form string.

See the commentary for the verdict shape."
  (pcase op
    ((or 'read 'write 'delete) (emagent-guard--path-verdict payload))
    ('shell (or (emagent-policy-check-shell payload) '(:allow . t)))
    ('eval  (or (emagent-policy-check-elisp payload) '(:allow . t)))
    (_ (cons :deny (format "unknown guarded operation: %S" op)))))

(defun emagent-guard-allow-p (verdict)
  "Return non-nil when VERDICT authorizes the effect."
  (eq (car-safe verdict) :allow))

(defun emagent-guard-deny-p (verdict)
  "Return non-nil when VERDICT refuses the effect outright."
  (eq (car-safe verdict) :deny))

(defun emagent-guard-resolved (verdict)
  "Return the resolved value of an allowing VERDICT, or nil."
  (and (eq (car-safe verdict) :allow) (cdr verdict)))

(defun emagent-guard-reason (verdict)
  "Return the human-readable reason string of VERDICT, or nil."
  (and (memq (car-safe verdict) '(:deny :confirm)) (cdr verdict)))

(provide 'emagent-guard)
;;; emagent-guard.el ends here
