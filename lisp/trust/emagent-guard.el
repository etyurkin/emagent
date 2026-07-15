;;; emagent-guard.el --- Single authorization choke point for agent effects  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6

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
