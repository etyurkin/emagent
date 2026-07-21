;;; emagent-acp-provider.el --- ACP provider adapter registry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

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

;; Provider-specific ACP integration (Cursor store.db, Claude quirks) lives in
;; adapter modules registered here.  Shared permission and tool-call logic calls
;; these hooks instead of branching on agent executable names.

;;; Code:

(require 'cl-lib)
(require 'emagent-acp-state)
(require 'map)

(defvar emagent-acp--provider-specs (make-hash-table :test 'eq)
  "Hash table mapping provider symbol to adapter property list.")

(cl-defun emagent-acp--register-provider (symbol &key detect enrich-tool-call
                                                 defer-tool-call-p
                                                 enqueue-tool-resolve
                                                 reset-tool-resolve
                                                 tool-resolve-active-p
                                                 generic-title-p
                                                 external-gate-reason
                                                 normalize-slash-prompt
                                                 context-usage-unavailable-p)
  "Register provider SYMBOL with adapter hooks.

Arguments: DETECT, ENRICH-TOOL-CALL, DEFER-TOOL-CALL-P,
ENQUEUE-TOOL-RESOLVE, RESET-TOOL-RESOLVE, TOOL-RESOLVE-ACTIVE-P,
GENERIC-TITLE-P, EXTERNAL-GATE-REASON, NORMALIZE-SLASH-PROMPT,
CONTEXT-USAGE-UNAVAILABLE-P."
  (puthash symbol
           (list :detect detect
                 :enrich-tool-call enrich-tool-call
                 :defer-tool-call-p defer-tool-call-p
                 :enqueue-tool-resolve enqueue-tool-resolve
                 :reset-tool-resolve reset-tool-resolve
                 :tool-resolve-active-p tool-resolve-active-p
                 :generic-title-p generic-title-p
                 :external-gate-reason external-gate-reason
                 :normalize-slash-prompt normalize-slash-prompt
                 :context-usage-unavailable-p context-usage-unavailable-p)
           emagent-acp--provider-specs))

(defun emagent-acp--provider-context-usage-unavailable-p (state)
  "Return non-nil when STATE's provider cannot report context-window usage."
  (and (emagent-acp--provider-hook state :context-usage-unavailable-p state) t))

(defun emagent-acp--provider-spec (state)
  "Return the adapter property list for STATE's provider, or nil."
  (when state
    (gethash (emagent-acp--provider-symbol state) emagent-acp--provider-specs)))

(defun emagent-acp--provider-symbol (state)
  "Return the provider symbol for STATE (`cursor' or `claude')."
  (or (emagent-acp-state-provider state)
      (cl-loop for sym being the hash-keys of emagent-acp--provider-specs
               for detect = (plist-get (gethash sym emagent-acp--provider-specs) :detect)
               when (and detect (funcall detect state))
               return sym
               finally return 'cursor)))

(defun emagent-acp--provider-hook (state prop &rest args)
  "Call provider hook PROP for STATE with ARGS, or nil when unset."
  (when-let ((fn (plist-get (emagent-acp--provider-spec state) prop)))
    (apply fn args)))

(defun emagent-acp--provider-enrich-tool-call (state update)
  "Return UPDATE enriched by the active provider adapter.

Arguments: STATE."
  (or (emagent-acp--provider-hook state :enrich-tool-call state update)
      update))

(defun emagent-acp--provider-defer-tool-call-p (state update)
  "Return non-nil when UPDATE tool-call display should wait for enrichment.

Arguments: STATE."
  (and (emagent-acp--provider-hook state :defer-tool-call-p state update)))

(defun emagent-acp--provider-enqueue-tool-resolve (state id &optional delay)
  "Queue tool-call ID for provider-specific arg resolution.

Arguments: STATE, DELAY."
  (emagent-acp--provider-hook state :enqueue-tool-resolve state id delay))

(defun emagent-acp--provider-reset-tool-resolve (state)
  "Clear provider-specific pending tool-call resolution state for STATE."
  (emagent-acp--provider-hook state :reset-tool-resolve state))

(defun emagent-acp--provider-tool-resolve-active-p (state)
  "Return non-nil while provider tool-call resolution is in flight.

Arguments: STATE."
  (and (emagent-acp--provider-hook state :tool-resolve-active-p state)))

(defun emagent-acp--provider-generic-title-p (state title)
  "Return non-nil when TITLE is too generic to show without provider detail.

Arguments: STATE."
  (and (emagent-acp--provider-hook state :generic-title-p title)))

(defun emagent-acp--provider-external-gate-reason (state)
  "Return a symbol naming this provider's external tool gate, or nil.

Arguments: STATE."
  (emagent-acp--provider-hook state :external-gate-reason state))

(defun emagent-acp--provider-normalize-slash-prompt (state text)
  "Return TEXT normalized for the active provider, or TEXT when unset.

Arguments: STATE."
  (or (emagent-acp--provider-hook state :normalize-slash-prompt text) text))

(provide 'emagent-acp-provider)
;;; emagent-acp-provider.el ends here
