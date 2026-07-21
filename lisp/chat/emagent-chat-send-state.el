;;; emagent-chat-send-state.el --- Pre-dispatch send tracking for emagent chat  -*- lexical-binding: t; -*-

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

;; Tracks whether the current buffer is preparing a send (connecting, or
;; switching to a per-turn `/model' override) before `emagent-acp-send-prompt'
;; actually dispatches the turn.  Kept out of `emagent-chat-actions' so the
;; mode line can require it directly without cycling back through the facade
;; `emagent-chat' (which requires `emagent-chat-actions').  Mode-line refresh
;; calls are optional (`fboundp'-guarded) rather than a `require', since the
;; mode line itself reads `emagent-chat--send-pending' from this file.

;;; Code:

(require 'cl-lib)

(defvar-local emagent-chat--send-pending nil
  "Non-nil from send until `emagent-acp-send-prompt' dispatches the turn.

Covers connecting, per-turn model switches (`/model'), and other pre-dispatch
work.  The mode line shows a spinner during this window so large resumed
sessions do not look idle while the agent re-hydrates context for a new model.")

(defvar-local emagent-chat--send-token nil
  "Token for the in-flight pre-dispatch send; cleared on cancel or dispatch.")

(defun emagent-chat--send-active-p (token)
  "Return non-nil when TOKEN is still the active pre-dispatch send."
  (and emagent-chat--send-pending (eq emagent-chat--send-token token)))

(defun emagent-chat--send-pending-begin ()
  "Mark the buffer as preparing a send and refresh the mode line."
  (setq emagent-chat--send-pending t
        emagent-chat--send-token (cl-gensym "emagent-send"))
  (when (fboundp 'emagent-chat--refresh-mode-line)
    (emagent-chat--refresh-mode-line))
  (when (fboundp 'emagent-chat--spinner-ensure-running)
    (emagent-chat--spinner-ensure-running)))

(defun emagent-chat--send-pending-end ()
  "Clear the pre-dispatch send marker and refresh the mode line."
  (when emagent-chat--send-pending
    (setq emagent-chat--send-pending nil
          emagent-chat--send-token nil)
    (when (fboundp 'emagent-chat--refresh-mode-line)
      (emagent-chat--refresh-mode-line))))

(provide 'emagent-chat-send-state)
;;; emagent-chat-send-state.el ends here