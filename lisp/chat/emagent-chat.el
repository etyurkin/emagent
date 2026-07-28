;;; emagent-chat.el --- Org scratch buffer UI for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.8
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

;; Org scratch buffer UI and session buffer helpers.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)

(require 'org)
(require 'map)
(require 'bookmark)
(require 'emagent-log)
(require 'emagent-context)
(require 'emagent-tools)
(require 'emagent-chat-markup)
(require 'emagent-chat-header)
(require 'emagent-session)
(require 'emagent-chat-compress)
(require 'emagent-chat-mcp)
(require 'emagent-chat-view)
(require 'emagent-chat-notify)
(require 'emagent-chat-response-state)
(require 'emagent-chat-model-ui)
(require 'emagent-chat-mode-line)
(require 'emagent-chat-slash)
(require 'emagent-chat-attach)
(require 'emagent-chat-extract)
(require 'emagent-chat-input)
(require 'emagent-chat-reasoning)
(require 'emagent-chat-thought)
(require 'emagent-chat-response)
(require 'emagent-chat-tools-ui)
(require 'emagent-chat-actions)
(require 'emagent-chat-mode)
(require 'emagent-permissions)
(require 'project)

(defvar-local emagent-chat--assistant-marker nil
  "Insert position for the in-flight emagent response.")

(defvar-local emagent-chat--response-content-marker nil
  "Marker at the start of the open `** Response' body content.
Owned once the Response headline exists, so the body bounds are read from it
instead of re-searching for the headline on every streamed chunk.")

(defvar-local emagent-chat--thought-open-p nil
  "Non-nil while a Reasoning quote block is open in the in-flight response.")

(defvar-local emagent-chat--thinking-headline-marker nil
  "Marker at the open `** Thinking' headline, owned once the scaffold is inserted.
Read instead of re-searching for the headline on every reasoning chunk.")

(defvar-local emagent-chat--thought-marker nil
  "Insert position for streaming agent reasoning text.")

(defvar-local emagent-chat--reasoning-streamed-p nil
  "Non-nil once reasoning text has been streamed into the open Reasoning block.")

(defvar-local emagent-chat--thought-pending ""
  "Reasoning chunks not yet inserted into the open Thinking block.")

(defvar-local emagent-chat--thought-flush-timer nil
  "Timer that batches reasoning stream inserts into the chat buffer.")

(defcustom emagent-chat-thought-stream-delay 0.05
  "Seconds to batch reasoning stream inserts before updating the buffer.

Lower values feel more live; higher values reduce org font-lock work while the
agent is thinking."
  :type 'number
  :group 'emagent-chat)

(defvar-local emagent-chat--response-pending ""
  "Assistant chunks not yet inserted into the open Response block.")

(defvar-local emagent-chat--response-flush-timer nil
  "Timer that batches assistant stream inserts into the chat buffer.")

(defcustom emagent-chat-response-stream-delay 0.05
  "Seconds to batch assistant stream inserts before updating the buffer.

Lower values feel more live; higher values reduce markdown conversion and
org font-lock work while the agent is answering.  Use 0 to flush every
chunk synchronously (also the effective value in `noninteractive' tests)."
  :type 'number
  :group 'emagent-chat)

(defvar emagent-chat--live-buffers (make-hash-table :weakness 'key :test 'eq)
  "Weak set of live `emagent-mode' buffers.

Used by focus/spinner refresh paths instead of scanning `buffer-list'.")

(defun emagent-chat--register-live-buffer (&optional buffer)
  "Register BUFFER (default current) as a live emagent chat buffer."
  (puthash (or buffer (current-buffer)) t emagent-chat--live-buffers))

(defun emagent-chat--unregister-live-buffer (&optional buffer)
  "Unregister BUFFER (default current) from the live emagent set."
  (remhash (or buffer (current-buffer)) emagent-chat--live-buffers))

(defun emagent-chat--map-live-buffers (fn)
  "Call FN with each live registered emagent buffer."
  (maphash (lambda (buf _)
             (when (buffer-live-p buf)
               (funcall fn buf)))
           emagent-chat--live-buffers))

(defvar-local emagent-chat--tool-call-lines nil
  "Map ACP toolCallId to (START . END) markers for displayed tool-call lines.
Created per buffer in `emagent-mode'; must not use a shared mutable default,
or concurrent chat buffers would alias one table.")

(defvar-local emagent-chat--user-zone-start-marker nil
  "Position where the next user prompt may begin.")

(defvar-local emagent-chat--send-pending nil
  "Non-nil from send until `emagent-acp-send-prompt' dispatches the turn.

Covers connecting, per-turn model switches (`/model'), and other pre-dispatch
work.  The mode line shows a spinner during this window so large resumed
sessions do not look idle while the agent re-hydrates context for a new model.")

(defvar-local emagent-chat--send-token nil
  "Token for the in-flight pre-dispatch send; cleared on cancel or dispatch.")

(defvar-local emagent-chat--turn-model nil
  "Model id overriding the buffer model for the in-flight turn, or nil.

Set at send from the `emagent://AGENT/MODEL' link that `/model' inserts
in the prompt.  It drives the transient ACP model switch and the
`** Thinking (MODEL)' indicator.  Cleared when a turn completes
successfully or when a post-failure dialog declines to keep it; kept
across a failure so `retry' reuses the model.")

(defvar-local emagent-chat--turn-model-base nil
  "Session model to restore to when a per-turn override ends, or nil.
Captured (once) from the live session model just before the first override
switch, so restoring returns to whatever the session was really on.")

(defvar-local emagent-chat-slug nil
  "Filesystem slug for the current emagent buffer.")

;; Per-buffer session identity (project root, model, session id, provider,
;; allowed tools/permissions) now lives in `emagent-session' so lower layers
;; can read it without depending on this UI module.  The buffer-local vars and
;; canonical accessors are defined there; `emagent-chat-*' names below remain as
;; thin compatibility wrappers.

(defface emagent-tool-detail
  '((t (:inherit fixed-pitch)))
  "Face for paths and commands on tool-call lines."
  :group 'emagent-chat)

(defface emagent-tool-permission-decision
  '((t (:inherit shadow)))
  "Face for the permission decision suffix on tool-call lines.
Used for the trailing (Allow: Session) / (Denied) annotation."
  :group 'emagent-chat)

(defface emagent-permission-prompt
  '((t (:inherit font-lock-warning-face)))
  "Face for the permission question line in the Thinking block."
  :group 'emagent-chat)

;; The `/model' marker is an org link — `[[emagent://AGENT/MODEL][short]]' —
;; so org owns its fontification entirely (default `org-link' face, no
;; custom font-lock matcher, no sticky text properties) and the marker
;; survives saving the session file.
(org-link-set-parameters
 "emagent"
 :follow #'emagent-chat--follow-model-link
 :help-echo #'emagent-chat--model-link-help-echo)

(defface emagent-model-choice-agent
  '((t (:inherit font-lock-keyword-face)))
  "Face for the agent name in model selector candidates."
  :group 'emagent-chat)

(defface emagent-model-choice-model
  '((t (:inherit success)))
  "Face for the model base name in model selector candidates."
  :group 'emagent-chat)

(defface emagent-model-choice-detail
  '((t (:inherit font-lock-warning-face)))
  "Face for bracket annotations and aliases in model selector candidates."
  :group 'emagent-chat)

(defconst emagent-chat-default-slug "emagent")

(defvar-local emagent-chat--switching-model-p nil
  "Non-nil while the open response shows a `** Switching model' headline.")

(defvar-local emagent-chat--preparing-p nil
  "Non-nil while the open response shows a `** Preparing…' headline.")

(defconst emagent-chat-switching-headline "** Switching model"
  "Org subsection headline shown while a per-turn `/model' switch is in flight.")

(defconst emagent-chat-preparing-headline "** Preparing…"
  "Org subsection headline shown while connect/pre-dispatch work runs.")

(defconst emagent-chat-thinking-headline "** Thinking"
  "Org subsection headline holding streamed reasoning and tool lines.")

(defconst emagent-chat-response-headline "** Response"
  "Org subsection headline holding the finalized assistant answer.")

(defconst emagent-chat--progress-line "/emagent is thinking…/\n"
  "Placeholder body line shown until a prompt finishes rendering.")

(defconst emagent-chat--thinking-headline-re
  "^\\*\\* Thinking\\(?: (\\[\\[emagent://[^][]+\\]\\[[^][]*\\]\\])\\| \\[\\[emagent://[^][]+\\]\\[[^][]*\\]\\]\\)?[ \t]*$"
  "Regexp matching the Thinking subsection headline.
An optional model link marks a per-turn override
\(see `emagent-chat--turn-model'); both `** Thinking ([[…]])' and
`** Thinking [[…]]' forms are recognized.")

(defconst emagent-chat--switching-headline-re
  "^\\*\\* Switching model to .+…[ \t]*$"
  "Regexp matching the transient model-switch subsection headline.")

(defconst emagent-chat--preparing-headline-re
  "^\\*\\* Preparing…[ \t]*$"
  "Regexp matching the transient preparing subsection headline.")

(defconst emagent-chat--response-headline-re
  "^\\*\\* Response[ \t]*$"
  "Regexp matching the Response subsection headline.")

(defconst emagent-chat--subsection-headline-re
  "^\\*\\* \\(?:Thinking\\|Switching model\\|Preparing…\\|Response\\|Request permissions\\)\\(?:[ \t]\\|$\\)"
  "Regexp matching any emagent response subsection headline.")

(defconst emagent-chat--reasoning-begin-re emagent-chat--thinking-headline-re
  "Regexp matching the Thinking subsection opener.")

(defcustom emagent-chat-fold-reasoning-on-done t
  "When non-nil, fold the Thinking subsection once the agent finishes.

Hides the body of the `** Thinking' Org subsection (`org-fold-hide-subtree'),
leaving its headline visible as a collapsed summary."
  :type 'boolean
  :group 'emagent-chat)

(defconst emagent-chat-initial-comment
  "# -*- mode: emagent -*-
# This buffer is a scratch pad for chatting with emagent.
# Type after '* username> ' and press C-c C-c to send.
#
# C-c ?     command palette: connect, model, attach, new prompt, log, project, trust, …
# ESC ESC   interrupt agent response
# C-x k     kill buffer and disconnect agent
# M-x emagent-mode to reconnect a saved session

")

(defgroup emagent-chat nil
  "Org scratch buffers for emagent."
  :group 'tools)

(defun emagent-chat-cycle-response (&optional _force)
  "Fold or unfold the Org subtree at point (responses are native headlines)."
  (interactive)
  (org-cycle))

(defun emagent-chat--sanitize-slug (name)
  "Return a filesystem-safe slug for NAME."
  (let ((slug (downcase (string-trim name))))
    (if (string-empty-p slug)
        emagent-chat-default-slug
      (replace-regexp-in-string "[^a-zA-Z0-9._-]+" "-" slug))))

(defun emagent-chat--buffers ()
  "Return all buffers in `emagent-mode'."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (eq major-mode 'emagent-mode)))
              (buffer-list)))

(defun emagent-chat--short-cwd-label (directory)
  "Return a short display label for DIRECTORY."
  (let* ((dir (file-truename (expand-file-name directory)))
         (home (file-truename "~"))
         (raw (cond
               ((string= dir home) "~")
               ((string-prefix-p (concat home "/") dir)
                (substring dir (1+ (length home))))
               (t (file-name-nondirectory dir)))))
    (emagent-chat--sanitize-slug (or raw emagent-chat-default-slug))))

(defun emagent-chat--buffer-name-for-label (label)
  "Return a unique *emagent LABEL* buffer name."
  (let ((base (format "*Emagent %s*" label))
        (names (mapcar #'buffer-name (emagent-chat--buffers))))
    (if (not (member base names))
        base
      (let ((n 2))
        (while (member (format "*emagent %s-%d*" label n) names)
          (setq n (1+ n)))
        (format "*emagent %s-%d*" label n)))))

(defun emagent-chat--window-configuration-change (&optional _frames)
  "Flush deferred font-lock and refresh mode lines on focus change."
  (emagent-chat--map-live-buffers
   (lambda (buf)
     (with-current-buffer buf
       (emagent-chat--flush-deferred-font-lock)
       (emagent-chat--refresh-mode-line-on-focus))))
  (emagent-chat--spinner-ensure-running))

(add-hook 'window-configuration-change-hook
          #'emagent-chat--window-configuration-change)

(provide 'emagent-chat)
;;; emagent-chat.el ends here
