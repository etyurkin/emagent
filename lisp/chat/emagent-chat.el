;;; emagent-chat.el --- Org scratch buffer UI for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.1

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
(require 'emagent-chat-mode-line)
(require 'emagent-chat-slash)
(require 'emagent-chat-attach)
(require 'emagent-chat-extract)
(require 'emagent-chat-input)
(require 'emagent-chat-reasoning)
(require 'emagent-chat-render)
(require 'emagent-chat-actions)
(require 'emagent-chat-mode)
(require 'emagent-permissions)

(declare-function emagent-chat--refresh-mode-line "emagent-chat-mode-line")
(declare-function emagent-chat--mode-line-recompute "emagent-chat-mode-line")
(declare-function emagent-chat--spinner-ensure-running "emagent-chat-mode-line")
(declare-function emagent-chat--refresh-mode-line-on-focus "emagent-chat-mode-line")

(declare-function emagent-set-model "emagent-acp")
(declare-function emagent-reset-permissions "emagent-acp")
(declare-function emagent-set-project-directory "emagent" (new-dir))
(declare-function emagent-trust-workspace "emagent" (&optional arg))
(declare-function emagent-trust-claude-reconnect "emagent" ())
(declare-function emagent-acp-ensure-connected "emagent")
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function flymake-diagnostic-beg "flymake")
(declare-function flymake-diagnostic-type "flymake")
(declare-function flymake-diagnostic-text "flymake")

(declare-function cl-find "cl-lib")
(declare-function cl-some "cl-lib")


;; emagent-mode-map was created by `define-derived-mode' in
;; emagent-chat-mode.el (inheriting from org-mode-map).  Add
;; emagent-specific bindings here.  Per Emacs conventions `C-c <letter>'
;; is reserved for users, so emagent commands live under the `C-c C-e'
;; prefix map.
;; Direct keys are kept minimal: send, interrupt, the palette, and a few
;; input-aware overrides that fall through to their org/default behavior outside
;; the prompt zone (TAB completes /slash commands then org-cycles; C-a, C-n/C-p,
;; C-y; C-c C-c runs babel in src blocks, sends only on or inside a `* user>'
;; prompt, and falls through to `org-ctrl-c-ctrl-c' everywhere else).  Every
;; other command lives in the `C-c ?' palette (`emagent-dispatch').  The one
;; org binding knowingly shadowed is `C-c ?' itself (org-table-field-info).
(define-key emagent-mode-map (kbd "C-c C-c") #'emagent-chat-send-or-babel)
(define-key emagent-mode-map (kbd "ESC ESC") #'emagent-chat-interrupt)
(define-key emagent-mode-map (kbd "C-c ?")   #'emagent-dispatch)
(define-key emagent-mode-map (kbd "TAB")     #'emagent-chat-tab)
(define-key emagent-mode-map (kbd "C-a")     #'emagent-chat-beginning-of-line)
(define-key emagent-mode-map (kbd "C-y")     #'emagent-chat-yank)
(define-key emagent-mode-map (kbd "C-p")     #'emagent-chat-history-previous-or-previous-line)
(define-key emagent-mode-map (kbd "C-n")     #'emagent-chat-history-next-or-next-line)

(defvar-local emagent-chat--assistant-marker nil
  "Insert position for the in-flight emagent response.")

(defvar-local emagent-chat--response-body-start nil
  "Start of the in-flight emagent response body.")

(defvar-local emagent-chat--response-content-marker nil
  "Marker at the start of the open `** Response' body content.
Owned once the Response headline exists, so the body bounds are read from it
instead of re-searching for the headline on every streamed chunk.")

(defvar-local emagent-chat--response-end-marker nil
  "End of the open response region.
A live marker at the following exchange's user heading (re-evaluating an earlier
prompt), the symbol `point-max' when the response is last in the buffer, or nil
before a response is open.  Owning it avoids re-scanning to the next heading on
every streamed chunk.")

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

(defvar-local emagent-chat--tool-call-lines nil
  "Map ACP toolCallId to (START . END) markers for displayed tool-call lines.
Created per buffer in `emagent-mode'; must not use a shared mutable default,
or concurrent chat buffers would alias one table.")

(defvar-local emagent-chat--user-zone-start-marker nil
  "Position where the next user prompt may begin.")

(defvar-local emagent-chat--on-send nil
  "Function called with user input when sending.")

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

(defconst emagent-chat--model-link-re
  "\\[\\[emagent://\\([^][]+\\)\\]\\(?:\\[\\([^][]*\\)\\]\\)?\\]"
  "Matches a `/model' override link `[[emagent://AGENT/MODEL][short]]'.
Group 1 is the link target `AGENT/MODEL' (shown on hover); group 2 the
short model label shown as the link text.  Being an org link, the
marker is fontified by org, survives saving the session to disk, and
reveals the full agent/model id on hover.  The `emagent://' scheme
tags this as the model marker so unrelated links a user pastes are not
mistaken for it.")

(defun emagent-chat--model-link-path-id (path)
  "Return the model id from a link PATH `AGENT/MODEL' (or bare MODEL).
PATH may carry a leading `//' authority slash from the raw link.  The
agent is the first segment; the model id is the rest, so model ids are
returned intact even if they contain slashes."
  (let ((path (string-remove-prefix "//" path)))
    (if (string-match "/" path)
        (substring path (match-end 0))
      path)))

(defun emagent-chat--region-turn-model (start end)
  "Return the model id of the first `/model' link between START and END."
  (save-excursion
    (goto-char start)
    (when (re-search-forward emagent-chat--model-link-re end t)
      (emagent-chat--model-link-path-id (match-string-no-properties 1)))))

(defun emagent-chat--strip-model-links (text)
  "Remove `/model' override links from outgoing TEXT.
The marker is client UI — the slash command is documented as never sent
to the agent."
  (string-trim
   (replace-regexp-in-string
    (concat "[ \t]*" emagent-chat--model-link-re) "" text)))

(defun emagent-chat--model-link (model-id)
  "Return the `/model' marker link for MODEL-ID.
The visible text is the short model name; the link target is
`agent/full-model-id', revealed on hover.  The `emagent://' scheme
(never shown) tags this as the model marker so unrelated links a user
pastes are not mistaken for it."
  (let* ((agent (emagent-chat-agent))
         (short (or (emagent-model-normalize-id model-id) model-id))
         (path (if agent (format "%s/%s" agent model-id) model-id)))
    (format "[[emagent://%s][%s]]" path short)))

(defun emagent-chat--follow-model-link (path &optional _prefix)
  "Describe the `/model' override link PATH when activated."
  (message "Model for this turn: %s (delete the link to cancel)"
           (string-remove-prefix "//" path)))

(defun emagent-chat--model-link-help-echo (_window object position)
  "Tooltip for a `/model' link: the `agent/model' target."
  (with-current-buffer (if (bufferp object) object (current-buffer))
    (save-excursion
      (goto-char position)
      (when (or (looking-at emagent-chat--model-link-re)
                (and (search-backward "[[" (max (point-min) (- position 200)) t)
                     (looking-at emagent-chat--model-link-re)))
        (format "Model for this turn: %s" (match-string-no-properties 1))))))

(defvar-local emagent-chat--on-attach nil
  "Function called with attachment text.")

(defvar-local emagent-chat--on-quit nil
  "Function called when quitting chat.")

(defvar-local emagent-chat-slug nil
  "Filesystem slug for the current emagent buffer.")

;; Per-buffer session identity (project root, model, session id, provider,
;; allowed tools/permissions) now lives in `emagent-session' so lower layers
;; can read it without depending on this UI module.  The buffer-local vars and
;; canonical accessors are defined there; `emagent-chat-*' names below remain as
;; thin compatibility wrappers.

(defface emagent-tool-detail
  '((t (:inherit fixed-pitch :slant normal)))
  "Face for paths and commands on tool-call lines."
  :group 'emagent-chat)

(defface emagent-tool-permission-decision
  '((t (:inherit shadow :slant normal)))
  "Face for the permission decision suffix on tool-call lines.
Used for the trailing (Allow: Session) / (Denied) annotation."
  :group 'emagent-chat)

(defface emagent-permission-prompt
  '((t (:inherit font-lock-warning-face :weight bold)))
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
(see `emagent-chat--turn-model'); both `** Thinking ([[…]])' and
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

(defcustom emagent-chat-inactive-bell t
  "When non-nil, ring bell when agent output arrives in an inactive buffer."
  :type 'boolean
  :group 'emagent-chat)

(defcustom emagent-chat-inactive-bell-cooldown 1.0
  "Minimum seconds between inactive-buffer bell notifications."
  :type 'number
  :group 'emagent-chat)

(defcustom emagent-chat-inactive-osx-notification t
  "When non-nil on macOS, show a notification for background attention."
  :type 'boolean
  :group 'emagent-chat)

(defcustom emagent-chat-inactive-notification-title "emagent needs attention"
  "Title for macOS background attention notifications."
  :type 'string
  :group 'emagent-chat)

(defcustom emagent-chat-macos-activate-bundle-id "org.gnu.Emacs"
  "Bundle id used by terminal-notifier to foreground Emacs on click.

Only used when terminal-notifier is installed."
  :type 'string
  :group 'emagent-chat)

(defvar-local emagent-chat--last-inactive-bell-time 0.0
  "Last `float-time' when inactive attention notifications were emitted.")

(defconst emagent-chat-initial-comment
  "# -*- mode: emagent -*-
# This buffer is a scratch pad for chatting with emagent.
# Type after '* username> ' and press C-c C-c to send.
#
# C-c ?     command palette: model, attach, new prompt, log, project, trust, …
# ESC ESC   interrupt agent response
# C-x k     kill buffer and disconnect agent
# M-x emagent-mode to reconnect a saved session

")

(defgroup emagent-chat nil
  "Org scratch buffers for emagent."
  :group 'tools)

(defun emagent-chat--open-response-p ()
  "Return non-nil when an emagent response is in flight.

A response is open from `emagent-chat--begin-response' until it is
finalized or failed; openness is tracked by the live body-start marker."
  (and emagent-chat--response-body-start
       (marker-buffer emagent-chat--response-body-start)
       (marker-position emagent-chat--response-body-start)
       t))

(defun emagent-chat--open-response-begin ()
  "Return the buffer position where the in-flight response begins, or nil."
  (when (emagent-chat--open-response-p)
    (marker-position emagent-chat--response-body-start)))

(defun emagent-chat--find-open-response-begin ()
  "Return the start of the in-flight response (the `** Thinking' line), or nil."
  (emagent-chat--open-response-begin))

(defun emagent-chat--response-region-end (begin)
  "Return the buffer position that ends the response region starting at BEGIN.

Read from the owned `emagent-chat--response-end-marker' (set once when the
response is opened): a live marker at the following exchange's user heading, or
`point-max' when the response is last.  Falls back to a forward scan only when
the marker was not set (e.g. a re-opened session).  Bounding the region here
keeps finalizing a mid-buffer response from deleting the exchanges below it."
  (cond
   ((markerp emagent-chat--response-end-marker)
    (marker-position emagent-chat--response-end-marker))
   ((eq emagent-chat--response-end-marker 'point-max)
    (point-max))
   (t
    (save-excursion
      (goto-char begin)
      (if (re-search-forward (emagent-chat--user-heading-re) nil t)
          (line-beginning-position)
        (point-max))))))

(defun emagent-chat--open-response-body-bounds ()
  "Return (BEG . END) for the open response body.

BEG is the response body start; END is the next user heading after it (the next
exchange), or `point-max' when this is the last exchange.  Returns nil when no
response is open."
  (when-let ((begin (emagent-chat--open-response-begin)))
    (cons begin (emagent-chat--response-region-end begin))))

(defun emagent-chat--finish-body-bounds ()
  "Return (BEG . END) for the open response body to finalize, or nil."
  (emagent-chat--open-response-body-bounds))

(defun emagent-chat-cycle-response (&optional _force)
  "Fold or unfold the Org subtree at point (responses are native headlines)."
  (interactive)
  (org-cycle))

(defun emagent-chat-cycle-or-org-cycle ()
  "Cycle visibility with `org-cycle' (responses fold as native Org subtrees)."
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

;; Session-identity accessors moved to `emagent-session'.  These aliases keep
;; the historical `emagent-chat-*' entry points working for UI callers.
(defalias 'emagent-chat-session-id #'emagent-session-id)
(defalias 'emagent-chat-set-session-id #'emagent-session-set-id)
(defalias 'emagent-chat-clear-session-id #'emagent-session-clear-id)
(defalias 'emagent-chat-set-project-directory #'emagent-session-set-project-directory)
(defalias 'emagent-chat-project-directory #'emagent-session-project-directory)
(defalias 'emagent-chat-model #'emagent-session-model)
(declare-function emagent-acp-current-model-id "emagent-acp")

(defun emagent-chat-model-display ()
  "Return a short model label for the mode line.
Prefer the pending `/model' target while preparing a send, then the live ACP
session model (including transient per-turn switches), otherwise the buffer's
saved #+EMAGENT_MODEL."
  (let ((id (cond
              ((and emagent-chat--send-pending emagent-chat--turn-model)
               emagent-chat--turn-model)
              ((and emagent-acp--session (emagent-acp-state-ready emagent-acp--session))
               (emagent-acp-current-model-id))
              (t (emagent-session-model)))))
    (when id (emagent-session-model-display id))))

(defalias 'emagent-chat-set-agent #'emagent-session-set-agent)
(defalias 'emagent-chat-agent #'emagent-session-agent)
(defalias 'emagent-chat-allowed-tools #'emagent-session-allowed-tools)
(defalias 'emagent-chat-add-allowed-tool #'emagent-session-add-allowed-tool)
(defalias 'emagent-chat-allowed-permissions #'emagent-session-allowed-permissions)
(defalias 'emagent-chat-add-allowed-permission #'emagent-session-add-allowed-permission)
(defalias 'emagent-chat-session-allowed-permissions #'emagent-session-allowed-permissions-for)
(defalias 'emagent-chat-add-session-permission #'emagent-session-add-session-permission)
(defalias 'emagent-chat-session-auto-approve-p #'emagent-session-auto-approve-p)
(defalias 'emagent-chat-set-session-auto-approve #'emagent-session-set-auto-approve)

(defun emagent-chat-set-model (model)
  "Store ACP MODEL id in the current buffer and refresh the mode line."
  (emagent-session-set-model model)
  (emagent-chat--refresh-mode-line))

(defun emagent-chat--window-at-bottom-p (window)
  "Return non-nil when WINDOW shows the end of the current buffer."
  (and window (window-live-p window)
       (with-selected-window window
         (pos-visible-in-window-p (point-max) nil t))))


(defvar-local emagent-chat--table-align-deferred-p nil
  "When non-nil, defer org table alignment until the emagent buffer is active.")


(defvar emagent-chat--emacs-focused-p t
  "Non-nil when Emacs currently has OS-level input focus.")

(defun emagent-chat--sync-focus-state ()
  "Update `emagent-chat--emacs-focused-p' from selected-frame focus."
  (setq emagent-chat--emacs-focused-p
        (if (fboundp 'frame-focus-state)
            (frame-focus-state)
          t)))


(condition-case nil
    (progn
      (unless (advice-member-p #'emagent-chat--sync-focus-state after-focus-change-function)
        (add-function :after after-focus-change-function
                      #'emagent-chat--sync-focus-state))
      (emagent-chat--sync-focus-state))
  (error nil))

(defun emagent-chat--inactive-attention-needed-p ()
  "Return non-nil when background attention notifications should fire."
  (and (not emagent-chat--emacs-focused-p)
       (null (get-buffer-window (current-buffer) 0))))

(defun emagent-chat--notify-macos-inactive-update ()
  "Show a macOS notification for background emagent attention.

Uses terminal-notifier when available (click can activate Emacs),
otherwise falls back to osascript notifications.  Any launcher error is
ignored so chat rendering never stalls on OS notifications."
  (when (and emagent-chat-inactive-osx-notification
             (eq system-type 'darwin)
             (not noninteractive))
    (let* ((title emagent-chat-inactive-notification-title)
           (message (or (buffer-name) "emagent"))
           (notifier (executable-find "terminal-notifier"))
           (osascript (executable-find "osascript")))
      (condition-case nil
          (if notifier
              (start-process
               "emagent-inactive-notify" nil notifier
               "-title" title
               "-message" message
               "-group" "emagent-attention"
               "-activate" emagent-chat-macos-activate-bundle-id)
            (when osascript
              (start-process
               "emagent-inactive-notify" nil osascript "-e"
               (format "display notification %s with title %s"
                       (prin1-to-string message)
                       (prin1-to-string title)))))
        (error nil)))))

(defun emagent-chat--notify-inactive-update ()
  "Emit throttled attention notifications for background permission prompts."
  (when (emagent-chat--inactive-attention-needed-p)
    (let ((now (float-time)))
      (when (>= (- now emagent-chat--last-inactive-bell-time)
                emagent-chat-inactive-bell-cooldown)
        (setq emagent-chat--last-inactive-bell-time now)
        (condition-case nil
            (progn
              (when emagent-chat-inactive-bell
                (ding t))
              (emagent-chat--notify-macos-inactive-update))
          (error nil))))))


(declare-function emagent-chat--font-lock-response-tail "emagent-chat-markup")

(defun emagent-chat--flush-deferred-font-lock ()
  "Font-lock the response tail when a deferred flush was requested."
  (when (and emagent-chat--font-lock-deferred-p
             (emagent-chat--buffer-active-p))
    (setq emagent-chat--font-lock-deferred-p nil)
    (emagent-chat--font-lock-response-tail)))

(declare-function emagent-chat--align-org-tables-in-region "emagent-chat-markup")

(defun emagent-chat--maybe-align-org-tables-in-region (start end)
  "Align org tables in START..END when active; defer otherwise."
  (if (emagent-chat--buffer-active-p)
      (emagent-chat--align-org-tables-in-region start end)
    (setq emagent-chat--table-align-deferred-p t)))

(defun emagent-chat--flush-deferred-table-align ()
  "Align deferred org tables when the buffer becomes active."
  (when (and emagent-chat--table-align-deferred-p
             (emagent-chat--buffer-active-p))
    (setq emagent-chat--table-align-deferred-p nil)
    (let ((was-modified (buffer-modified-p)))
      (unwind-protect
          (save-excursion
            (emagent-chat--align-org-tables-in-region (point-min) (point-max)))
        (set-buffer-modified-p was-modified)))))

(defun emagent-chat--maybe-force-mode-line-update ()
  "Refresh this buffer's mode line in every window that displays it.

Updates whenever the buffer is shown in a visible window, not only the selected
one, so the thinking spinner keeps animating in a side-by-side emagent window
after focus moves elsewhere."
  (when (emagent-chat--buffer-displayed-p)
    (force-mode-line-update)))

(defun emagent-chat--window-configuration-change (&optional _frames)
  "Flush deferred UI work and refresh mode lines when focus changes."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'emagent-mode)
          (emagent-chat--flush-deferred-font-lock)
          (emagent-chat--flush-deferred-table-align)
          (emagent-chat--refresh-mode-line-on-focus)))))
  (emagent-chat--spinner-ensure-running))

(add-hook 'window-configuration-change-hook
          #'emagent-chat--window-configuration-change)

(defun emagent-chat--save-window-views ()
  "Return saved scroll state for windows displaying the current buffer."
  (let (views)
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (push `(:window ,win
              :start ,(window-start win)
              :at-bottom ,(emagent-chat--window-at-bottom-p win))
            views))
    views))

(defun emagent-chat--restore-window-views (views)
  "Restore scroll state from VIEWS returned by `emagent-chat--save-window-views'."
  (dolist (view views)
    (let ((win (plist-get view :window)))
      (when (window-live-p win)
        (if (plist-get view :at-bottom)
            (with-selected-window win
              (save-excursion
                (goto-char (point-max))
                (recenter -1)))
          (set-window-start win (plist-get view :start) t))))))

(defun emagent-chat--with-stable-view (fn)
  "Run FN while preserving window scroll unless already at buffer end."
  (if (emagent-chat--buffer-active-p)
      (let ((saved-point (point-marker))
            (saved-windows (emagent-chat--save-window-views)))
        (unwind-protect
            (funcall fn)
          (when (marker-position saved-point)
            (goto-char saved-point))
          (set-marker saved-point nil)
          (emagent-chat--restore-window-views saved-windows)))
    (funcall fn)))

(defun emagent-chat--with-streaming-view (fn)
  "Run FN during live streaming without disturbing windows already at buffer end."
  (if (emagent-chat--buffer-active-p)
      (let* ((views (emagent-chat--save-window-views))
             (pinned (cl-remove-if (lambda (v) (plist-get v :at-bottom)) views)))
        (unwind-protect
            (funcall fn)
          (emagent-chat--restore-window-views pinned)))
    (funcall fn)))

(provide 'emagent-chat)
;;; emagent-chat.el ends here
