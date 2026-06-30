;;; emagent-chat.el --- Org scratch buffer UI for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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
;; emagent-specific bindings here.
(define-key emagent-mode-map (kbd "C-c C-c") #'emagent-chat-send-or-babel)
(define-key emagent-mode-map (kbd "C-c a")   #'emagent-chat-attach-buffer)
(define-key emagent-mode-map (kbd "C-c b")   #'emagent-btw)
(define-key emagent-mode-map (kbd "C-c d")   #'emagent-chat-attach-files)
(define-key emagent-mode-map (kbd "C-c e")   #'emagent-chat-attach-error-context)
(define-key emagent-mode-map (kbd "C-c i")   #'emagent-chat-attach-image)
(define-key emagent-mode-map (kbd "C-c m")   #'emagent-set-model)
(define-key emagent-mode-map (kbd "C-c p")   #'emagent-set-project-directory)
(define-key emagent-mode-map (kbd "C-c l")   #'emagent-log-view)
(define-key emagent-mode-map (kbd "C-y")     #'emagent-chat-yank)
(define-key emagent-mode-map (kbd "TAB")     #'emagent-chat-tab)
(define-key emagent-mode-map (kbd "<backtab>") #'org-shifttab)
(define-key emagent-mode-map (kbd "ESC ESC") #'emagent-chat-interrupt)
(define-key emagent-mode-map (kbd "C-c u")   #'emagent-chat-new-prompt)
(define-key emagent-mode-map (kbd "C-c ?")   #'emagent-dispatch)
(define-key emagent-mode-map (kbd "C-a")     #'emagent-chat-beginning-of-line)

(defvar-local emagent-chat--assistant-marker nil
  "Insert position for the in-flight emagent response.")

(define-key emagent-mode-map (kbd "C-p") #'emagent-chat-history-previous-or-previous-line)
(define-key emagent-mode-map (kbd "C-n") #'emagent-chat-history-next-or-next-line)

(defvar-local emagent-chat--response-body-start nil
  "Start of the in-flight emagent response body.")

(defvar-local emagent-chat--thought-open-p nil
  "Non-nil while a Reasoning quote block is open in the in-flight response.")

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

(defvar-local emagent-chat--tool-call-lines (make-hash-table :test 'equal)
  "Map ACP toolCallId to (START . END) markers for displayed tool-call lines.")

(defvar-local emagent-chat--user-zone-start-marker nil
  "Position where the next user prompt may begin.")

(defvar-local emagent-chat--on-send nil
  "Function called with user input when sending.")

(defvar-local emagent-chat--on-attach nil
  "Function called with attachment text.")

(defvar-local emagent-chat--on-quit nil
  "Function called when quitting chat.")

(defvar-local emagent-chat-slug nil
  "Filesystem slug for the current emagent buffer.")

(defvar-local emagent-chat-project-directory nil
  "Project directory for the current emagent buffer.")

(defvar-local emagent-chat-model nil
  "ACP model id for the current emagent buffer.")

(defvar-local emagent-chat-session-id nil
  "ACP session id for the current emagent buffer.")

(defvar-local emagent-chat-provider nil
  "ACP provider symbol (`cursor' or `claude') for the current emagent buffer.")

(defvar-local emagent-chat-cursor-acp-extra-args nil
  "When non-nil, replaces `emagent-cursor-acp-extra-args' for this buffer only.")

(defvar-local emagent-chat-allowed-tools nil
  "Extra MCP tools allowed without confirmation for this buffer session.

Project-wide choices persist under `emagent-permissions-directory'.")

(defvar-local emagent-chat-allowed-permissions nil
  "Legacy buffer-local permission fingerprints from #+EMAGENT_ALLOWED_PERMISSIONS.

New choices persist under `emagent-permissions-directory'.")

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

(defconst emagent-chat-thinking-headline "** Thinking"
  "Org subsection headline holding streamed reasoning and tool lines.")

(defconst emagent-chat-response-headline "** Response"
  "Org subsection headline holding the finalized assistant answer.")

(defconst emagent-chat--progress-line "/emagent is thinking…/\n"
  "Placeholder body line shown until a prompt finishes rendering.")

(defconst emagent-chat--thinking-headline-re
  "^\\*\\* Thinking[ \t]*$"
  "Regexp matching the Thinking subsection headline.")

(defconst emagent-chat--response-headline-re
  "^\\*\\* Response[ \t]*$"
  "Regexp matching the Response subsection headline.")

(defconst emagent-chat--subsection-headline-re
  "^\\*\\* \\(?:Thinking\\|Response\\|Request permissions\\)\\(?:[ \t]\\|$\\)"
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
#
# Type after '* username> ' and press C-c C-c to send.
# C-c C-c send (on a src block: execute with org-babel instead)
# C-c u   insert a new '* username>' prompt heading
# C-c a   attach buffer context to the next send
# C-c b   send a btw side note while the agent is thinking
# C-c d   pick project files to attach
# C-c e   attach compilation/flymake errors to the next send
# C-y     paste text normally; if clipboard has image, inserts [[file:...]] link
# C-c i   pick an image file and insert [[file:...]] link at point
# C-c l   show emagent log (*Emagent Log*)
# C-c m   set ACP model
# C-c p   change project directory (moves session, reconnects)
# C-c ?   command palette (transient menu)
# ESC ESC interrupt agent response
# C-x k   kill buffer and disconnect agent
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

(defun emagent-chat--open-response-body-bounds ()
  "Return (BEG . END) for the open response body.

BEG is the `** Thinking' headline; END is `point-max' while the response is
still streaming.  Returns nil when no response is open."
  (when-let ((begin (emagent-chat--open-response-begin)))
    (cons begin (point-max))))

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

(defun emagent-chat-session-id ()
  "Return the persisted ACP session id for the current buffer."
  (or emagent-chat-session-id (emagent-chat--read-session-property)))

(defun emagent-chat-set-project-directory (directory)
  "Store PROJECT directory in the current buffer."
  (let ((dir (expand-file-name directory)))
    (setq emagent-chat-project-directory dir)
    (setq-local default-directory dir)
    (emagent-chat--write-top-property "EMAGENT_PROJECT" dir)))

(defun emagent-chat-project-directory ()
  "Return the project directory for the current emagent buffer."
  (or emagent-chat-project-directory (emagent-chat--read-project-property)))

(defun emagent-chat-set-model (model)
  "Store ACP MODEL id in the current buffer."
  (setq model (emagent-chat--canonical-model-id model))
  (unless (equal emagent-chat-model model)
    (setq emagent-chat-model model)
    (emagent-chat--write-top-property "EMAGENT_MODEL" model))
  (setq emagent-chat-model (or emagent-chat-model model))
  (emagent-chat--refresh-mode-line))

(defun emagent-chat-model ()
  "Return the ACP model id for the current emagent buffer."
  (emagent-chat--canonical-model-id
   (or emagent-chat-model (emagent-chat--read-model-property))))

(defun emagent-chat-model-display (&optional model)
  "Return MODEL as a short label for the mode line."
  (emagent-chat--normalize-model-id
   (or model (emagent-chat-model))))

(defun emagent-chat-set-session-id (session-id)
  "Store ACP SESSION-ID in the current buffer."
  (unless (equal emagent-chat-session-id session-id)
    (setq emagent-chat-session-id session-id)
    (emagent-chat--write-top-property "EMAGENT_SESSION" session-id)))

(defun emagent-chat-clear-session-id ()
  "Remove the ACP session id from the current buffer."
  (setq emagent-chat-session-id nil)
  (emagent-chat--delete-top-property "EMAGENT_SESSION"))

(defun emagent-chat-set-agent (agent)
  "Store the ACP provider AGENT symbol in the current buffer."
  (when agent
    (setq emagent-chat-provider agent)
    (emagent-chat--write-top-property "EMAGENT_AGENT" (symbol-name agent))))

(defun emagent-chat-agent ()
  "Return the ACP provider symbol for the current emagent buffer, or nil."
  (or emagent-chat-provider
      (when-let* ((value (emagent-chat--read-agent-property))
                  ((not (string-empty-p value))))
        (intern value))))

(defun emagent-chat-allowed-tools ()
  "Return MCP tools allowed without confirmation for this buffer's project."
  (let* ((legacy (or emagent-chat-allowed-tools
                     (emagent-chat--read-allowed-tools-property)))
         (stored (when-let ((dir (emagent-chat-project-directory)))
                   (emagent-permissions-project-tools dir))))
    (cl-delete-duplicates (append legacy stored))))

(defun emagent-chat-add-allowed-tool (tool)
  "Allow TOOL for this project without confirmation and persist it."
  (let* ((sym (if (stringp tool) (intern tool) tool))
         (dir (emagent-chat-project-directory)))
    (unless (memq sym (emagent-chat-allowed-tools))
      (setq emagent-chat-allowed-tools (append (or emagent-chat-allowed-tools nil)
                                               (list sym)))
      (when dir
        (emagent-permissions-add-project-tool dir sym)))))

(defun emagent-chat-allowed-permissions ()
  "Return legacy buffer permission fingerprints still honored at the gate."
  (or emagent-chat-allowed-permissions
      (emagent-chat--read-allowed-permissions-property)))

(defun emagent-chat-add-allowed-permission (fingerprint)
  "Persist FINGERPRINT as globally allowed for ACP permission requests."
  (emagent-permissions-add-global-fingerprint fingerprint))

(defun emagent-chat-session-allowed-permissions (session-id)
  "Return session-scoped permission fingerprints for SESSION-ID."
  (emagent-permissions-session-fingerprints session-id))

(defun emagent-chat-add-session-permission (session-id fingerprint)
  "Record FINGERPRINT as session-scoped for SESSION-ID."
  (emagent-permissions-add-session-fingerprint session-id fingerprint))

(defun emagent-chat-session-auto-approve-p (session-id)
  "Return non-nil when SESSION-ID has allow-all enabled."
  (emagent-permissions-session-auto-approve-p session-id))

(defun emagent-chat-set-session-auto-approve (session-id)
  "Enable allow-all for SESSION-ID."
  (emagent-permissions-set-session-auto-approve session-id))

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


(defun emagent-chat--flush-deferred-font-lock ()
  "Font-lock the current buffer when a deferred flush was requested."
  (when (and emagent-chat--font-lock-deferred-p
             (emagent-chat--buffer-active-p))
    (setq emagent-chat--font-lock-deferred-p nil)
    (font-lock-flush)))

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
