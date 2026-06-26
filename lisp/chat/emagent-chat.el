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


(declare-function emagent-set-model "emagent-acp")
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
(define-key emagent-mode-map (kbd "C-g C-g") #'emagent-chat-interrupt)
(define-key emagent-mode-map (kbd "C-c u")   #'emagent-chat-new-prompt)
(define-key emagent-mode-map (kbd "C-c ?")   #'emagent-dispatch)
(define-key emagent-mode-map (kbd "C-a")     #'emagent-chat-beginning-of-line)

(defvar-local emagent-chat--assistant-marker nil
  "Insert position for the in-flight emagent response.")

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

(defvar-local emagent-chat--pending-prompt nil
  "Text queued via /btw to send after the current agent response finishes.")

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

(defface emagent-permission-prompt
  '((t (:inherit font-lock-warning-face :weight bold)))
  "Face for the permission question line in the Thinking block."
  :group 'emagent-chat)

(defconst emagent-chat-default-slug "emagent")

(defconst emagent-chat-response-headline "** emagent"
  "Org headline wrapping each agent response under the user prompt.")

(defconst emagent-chat-response-begin "# --- emagent ---")
(defconst emagent-chat-response-end "# --- /emagent ---")

(defconst emagent-chat--progress-line "/emagent is thinking…/\n"
  "Placeholder body line shown until a prompt finishes rendering.")

(defconst emagent-chat--response-headline-re
  (concat "^" (regexp-quote emagent-chat-response-headline) "\\s-*$")
  "Regexp matching the emagent response wrapper headline.")

(defconst emagent-chat--response-begin-re
  "^# --- emagent ---\\s-*$"
  "Regexp matching emagent response begin delimiter lines.")

(defconst emagent-chat--response-end-re
  "^# --- /emagent ---\\s-*$"
  "Regexp matching emagent response end delimiter lines.")

(defconst emagent-chat--thinking-block-label "Thinking"
  "Org quote-block title for streamed agent thought and tool lines.")

(defconst emagent-chat--reasoning-begin-re
  "^#\\+begin_quote \\(?:Thinking\\|Reasoning\\)\\s-*$"
  "Regexp matching the Thinking quote block opener (Reasoning is legacy).")

(defcustom emagent-chat-fold-reasoning-on-done t
  "When non-nil, hide Thinking quote blocks once the agent finishes.

Uses Org block folding (`org-fold-hide-block-toggle'), like #+STARTUP:
hideblocks / `org-cycle-hide-block-startup'."
  :type 'boolean
  :group 'emagent-chat)

(defconst emagent-chat-initial-comment
  "# -*- mode: emagent -*-
# This buffer is a scratch pad for chatting with emagent.
#
# Type after '* username> ' and press C-c C-c to send.
# C-c C-c send (on a src block: execute with org-babel instead)
# C-c u   insert a new '* username>' prompt heading
# C-c a   attach buffer context to the next send
# C-c b   queue a follow-up message (btw) for after agent finishes
# C-c d   pick project files to attach
# C-c e   attach compilation/flymake errors to the next send
# C-y     paste text normally; if clipboard has image, inserts [[file:...]] link
# C-c i   pick an image file and insert [[file:...]] link at point
# C-c l   show emagent log (*Emagent Log*)
# C-c m   set ACP model
# C-c p   change project directory (moves session, reconnects)
# C-c ?   command palette (transient menu)
# C-g C-g interrupt agent response
# C-x k   kill buffer and disconnect agent
# M-x emagent-mode to reconnect a saved session

")

(defgroup emagent-chat nil
  "Org scratch buffers for emagent."
  :group 'tools)

(defun emagent-chat--on-response-begin-p ()
  "Return non-nil when point is on an emagent response begin delimiter line."
  (save-excursion
    (beginning-of-line)
    (looking-at emagent-chat--response-begin-re)))

(defun emagent-chat--find-response-begin-backward ()
  "Return point at an emagent response begin delimiter at or before point."
  (let ((zone-start (emagent-chat--metadata-end)))
    (save-excursion
      (or (and (emagent-chat--on-response-begin-p)
               (>= (line-beginning-position) zone-start)
               (line-beginning-position))
          (and (re-search-backward emagent-chat--response-begin-re nil t)
               (>= (match-beginning 0) zone-start)
               (match-beginning 0))))))

(defun emagent-chat--find-open-response-begin ()
  "Return point at the newest emagent response begin that has no end delimiter."
  (let ((zone-start (emagent-chat--metadata-end))
        (found nil))
    (save-excursion
      (goto-char (point-max))
      (while (and (not found)
                  (re-search-backward emagent-chat--response-begin-re nil t))
        (when (>= (match-beginning 0) zone-start)
          (let ((beg (match-beginning 0)))
            (save-excursion
              (goto-char beg)
              (forward-line 1)
              (unless (re-search-forward emagent-chat--response-end-re (point-max) t)
                (setq found beg)))))))
    found))

(defun emagent-chat--open-response-p ()
  "Return non-nil when an emagent response block is open before point-max."
  (and (emagent-chat--find-open-response-begin) t))

(defun emagent-chat--find-response-end-forward (limit)
  "Return point at the end of an emagent response delimiter after point."
  (and (re-search-forward emagent-chat--response-end-re limit t)
       (match-end 0)))

(defun emagent-chat--response-fold-bounds ()
  "Return (BODY-START . BODY-END) for a closed emagent response at point."
  (save-excursion
    (when-let ((begin (emagent-chat--find-response-begin-backward)))
      (goto-char begin)
      (let ((body-start (line-end-position)))
        (when-let ((end-line (emagent-chat--find-response-end-forward nil)))
          (cons body-start (save-excursion
                             (goto-char end-line)
                             (line-beginning-position))))))))

(defun emagent-chat--open-response-body-bounds ()
  "Return (BEG . END) for the open emagent response body before point-max.

BEG is the first line after the begin delimiter; END is `point-max' when the
response is still open, otherwise nil."
  (when-let ((begin (emagent-chat--find-open-response-begin)))
    (save-excursion
      (goto-char begin)
      (forward-line 1)
      (cons (point) (point-max)))))

(defun emagent-chat--finish-body-bounds ()
  "Return (BEG . END) for the emagent response body to replace on finalize.

Uses the open response when present; otherwise the in-flight body marker or
the newest begin delimiter through `point-max'."
  (or (emagent-chat--open-response-body-bounds)
      (when (and emagent-chat--response-body-start
                 (marker-position emagent-chat--response-body-start))
        (cons (marker-position emagent-chat--response-body-start) (point-max)))
      (save-excursion
        (goto-char (point-max))
        (when (emagent-chat--find-response-begin-backward)
          (let ((begin (match-beginning 0)))
            (when (>= begin (emagent-chat--metadata-end))
              (goto-char begin)
              (forward-line 1)
              (cons (point) (point-max))))))))

(defun emagent-chat-cycle-response (&optional force)
  "Fold or unfold the emagent response body at point."
  (interactive)
  (when-let* ((bounds (emagent-chat--response-fold-bounds))
              (start (car bounds))
              (end (cdr bounds)))
    (org-fold-region
     start end
     (pcase force
       ('show nil)
       ('hide t)
       (_ (if (org-fold-folded-p start 'block) nil t)))
     'block)))

(defun emagent-chat-cycle-or-org-cycle ()
  "Fold the emagent response on its begin line, otherwise run `org-cycle'."
  (interactive)
  (if (and (emagent-chat--on-response-begin-p)
           (emagent-chat--response-fold-bounds))
      (emagent-chat-cycle-response)
    (org-cycle)))

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
  (setq model (emagent-chat--normalize-model-id model))
  (unless (equal (emagent-chat--normalize-model-id emagent-chat-model) model)
    (setq emagent-chat-model model)
    (emagent-chat--write-top-property "EMAGENT_MODEL" model))
  (setq emagent-chat-model (or emagent-chat-model model))
  (emagent-chat--refresh-mode-line))

(defun emagent-chat-model ()
  "Return the ACP model id for the current emagent buffer."
  (emagent-chat--normalize-model-id
   (or emagent-chat-model (emagent-chat--read-model-property))))

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
  (let ((saved-point (point-marker))
        (saved-windows (emagent-chat--save-window-views)))
    (unwind-protect
        (funcall fn)
      (when (marker-position saved-point)
        (goto-char saved-point))
      (set-marker saved-point nil)
      (emagent-chat--restore-window-views saved-windows))))

(defun emagent-chat--with-streaming-view (fn)
  "Run FN during live streaming without disturbing windows already at buffer end."
  (let* ((views (emagent-chat--save-window-views))
         (pinned (cl-remove-if (lambda (v) (plist-get v :at-bottom)) views)))
    (unwind-protect
        (funcall fn)
      (emagent-chat--restore-window-views pinned))))

(provide 'emagent-chat)
;;; emagent-chat.el ends here
