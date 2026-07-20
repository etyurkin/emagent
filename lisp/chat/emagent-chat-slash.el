;;; emagent-chat-slash.el --- Slash command completion for emagent  -*- lexical-binding: t; -*-

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

;; ACP agent slash command name normalization, merging with Cursor built-in
;; commands, and completion-at-point integration.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)

(defgroup emagent-chat nil
  "Emagent chat UI."
  :group 'emagent)

(declare-function emagent-session-project-directory "emagent-session")
(declare-function emagent-chat--user-zone-start "emagent-chat-input")
(declare-function emagent-chat--user-heading-re "emagent-chat-input")
(declare-function emagent-chat-cycle-or-org-cycle "emagent-chat")
(declare-function emagent-acp-ensure-connected "emagent")
(declare-function emagent-acp--session "emagent-acp-state")
(declare-function emagent-acp--connected-p "emagent-acp-state")
(declare-function emagent-acp--model-choices "emagent-acp-model")
(declare-function emagent-chat--slash-mcp-apply "emagent-chat-mcp")

(defconst emagent-chat--client-slash-commands
  '(((name . "model")
     (description . "switch model for this turn (marker stripped before send)"))
    ((name . "mcp")
     (description . "list/authenticate MCP servers (Claude or Cursor CLI)")))
  "Slash commands emagent handles itself; never sent to the agent.")

(defun emagent-chat--client-slash-command (name)
  "Return the client slash-command plist named NAME, or nil."
  (seq-find (lambda (c) (equal (map-elt c 'name) name))
            emagent-chat--client-slash-commands))

(defun emagent-chat--slash-model-apply-1 (bounds)
  "Prompt for a model and replace BOUNDS with the `/model' marker link."
  (let* ((state emagent-acp--session)
         (choices (and state (emagent-acp--model-choices state nil))))
    (cond
     ((not choices)
      (message "emagent: no models available yet — M-x emagent-connect"))
     (t
      (let* ((selection (completing-read "Model for this turn: "
                                         (mapcar #'car choices) nil t))
             (model-id (cdr (assoc-string selection choices))))
        (when model-id
          (delete-region (car bounds) (cdr bounds))
          (goto-char (car bounds))
          ;; An org link: agent/short-model as text, full id as target
          ;; (shown on hover).  Org fontifies it, it survives saving the
          ;; session file, deleting it cancels the override, and send strips
          ;; it from the outgoing prompt.
          (insert (emagent-chat--model-link model-id))))))))

(defun emagent-chat--slash-model-apply ()
  "Prompt for a model and replace the `/model' token with its marker link.
The `[[emagent://AGENT/MODEL][short]]' link makes the send path switch to
MODEL for this turn (restoring the buffer model afterward); the link is
stripped from the text sent to the agent."
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Turn on emagent-mode in this buffer first"))
  (let ((bounds (emagent-chat--slash-token-bounds))
        (buf (current-buffer)))
    (unless bounds
      (user-error "No `/model' token at point"))
    (if (emagent-acp--connected-p)
        (emagent-chat--slash-model-apply-1 bounds)
      (emagent-acp-ensure-connected
       :on-ready
       (lambda ()
         (with-current-buffer buf
           (emagent-chat--slash-model-apply-1
            (or (emagent-chat--slash-token-bounds) bounds))))))))

(defun emagent-chat--run-client-slash-command (name)
  "Run the client slash command NAME (dispatch after completion)."
  (pcase name
    ("model" (emagent-chat--slash-model-apply))
    ("mcp"
     (require 'emagent-chat-mcp)
     (emagent-chat--slash-mcp-apply))))

(defvar emagent-chat-provider)
(defvar-local emagent-chat-slash-commands nil
  "Slash commands for this buffer as plists (:name :description :hint).

Populated from the ACP agent via =available_commands_update= after the session
connects.  Cursor sessions also seed documented built-ins and custom commands
from ~/.cursor/commands and .cursor/commands/.")

(defcustom emagent-chat-notify-slash-commands t
  "When non-nil, show a message after the agent publishes slash commands."
  :type 'boolean
  :group 'emagent-chat)

(defun emagent-chat--slash-command-name (name)
  "Return slash command NAME without a leading \"/\"."
  (if (and (stringp name) (not (string-empty-p name)) (string-prefix-p "/" name))
      (substring name 1)
    name))

(defun emagent-chat--slash-command-plist (name description &optional hint)
  "Return a slash-command plist for NAME.

Arguments: DESCRIPTION, HINT."
  `((name . ,(emagent-chat--slash-command-name name))
    (description . ,(or description ""))
    (hint . ,(or hint ""))))

(defun emagent-chat--normalize-slash-commands (commands)
  "Normalize COMMANDS from ACP JSON into slash-command plists."
  (let ((items (cond
                ((vectorp commands) (append commands nil))
                ((listp commands) commands)
                (t nil))))
    (mapcar (lambda (cmd)
              (emagent-chat--slash-command-plist
               (map-elt cmd 'name)
               (map-elt cmd 'description)
               (map-nested-elt cmd '(input hint))))
            items)))

(defun emagent-chat--merge-slash-commands (base extra)
  "Merge EXTRA into BASE by command name; EXTRA overrides BASE."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (cmd base)
      (puthash (map-elt cmd 'name) cmd table))
    (dolist (cmd extra)
      (puthash (map-elt cmd 'name) cmd table))
    (let (result)
      (maphash (lambda (_ cmd) (push cmd result)) table)
      (sort result (lambda (a b) (string< (map-elt a 'name) (map-elt b 'name)))))))

;;;###autoload
(defun emagent-chat-clear-slash-commands ()
  "Clear slash commands until the agent publishes a fresh list."
  (setq emagent-chat-slash-commands nil))

;;;###autoload
(defun emagent-chat-seed-cursor-slash-commands ()
  "Merge Cursor built-in and custom slash commands into the buffer list."
  (when (and (eq emagent-chat-provider 'cursor)
             (fboundp 'emagent-cursor-slash-commands))
    (setq emagent-chat-slash-commands
          (emagent-chat--merge-slash-commands
           (emagent-cursor-slash-commands (emagent-session-project-directory))
           emagent-chat-slash-commands))))

;;;###autoload
(defun emagent-chat-set-slash-commands (commands)
  "Merge normalized COMMANDS from the agent into `emagent-chat-slash-commands'."
  (let ((incoming (emagent-chat--normalize-slash-commands commands)))
    (setq emagent-chat-slash-commands
          (if (and (eq emagent-chat-provider 'cursor)
                   (fboundp 'emagent-cursor-slash-commands))
              (emagent-chat--merge-slash-commands
               (emagent-cursor-slash-commands (emagent-session-project-directory))
               incoming)
            incoming))
    (when (and emagent-chat-notify-slash-commands
               emagent-chat-slash-commands)
      (emagent-log "%d slash commands from agent"
                   (length emagent-chat-slash-commands)))))

(defun emagent-chat--command-needle-base (needle)
  "Return NEEDLE with a trailing colon removed, for skill-name matching."
  (if (and (not (string-empty-p needle)) (string-suffix-p ":" needle))
      (substring needle 0 -1)
    needle))

(defun emagent-chat--command-skill-part (name)
  "Return the skill segment of slash command NAME (after the first colon)."
  (if (string-match ":" name)
      (substring name (match-end 0))
    name))

(defun emagent-chat--command-matches-needle-p (name needle)
  "Return non-nil when NEEDLE appears as a substring of NAME or its skill part."
  (or (string-empty-p needle)
      (string-match-p (regexp-quote needle) name)
      (string-match-p (regexp-quote needle)
                      (emagent-chat--command-skill-part name))))

(defun emagent-chat--slash-token-bounds ()
  "Return (START . END) for the slash command token at point, or nil.

Detects a `/name' token that point is within, anywhere on the line — at the
start (after the user heading) or mid-prompt (e.g. `commit, use /model') — so
long as the `/' is preceded by the heading, the line start, or whitespace."
  (let ((zone (emagent-chat--user-zone-start))
        (user-point (point)))
    (when (>= user-point zone)
      (save-excursion
        (let ((floor (line-beginning-position)))
          ;; Do not scan back into the user heading prefix.
          (goto-char floor)
          (when (looking-at (emagent-chat--user-heading-re))
            (setq floor (match-end 0)))
          ;; Walk back over the non-whitespace token containing point.
          (goto-char user-point)
          (skip-chars-backward "^ \t\n" floor)
          (when (looking-at "/")
            (let ((start (point))
                  (end (progn (goto-char user-point)
                              (skip-chars-forward "^ \t\n" (line-end-position))
                              (point))))
              (when (<= start user-point end)
                (cons start end)))))))))

;;;###autoload
(defun emagent-chat-slash-command-completion-at-point ()
  "Complete agent slash commands at point."
  (when-let* ((bounds (emagent-chat--slash-token-bounds))
              (slash-start (car bounds))
              (end (cdr bounds))
              (prefix (buffer-substring-no-properties slash-start end))
              ((string-prefix-p "/" prefix))
              ;; Client commands (e.g. /model) are always offered; agent
              ;; commands are merged in once the session publishes them.
              ;; Cursor built-ins are seeded on mode enable / `emagent-connect'
              ;; without waiting for a dummy prompt.
              (commands (append emagent-chat--client-slash-commands
                                emagent-chat-slash-commands)))
    ;; Start the completion region AFTER the "/" so the framework sees the
    ;; bare name (e.g. "relax", "session:relax") as its input.  This lets
    ;; any completion style (basic, orderless, flex) filter naturally without
    ;; the leading "/" confusing prefix or substring matching.
    (list (1+ slash-start) end
          (mapcar (lambda (cmd) (map-elt cmd 'name)) commands)
          :annotation-function
          (lambda (candidate)
            (concat "  " (or (map-elt (cl-find candidate commands
                                               :key (lambda (c) (map-elt c 'name))
                                               :test #'string=)
                                      'description)
                             "")))
          :exit-function
          (lambda (str status)
            ;; A client command runs its handler instead of staying as text.
            (when (and (memq status '(finished sole exact))
                       (emagent-chat--client-slash-command str))
              (emagent-chat--run-client-slash-command str)))
          :exclusive t)))

;;;###autoload
(defun emagent-chat-tab ()
  "On a slash-command token, complete or run it; otherwise run `org-cycle'."
  (interactive)
  (let* ((bounds (emagent-chat--slash-token-bounds))
         (name (and bounds
                    (buffer-substring-no-properties (1+ (car bounds)) (cdr bounds)))))
    (cond
     ;; A complete client command (e.g. fully-typed /model) runs directly, since
     ;; `completion-at-point' would treat an exact sole match as nothing to do
     ;; and never fire the exit-function.
     ((and name (emagent-chat--client-slash-command name))
      (emagent-chat--run-client-slash-command name))
     ;; Otherwise complete: client commands (/model) are always offered, agent
     ;; commands merge in once the session publishes them.
     (bounds
      (call-interactively #'completion-at-point))
     (t
      (call-interactively #'emagent-chat-cycle-or-org-cycle)))))

(provide 'emagent-chat-slash)
;;; emagent-chat-slash.el ends here
