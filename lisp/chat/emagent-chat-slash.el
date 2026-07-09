;;; emagent-chat-slash.el --- Slash command completion for emagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

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

(declare-function emagent-chat-project-directory "emagent-chat")
(declare-function emagent-chat--user-zone-start "emagent-chat")
(declare-function emagent-chat--user-heading-re "emagent-chat")
(declare-function emagent-chat-cycle-or-org-cycle "emagent-chat")
(declare-function emagent-acp-ensure-connected "emagent-acp")
(declare-function emagent-acp--session "emagent-acp")
(declare-function emagent-acp--model-choices "emagent-acp-model")
(declare-function cl-find "cl-lib")

(defconst emagent-chat--turn-model-property 'emagent-turn-model
  "Text property stamped on `/model'-inserted text.
Its value is the model id to use for the turn the prompt is sent in; carrying it
in the text (rather than a separate variable) ties the override to the prompt as
edited, so removing the word removes the override.")

(defconst emagent-chat--client-slash-commands
  '(((name . "model") (description . "switch model for this turn only")))
  "Slash commands emagent handles itself; never sent to the agent.")

(defun emagent-chat--client-slash-command (name)
  "Return the client slash-command plist named NAME, or nil."
  (seq-find (lambda (c) (equal (map-elt c 'name) name))
            emagent-chat--client-slash-commands))

(defun emagent-chat--slash-model-apply ()
  "Prompt for a model and replace the `/model' token with it for this turn.
The inserted model id is stamped with `emagent-chat--turn-model-property' so the
send path switches to it for the turn and restores the buffer model afterward."
  (let* ((state (and (fboundp 'emagent-acp--session) (emagent-acp--session)))
         (choices (and state (fboundp 'emagent-acp--model-choices)
                       (emagent-acp--model-choices state nil)))
         (bounds (emagent-chat--slash-token-bounds)))
    (cond
     ((not choices)
      (emagent-log "no models available yet — connect the agent first"))
     (bounds
      (let* ((selection (completing-read "Model for this turn: "
                                         (mapcar #'car choices) nil t))
             (model-id (cdr (assoc-string selection choices))))
        (when model-id
          (delete-region (car bounds) (cdr bounds))
          (goto-char (car bounds))
          (insert (propertize model-id
                              emagent-chat--turn-model-property model-id))))))))

(defun emagent-chat--run-client-slash-command (name)
  "Run the client slash command NAME (dispatch after completion)."
  (pcase name
    ("model" (emagent-chat--slash-model-apply))))

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
  "Return a slash-command plist for NAME."
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
           (emagent-cursor-slash-commands (emagent-chat-project-directory))
           emagent-chat-slash-commands))))

;;;###autoload
(defun emagent-chat-set-slash-commands (commands)
  "Merge normalized COMMANDS from the agent into `emagent-chat-slash-commands'."
  (let ((incoming (emagent-chat--normalize-slash-commands commands)))
    (setq emagent-chat-slash-commands
          (if (and (eq emagent-chat-provider 'cursor)
                   (fboundp 'emagent-cursor-slash-commands))
              (emagent-chat--merge-slash-commands
               (emagent-cursor-slash-commands (emagent-chat-project-directory))
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
  "On a slash-command line, complete; otherwise org-cycle."
  (interactive)
  (cond
   ;; Client commands (/model) are always completable; agent commands merge in.
   ((emagent-chat--slash-token-bounds)
    (call-interactively #'completion-at-point))
   (t
    (call-interactively #'emagent-chat-cycle-or-org-cycle))))

(provide 'emagent-chat-slash)
;;; emagent-chat-slash.el ends here
