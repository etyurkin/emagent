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
(declare-function cl-find "cl-lib")

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
  "Return (START . END) for the slash command token at point, or nil."
  (let ((zone (emagent-chat--user-zone-start))
        (user-point (point)))
    (when (>= user-point zone)
      (save-excursion
        (beginning-of-line)
        ;; Skip past user heading prefix when the line is a heading.
        (when (looking-at (emagent-chat--user-heading-re))
          (goto-char (match-end 0)))
        (when (looking-at "/")
          (let ((start (point))
                (end (or (and (search-forward-regexp "[ \t]" (line-end-position) t)
                              (match-beginning 0))
                         (line-end-position))))
            (when (<= start user-point end)
              (cons start end))))))))

;;;###autoload
(defun emagent-chat-slash-command-completion-at-point ()
  "Complete agent slash commands at point."
  (when-let* ((bounds (emagent-chat--slash-token-bounds))
              (commands emagent-chat-slash-commands)
              (slash-start (car bounds))
              (end (cdr bounds))
              (prefix (buffer-substring-no-properties slash-start end))
              ((string-prefix-p "/" prefix)))
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
          :exclusive t)))

;;;###autoload
(defun emagent-chat-tab ()
  "On a slash-command line, complete; otherwise org-cycle."
  (interactive)
  (cond
   ((and (emagent-chat--slash-token-bounds) emagent-chat-slash-commands)
    (call-interactively #'completion-at-point))
   ((emagent-chat--slash-token-bounds)
    (emagent-acp-ensure-connected)
    (emagent-log "slash commands load after the agent connects"))
   (t
    (call-interactively #'emagent-chat-cycle-or-org-cycle))))

(provide 'emagent-chat-slash)
;;; emagent-chat-slash.el ends here
