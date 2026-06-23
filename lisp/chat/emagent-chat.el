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

(declare-function emagent-chat--refresh-mode-line "emagent-chat-mode-line")

(declare-function emagent-set-model "emagent-acp")
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


(defvar emagent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'emagent-chat-send-or-babel)
    (define-key map (kbd "C-c a")   #'emagent-chat-attach-buffer)
    (define-key map (kbd "C-c b")   #'emagent-btw)
    (define-key map (kbd "C-c d")   #'emagent-chat-attach-files)
    (define-key map (kbd "C-c e")   #'emagent-chat-attach-error-context)
    (define-key map (kbd "C-c i")   #'emagent-chat-attach-image)
    (define-key map (kbd "C-c m")   #'emagent-set-model)
    (define-key map (kbd "C-c l")   #'emagent-log-view)
    (define-key map (kbd "C-y")     #'emagent-chat-yank)
    (define-key map (kbd "TAB") #'emagent-chat-tab)
    (define-key map (kbd "<backtab>") #'org-shifttab)
    (define-key map (kbd "C-g C-g") #'emagent-chat-interrupt)
    (define-key map (kbd "C-c p")   #'emagent-chat-new-prompt)
    (define-key map (kbd "C-c ?")   #'emagent-dispatch)
    (define-key map (kbd "C-a")     #'emagent-chat-beginning-of-line)
    map)
  "Keymap for `emagent-mode'.")

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
  "Tools allowed without confirmation for the current emagent buffer.

Persisted in the buffer header as =#+EMAGENT_ALLOWED_TOOLS= and added to when
the user answers \"allow all\" to a tool confirmation.")

(defface emagent-tool-detail
  '((t (:inherit fixed-pitch :slant normal)))
  "Face for paths and commands on tool-call lines."
  :group 'emagent-chat)

(defface emagent-permission-prompt
  '((t (:inherit font-lock-warning-face :weight bold)))
  "Face for the permission question line in the Thinking block."
  :group 'emagent-chat)

(defconst emagent-chat-default-slug "emagent")

(defconst emagent-chat-response-begin "# --- emagent ---")
(defconst emagent-chat-response-end "# --- /emagent ---")

(defconst emagent-chat--progress-line "/emagent is thinking…/\n"
  "Placeholder body line shown until a prompt finishes rendering.")

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
# C-c p   insert a new '* username>' prompt heading
# C-c a   attach buffer context to the next send
# C-c b   queue a follow-up message (btw) for after agent finishes
# C-c d   pick project files to attach
# C-c e   attach compilation/flymake errors to the next send
# C-y     paste text normally; if clipboard has image, inserts [[file:...]] link
# C-c i   pick an image file and insert [[file:...]] link at point
# C-c l   show emagent log (*Emagent Log*)
# C-c m   set ACP model
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
  (unless (equal emagent-chat-model model)
    (setq emagent-chat-model model)
    (emagent-chat--write-top-property "EMAGENT_MODEL" model))
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
  "Return the tools allowed without confirmation for this emagent buffer."
  (or emagent-chat-allowed-tools
      (emagent-chat--read-allowed-tools-property)))

(defun emagent-chat-add-allowed-tool (tool)
  "Allow TOOL for this buffer without confirmation and persist it.

TOOL is a symbol (or a string naming one).  Records it in
`emagent-chat-allowed-tools' and writes the merged list to the buffer header
as #+EMAGENT_ALLOWED_TOOLS, alongside the other #+EMAGENT_* properties."
  (let* ((sym (if (stringp tool) (intern tool) tool))
         (current (emagent-chat-allowed-tools)))
    (unless (memq sym current)
      (setq emagent-chat-allowed-tools (append current (list sym)))
      (emagent-chat--write-top-property
       emagent-chat--allowed-tools-property
       (mapconcat #'symbol-name emagent-chat-allowed-tools " ")))))

(defun emagent-chat--writable ()
  "Remove read-only state left by older emagent chat UIs."
  (setq buffer-read-only nil)
  (when (< (point-min) (point-max))
    (remove-text-properties (point-min) (point-max) '(read-only t))))

(defun emagent-chat--insert-initial-comment ()
  "Insert the scratch-style intro and initial user heading in a new buffer."
  (when (= (point-min) (point-max))
    (insert emagent-chat-initial-comment)
    (goto-char (point-max))
    (insert (emagent-chat--user-heading-prefix))))

(defun emagent-chat--line-text ()
  (string-trim (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))

(defun emagent-chat--user-heading-prefix ()
  "Return the org heading prefix for user turns, e.g. \"* etyurkin> \"."
  (format "* %s> " (user-login-name)))

(defun emagent-chat--user-heading-re ()
  "Return a regexp matching the user heading prefix at start of line."
  (format "^\\* %s> ?" (regexp-quote (user-login-name))))

(defun emagent-chat--user-prompt-input-pos ()
  "Return point after the user heading prefix on the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at (emagent-chat--user-heading-re))
      (match-end 0))))

(defun emagent-chat-beginning-of-line ()
  "On a user prompt heading, first \\[emagent-chat-beginning-of-line] jumps after \">\"."
  (interactive)
  (let ((input (emagent-chat--user-prompt-input-pos)))
    (cond
     ((and input (= (point) input))
      (move-beginning-of-line 1))
     ((and input (not (= (point) (line-beginning-position))))
      (goto-char input))
     (t
      (move-beginning-of-line 1)))))

(defun emagent-chat--strip-user-heading (text)
  "Strip the '* username> ' prefix from the first line of TEXT."
  (let* ((re (emagent-chat--user-heading-re))
         (lines (split-string text "\n" nil))
         (first (car lines)))
    (if (string-match re first)
        (string-join (cons (substring first (match-end 0)) (cdr lines)) "\n")
      text)))

(defun emagent-chat--format-as-user-heading (bounds raw)
  "Replace text at BOUNDS with RAW formatted as a user org heading.
Returns the buffer position after the formatted heading."
  (let* ((inhibit-read-only t)
         (prefix (emagent-chat--user-heading-prefix))
         (already (string-match-p "^\\* " raw))
         (lines (and (not already) (split-string raw "\n" t)))
         (formatted (if already
                        raw
                      (if (cdr lines)
                          (concat prefix (car lines) "\n"
                                  (string-join (cdr lines) "\n"))
                        (concat prefix (car lines))))))
    (emagent-chat--writable)
    (goto-char (car bounds))
    (delete-region (car bounds) (cdr bounds))
    (insert formatted)
    (unless (= (char-before) ?\n)
      (insert "\n"))
    (point)))

(defun emagent-chat--delete-following-response (pos)
  "Delete the response block after POS, stopping before the next user heading.

Deletes from the first '# --- emagent ---' after POS up to (but not
including) the next '* user>' heading, whether bare or with content."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t\n")
    (when (looking-at emagent-chat--response-begin-re)
      (let ((start (line-beginning-position))
            (inhibit-read-only t))
        (emagent-chat--writable)
        (when (re-search-forward emagent-chat--response-end-re nil t)
          (forward-line 1)
          (skip-chars-forward " \t\n")
          ;; Stop here — leave whatever follows (next heading, stub, or nothing).
          (delete-region start (point)))))))

(defun emagent-chat--user-heading-follows-p ()
  "Return non-nil when a '* user>' heading immediately follows point."
  (save-excursion
    (end-of-line)
    (skip-chars-forward " \t\n")
    (looking-at (emagent-chat--user-heading-re))))

(defun emagent-chat--insert-user-heading-stub ()
  "Insert a user heading stub unless one already follows the user zone."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (goto-char (emagent-chat--user-zone-start))
    (unless (emagent-chat--user-heading-follows-p)
      (unless (bolp) (insert "\n"))
      (insert (emagent-chat--user-heading-prefix)))
    (point)))

(defun emagent-chat--sendable-line-p (line)
  (or (string-empty-p line)
      (and (not (string-match-p "^#\\+" line))
           (not (string-match-p "^# " line))
           (not (string-match-p "^\\* Emagent\\b" line))
           (not (string-match-p emagent-chat--response-begin-re line))
           (not (string-match-p emagent-chat--response-end-re line))
           (not (string-match-p "^#\\+BEGIN_SRC" line))
           (not (string-match-p "^#\\+END_SRC" line))
           ;; Bare stub "* user> " with no text after it is not sendable
           (not (string-match-p (concat (emagent-chat--user-heading-re) "$") line)))))

(defun emagent-chat--sendable-text-p (text)
  "Return non-nil when TEXT is user prompt material, not buffer metadata."
  (and (not (string-empty-p text))
       (seq-every-p #'emagent-chat--sendable-line-p (split-string text "\n" t))))

(defun emagent-chat--skip-header ()
  (goto-char (point-min))
  (while (and (not (eobp))
              (or (looking-at "#\\+")
                  (looking-at "# ")
                  (looking-at "#$")))
    (forward-line 1))
  (skip-chars-forward "\n")
  (point))

(defun emagent-chat--after-last-response ()
  "Return the position after the last closed emagent response."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward emagent-chat--response-end-re nil t)
        (progn
          (while (re-search-forward emagent-chat--response-end-re nil t)
            nil)
          (goto-char (match-end 0))
          (skip-chars-forward "\n")
          (line-beginning-position))
      (if (re-search-forward "^\\* Emagent\\b" nil t)
          (progn
            (goto-char (point-max))
            (skip-chars-forward "\n")
            (point))
        (emagent-chat--skip-header)))))

(defun emagent-chat--sync-user-zone-marker ()
  "Update the user-zone marker from the buffer, without insertion-type."
  (let ((pos (emagent-chat--after-last-response)))
    (if (and emagent-chat--user-zone-start-marker
             (marker-position emagent-chat--user-zone-start-marker))
        (set-marker emagent-chat--user-zone-start-marker pos)
      (setq emagent-chat--user-zone-start-marker (copy-marker pos nil)))))

(defun emagent-chat--user-zone-start ()
  "Return the buffer position where the next user prompt may begin."
  (emagent-chat--after-last-response))

(defun emagent-chat--send-bounds-backward (end zone-start)
  "Return bounds for the nearest preceding sendable line before END."
  (save-excursion
    (goto-char end)
    (let (found)
      (while (and (not found) (>= (line-beginning-position) zone-start))
        (let ((text (emagent-chat--line-text)))
          (when (emagent-chat--sendable-text-p text)
            (setq found (cons (line-beginning-position) (line-end-position)))))
        (unless found
          (forward-line -1)))
      found)))

(defun emagent-chat--user-block-bounds (zone-start)
  "Return (START . END) for the '* username>' block enclosing point, or nil.
Captures the heading line and all body lines up to the next heading or
response delimiter."
  (let ((user-re (emagent-chat--user-heading-re)))
    (save-excursion
      (let ((heading-pos
             (or (and (looking-at "\\* ") (line-beginning-position))
                 (and (re-search-backward "^\\* " zone-start t)
                      (line-beginning-position)))))
        (when (and heading-pos (>= heading-pos zone-start)
                   (save-excursion
                     (goto-char heading-pos)
                     (looking-at user-re)))
          (let* ((start heading-pos)
                 (end (progn
                        (goto-char heading-pos)
                        (forward-line 1)
                        (if (re-search-forward
                             (concat "^\\* \\|" emagent-chat--response-begin-re)
                             (point-max) t)
                            (match-beginning 0)
                          (point-max)))))
            (cons start (save-excursion
                          (goto-char end)
                          (skip-chars-backward " \t\n")
                          (point)))))))))

(defun emagent-chat--send-bounds ()
  "Return (BEG . END) of text to send at point.

When point is inside a '* user>' heading (anywhere in the buffer), the zone
check is skipped so the user can re-evaluate any previous prompt."
  (cond
   ((region-active-p)
    (cons (region-beginning) (region-end)))
   (t
    (let* ((zone-start (emagent-chat--user-zone-start))
           (point0 (point))
           ;; Re-eval: cursor is on/inside a user heading anywhere in buffer.
           (re-eval (emagent-chat--user-block-bounds (point-min))))
      (if re-eval
          re-eval
        (when (< (line-beginning-position) zone-start)
          (user-error "Move point below the latest emagent response"))
        (or (emagent-chat--send-bounds-backward point0 zone-start)
            (user-error "No sendable text at point")))))))




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

(defmacro emagent-chat--with-stable-view (&rest body)
  "Run BODY while preserving window scroll unless already at buffer end."
  (declare (indent 0))
  `(let* ((emagent-chat--view-saved-point (point))
          (emagent-chat--view-saved-windows (emagent-chat--save-window-views)))
     (unwind-protect
         (progn ,@body)
       (goto-char emagent-chat--view-saved-point)
       (emagent-chat--restore-window-views emagent-chat--view-saved-windows))))

(defun emagent-chat--open-reasoning-begin ()
  "Return point at the last Reasoning opener in the open response body."
  (when-let ((bounds (emagent-chat--open-response-body-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (let (last)
        (while (re-search-forward emagent-chat--reasoning-begin-re (cdr bounds) t)
          (setq last (match-beginning 0)))
        last))))

(defun emagent-chat--last-reasoning-end-quote-pos (begin limit)
  "Return buffer position of the last #+end_quote line after BEGIN before LIMIT."
  (save-excursion
    (goto-char begin)
    (let (last)
      (while (re-search-forward "^#\\+end_quote\\s-*$" limit t)
        (setq last (match-beginning 0)))
      last)))

(defun emagent-chat--insert-reasoning-scaffold ()
  "Insert an empty Thinking block at `emagent-chat--response-body-start'."
  (when (and emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (insert (format "#+begin_quote %s\n" emagent-chat--thinking-block-label))
    (setq emagent-chat--thought-marker (point-marker))
    (insert "\n#+end_quote\n\n")
    (setq emagent-chat--thought-open-p t
          emagent-chat--assistant-marker (point-marker))
    (font-lock-flush)))

(defun emagent-chat--ensure-reasoning-end-quote ()
  "Insert #+end_quote when the open Thinking block has no closing line."
  (when-let* ((beg (emagent-chat--open-reasoning-begin))
              (bounds (emagent-chat--open-response-body-bounds))
              (limit (cdr bounds))
              (search-from (save-excursion (goto-char beg) (line-end-position))))
    (unless (emagent-chat--last-reasoning-end-quote-pos search-from limit)
      (let ((insert-at (or (and emagent-chat--thought-marker
                                (marker-position emagent-chat--thought-marker))
                           (save-excursion
                             (goto-char beg)
                             (forward-line 1)
                             (point)))))
        (save-excursion
          (goto-char insert-at)
          (unless (bolp) (insert "\n"))
          (insert "#+end_quote\n\n")
          (setq emagent-chat--assistant-marker (point-marker)))
        (font-lock-flush)
        t))))

(defun emagent-chat--ensure-reasoning-scaffold ()
  "Ensure the open response has a Thinking block with #+end_quote present."
  (when (emagent-chat--open-response-p)
    (cond
     (emagent-chat--thought-open-p
      (emagent-chat--ensure-reasoning-end-quote)
      (emagent-chat--sync-thought-marker))
     ((emagent-chat--reasoning-stream-marker)
      (setq emagent-chat--thought-marker (emagent-chat--reasoning-stream-marker)
            emagent-chat--thought-open-p t))
     ((not (emagent-chat--open-reasoning-begin))
      (emagent-chat--insert-reasoning-scaffold))
     (t
      (emagent-chat--ensure-reasoning-end-quote)
      (let ((stream (emagent-chat--reasoning-stream-marker)))
        (setq emagent-chat--thought-marker stream
              emagent-chat--thought-open-p t)
        (unless stream
          (emagent-log "emagent: cannot find reasoning stream marker after ensure")))))))

(defun emagent-chat--reasoning-stream-marker ()
  "Return insert marker before the closing #+end_quote in the open Reasoning block.

Uses the last #+end_quote after the Reasoning opener so streamed text that
contains a literal #+end_quote line cannot steal the insertion point."
  (when-let* ((bounds (emagent-chat--open-response-body-bounds))
              (beg (emagent-chat--open-reasoning-begin))
              (end-quote (emagent-chat--last-reasoning-end-quote-pos
                           (save-excursion (goto-char beg) (line-end-position))
                           (cdr bounds))))
    (save-excursion
      (goto-char end-quote)
      (beginning-of-line)
      (when (and (> (point) (point-min))
                 (= (char-before) ?\n))
        (backward-char 1))
      (point-marker))))

(defun emagent-chat--reasoning-block-tail ()
  "Return point after the last Reasoning block in the open response, or nil."
  (when-let* ((bounds (emagent-chat--open-response-body-bounds))
              (beg (emagent-chat--open-reasoning-begin))
              (end-quote (emagent-chat--last-reasoning-end-quote-pos
                           (save-excursion (goto-char beg) (line-end-position))
                           (cdr bounds))))
    (save-excursion
      (goto-char end-quote)
      (goto-char (line-end-position))
      (skip-chars-forward "\n")
      (point))))

(defun emagent-chat--sync-thought-marker ()
  "Realign `emagent-chat--thought-marker' before the true Reasoning tail."
  (when emagent-chat--thought-open-p
    (when-let ((stream (emagent-chat--reasoning-stream-marker)))
      (let ((cur (and emagent-chat--thought-marker
                      (marker-position emagent-chat--thought-marker))))
        (when (or (not cur) (< cur (marker-position stream)))
          (setq emagent-chat--thought-marker stream))))))

(defun emagent-chat--can-resume-reasoning-p ()
  "Return non-nil when streaming can continue in an existing Reasoning block."
  (when-let* ((tail (emagent-chat--reasoning-block-tail))
              (bounds (emagent-chat--open-response-body-bounds))
              ((>= tail (car bounds))))
    (string-empty-p
     (string-trim (buffer-substring-no-properties tail (cdr bounds))))))

(defun emagent-chat--ensure-thought-stream ()
  "Open or resume the streaming Reasoning block in the in-flight response."
  (emagent-chat--ensure-reasoning-scaffold))

(defun emagent-chat--begin-response (&optional at)
  "Insert a new emagent response block at AT or point."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (goto-char (or at (point)))
    (unless (bolp)
      (insert "\n"))
    (insert (format "\n%s\n" emagent-chat-response-begin))
    (setq emagent-chat--response-body-start (point-marker))
    (emagent-chat--insert-reasoning-scaffold)))

(defun emagent-chat-insert-system (message)
  "Append system MESSAGE to `emagent-log-buffer-name'."
  (emagent-log "%s" message))

(defun emagent-chat-start-assistant ()
  "Begin a new emagent response section."
  (with-current-buffer (current-buffer)
    (emagent-chat--begin-response)))

(defun emagent-chat--goto-active-response-point ()
  "Go to the insertion point for the in-flight response."
  (if (and emagent-chat--assistant-marker
           (marker-position emagent-chat--assistant-marker))
      (goto-char emagent-chat--assistant-marker)
    (if (emagent-chat--open-response-p)
        (goto-char (point-max))
      (error "No open emagent response"))))

(defun emagent-chat--format-thought-block (text)
  "Return org markup for agent reasoning TEXT, or \"\" when empty."
  (let ((trimmed (string-trim (or text ""))))
    (if (string-empty-p trimmed)
        ""
      (format "#+begin_quote %s\n%s\n#+end_quote\n\n"
              emagent-chat--thinking-block-label trimmed))))

(defun emagent-chat--reasoning-block-bounds ()
  "Return (CONTENT-START . CONTENT-END) for a closed Reasoning block at point."
  (save-excursion
    (unless (looking-at emagent-chat--reasoning-begin-re)
      (re-search-backward emagent-chat--reasoning-begin-re nil t))
    (beginning-of-line)
    (let ((content-start (line-end-position))
          (limit (save-excursion
                   (forward-line 1)
                   (or (and (re-search-forward emagent-chat--reasoning-begin-re
                                               (point-max) t)
                            (match-beginning 0))
                       (point-max)))))
      (when-let ((content-end (emagent-chat--last-reasoning-end-quote-pos
                                content-start limit)))
        (when (> content-end content-start)
          (cons content-start content-end))))))

(defun emagent-chat--hide-reasoning-by-region (bounds)
  "Hide Reasoning content between BOUNDS using `org-fold-region'."
  (when bounds
    (ignore-errors
      (org-fold-region (car bounds) (cdr bounds) t 'block))))

(defun emagent-chat--hide-reasoning-at-point ()
  "Hide Reasoning quote content at or near point.

Prefer Org block folding when the parser accepts the block; fall back to
folding the inner region only so incomplete parses never break the buffer."
  (when-let ((bounds (emagent-chat--reasoning-block-bounds)))
    (condition-case _
        (progn
          (when (fboundp 'org-element-cache-reset)
            (org-element-cache-reset))
          (save-excursion
            (goto-char (car bounds))
            (beginning-of-line)
            (let ((element (org-element-at-point)))
              (if (eq (org-element-type element) 'quote-block)
                  (progn
                    (require 'org-fold)
                    (org-fold-hide-block-toggle 'hide nil element))
                (emagent-chat--hide-reasoning-by-region bounds)))))
      (error
       (emagent-chat--hide-reasoning-by-region bounds)))))

(defun emagent-chat--hide-reasoning-deferred (&optional pos)
  "Hide the Reasoning block near POS after the next redisplay."
  (when emagent-chat-fold-reasoning-on-done
    (let ((buffer (current-buffer))
          (at (or pos
                  (save-excursion
                    (unless (looking-at emagent-chat--reasoning-begin-re)
                      (re-search-backward emagent-chat--reasoning-begin-re nil t))
                    (point)))))
      (when (and buffer at)
        (run-with-idle-timer
         0 nil
         (lambda ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (save-excursion
                 (goto-char at)
                 (emagent-chat--hide-reasoning-at-point))))))))))








(defun emagent-chat--goto-response-insertion-point ()
  "Go to the tail of the open emagent response at or before point."
  (cond
   ((and emagent-chat--assistant-marker
         (marker-position emagent-chat--assistant-marker))
    (goto-char emagent-chat--assistant-marker))
   ((save-excursion
      (and (emagent-chat--find-response-begin-backward)
           (not (re-search-forward emagent-chat--response-end-re (point-max) t))))
    (goto-char (point-max)))
   (t
    (goto-char (point-max)))))

(defun emagent-chat--response-end-present-p ()
  "Return non-nil when an end delimiter sits on the current line."
  (looking-at (concat (regexp-quote emagent-chat-response-end) "\\s-*")))

(defun emagent-chat--split-glued-response-end ()
  "Move a glued # --- /emagent --- delimiter onto its own line."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat "\\([^\n]\\)" (regexp-quote emagent-chat-response-end))
            nil t)
      (replace-match (concat "\\1\n" emagent-chat-response-end) t))))

(defun emagent-chat--insert-response-end ()
  "Insert a response end delimiter at the open response tail."
  (when (emagent-chat--open-response-p)
    (emagent-chat--split-glued-response-end)
    (goto-char (point-max))
    (unless (save-excursion
              (goto-char (point-max))
              (skip-chars-backward "\n")
              (beginning-of-line)
              (emagent-chat--response-end-present-p))
      (unless (bolp)
        (insert "\n"))
      (insert (format "%s\n\n" emagent-chat-response-end)))))

(defun emagent-chat--reset-response-state ()
  (setq emagent-chat--assistant-marker nil
        emagent-chat--response-body-start nil
        emagent-chat--thought-open-p nil
        emagent-chat--thought-marker nil
        emagent-chat--reasoning-streamed-p nil)
  (clrhash emagent-chat--tool-call-lines))

(defun emagent-chat--ensure-response-markers ()
  "Set body markers for the open response when they were lost."
  (unless (and emagent-chat--response-body-start
               (marker-position emagent-chat--response-body-start)
               emagent-chat--assistant-marker
               (marker-position emagent-chat--assistant-marker))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (setq emagent-chat--response-body-start (copy-marker (car bounds) nil)
            emagent-chat--assistant-marker (copy-marker (cdr bounds) nil)))))

(defun emagent-chat--fail-response-p ()
  "Return non-nil when an emagent response can be closed with an error."
  (or (and emagent-chat--response-body-start
           (marker-position emagent-chat--response-body-start))
      (save-excursion
        (and (emagent-chat--find-response-begin-backward)
             (not (re-search-forward emagent-chat--response-end-re (point-max) t))))))

(defun emagent-chat-begin-thought ()
  "Resume or open the Thinking block in the in-flight emagent response."
  (emagent-chat--with-stable-view
    (with-current-buffer (current-buffer)
      (when (emagent-chat--open-response-p)
        (let ((inhibit-read-only t))
          (emagent-chat--writable)
          (emagent-chat--ensure-response-markers)
          (emagent-chat--ensure-reasoning-scaffold))))))

(defun emagent-chat-append-thought (text)
  "Append reasoning TEXT to the open Reasoning block."
  (when (not (string-empty-p text))
    (emagent-chat--with-stable-view
      (with-current-buffer (current-buffer)
        (when (emagent-chat--open-response-p)
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat--ensure-response-markers)
            (emagent-chat--ensure-thought-stream)
            (when (and emagent-chat--thought-marker
                       (marker-position emagent-chat--thought-marker))
              (save-excursion
                (goto-char emagent-chat--thought-marker)
                (emagent-chat--insert-reasoning-text text)
                (setq emagent-chat--thought-marker (point-marker)
                      emagent-chat--assistant-marker (point-marker)
                      emagent-chat--reasoning-streamed-p t)))
            (font-lock-flush)))))))

(defun emagent-chat-close-thought ()
  "Close and hide the open Reasoning block, if any."
  (emagent-chat--with-stable-view
    (with-current-buffer (current-buffer)
      (when emagent-chat--thought-open-p
        (let ((inhibit-read-only t)
              (hide-at nil))
          (emagent-chat--writable)
          (emagent-chat--ensure-reasoning-end-quote)
          (when (and emagent-chat--thought-marker
                     (marker-position emagent-chat--thought-marker))
            (save-excursion
              (goto-char emagent-chat--thought-marker)
              (unless (bolp)
                (insert "\n"))))
          (when-let ((beg (emagent-chat--open-reasoning-begin))
                     (bounds (emagent-chat--open-response-body-bounds))
                     (end-quote (emagent-chat--last-reasoning-end-quote-pos
                                  (save-excursion (goto-char beg) (line-end-position))
                                  (cdr bounds))))
            (setq hide-at beg)
            (save-excursion
              (goto-char end-quote)
              (goto-char (line-end-position))
              (skip-chars-forward "\n")
              (setq emagent-chat--assistant-marker (point-marker))))
          (setq emagent-chat--thought-open-p nil
                emagent-chat--thought-marker nil)
          (font-lock-flush)
          (when hide-at
            (emagent-chat--hide-reasoning-deferred hide-at)))))))

(defun emagent-chat--finish-tool-line-in-reasoning ()
  "Leave `emagent-chat--thought-marker' ready for streamed reasoning text."
  (let ((end (line-end-position)))
    (if (save-excursion
          (goto-char end)
          (and (not (eobp))
               (looking-at "\n")
               (progn (forward-line 1) t)
               (looking-at "#\\+end_quote")))
        (setq emagent-chat--thought-marker (copy-marker end nil))
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (setq emagent-chat--thought-marker (point-marker)))))

(defun emagent-chat--insert-reasoning-text (text)
  "Insert TEXT at `emagent-chat--thought-marker' before the Reasoning tail."
  (if (and (eolp)
           (save-excursion
             (forward-line 1)
             (looking-at "#\\+end_quote"))
           (save-excursion
             (beginning-of-line)
             (looking-at "→ ")))
      (insert "\n" text)
    (insert text)))

(defun emagent-chat--org-verbatim-paths (text)
  "Wrap absolute paths in org =verbatim= so /Users/ is not parsed as /italic/."
  (replace-regexp-in-string "\\(/[^ \t\n]+\\)" "=\\1=" text))

(defun emagent-chat--format-tool-line (label)
  "Return a Thinking-block tool line for LABEL, safe in org-mode."
  (format "→ %s" (emagent-chat--org-verbatim-paths label)))

(defun emagent-chat--format-permission-line (question)
  "Return a Thinking-block permission question line for QUESTION."
  (format "? %s" (emagent-chat--org-verbatim-paths question)))

(defun emagent-chat--repair-tool-line-faces (start end)
  "Re-apply path faces after org font-lock on tool-call lines."
  (when (and start end (< start end))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward "\\=/[^ \t\n]+" end t))
        (let ((s (match-beginning 0))
              (e (match-end 0)))
          (remove-list-of-text-properties s e '(face))
          (put-text-property s e 'face 'emagent-tool-detail))))))

(defun emagent-chat--after-fontify-repair-tool-lines (beg end)
  "Repair org /italic/ on tool-call lines after each font-lock pass."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^→ " end t)
      (emagent-chat--repair-tool-line-faces (line-beginning-position)
                                             (line-end-position)))))

(defun emagent-chat--fontify-tool-line (start end)
  "Font-lock tool line START..END and repair org emphasis on paths."
  (when (and start end (<= start end))
    (font-lock-flush)
    (emagent-chat--repair-tool-line-faces start end)))

(defun emagent-chat--ensure-reasoning-for-tool ()
  "Ensure the open response can accept tool annotations in Reasoning."
  (when (emagent-chat--open-response-p)
    (emagent-chat--ensure-reasoning-scaffold)))

(defun emagent-chat--append-tool-line (label &optional id)
  "Append tool LABEL to the open Reasoning block.
When ID is non-nil, remember the line span for later in-place updates."
  (when (and label (not (string-empty-p label))
               (emagent-chat--open-response-p))
    (emagent-chat--with-stable-view
     (with-current-buffer (current-buffer)
       (let ((inhibit-read-only t))
         (emagent-chat--writable)
         (emagent-chat--ensure-response-markers)
         (emagent-chat--ensure-reasoning-for-tool)
         (unless (and id (emagent-chat--update-tool-call-line id label))
           (when (and emagent-chat--thought-open-p
                      emagent-chat--thought-marker
                      (marker-position emagent-chat--thought-marker))
             (save-excursion
               (goto-char emagent-chat--thought-marker)
               (unless (bolp) (insert "\n"))
               (let ((line-start (line-beginning-position)))
                 (insert (emagent-chat--format-tool-line label))
                 (let ((line-end (line-end-position)))
                   (when id
                     (puthash id (cons (copy-marker line-start nil)
                                       (copy-marker line-end nil))
                              emagent-chat--tool-call-lines))
                   (emagent-chat--fontify-tool-line line-start line-end))
                 (emagent-chat--finish-tool-line-in-reasoning))))))))))

(defun emagent-chat--update-tool-call-line (id label)
  "Replace the displayed tool-call line for ID with LABEL.
Return non-nil when a line was updated."
  (let ((entry (gethash id emagent-chat--tool-call-lines)))
    (when (and entry
               (markerp (car entry)) (marker-position (car entry))
               (markerp (cdr entry)) (marker-position (cdr entry)))
      (let* ((start (car entry))
             (end (cdr entry))
             (display (emagent-chat--format-tool-line label)))
        (unless (string= (buffer-substring-no-properties start end) display)
          (save-excursion
            (delete-region start end)
            (goto-char start)
            (insert display)
            (set-marker end (point))
            (emagent-chat--fontify-tool-line (marker-position start)
                                             (marker-position end))
            (when emagent-chat--thought-open-p
              (setq emagent-chat--thought-marker
                    (emagent-chat--reasoning-stream-marker)))))
        t))))

(defun emagent-chat-show-tool-call (id label)
  "Show or update a tool-call line for ACP toolCallId ID with LABEL."
  (emagent-chat--append-tool-line label id))

(defun emagent-chat-permission-prompt (question choices &optional tool-call)
  "Show QUESTION inside the Thinking block; CHOICES as buttons below #+end_quote.
When TOOL-CALL is non-nil and has a command or content, a formatted org
subsection is inserted between the question line and the buttons.

CHOICES is a list of (LABEL . VALUE) pairs.  Blocks via `recursive-edit'
until a button is clicked or C-g is pressed; removes the question line,
content block, and buttons afterward.  Returns the VALUE of the clicked
button, or nil on C-g.

Keyboard shortcuts (active only during the prompt, via `overriding-local-map'):
  y / RET  — Allow the first allow-type choice
  n        — Deny the first deny-type choice
  a        — Allow All (session)
  q        — Cancel (same as C-g)"
  (when (emagent-chat--open-response-p)
    (let ((result nil)
          (buf (current-buffer))
          question-beg question-end
          content-beg content-end
          buttons-beg buttons-end
          key-map)
      ;; Build keymap for keyboard shortcuts in the body so the
      ;; byte-compiler sees `result' as a lexical variable caught
      ;; by the key-binding closures.
      (setq key-map
            (let ((map (make-sparse-keymap))
                  (allow-key nil)
                  (always-key nil)
                  (deny-key nil))
              ;; Allow all (session) — always present
              (define-key map (kbd "a")
                (lambda () (interactive) (setq result :allow-all) (exit-recursive-edit)))
              ;; Quit / cancel
              (define-key map (kbd "q")
                (lambda () (interactive) (keyboard-quit)))
              ;; Scan choices for allow and deny options
              (dolist (choice choices)
                (let* ((val (cdr choice))
                       (id (and (stringp val) (downcase val))))
                  (cond
                   ((eq val :allow-all)) ;; already handled above
                   ((and id (string-match-p "allow_always\\|always" id))
                    (unless always-key
                      (setq always-key val)
                      (define-key map (kbd "w")
                        (lambda () (interactive) (setq result always-key) (exit-recursive-edit)))))
                   ((and id (string-match-p "allow\\|yes\\|run" id))
                    (unless allow-key
                      (setq allow-key val)
                      (define-key map (kbd "y")
                        (lambda () (interactive) (setq result allow-key) (exit-recursive-edit)))
                      (define-key map (kbd "RET")
                        (lambda () (interactive) (setq result allow-key) (exit-recursive-edit)))))
                   ((and id (string-match-p "deny\\|no\\|reject" id))
                    (unless deny-key
                      (setq deny-key val)
                      (define-key map (kbd "n")
                        (lambda () (interactive) (setq result deny-key) (exit-recursive-edit))))))))
              map))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (emagent-chat--writable)
          (emagent-chat--ensure-response-markers)
          (emagent-chat--ensure-reasoning-scaffold)
          (emagent-chat--ensure-reasoning-end-quote)
          (if-let ((insert-at (emagent-chat--reasoning-stream-marker)))
              (progn
                (goto-char insert-at)
                (unless (bolp) (insert "\n"))
                (setq question-beg (copy-marker (point) nil))
                (insert (emagent-chat--format-permission-line question))
                (put-text-property (marker-position question-beg) (point)
                                   'face 'emagent-permission-prompt)
                (emagent-chat--repair-tool-line-faces (marker-position question-beg) (point))
                (insert "\n")
                (setq question-end (copy-marker (point) nil))
                ;; Insert content block (command, edit, etc.) when available.
                (when-let ((block (and tool-call
                                       (fboundp 'emagent-acp--tool-call-content-block)
                                       (emagent-acp--tool-call-content-block tool-call))))
                  (goto-char (or (emagent-chat--reasoning-block-tail)
                                 (marker-position question-end)))
                  (unless (and (bolp)
                               (save-excursion
                                 (forward-line -1)
                                 (and (bolp) (not (looking-at "^#\\+end_quote")))))
                    (insert "\n"))
                  (setq content-beg (copy-marker (point) nil))
                  (insert block "\n")
                  (setq content-end (copy-marker (point) nil)))
                ;; Move to insertion point for buttons.
                (goto-char (or (and content-end (marker-position content-end))
                               (emagent-chat--reasoning-block-tail)
                               (marker-position question-end)))
                (unless (and (bolp)
                             (save-excursion
                               (forward-line -1)
                               (and (bolp) (not (looking-at "^#\\+end_quote")))))
                  (insert "\n"))
                (setq buttons-beg (copy-marker (point) nil))
                (let ((allow-shown nil)
                      (always-shown nil)
                      (deny-shown nil))
                  (dolist (choice choices)
                    (let* ((val (cdr choice))
                           (id (and (stringp val) (downcase val)))
                           (key-hint (cond
                                      ((eq val :allow-all) "a")
                                      ((and (not always-shown) id
                                            (string-match-p "allow_always\\|always" id))
                                       (setq always-shown t)
                                       "w")
                                      ((and (not allow-shown) id
                                            (string-match-p "allow\\|yes\\|run" id))
                                       (setq allow-shown t)
                                       "y")
                                      ((and (not deny-shown) id
                                            (string-match-p "deny\\|no\\|reject" id))
                                       (setq deny-shown t)
                                       "n")
                                      (t nil))))
                    (insert-button (concat "[" (car choice) "]")
                                   'action (let ((v (cdr choice)))
                                             (lambda (_b)
                                               (setq result v)
                                               (exit-recursive-edit)))
                                   'follow-link t)
                    (when key-hint
                      (insert (propertize (format " [%s]" key-hint) 'face 'shadow)))
                    (insert "  "))))
                (insert (propertize " [C-g]" 'face 'shadow) " cancel")
                (insert "\n")
                (setq buttons-end (copy-marker (point) nil)))
            (setq question-beg nil content-beg nil buttons-beg nil))))
      (if (not buttons-beg)
          (setq result (emagent-tools--buttons-prompt question choices buf))
        (when-let ((win (get-buffer-window buf)))
          (with-selected-window win
            (when (and buttons-end (marker-position buttons-end))
              (goto-char (marker-position buttons-end))
              (recenter -3))))
        (unwind-protect
            (condition-case nil
                (let ((overriding-local-map key-map))
                  (recursive-edit))
              (quit nil))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (emagent-chat--writable)
              (when (and question-beg question-end
                         (marker-buffer question-beg)
                         (marker-buffer question-end))
                (delete-region (marker-position question-beg)
                               (marker-position question-end)))
              (when (and buttons-beg buttons-end
                         (marker-buffer buttons-beg)
                         (marker-buffer buttons-end))
                (delete-region (marker-position buttons-beg)
                               (marker-position buttons-end)))
              (when (and content-beg content-end
                         (marker-buffer content-beg)
                         (marker-buffer content-end))
                (delete-region (marker-position content-beg)
                               (marker-position content-end)))
              (when-let ((stream (emagent-chat--reasoning-stream-marker)))
                (setq emagent-chat--thought-marker stream))))))
      result)))

(defun emagent-chat-append-assistant (text)
  "Append TEXT to the current emagent response section."
  (when (not (string-empty-p text))
    (emagent-chat--with-stable-view
      (with-current-buffer (current-buffer)
        (when (emagent-chat--open-response-p)
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat--ensure-reasoning-end-quote)
            (emagent-chat-close-thought)
            (save-excursion
              (emagent-chat--goto-active-response-point)
              (insert text)
              (setq emagent-chat--assistant-marker (point-marker)))
            (font-lock-flush)))))))

(defun emagent-chat-fail-assistant (message)
  "Close the in-flight emagent response with error MESSAGE."
  (with-current-buffer (current-buffer)
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (when (emagent-chat--fail-response-p)
        (emagent-chat--goto-response-insertion-point)
        (insert (format "\n\n*Error:* %s\n" message))
        (emagent-chat--insert-response-end)
        (emagent-chat--reset-response-state)
        (emagent-chat--sync-user-zone-marker)
        (font-lock-flush)
        (emagent-chat--flush-pending-prompt)))))

(defun emagent-chat--inject-reasoning-thought (thought-text)
  "Prepend THOUGHT-TEXT inside the open Reasoning block when it was not streamed."
  (let ((trimmed (string-trim (or thought-text ""))))
    (when (not (string-empty-p trimmed))
      (when-let ((beg (emagent-chat--open-reasoning-begin)))
        (save-excursion
          (goto-char beg)
          (forward-line 1)
          (insert trimmed)
          (unless (bolp) (insert "\n")))))))

(defun emagent-chat--finalize-streamed-assistant (converted)
  "Insert CONVERTED assistant text after the streamed Reasoning block."
  (when-let* ((bounds (emagent-chat--open-response-body-bounds))
              (body-end (cdr bounds))
              (insert-at (or (and emagent-chat--assistant-marker
                                   (marker-position emagent-chat--assistant-marker))
                             (emagent-chat--reasoning-block-tail)
                             (car bounds))))
    (when (< insert-at body-end)
      (delete-region insert-at body-end))
    (goto-char insert-at)
    (let ((start (point)))
      (insert converted)
      (when (string-match-p "|" converted)
        (ignore-errors
          (emagent-chat--align-org-tables-in-region start (point))))
      (setq emagent-chat--assistant-marker (point-marker)))))

(defun emagent-chat-finish-assistant (text &optional thought-text)
  "Finalize the latest emagent response.

When reasoning and tool lines were streamed live, keep that block and only
render the assistant body.  Otherwise build the Reasoning block from
THOUGHT-TEXT."
  (emagent-chat--with-stable-view
    (with-current-buffer (current-buffer)
      (let ((inhibit-read-only t)
            (converted (emagent-chat--convert-agent-markup text))
            (rendered nil)
            (hide-at nil))
        (emagent-chat--writable)
        (if-let ((reasoning-beg (emagent-chat--open-reasoning-begin)))
            (progn
              (setq hide-at reasoning-beg)
              (emagent-chat--ensure-reasoning-end-quote)
              (unless emagent-chat--reasoning-streamed-p
                (emagent-chat--inject-reasoning-thought thought-text))
              (emagent-chat-close-thought)
              (emagent-chat--finalize-streamed-assistant converted)
              (setq rendered t))
          (progn
            (emagent-chat-close-thought)
            (when-let* ((bounds (emagent-chat--finish-body-bounds))
                        (body-beg (car bounds))
                        (body-end (cdr bounds))
                        (thought (emagent-chat--format-thought-block thought-text))
                        ((<= body-beg body-end)))
              (delete-region body-beg body-end)
              (goto-char body-beg)
              (let ((insert-start (point)))
                (insert thought converted)
                (setq rendered t)
                (when (not (string-empty-p thought))
                  (setq hide-at insert-start))
                (when (string-match-p "|" converted)
                  (ignore-errors
                    (emagent-chat--align-org-tables-in-region
                     (+ insert-start (length thought)) (point))))
                (setq emagent-chat--assistant-marker (point-marker))))))
        (when rendered
          (emagent-chat--insert-response-end))
        (when (and (not rendered) (emagent-chat--open-response-p))
          (emagent-chat-close-thought)
          (emagent-chat--insert-response-end))
        (emagent-chat--reset-response-state)
        (emagent-chat--sync-user-zone-marker)
        (setq emagent-chat--view-saved-point (emagent-chat--insert-user-heading-stub))
        (font-lock-flush)
        (when hide-at
          (emagent-chat--hide-reasoning-deferred hide-at))
        (emagent-chat--flush-pending-prompt)))))

(defun emagent-chat--clear-btw-indicator ()
  "Remove the btw queued indicator line from the buffer, if present."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (get-text-property (point) 'emagent-btw)
            (delete-region (line-beginning-position)
                           (min (1+ (line-end-position)) (point-max)))
          (forward-line 1))))))

(defun emagent-chat--insert-user-heading-with-text (text)
  "Insert TEXT as a complete `* username> TEXT' heading and return point after it."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (goto-char (emagent-chat--user-zone-start))
    (unless (bolp) (insert "\n"))
    (insert (emagent-chat--user-heading-prefix) text)
    (unless (= (char-before) ?\n) (insert "\n"))
    (point)))

(defun emagent-chat--flush-pending-prompt ()
  "Send the pending btw prompt if one exists.  Called after agent finishes."
  (when emagent-chat--pending-prompt
    (let ((text emagent-chat--pending-prompt))
      (setq emagent-chat--pending-prompt nil)
      (emagent-chat--clear-btw-indicator)
      (emagent-chat--refresh-mode-line)
      (when emagent-chat--on-send
        (emagent-log "btw send: %s" (emagent-log-truncate-line text 80))
        (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
          (emagent-chat--begin-response response-pos))
        (funcall emagent-chat--on-send text)))))

(defun emagent-btw (text)
  "Queue TEXT to send after the current agent response finishes (C-c b).

When the agent is idle, sends immediately.  When busy, stores TEXT and
shows a `# [btw]' indicator; it is removed and TEXT is sent automatically
when the agent finishes."
  (interactive "sBTW: ")
  (when (string-empty-p (string-trim text))
    (user-error "BTW message is empty"))
  (let ((text (format "btw, %s" (string-trim text))))
  (if (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
      (progn
        (setq emagent-chat--pending-prompt text)
        (let ((inhibit-read-only t))
          (emagent-chat--writable)
          (emagent-chat--clear-btw-indicator)
          (save-excursion
            (goto-char (emagent-chat--user-zone-start))
            (unless (bolp) (insert "\n"))
            (insert (propertize (format "# [btw] %s\n" text)
                                'face 'shadow
                                'emagent-btw t))))
        (emagent-chat--refresh-mode-line)
        (message "emagent: btw queued — will send when agent finishes"))
    ;; Agent is idle: send immediately.
    (emagent-log "btw send: %s" (emagent-log-truncate-line text 80))
    (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
      (emagent-chat--begin-response response-pos))
    (when emagent-chat--on-send
      (funcall emagent-chat--on-send text)))))

(defun emagent-chat-send ()
  "Send region or line at point to the agent (C-c C-c).

With an active region, send the selection.  Otherwise send the current
line (or nearest preceding sendable line in the user zone).  The text is
formatted as a '* username> ' org heading in the buffer; the heading
prefix is stripped before the text is sent to the agent.

If the text starts with /btw, the remainder is queued and sent after
the current agent response finishes.  An empty /btw opens a minibuffer."
  (interactive)
  (let* ((bounds (emagent-chat--send-bounds))
         (raw (string-trim (buffer-substring-no-properties
                            (car bounds) (cdr bounds)))))
    (when (string-empty-p raw)
      (user-error "No sendable text at point"))
    (let* ((response-pos (emagent-chat--format-as-user-heading bounds raw))
           (input (string-trim (emagent-chat--strip-user-heading raw))))
      (emagent-chat--delete-following-response response-pos)
      (emagent-log "send: %s" (emagent-log-truncate-line input 80))
      (emagent-chat--begin-response response-pos)
      (when emagent-chat--on-send
        (funcall emagent-chat--on-send input)))))

(declare-function emagent-acp-interrupt "emagent-acp")

(defun emagent-chat-interrupt ()
  "Interrupt the running agent response (C-g C-g).

When the agent is busy, closes the response block with a stop notice and
returns the session to idle.  When idle, falls through to `keyboard-quit'."
  (interactive)
  (if (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
      (progn
        (emagent-acp-interrupt)
        (message "emagent: interrupted"))
    (keyboard-quit)))

(defun emagent-chat-new-prompt ()
  "Insert a '* username>' heading at point for a new prompt (C-c C-n).

Useful when the heading stub was accidentally deleted.  If point is
above the user zone, jumps to the end of the buffer first."
  (interactive)
  (let ((inhibit-read-only t)
        (zone-start (emagent-chat--user-zone-start)))
    (when (< (point) zone-start)
      (goto-char (point-max)))
    (emagent-chat--writable)
    (unless (bolp) (insert "\n"))
    (insert (emagent-chat--user-heading-prefix))))

(defun emagent-chat-attach-buffer ()
  "Attach a buffer summary to the next prompt."
  (interactive)
  (let ((text (emagent-context-buffer-summary)))
    (emagent-log "attached buffer summary to next prompt")
    (when emagent-chat--on-attach
      (funcall emagent-chat--on-attach text))))

(defun emagent-chat-yank (&optional arg)
  "Yank text or paste a clipboard image (C-y).

If the clipboard contains an image, saves it to a temp file under
`emagent-chat--image-dir' and inserts a [[file:...]] org link at point.
Otherwise behaves exactly like `yank' (ARG is forwarded)."
  (interactive "*P")
  (let ((clip (emagent-chat--clipboard-image)))
    (if clip
        (let ((file (emagent-chat--save-clipboard-image (car clip) (cdr clip))))
          (insert (format "[[file:%s]]" file))
          (message "emagent: clipboard image → %s" (file-name-nondirectory file)))
      (yank arg))))

(defvar emagent-chat--image-dir
  (expand-file-name "emagent/images" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory where clipboard images pasted into emagent buffers are saved.")

(defun emagent-chat--ensure-image-dir ()
  "Ensure `emagent-chat--image-dir' exists and return its path."
  (unless (file-directory-p emagent-chat--image-dir)
    (make-directory emagent-chat--image-dir t))
  emagent-chat--image-dir)

(defun emagent-chat--clipboard-image ()
  "Return (MIME-TYPE-STRING . RAW-BYTES) for a clipboard image, or nil.

Tries PNG, JPEG, GIF, WebP in order and returns the first available type."
  (when (fboundp 'gui-get-selection)
    (let ((targets (ignore-errors (gui-get-selection 'CLIPBOARD 'TARGETS))))
      (when targets
        (let ((target-list (cond ((vectorp targets) (append targets nil))
                                 ((listp targets)   targets)
                                 (t                 nil))))
          (cl-some
           (lambda (mime)
             (when (memq (intern mime) target-list)
               (let ((data (ignore-errors (gui-get-selection 'CLIPBOARD (intern mime)))))
                 (when (and data (not (equal data "")))
                   (cons mime data)))))
           '("image/png" "image/jpeg" "image/gif" "image/webp")))))))

(defun emagent-chat--save-clipboard-image (mime data)
  "Write clipboard image DATA (raw bytes) of MIME type to a temp file.
Returns the file path."
  (let* ((ext (pcase mime
                ("image/jpeg" "jpg")
                ("image/gif"  "gif")
                ("image/webp" "webp")
                (_            "png")))
         (file (expand-file-name
                (format "img-%s.%s" (format-time-string "%Y%m%d-%H%M%S") ext)
                (emagent-chat--ensure-image-dir))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (write-region (point-min) (point-max) file nil 'silent))
    file))

(defun emagent-chat-attach-image ()
  "Insert an image link at point for the next prompt (C-c i).

If the clipboard contains an image, saves it to a temp file under
`emagent-chat--image-dir' and inserts a [[file:...]] org link at point.
Otherwise opens a file picker.

On C-c C-c emagent finds all [[file:...]] image links in the heading,
base64-encodes them, and sends them as multimodal content blocks alongside
the prompt text."
  (interactive)
  (let ((clip (emagent-chat--clipboard-image)))
    (if clip
        (let ((file (emagent-chat--save-clipboard-image (car clip) (cdr clip))))
          (insert (format "[[file:%s]]" file))
          (message "emagent: clipboard image → %s (C-c C-c to send)"
                   (file-name-nondirectory file)))
      (let ((file (expand-file-name
                   (read-file-name "Attach image: " nil nil t))))
        (insert (format "[[file:%s]]" file))
        (message "emagent: %s attached (C-c C-c to send)"
                 (file-name-nondirectory file))))))

(defun emagent-chat-quit ()
  "Disconnect this buffer's ACP agent and bury the window."
  (interactive)
  (when emagent-chat--on-quit
    (funcall emagent-chat--on-quit))
  (bury-buffer))

(defun emagent-chat--ensure-org-startup ()
  "Ensure the buffer requests Org block folding on startup."
  (unless (save-excursion
            (goto-char (point-min))
            (re-search-forward "^#\\+STARTUP:.*\\bhideblocks\\b"
                               (emagent-chat--metadata-end) t))
    (emagent-chat--write-top-property "STARTUP" "hideblocks")))

(defun emagent-chat--setup-faces ()
  "Configure org highlighting, line wrap, and block folding for emagent buffers."
  (setq-local org-src-fontify-natively t
              org-ellipsis "…"
              org-fontify-quote-and-verse-blocks t
              org-cycle-hide-block-startup t
              org-startup-truncated nil
              truncate-lines nil)
  (when (boundp 'word-wrap)
    (setq-local word-wrap t))
  (visual-line-mode 1)
  ;; org-phscroll: horizontal scroll for wide tables while prose wraps.
  (when (fboundp 'org-phscroll-mode)
    (org-phscroll-mode 1)))

(defun emagent-chat--setup-faces-deferred ()
  "Re-apply `emagent-chat--setup-faces' after org startup hooks finish."
  (when (derived-mode-p 'emagent-mode)
    (emagent-chat--setup-faces)))

;;;###autoload
(define-derived-mode emagent-mode org-mode "Emagent"
  "Major mode for emagent chat scratch buffers.

Derived from `org-mode'.  Type naturally, then \\[emagent-chat-send] to send
the line at point.  Select a region first to send multiline text.
On a slash-command line (plugin skills such as /workflow:dev), \\[emagent-chat-tab]
completes available commands.  Agent responses are inserted between
# --- emagent --- delimiter lines (TAB on that line folds the response).

Run \\[emagent-mode] to reconnect a saved session."
  (require 'emagent)
  (setq-local buffer-read-only nil)
  (emagent-chat--writable)
  (setq emagent-chat-project-directory
        (or emagent-chat-project-directory (emagent-chat--read-project-property))
        emagent-chat-session-id (or emagent-chat-session-id
                                    (emagent-chat--read-session-property))
        emagent-chat-provider (emagent-chat-agent)
        emagent-chat-allowed-tools (or emagent-chat-allowed-tools
                                       (emagent-chat--read-allowed-tools-property)))
  (when-let ((model (or emagent-chat-model (emagent-chat--read-model-property))))
    (emagent-chat-set-model model))
  (setq-local default-directory (emagent-chat--session-directory))
  (if (bound-and-true-p doom-modeline-mode)
      (emagent-chat--setup-doom-modeline)
    (setq-local mode-line-format (list "" 'emagent-mode-line "")))
  (org-indent-mode -1)
  (when-let ((dir (emagent-chat-project-directory)))
    (rename-buffer (emagent-chat--buffer-name-for-label
                    (emagent-chat--short-cwd-label dir))
                   t))
  (emagent-chat--insert-initial-comment)
  (emagent-chat--ensure-org-startup)
  (emagent-chat--sync-user-zone-marker)
  (add-hook 'completion-at-point-functions
            #'emagent-chat-slash-command-completion-at-point -90 t)
  (setq-local imenu-create-index-function #'emagent-chat--imenu-create-index)
  (add-hook 'font-lock-after-fontify-region-hook
            #'emagent-chat--after-fontify-repair-tool-lines nil t)
  (setq-local bookmark-make-record-function #'emagent-chat--bookmark-make-record)
  (emagent-chat--setup-faces)
  (emagent-chat--mode-line-recompute)
  (run-with-idle-timer 0 nil #'emagent-chat--setup-faces-deferred))

(cl-defun emagent-chat-open (&key project-dir)
  "Open or create an emagent buffer for PROJECT-DIR.

Buffer names look like *emagent .emacs.d* from a short cwd label.
PROJECT-DIR is stored as #+EMAGENT_PROJECT and passed to the ACP agent as cwd."
  (unless project-dir
    (user-error "PROJECT-DIR is required"))
  (let* ((dir (expand-file-name project-dir))
         (label (emagent-chat--short-cwd-label dir))
         (slug (emagent-chat--sanitize-slug label))
         (buffer-name (emagent-chat--buffer-name-for-label label))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'emagent-mode)
        (emagent-mode))
      (rename-buffer buffer-name t)
      (setq emagent-chat-slug slug
            emagent-chat-session-id (or emagent-chat-session-id
                                        (emagent-chat--read-session-property)))
      (emagent-chat-set-project-directory dir))
    buffer))

;;;; Context-sensitive C-c C-c

(defun emagent-chat-send-or-babel ()
  "Send the prompt at point, or execute a src block when point is inside one.

On a `#+BEGIN_SRC ... #+END_SRC' block, delegates to
`org-babel-execute-src-block' so code blocks in agent responses are
executable without leaving `emagent-mode'.  Otherwise calls `emagent-chat-send'."
  (interactive)
  (if (org-in-src-block-p)
      (call-interactively #'org-babel-execute-src-block)
    (call-interactively #'emagent-chat-send)))

;;;; Imenu

(defun emagent-chat--imenu-create-index ()
  "Build an imenu index of conversation turns for `emagent-mode' buffers."
  (let ((user-re (emagent-chat--user-heading-re))
        index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward user-re nil t)
        (let* ((pos (line-beginning-position))
               (text (string-trim
                      (buffer-substring-no-properties pos (line-end-position))))
               (label (if (string-match user-re text)
                          (let ((rest (substring text (match-end 0))))
                            (if (string-empty-p rest)
                                (format "turn %d" (length index))
                              (truncate-string-to-width rest 60 nil nil "…")))
                        text)))
          (push (cons label pos) index)))
      (nreverse index))))

;;;; Bookmark

(defun emagent-chat--bookmark-make-record ()
  "Make a bookmark record for this emagent buffer."
  (let ((session-id (emagent-chat-session-id))
        (project-dir (emagent-chat-project-directory))
        (model (emagent-chat-model))
        (provider (when emagent-chat-provider (symbol-name emagent-chat-provider))))
    `(,(buffer-name)
      (handler . emagent-chat--bookmark-jump)
      (session-id . ,session-id)
      (project-dir . ,project-dir)
      (model . ,model)
      (provider . ,provider)
      (position . ,(point)))))

(defun emagent-chat--bookmark-jump (bookmark)
  "Jump to an emagent BOOKMARK, reopening or reconnecting the session."
  (let* ((session-id (bookmark-prop-get bookmark 'session-id))
         (project-dir (bookmark-prop-get bookmark 'project-dir))
         (model (bookmark-prop-get bookmark 'model))
         (provider (when-let ((p (bookmark-prop-get bookmark 'provider)))
                     (intern p)))
         (pos (bookmark-prop-get bookmark 'position))
         (buffer (when project-dir
                   (emagent-chat-open :project-dir project-dir))))
    (when buffer
      (with-current-buffer buffer
        (when model (emagent-chat-set-model model))
        (when provider (emagent-chat-set-agent provider))
        (when session-id (emagent-chat-set-session-id session-id)))
      (pop-to-buffer buffer)
      (when pos (goto-char pos)))))

;;;; Error context attachment

(defun emagent-chat--compilation-error-lines ()
  "Return error lines from *compilation* buffer using text properties, or nil."
  (when-let ((buf (get-buffer "*compilation*")))
    (with-current-buffer buf
      (let (lines)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (get-text-property (point) 'compilation-message)
              (let ((text (string-trim
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))
                (unless (string-empty-p text)
                  (push text lines))))
            (forward-line 1)))
        (nreverse lines)))))

(defun emagent-chat--flymake-error-lines ()
  "Return flymake diagnostic lines from all open file-visiting buffers."
  (when (fboundp 'flymake-diagnostics)
    (let (lines)
      (dolist (buf (buffer-list))
        (when (and (buffer-file-name buf)
                   (buffer-local-value 'flymake-mode buf))
          (let ((diags (with-current-buffer buf (flymake-diagnostics))))
            (dolist (d diags)
              (push (format "%s:%s [%s] %s"
                            (abbreviate-file-name (buffer-file-name buf))
                            (with-current-buffer buf
                              (line-number-at-pos
                               (flymake-diagnostic-beg d)))
                            (flymake-diagnostic-type d)
                            (flymake-diagnostic-text d))
                    lines)))))
      (nreverse lines))))

(defun emagent-chat-attach-error-context ()
  "Attach compilation errors and flymake diagnostics to the next prompt.

Scans `*compilation*' for error lines and all open file buffers for
active flymake diagnostics.  Attaches a combined error context block."
  (interactive)
  (let* ((compile-lines (emagent-chat--compilation-error-lines))
         (flymake-lines (emagent-chat--flymake-error-lines))
         (all (append compile-lines flymake-lines)))
    (if all
        (let ((text (concat "[Error context]\n" (string-join all "\n"))))
          (emagent-log "attached %d error(s) to next prompt" (length all))
          (when emagent-chat--on-attach
            (funcall emagent-chat--on-attach text)))
      (message "emagent: no errors found in compilation buffer or flymake"))))

;;;; File attachment (pick from project)

(defun emagent-chat-attach-files ()
  "Pick project files and attach summaries to the next prompt.

Presents `completing-read-multiple' over files under
`emagent-chat-project-directory'.  For each chosen file includes its
relative path, size in lines, and a short content preview."
  (interactive)
  (let* ((root (or (emagent-chat-project-directory)
                   default-directory))
         (all-files (directory-files-recursively root "[^.].*" nil t))
         (rel-files (seq-filter
                     (lambda (f)
                       (not (string-match-p "/\\.git/" f)))
                     (mapcar (lambda (f) (file-relative-name f root))
                             all-files)))
         (chosen (completing-read-multiple
                  "Attach files (comma-separated): " rel-files nil t))
         (blocks
          (mapcar (lambda (rel)
                    (let* ((abs (expand-file-name rel root))
                           (size (and (file-exists-p abs)
                                      (with-temp-buffer
                                        (insert-file-contents abs nil 0 4096)
                                        (count-lines (point-min) (point-max))))))
                      (format "[File: %s (%s lines)]\n%s"
                              rel (or size "?")
                              (condition-case nil
                                  (with-temp-buffer
                                    (insert-file-contents abs nil 0 2048)
                                    (buffer-string))
                                (error "(unreadable)")))))
                  chosen)))
    (if blocks
        (let ((text (string-join blocks "\n\n")))
          (emagent-log "attached %d file(s) to next prompt" (length blocks))
          (when emagent-chat--on-attach
            (funcall emagent-chat--on-attach text)))
      (message "emagent: no files selected"))))

;;;; Response extraction

(defun emagent-chat--last-response-bounds ()
  "Return (BEG . END) for the last completed response body, or nil."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward emagent-chat--response-end-re nil t)
      (let ((end (match-beginning 0)))
        (when (re-search-backward emagent-chat--response-begin-re nil t)
          (forward-line 1)
          (skip-chars-forward "\n")
          (cons (point) end))))))

(defun emagent-chat--collect-src-blocks (beg end)
  "Return list of (LANG . CODE) for each src block between BEG and END."
  (let (blocks)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^#\\+BEGIN_SRC \\(.*\\)\n" end t)
        (let* ((lang (string-trim (match-string 1)))
               (start (point))
               (block-end (and (re-search-forward "^#\\+END_SRC\\s-*$" end t)
                               (match-beginning 0))))
          (when block-end
            (push (cons lang (buffer-substring-no-properties start block-end))
                  blocks)))))
    (nreverse blocks)))

(defun emagent-chat-insert-last-response ()
  "Insert the last completed agent response into another buffer.

Prompts for a target buffer with `completing-read'."
  (interactive)
  (if-let* ((bounds (emagent-chat--last-response-bounds))
            (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (let* ((others (seq-filter (lambda (b) (not (eq b (current-buffer))))
                                 (buffer-list)))
             (choice (completing-read "Insert response into buffer: "
                                      (mapcar #'buffer-name others) nil t))
             (target (get-buffer choice)))
        (with-current-buffer target
          (insert text))
        (message "emagent: inserted response into %s" choice))
    (message "emagent: no completed response found")))

(defun emagent-chat-insert-src-block ()
  "Pick a src block from the last response and insert it into another buffer."
  (interactive)
  (if-let* ((bounds (emagent-chat--last-response-bounds))
            (blocks (emagent-chat--collect-src-blocks (car bounds) (cdr bounds))))
      (let* ((choices
              (cl-loop for (lang . code) in blocks
                       for i from 1
                       collect
                       (cons (format "%d [%s] %s" i lang
                                     (truncate-string-to-width
                                      (car (split-string code "\n")) 60 nil nil "…"))
                             code)))
             (pick (completing-read "Insert src block: "
                                    (mapcar #'car choices) nil t))
             (code (cdr (assoc pick choices)))
             (others (seq-filter (lambda (b) (not (eq b (current-buffer))))
                                 (buffer-list)))
             (target (get-buffer
                      (completing-read "Into buffer: "
                                       (mapcar #'buffer-name others) nil t))))
        (with-current-buffer target
          (insert code))
        (message "emagent: inserted src block into %s" (buffer-name target)))
    (message "emagent: no src blocks found in last response")))

;;;; Transient command palette

(declare-function emagent--transient-menu "emagent-chat")

(defun emagent-dispatch ()
  "Show the emagent command palette."
  (interactive)
  (if (fboundp 'transient-define-prefix)
      (progn
        (unless (fboundp 'emagent--transient-menu)
          (eval
           '(transient-define-prefix emagent--transient-menu ()
              "Emagent commands."
              ["Send & navigate"
               ("SPC" "Send / execute src block" emagent-chat-send-or-babel)
               ("p" "New prompt heading" emagent-chat-new-prompt)
               ("g" "Interrupt agent (C-g C-g)" emagent-chat-interrupt)]
              ["Attach"
               ("a" "Attach buffer context" emagent-chat-attach-buffer)
               ("b" "Queue btw message for after agent" emagent-btw)
               ("d" "Attach project files" emagent-chat-attach-files)
               ("e" "Attach error context" emagent-chat-attach-error-context)
               ("i" "Attach image" emagent-chat-attach-image)]
              ["Extract response"
               ("r" "Insert last response into buffer" emagent-chat-insert-last-response)
               ("s" "Insert src block into buffer" emagent-chat-insert-src-block)]
              ["Session"
               ("m" "Set model" emagent-set-model)
               ("t" "Trust workspace on disk" emagent-trust-workspace)
               ("R" "Claude: new session (trust)" emagent-trust-claude-reconnect)
               ("l" "View log" emagent-log-view)])
           t))
        (call-interactively #'emagent--transient-menu))
    (message "emagent: SPC=send, p=prompt, g=interrupt, a=attach, i=image, m=model, t=trust, R=reconnect, l=log")))

(add-hook 'emagent-mode-hook #'emagent-chat--setup-faces 100 t)
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'emagent-mode)
      (emagent-chat--setup-faces))))

(provide 'emagent-chat)

;;; emagent-chat.el ends here
