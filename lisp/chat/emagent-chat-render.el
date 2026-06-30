;;; emagent-chat-render.el --- render module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'map)
(require 'emagent-log)
(require 'emagent-chat-header)
(require 'emagent-chat-markup)
(require 'emagent-chat-reasoning)
(require 'emagent-tools)

(defun emagent-chat--begin-response (&optional at)
  "Open a new emagent response at AT or point.

Set up the response body markers but do not insert a `** Thinking'
subsection yet.  Thinking is created lazily, only when reasoning or a tool
line actually arrives, so responses without reasoning never show an empty
Thinking block."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    (goto-char (or at (point)))
    (unless (bolp)
      (insert "\n"))
    (insert "\n")
    (setq emagent-chat--response-body-start (copy-marker (point) nil)
          emagent-chat--assistant-marker (copy-marker (point) nil)
          emagent-chat--thought-marker nil
          emagent-chat--thought-open-p nil
          emagent-chat--reasoning-streamed-p nil)))

(defun emagent-chat-insert-system (message)
  "Append system MESSAGE to `emagent-log-buffer-name'."
  (emagent-log "%s" message))

(defun emagent-chat-start-assistant ()
  "Begin a new emagent response section."
  (with-current-buffer (current-buffer)
    (emagent-chat--begin-response)))

(declare-function emagent-chat--notify-inactive-update "emagent-chat")

(defun emagent-chat--format-thought-block (text)
  "Return `** Thinking' subsection markup for reasoning TEXT, or \"\" when empty."
  (let ((trimmed (string-trim (or text ""))))
    (if (string-empty-p trimmed)
        ""
      (format "%s\n%s\n\n"
              emagent-chat-thinking-headline
              (emagent-chat--escape-reasoning-text trimmed)))))

(defun emagent-chat--reasoning-block-bounds ()
  "Return (CONTENT-START . CONTENT-END) for the `** Thinking' subsection at point."
  (save-excursion
    (unless (looking-at emagent-chat--thinking-headline-re)
      (re-search-backward emagent-chat--thinking-headline-re nil t))
    (beginning-of-line)
    (when (looking-at emagent-chat--thinking-headline-re)
      (let ((content-start (line-end-position))
            (content-end (emagent-chat--thinking-content-end
                          (line-end-position) (point-max))))
        (when (> content-end content-start)
          (cons content-start content-end))))))

(defun emagent-chat--hide-reasoning-by-region (bounds)
  "Hide Reasoning content between BOUNDS using `org-fold-region'."
  (when bounds
    (ignore-errors
      (org-fold-region (car bounds) (cdr bounds) t 'block))))

(defun emagent-chat--hide-reasoning-at-point ()
  "Fold the `** Thinking' subsection at or near point.

Hides the subsection body with `org-fold-hide-subtree', leaving its headline
visible as a collapsed summary.  Falls back to folding the inner region only
so incomplete parses never break the buffer."
  (when-let ((bounds (emagent-chat--reasoning-block-bounds)))
    ;; Fontify the body synchronously before folding.  jit-lock skips text
    ;; that is already invisible, so folding an unfontified subsection (as
    ;; happens on interrupt, where finalize and fold run back-to-back with no
    ;; intervening redisplay) leaves the collapsed Thinking line unrendered
    ;; until a manual fold/unfold.
    (ignore-errors
      (font-lock-ensure (save-excursion (goto-char (car bounds))
                                        (line-beginning-position))
                        (cdr bounds)))
    (condition-case _
        (save-excursion
          (goto-char (car bounds))
          (beginning-of-line)
          (re-search-backward emagent-chat--thinking-headline-re nil t)
          (require 'org-fold)
          (org-fold-hide-subtree))
      (error
       (emagent-chat--hide-reasoning-by-region bounds)))))

(defun emagent-chat--hide-reasoning-deferred (&optional pos)
  "Hide the Reasoning block near POS after the next redisplay.

Skips hiding while the ACP session is busy (tool call in progress) so
that non-blocking shell waits — which allow idle timers to fire — do not
fold the block prematurely.  `emagent-chat-finish-assistant' re-schedules
the hide when the response is fully complete and the session is idle."
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
               (unless (and (fboundp 'emagent-acp-busy-p) (emagent-acp-busy-p))
                 (save-excursion
                   (goto-char at)
                   (emagent-chat--hide-reasoning-at-point)))))))))))








(defun emagent-chat--goto-response-insertion-point ()
  "Go to the tail of the open emagent response at or before point."
  (cond
   ((and emagent-chat--assistant-marker
         (marker-position emagent-chat--assistant-marker))
    (goto-char emagent-chat--assistant-marker))
   (t
    (goto-char (point-max)))))

(defun emagent-chat--finish-response-spacing ()
  "Ensure a trailing blank line after a finalized response body."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (unless (save-excursion (forward-line -1) (looking-at-p "[ \t]*$"))
    (insert "\n")))

(defun emagent-chat--reset-response-state ()
  (emagent-chat--cancel-thought-flush)
  (setq emagent-chat--assistant-marker nil
        emagent-chat--response-body-start nil
        emagent-chat--thought-open-p nil
        emagent-chat--thought-marker nil
        emagent-chat--reasoning-streamed-p nil)
  (clrhash emagent-chat--tool-call-lines))

(defun emagent-chat--cancel-thought-flush ()
  "Discard pending reasoning chunks and cancel any flush timer."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer)
    (setq emagent-chat--thought-flush-timer nil))
  (setq emagent-chat--thought-pending ""))

(defun emagent-chat--schedule-thought-flush ()
  "Debounce reasoning inserts using `emagent-chat-thought-stream-delay'."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer))
  (setq emagent-chat--thought-flush-timer
        (run-with-timer emagent-chat-thought-stream-delay nil
                        (lambda ()
                          (setq emagent-chat--thought-flush-timer nil)
                          (emagent-chat--flush-thought-pending)))))

(defun emagent-chat--insert-thought-now (text)
  "Insert reasoning TEXT at the open Thinking marker."
  (when (and (not (string-empty-p text))
             (emagent-chat--open-response-p))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (emagent-chat--ensure-response-markers)
      (emagent-chat--ensure-thought-stream)
      (when (and emagent-chat--thought-marker
                 (marker-position emagent-chat--thought-marker))
        (let ((inhibit-modification-hooks t))
          (save-excursion
            (goto-char emagent-chat--thought-marker)
            (emagent-chat--insert-reasoning-text text)
            (setq emagent-chat--thought-marker (point-marker)
                  emagent-chat--assistant-marker (point-marker)
                  emagent-chat--reasoning-streamed-p t)))))))

(defun emagent-chat--flush-thought-pending ()
  "Insert any batched reasoning text into the open Thinking block."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer)
    (setq emagent-chat--thought-flush-timer nil))
  (let ((text emagent-chat--thought-pending))
    (setq emagent-chat--thought-pending "")
    (when (not (string-empty-p text))
      (emagent-chat--with-streaming-view
       (lambda ()
         (emagent-chat--insert-thought-now text))))))

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
  "Return non-nil when an emagent response is open and can be closed with error."
  (emagent-chat--open-response-p))

(defun emagent-chat-begin-thought ()
  "Resume or open the Thinking block in the in-flight emagent response."
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (when (emagent-chat--open-response-p)
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat--ensure-response-markers)
            (emagent-chat--ensure-reasoning-scaffold)))))))

(defun emagent-chat-append-thought (text)
  "Append reasoning TEXT to the open Reasoning block."
  (when (not (string-empty-p text))
    (setq emagent-chat--thought-pending
          (concat emagent-chat--thought-pending text))
    (if (or noninteractive (<= emagent-chat-thought-stream-delay 0))
        (emagent-chat--flush-thought-pending)
      (emagent-chat--schedule-thought-flush))))

(defun emagent-chat-close-thought ()
  "Close the open `** Thinking' subsection, if any, and schedule folding."
  (emagent-chat--flush-thought-pending)
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (when emagent-chat--thought-open-p
          (let ((inhibit-read-only t)
                (hide-at (emagent-chat--open-reasoning-begin)))
            (emagent-chat--writable)
            (when-let ((tail (emagent-chat--reasoning-block-tail)))
              (setq emagent-chat--assistant-marker (copy-marker tail nil)))
            (setq emagent-chat--thought-open-p nil
                  emagent-chat--thought-marker nil)
            (emagent-chat--maybe-font-lock-flush)
            (when hide-at
              (emagent-chat--hide-reasoning-deferred hide-at))))))))

(defun emagent-chat--finish-tool-line-in-reasoning ()
  "Leave `emagent-chat--thought-marker' on a fresh line after a tool line."
  (goto-char (line-end-position))
  (insert "\n")
  (setq emagent-chat--thought-marker (copy-marker (point) nil)))

(defun emagent-chat--insert-reasoning-text (text)
  "Insert TEXT at `emagent-chat--thought-marker' in the open Thinking content."
  (let ((safe (emagent-chat--escape-reasoning-text text)))
    ;; Drop a leading blank line right after the `** Thinking' headline.
    (when (save-excursion
            (beginning-of-line)
            (and (looking-at "[ \t]*$")
                 (progn (forward-line -1) t)
                 (looking-at emagent-chat--thinking-headline-re)))
      (setq safe (replace-regexp-in-string "\\`[\n\r]+" "" safe)))
    ;; When point sits at the end of a tool line, start reasoning on a new line.
    (when (and (eolp)
               (save-excursion (beginning-of-line) (looking-at "→ ")))
      (setq safe (concat "\n" safe)))
    (insert safe)))

(defun emagent-chat--org-verbatim-paths (text)
  "Wrap absolute paths in org =verbatim= so /Users/ is not parsed as /italic/."
  (replace-regexp-in-string "\\(/[^ \t\n]+\\)" "=\\1=" text))

(defun emagent-chat--format-tool-line (label)
  "Return a Thinking-block tool line for LABEL, safe in org-mode."
  (format "→ %s" (emagent-chat--org-verbatim-paths label)))

(defun emagent-chat--format-permission-line (question)
  "Return a permission question line for QUESTION."
  (format "? %s" (emagent-chat--org-verbatim-paths question)))

(defun emagent-chat--permission-content-block (tool-call)
  "Return org subsection markup for TOOL-CALL, or nil."
  (when (and tool-call (fboundp 'emagent-acp--tool-call-content-block))
    (emagent-acp--tool-call-content-block tool-call)))

(defun emagent-chat--insert-permission-newline-if-needed ()
  "Insert a separating newline unless point already starts a fresh line."
  (unless (bolp)
    (insert "\n")))

(defconst emagent-chat--tool-decision-re
  " \\((Allow: [^)\n]+)\\|(Allow)\\|(Denied)\\)$"
  "Regexp matching the permission decision suffix on a tool-call line.")

(defun emagent-chat--repair-tool-line-faces (start end)
  "Re-apply path and decision faces after org font-lock on tool-call lines."
  (when (and start end (< start end))
    (with-silent-modifications
      (save-excursion
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward "\\=/[^ \t\n]+" end t))
          (let ((s (match-beginning 0))
                (e (match-end 0)))
            (remove-list-of-text-properties s e '(face))
            (put-text-property s e 'face 'emagent-tool-detail)))
        (goto-char start)
        (when (re-search-forward emagent-chat--tool-decision-re end t)
          (let ((s (match-beginning 1))
                (e (match-end 1)))
            (remove-list-of-text-properties s e '(face))
            (put-text-property s e 'face 'emagent-tool-permission-decision)))))))

(defun emagent-chat--after-fontify-repair-tool-lines (beg end)
  "Repair org /italic/ on tool-call lines after each font-lock pass."
  (when (emagent-chat--buffer-active-p)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^→ " end t)
        (emagent-chat--repair-tool-line-faces (line-beginning-position)
                                               (line-end-position))))))

(defun emagent-chat--fontify-tool-line (start end)
  "Font-lock tool line START..END and repair org emphasis on paths."
  (when (and start end (<= start end))
    (emagent-chat--maybe-font-lock-flush)
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
     (lambda ()
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
                   (emagent-chat--finish-tool-line-in-reasoning)))))))))))

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

(defun emagent-chat-permission-prompt (question choices callback &optional tool-call)
  "Show permission UI at the end of the open `** Thinking' subsection.

When TOOL-CALL carries a shell command or edit payload, inserts that content,
then CHOICES as buttons.  Otherwise inserts a ? question line before the
buttons.

CHOICES is a list of (LABEL . VALUE) pairs.  Non-blocking: inserts the dialog
and returns immediately.  CALLBACK is called with the chosen VALUE when a
button is clicked.

Keyboard shortcuts (via keymap text property on the buttons line):
  y / RET  — Allow once    s — Allow for session
  w        — Allow always  a — Allow all (session)
  n        — Deny"
  (when (emagent-chat--open-response-p)
    (let ((buf (current-buffer))
          (content-block (emagent-chat--permission-content-block tool-call))
          (responded nil)
          btn-keymap
          question-beg question-end
          content-beg content-end
          buttons-beg buttons-end)
      (let ((cleanup
             (lambda ()
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (emagent-chat--writable)
                   (when (and question-beg question-end
                              (marker-buffer question-beg) (marker-buffer question-end))
                     (delete-region (marker-position question-beg) (marker-position question-end)))
                   (when (and buttons-beg buttons-end
                              (marker-buffer buttons-beg) (marker-buffer buttons-end))
                     (delete-region (marker-position buttons-beg) (marker-position buttons-end)))
                   (when (and content-beg content-end
                              (marker-buffer content-beg) (marker-buffer content-end))
                     (delete-region (marker-position content-beg) (marker-position content-end)))
                   (when-let ((stream (emagent-chat--reasoning-stream-marker)))
                     (setq emagent-chat--thought-marker stream)))))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat--ensure-response-markers)
            (emagent-chat--ensure-reasoning-scaffold)
            (if-let ((insert-at (emagent-chat--reasoning-block-tail)))
                (progn
                  (goto-char insert-at)
                  (when content-block
                    (setq content-beg (copy-marker (point) nil))
                    (emagent-chat--insert-permission-newline-if-needed)
                    (insert content-block "\n")
                    (setq content-end (copy-marker (point) nil)))
                  (goto-char (or (and content-end (marker-position content-end))
                                 insert-at))
                  (unless content-block
                    (setq question-beg (copy-marker (point) nil))
                    (emagent-chat--insert-permission-newline-if-needed)
                    (insert (emagent-chat--format-permission-line question))
                    (put-text-property (marker-position question-beg) (point)
                                       'face 'emagent-permission-prompt)
                    (emagent-chat--repair-tool-line-faces (marker-position question-beg) (point))
                    (insert "\n")
                    (setq question-end (copy-marker (point) nil)))
                  (goto-char (or (and question-end (marker-position question-end))
                                 (and content-end (marker-position content-end))
                                 insert-at))
                  (setq buttons-beg (copy-marker (point) nil))
                  (emagent-chat--insert-permission-newline-if-needed)
                  (setq btn-keymap (make-sparse-keymap))
                  (set-keymap-parent btn-keymap button-map)
                  ;; Build key-hints alist and populate btn-keymap first
                  (let* ((allow-once-shown nil) (allow-always-shown nil) (deny-shown nil)
                         (hints
                          (mapcar
                           (lambda (choice)
                             (let* ((val (cdr choice))
                                    (id (and (stringp val) (downcase val)))
                                    (kh (cond
                                         ((eq val :allow-once) (setq allow-once-shown t) "y")
                                         ((eq val :allow-session) "s")
                                         ((eq val :allow-always) (setq allow-always-shown t) "w")
                                         ((eq val :allow-all) "a")
                                         ((eq val :deny) (setq deny-shown t) "n")
                                         ((and (not allow-always-shown) id
                                               (string-match-p "allow_always\\|always" id))
                                          (setq allow-always-shown t) "w")
                                         ((and (not allow-once-shown) id
                                               (string-match-p "allow\\|yes\\|run" id))
                                          (setq allow-once-shown t) "y")
                                         ((and (not deny-shown) id
                                               (string-match-p "deny\\|no\\|reject" id))
                                          (setq deny-shown t) "n")
                                         (t nil))))
                               (when kh
                                 (define-key btn-keymap (kbd kh)
                                             (let ((v val))
                                               (lambda ()
                                                 (interactive)
                                                 (unless responded
                                                   (setq responded t)
                                                   (funcall cleanup)
                                                   (funcall callback v))))))
                               kh))
                           choices)))
                    ;; Now insert buttons with btn-keymap as their keymap
                    (cl-mapc
                     (lambda (choice kh)
                       (let ((val (cdr choice)))
                         (insert-button
                          (concat "[" (car choice) "]")
                          'keymap btn-keymap
                          'action
                          (let ((v val))
                            (lambda (_b)
                              (unless responded
                                (setq responded t)
                                (funcall cleanup)
                                (funcall callback v))))
                          'follow-link t)
                         (when kh
                           (insert (propertize (format " [%s]" kh) 'face 'shadow)))
                         (insert "  ")))
                     choices hints))
                  (insert "\n")
                  (setq buttons-end (copy-marker (point) nil))
                  (when-let ((stream (emagent-chat--reasoning-stream-marker)))
                    (setq emagent-chat--thought-marker stream)))
              (setq question-beg nil content-beg nil buttons-beg nil))))
        (emagent-chat--notify-inactive-update)
        (if (not buttons-beg)
            (let ((content-block (or content-block
                                     (emagent-chat--permission-content-block tool-call)))
                  (preamble (concat
                             "\n** Request permissions\n"
                             (when content-block
                               (concat content-block "\n")))))
              (emagent-tools--buttons-prompt
               (if content-block "" question)
               choices buf callback preamble))
          (when-let ((win (get-buffer-window buf)))
            (with-selected-window win
              (when (and buttons-beg (marker-position buttons-beg))
                (goto-char (marker-position buttons-beg))
                (recenter -3)))))))))

(defun emagent-chat--response-body-bounds ()
  "Return (CONTENT-START . END) for the `** Response' body, or nil."
  (when-let ((bounds (emagent-chat--open-response-body-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (when (re-search-forward emagent-chat--response-headline-re (cdr bounds) t)
        (forward-line 1)
        (cons (point) (cdr bounds))))))

(defun emagent-chat--ensure-response-headline ()
  "Ensure the open response has a `** Response' headline; return its content start."
  (or (car (emagent-chat--response-body-bounds))
      (let ((tail (emagent-chat--reasoning-block-tail)))
        (if tail
            ;; A `** Thinking' subsection exists: place Response after its content.
            (progn
              (goto-char tail)
              (skip-chars-backward "\n" (or (emagent-chat--open-response-begin)
                                            (point-min)))
              (insert "\n\n" emagent-chat-response-headline "\n")
              (point))
          ;; No reasoning was rendered: place Response at the response body start.
          (when-let ((beg (emagent-chat--open-response-begin)))
            (goto-char beg)
            (insert emagent-chat-response-headline "\n")
            (point))))))

(defun emagent-chat-append-assistant (text)
  "Append streamed assistant TEXT under the `** Response' subsection."
  (when (not (string-empty-p text))
    (emagent-chat--with-stable-view
      (lambda ()
        (with-current-buffer (current-buffer)
          (when (emagent-chat--open-response-p)
            (let ((inhibit-read-only t))
              (emagent-chat--writable)
              (emagent-chat-close-thought)
              (let* ((existing (emagent-chat--response-body-bounds))
                     (insert-at
                      (cond
                       ((and existing
                             emagent-chat--assistant-marker
                             (marker-position emagent-chat--assistant-marker)
                             (>= (marker-position emagent-chat--assistant-marker)
                                 (car existing)))
                        (marker-position emagent-chat--assistant-marker))
                       (existing (cdr existing))
                       (t (emagent-chat--ensure-response-headline)))))
                (when insert-at
                  (save-excursion
                    (goto-char insert-at)
                    (insert text)
                    (setq emagent-chat--assistant-marker (point-marker)))))
              (emagent-chat--maybe-font-lock-flush))))))))

(defun emagent-chat-fail-assistant (message)
  "Close the in-flight emagent response with error MESSAGE under `** Response'."
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t))
          (emagent-chat--writable)
          (when (emagent-chat--fail-response-p)
            (emagent-chat-close-thought)
            (emagent-chat--ensure-response-headline)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "\n*Error:* %s\n" message))
            (emagent-chat--finish-response-spacing)
            (emagent-chat--reset-response-state)
            (emagent-chat--sync-user-zone-marker)
            (emagent-chat--maybe-font-lock-flush))))))
  (emagent-chat--insert-user-heading-stub))

(defun emagent-chat--inject-reasoning-thought (thought-text)
  "Insert THOUGHT-TEXT under `** Thinking' when reasoning was not streamed.

Create the `** Thinking' subsection on demand, since responses no longer
open one eagerly."
  (let ((trimmed (string-trim (or thought-text ""))))
    (when (not (string-empty-p trimmed))
      (unless (emagent-chat--open-reasoning-begin)
        (emagent-chat--insert-reasoning-scaffold))
      (when-let ((beg (emagent-chat--open-reasoning-begin)))
        (save-excursion
          (goto-char beg)
          (forward-line 1)
          (insert (emagent-chat--escape-reasoning-text trimmed))
          (unless (bolp) (insert "\n")))))))

(defun emagent-chat--finalize-streamed-assistant (converted)
  "Replace the `** Response' body with CONVERTED assistant text."
  (when-let ((content-start (or (car (emagent-chat--response-body-bounds))
                                (emagent-chat--ensure-response-headline))))
    (let ((body-end (cdr (emagent-chat--open-response-body-bounds))))
      (when (and body-end (< content-start body-end))
        (delete-region content-start body-end))
      (goto-char content-start)
      (let ((start (point)))
        (insert converted)
        (when (string-match-p "|" converted)
          (ignore-errors
            (emagent-chat--maybe-align-org-tables-in-region start (point))))
        (setq emagent-chat--assistant-marker (point-marker))))))

(defun emagent-chat--remove-empty-thinking ()
  "Remove the open `** Thinking' headline when its content is blank.

Scoped to the in-flight response so it never deletes a `** Thinking'
subsection that belongs to an earlier response."
  (when-let ((beg (emagent-chat--open-reasoning-begin)))
    (let ((content-start (save-excursion (goto-char beg) (line-end-position)))
          (content-end (emagent-chat--reasoning-block-tail)))
      (when (and content-end
                 (string-empty-p
                  (string-trim
                   (buffer-substring-no-properties content-start content-end))))
        (save-excursion
          (goto-char beg)
          (delete-region (line-beginning-position) content-end))
        t))))

(defun emagent-chat-finish-assistant (text &optional thought-text)
  "Finalize the latest emagent response.

Render the assistant answer under `** Response'.  When reasoning was streamed
keep its `** Thinking' subsection; otherwise build one from THOUGHT-TEXT.
A response without any reasoning has no `** Thinking' subsection at all."
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t)
              (converted (emagent-chat--demote-response-headings
                          (emagent-chat--convert-agent-markup text)))
              (hide-at nil))
          (emagent-chat--writable)
          (when (emagent-chat--open-response-p)
            (unless emagent-chat--reasoning-streamed-p
              (emagent-chat--inject-reasoning-thought thought-text))
            (setq hide-at (emagent-chat--open-reasoning-begin))
            (emagent-chat-close-thought)
            (when (emagent-chat--remove-empty-thinking)
              (setq hide-at nil))
            (emagent-chat--finalize-streamed-assistant converted)
            (emagent-chat--finish-response-spacing)
            (emagent-chat--reset-response-state)
            (emagent-chat--sync-user-zone-marker)
            (emagent-chat--maybe-font-lock-flush)
            (when hide-at
              (emagent-chat--hide-reasoning-deferred hide-at)))))))
  ;; Insert stub after stable view is restored, so point ends up at the
  ;; user prompt heading rather than being restored to the entry position.
  (emagent-chat--insert-user-heading-stub))

(provide 'emagent-chat-render)
;;; emagent-chat-render.el ends here
