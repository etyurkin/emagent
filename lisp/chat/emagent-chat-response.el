;;; emagent-chat-response.el --- assistant response module  -*- lexical-binding: t; -*-

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
;;
;; Assistant response rendering and context compression.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'org)
(require 'emagent-chat-input)
(require 'emagent-chat-markup)
(require 'emagent-chat-response-state)
(require 'emagent-chat-thought)
(require 'emagent-chat-ui)
(require 'emagent-log)
(require 'emagent-session)

(defvar-local emagent-chat--follow-output nil
  "Non-nil when this buffer should keep the live response in view.

Set when the user sends a prompt or the window sits on the live tail;
cleared when they scroll so the follow position is off-screen or move
point into earlier history.")

















(defun emagent-chat--bare-slash-command-p (text)
  "Return non-nil when TEXT is a single-line slash command."
  (let ((trimmed (string-trim text)))
    (and (not (string-empty-p trimmed))
         (string-prefix-p "/" trimmed)
         (not (string-match-p "\n" trimmed))
         (let* ((body (substring trimmed 1))
                (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
                (cmd (if space (substring body 0 space) body)))
           (and (> (length cmd) 0)
                (string-match-p "\\`[-a-z0-9:]+\\'" cmd))))))

(defun emagent-chat--compress-command-p (text)
  "Return non-nil when TEXT is a conversation compression slash command."
  (let ((trimmed (string-trim text)))
    (when (string-prefix-p "/" trimmed)
      (let* ((body (substring trimmed 1))
             (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
             (cmd (if space (substring body 0 space) body)))
        (member cmd '("compress" "compact" "summarize"))))))

(defconst emagent-chat--compress-history-limit 200000
  "Maximum conversation chars included in a /compress request.")

(defun emagent-chat--compress-boundary ()
  "Return point at the user heading before an open response, or nil."
  (save-excursion
    (when-let ((resp (emagent-chat--find-open-response-begin)))
      (goto-char resp)
      (when (re-search-backward (emagent-chat--user-heading-re) nil t)
        (line-beginning-position)))))

(defun emagent-chat--conversation-history-text ()
  "Return prior conversation text for /compress, or \"\"."
  (save-excursion
    (let* ((zone (emagent-session-store-metadata-end))
           (end (or (emagent-chat--compress-boundary) (point))))
      (when (and end (> end zone))
        (string-trim (buffer-substring-no-properties zone end))))))

(defun emagent-chat--compress-prompt-text (history)
  "Return a summarization prompt for compression using HISTORY."
  (let ((body (if (> (length history) emagent-chat--compress-history-limit)
                  (concat (substring history 0 emagent-chat--compress-history-limit)
                          "\n\n[...truncated for compression request...]")
                history)))
    (format "Summarize the conversation below for context compression. Preserve key decisions, file paths, errors, and open tasks. Output only the summary.\n\n<conversation>\n%s\n</conversation>"
            body)))

(defun emagent-chat--begin-response (&optional at)
  "Open a new emagent response at AT or point.

Set up the response body markers but do not insert a `** Thinking'
subsection yet.  Thinking is created lazily, only when reasoning or a tool
line actually arrives, so responses without reasoning never show an empty
Thinking block."
  (let ((inhibit-read-only t))
    (emagent-chat--writable)
    ;; Pin follow for this turn; cleared if the user scrolls the live end
    ;; off-screen or moves point into earlier history.
    (setq emagent-chat--follow-output t)
    (goto-char (or at (point)))
    (unless (bolp)
      (insert "\n"))
    (insert "\n")
    (setq emagent-chat--response-body-start (copy-marker (point) nil)
          emagent-chat--assistant-marker (copy-marker (point) nil)
          emagent-chat--response-end-marker
          (save-excursion
            (if (re-search-forward (emagent-chat--user-heading-re) nil t)
                (copy-marker (line-beginning-position) nil)
              'point-max))
          ;; The Response headline / Thinking headline don't exist yet; clear
          ;; their owned markers so a stale one from a prior turn can't mislocate
          ;; inserted body text even if a reset was skipped.
          emagent-chat--response-content-marker nil
          emagent-chat--thinking-headline-marker nil
          emagent-chat--switching-model-p nil
          emagent-chat--preparing-p nil
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

(defun emagent-chat--cancel-response-flush ()
  "Cancel the assistant flush timer and drop pending text without inserting."
  (when emagent-chat--response-flush-timer
    (cancel-timer emagent-chat--response-flush-timer)
    (setq emagent-chat--response-flush-timer nil))
  (setq emagent-chat--response-pending ""))

(defun emagent-chat--reset-response-state ()
  "Clear response markers and cancel pending stream flushes."
  (emagent-chat--cancel-thought-flush)
  (emagent-chat--cancel-response-flush)
  (setq emagent-chat--assistant-marker nil
        emagent-chat--response-body-start nil
        emagent-chat--response-content-marker nil
        emagent-chat--response-end-marker nil
        emagent-chat--thought-open-p nil
        emagent-chat--switching-model-p nil
        emagent-chat--preparing-p nil
        emagent-chat--thinking-headline-marker nil
        emagent-chat--thought-marker nil
        emagent-chat--reasoning-streamed-p nil
        emagent-chat--fence-state nil
        emagent-chat--response-fence-state nil
        emagent-chat--response-pending ""
        emagent-chat--permission-pending nil)
  (if emagent-chat--tool-call-lines
      (clrhash emagent-chat--tool-call-lines)
    (setq-local emagent-chat--tool-call-lines (make-hash-table :test 'equal))))

(defvar-local emagent-chat--response-fence-state nil
  "Streaming fence state for the response body.
Nil when outside a fenced block; (lang . body-so-far) while buffering.
Mirrors emagent-chat--fence-state but tracks the `** Response' stream.")

(defun emagent-chat--fail-response-p ()
  "Return non-nil when an emagent response is open and can be closed with error."
  (emagent-chat--open-response-p))

(defun emagent-chat--response-body-text ()
  "Return the current `** Response' body text, or nil when unavailable.

Spans from just after the `** Response' headline (the content marker) to
the end of the open response (the next user heading, or `point-max')."
  (when-let* ((bounds (emagent-chat--response-body-bounds))
              (start (car bounds))
              (end (cdr bounds))
              ((<= start end)))
    (buffer-substring-no-properties start end)))

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
              ;; Replace whatever newline run the reasoning stream left at its
              ;; tail with exactly one blank line — inserting the headline
              ;; before the run used to push those stray blank lines into the
              ;; Response body.
              (delete-region (point) tail)
              (insert "\n\n" emagent-chat-response-headline "\n")
              (setq emagent-chat--response-content-marker (copy-marker (point) nil))
              (point))
          ;; No reasoning was rendered: place Response at the response body start.
          (when-let ((beg (emagent-chat--open-response-begin)))
            (goto-char beg)
            (insert emagent-chat-response-headline "\n")
            (setq emagent-chat--response-content-marker (copy-marker (point) nil))
            (point))))))

(defun emagent-chat--schedule-response-flush ()
  "Debounce assistant insertion using `emagent-chat-response-stream-delay'."
  (when emagent-chat--response-flush-timer
    (cancel-timer emagent-chat--response-flush-timer))
  (setq emagent-chat--response-flush-timer
        (run-with-timer emagent-chat-response-stream-delay nil
                        (lambda ()
                          (setq emagent-chat--response-flush-timer nil)
                          (emagent-chat--flush-response-pending)))))

(defun emagent-chat--insert-assistant-now (text)
  "Insert assistant TEXT into the open Response block after fence conversion."
  (emagent-chat--end-send-pending-if-active)
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (when (emagent-chat--open-response-p)
          (let ((inhibit-read-only t))
            (emagent-chat--writable)
            (emagent-chat-close-thought)
            (let* ((result (emagent-chat--feed-fences
                            text emagent-chat--response-fence-state))
                   (safe (emagent-chat--demote-response-headings
                          (emagent-chat--map-outside-src-blocks
                           (lambda (s)
                             (let ((case-fold-search nil))
                               (emagent-chat--convert-markdown-tables
                                (emagent-chat--convert-markdown-headings
                                 (emagent-chat--convert-inline-code-spans
                                  (replace-regexp-in-string
                                   "\\*\\*\\([^*\n]+\\)\\*\\*" "*\\1*"
                                   (replace-regexp-in-string
                                    "\\[\\([^][\n]+\\)\\](\\([^)\n]+\\))"
                                    "[[\\2][\\1]]"
                                    s)))))))
                           (car result))))
                   (existing (emagent-chat--response-body-bounds))
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
              (setq emagent-chat--response-fence-state (cdr result))
              (when (and insert-at (not (string-empty-p safe)))
                (save-excursion
                  (goto-char insert-at)
                  (let ((beg (point)))
                    (insert safe)
                    (emagent-chat--demote-headlines-in-region beg (point))
                    (setq emagent-chat--assistant-marker (point-marker))))))
            (emagent-chat--maybe-font-lock-flush)))))))

(defun emagent-chat--flush-response-pending (&optional _final)
  "Insert any batched assistant text into the open Response block.
With FINAL non-nil, emit everything buffered (used before finish/fail)."
  (when emagent-chat--response-flush-timer
    (cancel-timer emagent-chat--response-flush-timer)
    (setq emagent-chat--response-flush-timer nil))
  (let ((text emagent-chat--response-pending))
    (setq emagent-chat--response-pending "")
    (when (not (string-empty-p text))
      (emagent-chat--insert-assistant-now text))))

(defun emagent-chat-append-assistant (text)
  "Append streamed assistant TEXT under the `** Response' subsection.
Each chunk is passed through the streaming markdown->org converter so the
buffer shows formatted org while the response is still arriving.  Chunks
are batched using `emagent-chat-response-stream-delay'."
  ;; Normalize line endings so CRLF/CR output does not leave stray ^M in the
  ;; buffer or defeat the LF-based fence/src-block segmentation below.
  (setq text (replace-regexp-in-string "\r\n?" "\n" text))
  (when (not (string-empty-p text))
    (setq emagent-chat--response-pending
          (concat emagent-chat--response-pending text))
    (if (or noninteractive (<= emagent-chat-response-stream-delay 0))
        (emagent-chat--flush-response-pending)
      (emagent-chat--schedule-response-flush))))

(defun emagent-chat-fail-assistant (message)
  "Close the in-flight emagent response with error MESSAGE under `** Response'."
  (emagent-chat--flush-response-pending t)
  (emagent-chat--send-pending-end)
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t))
          (emagent-chat--writable)
          (when (emagent-chat--fail-response-p)
            (emagent-chat--clear-transient-reasoning-scaffold)
            (emagent-chat-close-thought)
            (emagent-chat--ensure-response-headline)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "\n*Error:* %s\n" message))
            (emagent-chat--finish-response-spacing)
            (emagent-chat--reset-response-state)
            (emagent-chat--sync-user-zone-marker)
            (emagent-chat--maybe-font-lock-flush))))))
  (emagent-chat--flush-deferred-font-lock)
  (emagent-chat--insert-user-heading-stub))

(defun emagent-chat--finalize-streamed-assistant (converted)
  "Replace the `** Response' body with CONVERTED assistant text.

When the body text already streamed into the buffer equals CONVERTED,
trimming trailing whitespace, skip the `delete-region' + reinsert
entirely so a finish that mirrors the stream does not blank and redraw
the whole response body.

Org tables are aligned synchronously here.  An idle timer is unreliable:
ACP/MCP process output keeps resetting Emacs idle, so deferred align
often never ran and left pipe tables unaligned until a manual TAB."
  ;; Streaming fence state is no longer needed — finalization replaces the
  ;; buffer content with the fully-converted text.
  (setq emagent-chat--response-fence-state nil)
  (when-let ((content-start (or (car (emagent-chat--response-body-bounds))
                                (emagent-chat--ensure-response-headline))))
    (let* ((body-end (cdr (emagent-chat--open-response-body-bounds)))
           (existing (emagent-chat--response-body-text))
           (unchanged (and existing
                           (string-equal (string-trim-right existing)
                                         (string-trim-right converted))))
           (end (if unchanged
                    body-end
                  (progn
                    (when (and body-end (< content-start body-end))
                      (delete-region content-start body-end))
                    (goto-char content-start)
                    (insert converted)
                    (point)))))
      (when (and end (string-match-p "|" converted))
        (let ((was-modified (buffer-modified-p)))
          (unwind-protect
              (ignore-errors
                (emagent-chat--align-org-tables-in-region content-start end))
            (set-buffer-modified-p was-modified))))
      (setq emagent-chat--assistant-marker (copy-marker end nil)))))

(defvar emagent-chat--finish-close t
  "When non-nil, `emagent-chat-finish-assistant' resets markers and inserts a stub.

ACP prompt rendering binds this to nil so late agent chunks that arrive
while a debounced finish is in flight can still update the open response;
the render loop closes the response only after assistant text is stable.")

(defvar-local emagent-chat--pending-hide-reasoning nil
  "Buffer position to fold after a deferred finish close, or nil.")

(defun emagent-chat--close-finished-response ()
  "Reset response markers and insert the next user heading stub.

Called once the ACP finish render has settled (no newer assistant text)."
  (emagent-chat--flush-response-pending t)
  (when emagent-chat--pending-hide-reasoning
    (emagent-chat--hide-reasoning-deferred
     emagent-chat--pending-hide-reasoning)
    (setq emagent-chat--pending-hide-reasoning nil))
  (emagent-chat--reset-response-state)
  (emagent-chat--sync-user-zone-marker)
  ;; Font-lock before the stub so settle work cannot move point off the
  ;; prompt input position.
  (emagent-chat--flush-deferred-font-lock)
  (emagent-chat--insert-user-heading-stub))

(defun emagent-chat--demote-headlines-in-region (beg end)
  "Demote org headlines at level 1-2 between BEG and END to level 3.

Used after streaming inserts so a `**' split across chunks cannot leave a
level-2 sibling of `** Response' under the user heading."
  (save-excursion
    (goto-char beg)
    (forward-line 0)
    (while (< (point) end)
      (when (looking-at "^\\*\\{1,2\\} ")
        (let* ((star-beg (match-beginning 0))
               (star-end (1- (match-end 0)))
               (old-n (- star-end star-beg)))
          (delete-region star-beg star-end)
          (insert "***")
          (setq end (+ end (- 3 old-n)))))
      (forward-line 1))))

(defun emagent-chat-finish-assistant (text &optional thought-text)
  "Finalize the latest emagent response with TEXT.

Render the assistant answer under `** Response'.  When reasoning was streamed
keep its `** Thinking' subsection; otherwise build one from THOUGHT-TEXT.
A response without any reasoning has no `** Thinking' subsection at all.

When `emagent-chat--finish-close' is nil, leave the response open (no stub)
so a subsequent finish can replace the body if more assistant text arrives."
  (emagent-chat--flush-response-pending t)
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
            (unless (emagent-chat--open-reasoning-begin)
              (emagent-chat--clear-transient-reasoning-scaffold))
            (setq hide-at (emagent-chat--open-reasoning-begin))
            (emagent-chat-close-thought)
            (when (emagent-chat--remove-empty-thinking)
              (setq hide-at nil))
            (emagent-chat--finalize-streamed-assistant converted)
            (emagent-chat--finish-response-spacing)
            (emagent-chat--maybe-font-lock-flush)
            (if emagent-chat--finish-close
                (progn
                  (when hide-at
                    (emagent-chat--hide-reasoning-deferred hide-at))
                  (emagent-chat--reset-response-state)
                  (emagent-chat--sync-user-zone-marker))
              (setq emagent-chat--pending-hide-reasoning hide-at)))))))
  (when emagent-chat--finish-close
    ;; Font-lock then stub after stable view is restored, so point ends at
    ;; the user prompt input rather than being restored to the entry position.
    (emagent-chat--flush-deferred-font-lock)
    (emagent-chat--insert-user-heading-stub)))

(provide 'emagent-chat-response)
;;; emagent-chat-response.el ends here
