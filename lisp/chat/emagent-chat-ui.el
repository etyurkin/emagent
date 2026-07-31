;;; emagent-chat-ui.el --- Inline permission-button UI for emagent chat  -*- lexical-binding: t; -*-

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
;; Chat buffer UI: markup, input zones, response/thought streaming.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'org)
(require 'project)
(require 'emagent-acp-protocol)
(require 'emagent-log)
(require 'emagent-session)

;; The markers below stay defvar-local/defconst in the facade `emagent-chat';
;; forward-declared here so this leaf never requires it back.
(defvar emagent-chat--assistant-marker)

(defvar emagent-chat--response-content-marker)

(defvar emagent-chat--response-headline-re)

(defvar-local emagent-chat--response-body-start nil
  "Start of the in-flight emagent response body.")

(defvar-local emagent-chat--response-end-marker nil
  "End of the open response region.
A live marker at the following exchange's user heading (re-evaluating an earlier
prompt), the symbol `point-max' when the response is last in the buffer, or nil
before a response is open.  Owning it avoids re-scanning to the next heading on
every streamed chunk.")

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

(defun emagent-chat--ensure-response-markers ()
  "Set body markers for the open response when they were lost."
  (unless (and emagent-chat--response-body-start
               (marker-position emagent-chat--response-body-start)
               emagent-chat--assistant-marker
               (marker-position emagent-chat--assistant-marker))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (setq emagent-chat--response-body-start (copy-marker (car bounds) nil)
            emagent-chat--assistant-marker (copy-marker (cdr bounds) nil)))))

(defun emagent-chat--response-body-bounds ()
  "Return (CONTENT-START . END) for the `** Response' body, or nil.
CONTENT-START comes from the owned `emagent-chat--response-content-marker' once
the headline exists; otherwise the headline is located by search and cached."
  (if (and emagent-chat--response-content-marker
           (marker-position emagent-chat--response-content-marker)
           (emagent-chat--open-response-p))
      (cons (marker-position emagent-chat--response-content-marker)
            (emagent-chat--response-region-end
             (emagent-chat--open-response-begin)))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--response-headline-re (cdr bounds) t)
          (forward-line 1)
          (setq emagent-chat--response-content-marker (copy-marker (point) nil))
          (cons (point) (cdr bounds)))))))

(defun emagent-chat--writable ()
  "Remove read-only state left by older emagent chat UIs."
  (setq buffer-read-only nil)
  (when (< (point-min) (point-max))
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) '(read-only t)))))

(defun emagent-chat--insert-initial-comment ()
  "Insert the scratch-style intro and initial user heading in a new buffer."
  (when (= (point-min) (point-max))
    (insert emagent-chat-initial-comment)
    (goto-char (point-max))
    (insert (emagent-chat--user-heading-prefix))))

(defun emagent-chat--user-heading-prefix ()
  "Return the org heading prefix for user messages, e.g. \"* etyurkin> \"."
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

(defvar-local emagent-chat--history-items nil
  "Most recent user prompts, newest first, for history navigation.")

(defvar-local emagent-chat--history-index nil
  "Current history index while navigating, or nil when inactive.")

(defvar-local emagent-chat--history-base-input nil
  "Original input line text before history navigation started.")

(defvar-local emagent-chat--history-origin-line nil
  "Line start position where history navigation was initiated.")

(defun emagent-chat--on-user-input-line-p ()
  "Return non-nil when point is after `* user> ' on the current line."
  (when-let ((input-pos (emagent-chat--user-prompt-input-pos)))
    (>= (point) input-pos)))

(defun emagent-chat--current-user-input ()
  "Return current line input after the `* user> ' prefix, or nil."
  (when-let ((input-pos (emagent-chat--user-prompt-input-pos)))
    (buffer-substring-no-properties input-pos (line-end-position))))

(defun emagent-chat--replace-current-user-input (text)
  "Replace current line input after `* user> ' with TEXT."
  (when-let ((input-pos (emagent-chat--user-prompt-input-pos)))
    (let ((inhibit-read-only t))
      (goto-char input-pos)
      (delete-region input-pos (line-end-position))
      (insert (or text ""))
      (goto-char (line-end-position)))))

(defun emagent-chat--collect-history-items ()
  "Return previous user heading inputs, newest first, excluding current line."
  (let ((current-line (line-beginning-position))
        (user-re (emagent-chat--user-heading-re))
        items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward user-re nil t)
        (let ((line-start (line-beginning-position)))
          (when (< line-start current-line)
            (let ((text (string-trim
                         (buffer-substring-no-properties (match-end 0)
                                                         (line-end-position)))))
              (unless (string-empty-p text)
                (push text items)))))))
    items))

(defun emagent-chat--history-reset ()
  "Reset transient prompt-history navigation state for current buffer."
  (setq emagent-chat--history-items nil
        emagent-chat--history-index nil
        emagent-chat--history-base-input nil
        emagent-chat--history-origin-line nil))

(defun emagent-chat--history-ensure-state ()
  "Initialize prompt-history navigation state for the current input line."
  (let ((line-start (line-beginning-position)))
    (unless (and (integerp emagent-chat--history-index)
                 (equal emagent-chat--history-origin-line line-start))
      (setq emagent-chat--history-origin-line line-start
            emagent-chat--history-base-input (or (emagent-chat--current-user-input) "")
            emagent-chat--history-items (emagent-chat--collect-history-items)
            emagent-chat--history-index -1))))

(defun emagent-chat-history-previous ()
  "Replace current `* user> ' input with the previous prompt from history."
  (interactive)
  (unless (emagent-chat--on-user-input-line-p)
    (user-error "Place point after '* user> ' to use prompt history"))
  (emagent-chat--history-ensure-state)
  (if (null emagent-chat--history-items)
      (message "No prompt history")
    (if (< emagent-chat--history-index (1- (length emagent-chat--history-items)))
        (setq emagent-chat--history-index (1+ emagent-chat--history-index))
      (message "Start of prompt history"))
    (emagent-chat--replace-current-user-input
     (nth emagent-chat--history-index emagent-chat--history-items))))

(defun emagent-chat-history-next ()
  "Replace current `* user> ' input with the newer prompt from history."
  (interactive)
  (unless (emagent-chat--on-user-input-line-p)
    (user-error "Place point after '* user> ' to use prompt history"))
  (emagent-chat--history-ensure-state)
  (cond
   ((not (integerp emagent-chat--history-index))
    (message "No prompt history"))
   ((> emagent-chat--history-index 0)
    (setq emagent-chat--history-index (1- emagent-chat--history-index))
    (emagent-chat--replace-current-user-input
     (nth emagent-chat--history-index emagent-chat--history-items)))
   ((= emagent-chat--history-index 0)
    (setq emagent-chat--history-index -1)
    (emagent-chat--replace-current-user-input emagent-chat--history-base-input))
   (t
    (message "End of prompt history"))))

(defun emagent-chat-history-previous-or-previous-line ()
  "Prompt history on user input; otherwise delegate to `previous-line'."
  (interactive)
  (if (emagent-chat--on-user-input-line-p)
      (emagent-chat-history-previous)
    (call-interactively #'previous-line)))

(defun emagent-chat-history-next-or-next-line ()
  "Prompt history on user input; otherwise delegate to `next-line'."
  (interactive)
  (if (emagent-chat--on-user-input-line-p)
      (emagent-chat-history-next)
    (call-interactively #'next-line)))

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

(defun emagent-chat--delete-following-response (pos)
  "Delete the response subsections after POS, before the next user heading.

Deletes from the first `** Thinking'/`** Response' subsection headline after POS
up to (but not including) the next `* user>' heading."
  (save-excursion
    (goto-char pos)
    (let ((limit (save-excursion
                   (if (re-search-forward "^\\* " nil t)
                       (line-beginning-position)
                     (point-max)))))
      (when (re-search-forward emagent-chat--subsection-headline-re limit t)
        (let ((start (line-beginning-position))
              (inhibit-read-only t))
          (emagent-chat--writable)
          (delete-region start limit))))))

(defun emagent-chat--user-heading-at-point-p ()
  "Return non-nil when point is on a `* user>' heading line."
  (looking-at (emagent-chat--user-heading-re)))

(defun emagent-chat--user-heading-follows-p ()
  "Return non-nil when a '* user>' heading immediately follows point."
  (save-excursion
    (end-of-line)
    (skip-chars-forward " \t\n")
    (looking-at (emagent-chat--user-heading-re))))

(defvar-local emagent-chat--defer-user-stub nil
  "When non-nil, skip inserting the next `* user>' stub.

Set while a post-create_plan Build turn is queued so Accept does not
leave an empty prompt between the plan dialog and Build output.")

(defun emagent-chat--insert-user-heading-stub ()
  "Insert a user heading stub unless one already follows the user zone.

Leave point after the `* user> ' prefix so the user can type immediately.
When `emagent-chat--defer-user-stub' is set (queued plan Build), skip
insertion so Accept does not flash an empty prompt before Build starts."
  (if emagent-chat--defer-user-stub
      (progn
        (emagent-chat--sync-user-zone-marker)
        (goto-char (emagent-chat--user-zone-start))
        nil)
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (goto-char (emagent-chat--user-zone-start))
      (unless (emagent-chat--user-heading-follows-p)
        (unless (bolp) (insert "\n"))
        (insert (emagent-chat--user-heading-prefix)))
      ;; When a stub already exists, zone-start is its bol — move to the
      ;; input position after the prefix rather than leaving point on `*'.
      (goto-char (or (emagent-chat--user-prompt-input-pos) (point)))
      (point))))

(defun emagent-chat--skip-header ()
  
  "Internal helper."
  (goto-char (point-min))
  (while (and (not (eobp))
              (or (looking-at "#\\+")
                  (looking-at "# ")
                  (looking-at "#$")))
    (forward-line 1))
  (skip-chars-forward "\n")
  (point))

(defun emagent-chat--after-last-response ()
  "Return the position where the trailing user zone begins.

That is the `* user>' heading after the last `** Thinking'/`** Response'
subsection, or the start of the conversation when no response exists yet."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward emagent-chat--subsection-headline-re nil t)
        (progn
          (goto-char (match-beginning 0))
          (if (re-search-forward "^\\* " nil t)
              (line-beginning-position)
            (point-max)))
      (emagent-chat--skip-header))))

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

(defun emagent-chat--user-block-bounds ()
  "Return (START . END) for the `* username>' block above point, or nil.
Captures the heading line and all body lines up to the next heading or
response delimiter, trailing whitespace trimmed.  Does not check that
point is within the block; `emagent-chat--send-bounds' does."
  (let ((user-re (emagent-chat--user-heading-re)))
    (save-excursion
      (let ((heading-pos
             (or (and (looking-at "\\* ") (line-beginning-position))
                 (and (re-search-backward "^\\* " (point-min) t)
                      (line-beginning-position)))))
        (when (and heading-pos
                   (save-excursion
                     (goto-char heading-pos)
                     (looking-at user-re)))
          (let* ((start heading-pos)
                 (end (progn
                        (goto-char heading-pos)
                        (forward-line 1)
                        (if (re-search-forward
                             (concat "^\\* \\|"
                                     emagent-chat--subsection-headline-re)
                             (point-max) t)
                            (match-beginning 0)
                          (point-max)))))
            (cons start (save-excursion
                          (goto-char end)
                          (skip-chars-backward " \t\n")
                          (point)))))))))

(defun emagent-chat--send-bounds ()
  "Return (BEG . END) of the `* user>' prompt at point, or nil.

Point must be on the prompt's heading line or within its direct body,
above its response subsections.  Anywhere else there is nothing to
send; then fall through to org."
  (let ((block (emagent-chat--user-block-bounds)))
    (when (and block
               (>= (point) (car block))
               (<= (line-beginning-position) (cdr block)))
      block)))

(defun emagent-chat--open-reasoning-begin ()
  "Return point at the `** Thinking' headline in the open response body.
Read from the owned `emagent-chat--thinking-headline-marker' when set; otherwise
locate the headline by search and cache it."
  (if (and emagent-chat--thinking-headline-marker
           (marker-position emagent-chat--thinking-headline-marker)
           (emagent-chat--open-response-p))
      (save-excursion
        (goto-char emagent-chat--thinking-headline-marker)
        (beginning-of-line)
        (when (looking-at emagent-chat--thinking-headline-re)
          (marker-position emagent-chat--thinking-headline-marker)))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--thinking-headline-re (cdr bounds) t)
          (setq emagent-chat--thinking-headline-marker
                (copy-marker (match-beginning 0) nil))
          (match-beginning 0))))))

(defun emagent-chat--open-switching-begin ()
  "Return point at the `** Switching model' headline, or nil."
  (if (and emagent-chat--thinking-headline-marker
           (marker-position emagent-chat--thinking-headline-marker)
           emagent-chat--switching-model-p
           (emagent-chat--open-response-p))
      (save-excursion
        (goto-char emagent-chat--thinking-headline-marker)
        (beginning-of-line)
        (when (looking-at emagent-chat--switching-headline-re)
          (marker-position emagent-chat--thinking-headline-marker)))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--switching-headline-re (cdr bounds) t)
          (setq emagent-chat--thinking-headline-marker
                (copy-marker (match-beginning 0) nil)
                emagent-chat--switching-model-p t)
          (match-beginning 0))))))

(defun emagent-chat--open-preparing-begin ()
  "Return point at the `** Preparing…' headline, or nil."
  (if (and emagent-chat--thinking-headline-marker
           (marker-position emagent-chat--thinking-headline-marker)
           emagent-chat--preparing-p
           (emagent-chat--open-response-p))
      (save-excursion
        (goto-char emagent-chat--thinking-headline-marker)
        (beginning-of-line)
        (when (looking-at emagent-chat--preparing-headline-re)
          (marker-position emagent-chat--thinking-headline-marker)))
    (when-let ((bounds (emagent-chat--open-response-body-bounds)))
      (save-excursion
        (goto-char (car bounds))
        (when (re-search-forward emagent-chat--preparing-headline-re (cdr bounds) t)
          (setq emagent-chat--thinking-headline-marker
                (copy-marker (match-beginning 0) nil)
                emagent-chat--preparing-p t)
          (match-beginning 0))))))

(defun emagent-chat--thinking-headline-text (&optional model-id)
  "Return the `** Thinking' headline, optionally suffixing MODEL-ID link."
  (if model-id
      (concat emagent-chat-thinking-headline " "
              (emagent-chat--model-link model-id))
    emagent-chat-thinking-headline))

(defun emagent-chat--switching-headline-text (model-id)
  "Return the `** Switching model' headline for per-turn MODEL-ID."
  (concat emagent-chat-switching-headline " to "
          (emagent-chat--model-link model-id) "…"))

(defun emagent-chat--insert-switching-scaffold ()
  "Insert a `** Switching model' subsection at the response body start."
  (when (and emagent-chat--turn-model
             emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (setq emagent-chat--thinking-headline-marker (copy-marker (point) nil)
          emagent-chat--switching-model-p t
          emagent-chat--preparing-p nil
          emagent-chat--thought-open-p nil
          emagent-chat--thought-marker nil)
    (insert (emagent-chat--switching-headline-text emagent-chat--turn-model) "\n")
    (setq emagent-chat--assistant-marker (copy-marker (point) nil))))

(defun emagent-chat--insert-preparing-scaffold ()
  "Insert a `** Preparing…' subsection at the response body start."
  (when (and emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (setq emagent-chat--thinking-headline-marker (copy-marker (point) nil)
          emagent-chat--preparing-p t
          emagent-chat--switching-model-p nil
          emagent-chat--thought-open-p nil
          emagent-chat--thought-marker nil)
    (insert emagent-chat-preparing-headline "\n")
    (setq emagent-chat--assistant-marker (copy-marker (point) nil))))

(defun emagent-chat--promote-switching-to-thinking ()
  "Replace a `** Switching model' headline with `** Thinking'."
  (when-let ((beg (emagent-chat--open-switching-begin)))
    (goto-char beg)
    (delete-region (line-beginning-position) (line-end-position))
    (insert (emagent-chat--thinking-headline-text emagent-chat--turn-model))
    (unless (bolp) (insert "\n"))
    (setq emagent-chat--switching-model-p nil
          emagent-chat--thought-open-p t
          emagent-chat--thought-marker (copy-marker (point) nil)
          emagent-chat--assistant-marker (copy-marker (point) nil))))

(defun emagent-chat--promote-preparing-to-thinking ()
  "Replace a `** Preparing…' headline with `** Thinking'."
  (when-let ((beg (emagent-chat--open-preparing-begin)))
    (goto-char beg)
    (delete-region (line-beginning-position) (line-end-position))
    (insert (emagent-chat--thinking-headline-text emagent-chat--turn-model))
    (unless (bolp) (insert "\n"))
    (setq emagent-chat--preparing-p nil
          emagent-chat--thought-open-p t
          emagent-chat--thought-marker (copy-marker (point) nil)
          emagent-chat--assistant-marker (copy-marker (point) nil))))

(defun emagent-chat--promote-transient-to-thinking ()
  "Promote preparing/switching headline to `** Thinking' when a turn begins."
  (when (emagent-chat--open-response-p)
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (when (emagent-chat--open-switching-begin)
        (emagent-chat--promote-switching-to-thinking))
      (when (emagent-chat--open-preparing-begin)
        (emagent-chat--promote-preparing-to-thinking)))))

(defun emagent-chat--clear-switching-scaffold ()
  "Remove a `** Switching model' headline from the open response, if present."
  (when-let ((beg (emagent-chat--open-switching-begin)))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (goto-char beg)
      (delete-region (line-beginning-position)
                     (min (point-max) (1+ (line-end-position)))))
    (setq emagent-chat--switching-model-p nil
          emagent-chat--thinking-headline-marker nil)))

(defun emagent-chat--clear-preparing-scaffold ()
  "Remove a `** Preparing…' headline from the open response, if present."
  (when-let ((beg (emagent-chat--open-preparing-begin)))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (goto-char beg)
      (delete-region (line-beginning-position)
                     (min (point-max) (1+ (line-end-position)))))
    (setq emagent-chat--preparing-p nil
          emagent-chat--thinking-headline-marker nil)))

(defun emagent-chat--clear-transient-reasoning-scaffold ()
  "Remove any transient preparing/switching headline from the open response."
  (emagent-chat--clear-switching-scaffold)
  (emagent-chat--clear-preparing-scaffold))

(defun emagent-chat--insert-reasoning-scaffold ()
  "Insert an empty `** Thinking' subsection at the response body start."
  (when (and emagent-chat--response-body-start
             (marker-position emagent-chat--response-body-start))
    (goto-char emagent-chat--response-body-start)
    (setq emagent-chat--thinking-headline-marker (copy-marker (point) nil))
    (insert (emagent-chat--thinking-headline-text emagent-chat--turn-model) "\n")
    (setq emagent-chat--thought-marker (point-marker)
          emagent-chat--thought-open-p t
          emagent-chat--assistant-marker (point-marker))))

(defun emagent-chat--thinking-content-end (begin limit)
  "Return where the Thinking content ends after BEGIN, before LIMIT.

That is the start of the `** Response' headline when present, otherwise LIMIT.
Uses the owned `emagent-chat--response-content-marker' (the Response headline
sits on the line just above it) so it does not scan to LIMIT for a Response
headline that has not been created yet — that scan was O(reasoning^2) while
reasoning streamed with no Response below."
  (if (and emagent-chat--response-content-marker
           (marker-position emagent-chat--response-content-marker)
           (< begin (marker-position emagent-chat--response-content-marker)))
      ;; Response headline exists (owned marker sits on the line below it).
      (save-excursion
        (goto-char emagent-chat--response-content-marker)
        (forward-line -1)
        (line-beginning-position))
    ;; No Response headline: reasoning owns the marker whenever the headline is
    ;; created, so a nil marker in this streaming path means Thinking runs to
    ;; the end of the region.
    limit))

(defun emagent-chat--ensure-reasoning-scaffold ()
  "Ensure the open response has a `** Thinking' subsection ready to stream."
  (when (emagent-chat--open-response-p)
    (when (emagent-chat--open-switching-begin)
      (emagent-chat--promote-switching-to-thinking))
    (when (emagent-chat--open-preparing-begin)
      (emagent-chat--promote-preparing-to-thinking))
    (cond
     (emagent-chat--thought-open-p
      (emagent-chat--sync-thought-marker))
     ((not (emagent-chat--open-reasoning-begin))
      (emagent-chat--insert-reasoning-scaffold))
     (t
      (let ((stream (emagent-chat--reasoning-stream-marker)))
        (setq emagent-chat--thought-marker stream
              emagent-chat--thought-open-p t)
        (unless stream
          (emagent-log "emagent: cannot find reasoning stream marker after ensure")))))))

(defun emagent-chat--reasoning-stream-marker ()
  "Return the insert marker at the end of the open Thinking content.

Streamed reasoning is inserted here, before any `** Response' headline."
  (when-let ((tail (emagent-chat--reasoning-block-tail)))
    (save-excursion
      (goto-char tail)
      (skip-chars-backward "\n" (or (emagent-chat--open-response-begin) (point-min)))
      (point-marker))))

(defun emagent-chat--reasoning-block-tail ()
  "Return point at the end of the open Thinking content, or nil.

This is the start of the `** Response' headline when present, otherwise the
end of the open response body."
  (when-let* ((bounds (emagent-chat--open-response-body-bounds))
              (beg (emagent-chat--open-reasoning-begin)))
    (emagent-chat--thinking-content-end
     (save-excursion (goto-char beg) (line-end-position))
     (cdr bounds))))

(defun emagent-chat--sync-thought-marker ()
  "Realign `emagent-chat--thought-marker' to the true Thinking tail."
  (when emagent-chat--thought-open-p
    (when-let ((stream (emagent-chat--reasoning-stream-marker)))
      (let ((cur (and emagent-chat--thought-marker
                      (marker-position emagent-chat--thought-marker))))
        (when (or (not cur) (< cur (marker-position stream)))
          (setq emagent-chat--thought-marker stream))))))

(defun emagent-chat--ensure-thought-stream ()
  "Open or resume the streaming Thinking subsection in the in-flight response."
  (emagent-chat--ensure-reasoning-scaffold))

(defun emagent-chat--format-thought-block (text)
  "Return `** Thinking' markup for reasoning TEXT, or \"\" when empty."
  (let ((trimmed (string-trim (or text ""))))
    (if (string-empty-p trimmed)
        ""
      (format "%s\n%s\n\n"
              emagent-chat-thinking-headline
              (emagent-chat--escape-reasoning-text trimmed)))))

(defun emagent-chat--reasoning-block-bounds ()
  "Return (CONTENT-START . CONTENT-END) for Thinking at point."
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
  "Hide Reasoning content between BOUNDS using `outline-flag-region'."
  (when bounds
    (ignore-errors
      (outline-flag-region (car bounds) (cdr bounds) t))))

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
    ;; `(car bounds)' is the line-end-position of the `** Thinking' headline
    ;; (i.e. the \n that ends it).  `beginning-of-line' from there lands on
    ;; the `** Thinking' headline itself — exactly where `org-fold-hide-subtree'
    ;; must be called.  We must NOT use `re-search-backward' here: with multiple
    ;; exchanges each containing `** Thinking', a backward search would jump to
    ;; the previous response's headline and fold the wrong block.
    (condition-case _
        (save-excursion
          (goto-char (car bounds))
          (beginning-of-line)
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

(defvar-local emagent-chat--fence-state nil
  "Streaming code-fence buffer for the open Thinking block.
Nil when not inside a fenced code block.
Non-nil: (lang . accumulated-body-so-far) while waiting for the closing ```.")

(defun emagent-chat--split-fences (text)
  "Convert complete markdown fences in TEXT to org src blocks.
Returns (safe-text . fence-state) where fence-state is nil when all fences
are closed, or (lang . body-so-far) for the last unclosed fence."
  (let ((pos 0)
        (parts nil)
        (incomplete nil))
    (while (and (not incomplete) (string-match "```" text pos))
      (let* ((fence-pos (match-beginning 0))
             (after-fence (match-end 0)))
        (push (substring text pos fence-pos) parts)
        (cond
         ;; No newline after ``` — partial lang tag at end of chunk
         ((not (string-match "\n" text after-fence))
          (setq incomplete (cons (substring text after-fence) ""))
          (setq pos (length text)))
         ;; Newline found — complete lang tag, look for closing ```
         (t
          (let* ((tag-end (match-beginning 0))
                 (body-start (match-end 0))
                 (lang (string-trim (substring text after-fence tag-end))))
            (if (not (string-match "```" text body-start))
                ;; No closing fence — buffer the rest
                (progn
                  (setq incomplete (cons lang (substring text body-start)))
                  (setq pos (length text)))
              ;; Complete fence — emit as org src block when lang is given,
              ;; or as plain text (fence stripped) when lang is empty so
              ;; reasoning notes wrapped in plain ``` don't become src blocks.
              (let* ((body-end (match-beginning 0))
                     (close-end (match-end 0))
                     (body (string-trim-right
                            (substring text body-start body-end))))
                (push (if (string-empty-p lang)
                          body
                        (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                                (emagent-chat--lang-from-src-tag lang)
                                (emagent-chat--escape-src-body body)))
                      parts)
                (setq pos close-end))))))))
    (unless incomplete
      (push (substring text pos) parts))
    (cons (apply #'concat (nreverse parts)) incomplete)))

(defun emagent-chat--feed-fences (text state)
  "Continue fence conversion for TEXT from prior STATE.

STATE is nil or (LANG . BODY) as returned by `emagent-chat--split-fences'.
When BODY is empty, fall back to reconstructing an open fence (handles an
incomplete language tag cheaply).  When BODY is non-empty, append TEXT and
search for a closing fence from a 2-character overlap so open code blocks
stay O(chunk) rather than re-scanning the full buffered body each time."
  (let ((fence (make-string 3 ?`)))
    (cond
     ((null state)
      (emagent-chat--split-fences text))
     ((zerop (length (cdr state)))
      (emagent-chat--split-fences
       (concat fence (car state) "\n" (cdr state) text)))
     (t
      (let* ((lang (car state))
             (body (cdr state))
             (overlap (min 2 (length body)))
             (combined (concat body text))
             (close (cl-search fence combined :start2
                               (max 0 (- (length body) overlap)))))
        (if (not close)
            (cons "" (cons lang combined))
          (let* ((complete (string-trim-right (substring combined 0 close)))
                 (rest (substring combined (+ close 3)))
                 (block (if (string-empty-p lang)
                            complete
                          (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                                  (emagent-chat--lang-from-src-tag lang)
                                  (emagent-chat--escape-src-body complete))))
                 (after (emagent-chat--split-fences rest)))
            (cons (concat block (car after)) (cdr after)))))))))

(defun emagent-chat--count-substring (needle s end)
  "Return the count of non-overlapping NEEDLE occurrences in S before END."
  (let ((n 0) (pos 0) (len (length needle)) hit)
    (while (setq hit (cl-search needle s :start2 pos :end2 end))
      (setq n (1+ n)
            pos (+ hit len)))
    n))

(defun emagent-chat--open-markup-start (text)
  "Return start index of a trailing incomplete markdown span in TEXT.
Return nil when TEXT ends on a complete boundary.  Covers inline code,
bold, and links whose closing delimiter may still arrive later.  Each
pattern is anchored to the end of TEXT and forbids an interior newline."
  (let (starts)
    ;; Inline code: a trailing unmatched opening backtick.
    (when (and (string-match "`[^`\n]*\\'" text)
               (cl-evenp (cl-count ?` text :end (match-beginning 0))))
      (push (match-beginning 0) starts))
    ;; Bold: a trailing `**' opener with no closing `**' yet.
    (when (and (string-match "\\*\\*[^*\n]*\\'" text)
               (cl-evenp (emagent-chat--count-substring "**" text
                                                        (match-beginning 0))))
      (push (match-beginning 0) starts))
    ;; Link, in any partial state: `[text', `[text]', or `[text](url'.
    (dolist (re '("\\[[^][\n]*\\'"
                  "\\[[^][\n]+\\][ \t]*\\'"
                  "\\[[^][\n]+\\][ \t]*([^)\n]*\\'"))
      (when (string-match re text)
        (push (match-beginning 0) starts)))
    (when starts (apply #'min starts))))

(defun emagent-chat--split-open-markup (text)
  "Split TEXT before a trailing, still-incomplete markdown span.
Return (EMIT . HOLD): HOLD begins at an inline code, bold, or link span whose
closing delimiter may arrive in a later chunk, or \"\" when TEXT ends on a
complete boundary.  Holding the partial span keeps markup that a streaming
boundary split from rendering as raw `*', backtick, or bracket characters."
  (if-let ((start (emagent-chat--open-markup-start text)))
      (cons (substring text 0 start) (substring text start))
    (cons text "")))

(defun emagent-chat--end-send-pending-if-active ()
  "End the pre-dispatch phase once agent output begins arriving."
  (emagent-chat--send-pending-end))

(defun emagent-chat--insert-thought-now (text)
  "Insert reasoning TEXT at the open Thinking marker."
  (emagent-chat--end-send-pending-if-active)
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

(defun emagent-chat--reasoning-after-tool-artifact-p ()
  "Return non-nil when point follows a tool line or src block, modulo blanks."
  (save-excursion
    (skip-chars-backward " \t\n")
    (beginning-of-line)
    (looking-at-p "\\(?:→ \\|#\\+[Ee][Nn][Dd]_[Ss][Rr][Cc]\\)")))

(defun emagent-chat--thinking-leading-blank-p ()
  "Return non-nil when point sits in the blank run after `** Thinking'."
  (when-let* ((beg (emagent-chat--open-reasoning-begin))
              (pos (point)))
    (save-excursion
      (goto-char beg)
      (forward-line 1)
      (let ((start (point)))
        (and (> pos start)
             (string-match-p "\\`[ \t\n]*\\'"
                             (buffer-substring-no-properties start pos)))))))

(defun emagent-chat--collapse-thinking-leading-blanks ()
  "Delete the blank run after `** Thinking' up to point."
  (when-let ((beg (emagent-chat--open-reasoning-begin)))
    (save-excursion
      (goto-char beg)
      (forward-line 1)
      (let ((start (point)))
        (when (< start (point))
          (delete-region start (point))
          (goto-char start))))))

(defun emagent-chat--newlines-before-point ()
  "Return the count of consecutive newlines immediately before point."
  (save-excursion
    (let ((n 0))
      (while (eq (char-before) ?\n)
        (setq n (1+ n))
        (backward-char))
      n)))

(defun emagent-chat--newlines-after-point ()
  "Return the count of consecutive newlines immediately after point."
  (save-excursion
    (let ((n 0))
      (while (eq (char-after) ?\n)
        (setq n (1+ n))
        (forward-char))
      n)))

(defun emagent-chat--insert-reasoning-text (text)
  "Insert TEXT at `emagent-chat--thought-marker' in the open Thinking content.
Prose that resumes after a tool line or src block is separated from it by
exactly one blank line, so the two never glue onto the same line.  Some
agents stream bare paragraph-break deltas (chunks that are only newlines)
while still composing; a run of those is collapsed to at most one blank
line rather than piling up as a growing blank tail.

The streaming marker re-syncs to the content tail by skipping back over
trailing newlines (see `emagent-chat--reasoning-stream-marker'), which
strands the previous chunk's own trailing newlines after point.  That run is
folded into TEXT's leading separation, so a thought that resumes after a
close/reopen cycle (each assistant chunk closes the thought) neither glues
onto the previous one nor grows the blank tail two lines per cycle.  When
the run separates the content from following text — the `** Response'
headline, or a tool line's trailing newline — it is structural and is
re-inserted after TEXT, leaving the marker at the true content end."
  (cl-block emagent-chat--insert-reasoning-text
    (let* ((safe (replace-regexp-in-string
                "\n\\{3,\\}" "\n\n"
                (emagent-chat--escape-reasoning-text text (not (bolp)))))
         (before (emagent-chat--newlines-before-point))
         (after (emagent-chat--newlines-after-point))
         (tail-sep (when (> after 0)
                     (save-excursion
                       (forward-char after)
                       (unless (eobp) (min after 2))))))
    (when (> after 0)
      (delete-char after)
      (setq safe (replace-regexp-in-string
                  "\\`\n\\{3,\\}" "\n\n"
                  (concat (make-string after ?\n) safe))))
    (when (emagent-chat--thinking-leading-blank-p)
      (setq safe (replace-regexp-in-string "\\`[\n\r]+" "" safe))
      (if (string-empty-p (string-trim safe))
          (progn
            (emagent-chat--collapse-thinking-leading-blanks)
            (cl-return-from emagent-chat--insert-reasoning-text nil))
        (emagent-chat--collapse-thinking-leading-blanks)
        (setq before (emagent-chat--newlines-before-point)
              after (emagent-chat--newlines-after-point)
              tail-sep (when (> after 0)
                         (save-excursion
                           (forward-char after)
                           (unless (eobp) (min after 2)))))))
    (cond
     ;; Resuming after a tool line/block: keep exactly one blank line of
     ;; separation.
     ((emagent-chat--reasoning-after-tool-artifact-p)
      (setq safe (concat (make-string (max 0 (- 2 before)) ?\n)
                         (replace-regexp-in-string "\\`[\n\r]+" "" safe))))
     ;; TEXT itself opens with blank line(s): only trim when that run,
     ;; combined with the newlines already before point, would exceed one
     ;; blank line — a lone paragraph break is left untouched.
     ((string-match "\\`\n+" safe)
      (let ((leading (match-end 0)))
        (when (> (+ before leading) 2)
          (setq safe (concat (make-string (max 0 (- 2 before)) ?\n)
                             (substring safe leading)))))))
    (when (string-empty-p safe)
      (cl-return-from emagent-chat--insert-reasoning-text nil))
    (if (not tail-sep)
        (insert safe)
      ;; Structural separation follows: share it with SAFE's own trailing
      ;; newlines (a paragraph break at the tail and the separation are the
      ;; same blank line) and keep point before it, at the content end.
      (let ((trailing 0))
        (when (string-match "\n+\\'" safe)
          (setq trailing (- (match-end 0) (match-beginning 0))
                safe (substring safe 0 (match-beginning 0))))
        (insert safe)
        (save-excursion
          (insert (make-string (max tail-sep (min trailing 2)) ?\n))))))))

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

(defun emagent-chat--cancel-thought-flush ()
  "Flush any pending reasoning content and cancel the flush timer.
Empty-lang fences (reasoning notes) are emitted as plain text; language-tagged
fences are closed as org src blocks so buffered content stays readable."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer)
    (setq emagent-chat--thought-flush-timer nil))
  (let ((fence emagent-chat--fence-state)
        (pending emagent-chat--thought-pending))
    (setq emagent-chat--fence-state nil
          emagent-chat--thought-pending "")
    (let ((to-insert
           (concat
            (when fence
              (let* ((raw-lang (car fence))
                     (body (string-trim-right (cdr fence))))
                (cond
                 ((string-empty-p body) "")
                 ((string-empty-p raw-lang) (concat body "\n"))
                 (t (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n"
                            (emagent-chat--lang-from-src-tag raw-lang) body)))))
            pending)))
      (when (and (not (string-empty-p to-insert))
                 (emagent-chat--open-response-p))
        (emagent-chat--insert-thought-now to-insert)))))

(defun emagent-chat--schedule-thought-flush ()
  "Debounce reasoning insertion using `emagent-chat-thought-stream-delay'."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer))
  (setq emagent-chat--thought-flush-timer
        (run-with-timer emagent-chat-thought-stream-delay nil
                        (lambda ()
                          (setq emagent-chat--thought-flush-timer nil)
                          (emagent-chat--flush-thought-pending)))))

(defun emagent-chat--flush-thought-pending (&optional final)
  "Insert any batched reasoning text into the open Thinking block.
Converts complete markdown code fences to org src blocks; buffers incomplete
fences in `emagent-chat--fence-state' until the closing ``` arrives, and
buffers a trailing partial inline span (code, bold, or link) in
`emagent-chat--thought-pending' until its closing delimiter arrives, so markup
split across streaming chunks never renders raw.  With FINAL non-nil (thought
close or an interrupting tool call) everything buffered is emitted as-is."
  (when emagent-chat--thought-flush-timer
    (cancel-timer emagent-chat--thought-flush-timer)
    (setq emagent-chat--thought-flush-timer nil))
  (let ((text emagent-chat--thought-pending))
    (setq emagent-chat--thought-pending "")
    (when (not (string-empty-p text))
      (let* ((result (emagent-chat--feed-fences text emagent-chat--fence-state))
             (to-insert (car result))
             (new-fence (cdr result)))
        (setq emagent-chat--fence-state new-fence)
        ;; With no open fence and more chunks still coming, hold back a
        ;; trailing partial markup span (inline code, bold, or link) whose
        ;; closing delimiter may arrive in the next chunk.
        (unless (or final new-fence)
          (let ((split (emagent-chat--split-open-markup to-insert)))
            (setq to-insert (car split)
                  emagent-chat--thought-pending (cdr split))))
        (when (not (string-empty-p to-insert))
          (emagent-chat--with-streaming-view
           (lambda ()
             (emagent-chat--insert-thought-now to-insert))))))))

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
  (emagent-chat--flush-thought-pending t)
  (emagent-chat--with-stable-view
    (lambda ()
      (with-current-buffer (current-buffer)
        (when emagent-chat--thought-open-p
          (let ((inhibit-read-only t)
                (hide-at (emagent-chat--open-reasoning-begin)))
            (emagent-chat--writable)
            (when-let ((tail (emagent-chat--reasoning-block-tail)))
              ;; Only reset assistant-marker to the reasoning tail when response
              ;; text hasn't started yet.  If the Response section already exists
              ;; (streaming started), keep the marker tracking the response content.
              (unless (emagent-chat--response-body-bounds)
                (setq emagent-chat--assistant-marker (copy-marker tail nil))))
            (setq emagent-chat--thought-open-p nil
                  emagent-chat--thought-marker nil)
            (emagent-chat--maybe-font-lock-flush)
            (when hide-at
              (emagent-chat--hide-reasoning-deferred hide-at))))))))

(defun emagent-chat--finish-tool-line-in-reasoning ()
  "Leave `emagent-chat--thought-marker' on a fresh line after a tool line."
  (goto-char (line-end-position))
  (unless (or (eobp) (eq (char-after) ?\n))
    (insert "\n"))
  (setq emagent-chat--thought-marker (copy-marker (point) nil)))

(defun emagent-chat--sync-thought-marker-after-tool (end)
  "Place `emagent-chat--thought-marker' after tool display ending at END."
  (goto-char (marker-position end))
  (unless (bolp)
    (goto-char (line-end-position)))
  (unless (or (eobp) (eq (char-after) ?\n))
    (insert "\n"))
  (setq emagent-chat--thought-marker (copy-marker (point) nil)))

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

(defun emagent-chat--conversation-history-for-compress (raw)
  "Return RAW history with Thinking and tool chrome stripped.

Keeps user headings and `** Response' bodies for the compress prompt."
  (let ((lines (split-string (or raw "") "\n"))
        (out nil)
        (in-thinking nil)
        (in-response nil))
    (dolist (line lines)
      (cond
       ((string-match-p "\\`\\*\\* Thinking" line)
        (setq in-thinking t in-response nil))
       ((string-match-p "\\`\\*\\* Response" line)
        (setq in-thinking nil in-response t)
        (push line out))
       ((string-match-p (emagent-chat--user-heading-re) line)
        (setq in-thinking nil in-response nil)
        (push line out))
       (in-thinking nil)
       ((and (not in-response)
             (string-match-p "\\`→ " line))
        nil)
       (t (push line out))))
    (string-join (nreverse out) "\n")))

(defun emagent-chat--conversation-history-text ()
  "Return prior conversation text for /compress, or \"\".

Strips `** Thinking' subsections and tool arrow lines so compress
prompts stay small."
  (save-excursion
    (let* ((zone (emagent-session-store-metadata-end))
           (end (or (emagent-chat--compress-boundary) (point))))
      (when (and end (> end zone))
        (emagent-chat--conversation-history-for-compress
         (string-trim (buffer-substring-no-properties zone end)))))))

(defun emagent-chat--compress-prompt-text (history)
  "Return a summarization prompt for compression using HISTORY."
  (let ((body (if (> (length history) emagent-chat--compress-history-limit)
                  (concat (substring history 0 emagent-chat--compress-history-limit)
                          "\n\n[...truncated for compression request...]")
                history)))
    (format
     (concat
      "Compress the conversation below into a short durable brief.\n"
      "Do not use tools. Reply with only the SUMMARY and FACTS text.\n"
      "Format exactly:\n"
      "1) SUMMARY: 5-12 short bullets (decisions, paths, errors, open tasks)\n"
      "2) FACTS: durable bullets only (paths, decisions, TODOs) — no prose\n"
      "Do not invent details. Prefer paths and concrete outcomes.\n\n"
      "<conversation>\n%s\n</conversation>")
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

(defun emagent-chat--context-fill-percent ()
  "Return current session context fill percent, or nil when unknown.

When the provider does not report context usage (Cursor ACP today),
estimate from MCP payload bytes plus Org transcript size against
`emagent-acp-ctx-proxy-size'.  Transcript chars are scaled by
`emagent-acp-ctx-proxy-buffer-divisor', then mapped with
`1 - exp(-raw/size)' so the percentage grows with the session without
a hard low cap (which looked static) or uncapped blow-ups."
  (or
   (when-let* ((pair (and (fboundp 'emagent-acp-context-usage)
                          (emagent-acp-context-usage)))
               (used (car pair))
               (size (cdr pair))
               ((and (numberp used) (numberp size) (> size 0))))
     (min 100.0 (* 100.0 (/ (float used) size))))
   (when-let* ((size (and (boundp 'emagent-acp-ctx-proxy-size)
                          emagent-acp-ctx-proxy-size))
               ((and (integerp size) (> size 0)))
               (mcp (if (fboundp 'emagent-tools-age-bytes)
                        (or (emagent-tools-age-bytes) 0)
                      0))
               (mcp-tok (/ (max 0 mcp) 4))
               (div (if (boundp 'emagent-acp-ctx-proxy-buffer-divisor)
                        emagent-acp-ctx-proxy-buffer-divisor
                      40))
               (buf-tok (if (and (integerp div) (> div 0))
                            (/ (buffer-size) div)
                          0))
               (raw (+ mcp-tok buf-tok))
               ((> raw 0))
               ;; Soft saturation: raw≈size → ~63%, grows toward 100%.
               (est (floor (* (float size)
                             (- 1.0 (exp (/ (- (float raw)) (float size))))))))
     (min 100.0 (* 100.0 (/ (float est) size))))))

(defvar-local emagent-chat--last-compact-hint nil
  "Time of the last /compact hint inserted in this buffer, or nil.")

(defun emagent-chat--compact-hint-due-p ()
  "Return non-nil when a compact hint may be shown (cooldown elapsed)."
  (let ((last emagent-chat--last-compact-hint)
        (cooldown (and (boundp 'emagent-acp-compact-hint-cooldown)
                       emagent-acp-compact-hint-cooldown)))
    (or (null last)
        (null cooldown)
        (<= cooldown 0)
        (>= (float-time (time-subtract (current-time) last))
            cooldown))))

(defun emagent-chat--reset-compact-hint-cooldown ()
  "Allow the next high-context turn to show a /compact hint again."
  (setq emagent-chat--last-compact-hint nil))

(defun emagent-chat--maybe-insert-compact-hint ()
  "Append a /compact suggestion under the open Response when context is high.

Controlled by `emagent-acp-compact-hint-threshold' and
`emagent-acp-compact-hint-cooldown'.  Also triggers when MCP payload
bytes cross `emagent-tools-age-bytes-threshold'."
  (let* ((threshold (and (boundp 'emagent-acp-compact-hint-threshold)
                         emagent-acp-compact-hint-threshold))
         (pct (emagent-chat--context-fill-percent))
         (bytes-hint (and (fboundp 'emagent-tools-age-bytes-hint-p)
                          (emagent-tools-age-bytes-hint-p)))
         (ctx-na (emagent-chat--stat :ctx-unavailable)))
    (when (and (integerp threshold)
               (> threshold 0)
               (emagent-chat--compact-hint-due-p)
               (or bytes-hint (and pct (>= pct threshold)))
               (emagent-chat--open-response-p))
      (when-let* ((bounds (emagent-chat--response-body-bounds))
                  (start (car bounds))
                  (end (cdr bounds)))
        (let ((body (buffer-substring-no-properties start end)))
          (unless (string-match-p "consider.*/compact" body)
            (let* ((inhibit-read-only t)
                   (msg (cond
                         (bytes-hint
                          "/MCP tool payloads are large; consider ~/compact~./")
                         ((and ctx-na (not pct))
                          "/Context usage unavailable; consider ~/compact~ if replies degrade./")
                         (t
                          (format "/context is over %d%%, consider ~/compact~./"
                                  threshold)))))
              (emagent-chat--writable)
              (save-excursion
                (goto-char end)
                (skip-chars-backward " \t\n" start)
                (insert "\n\n" msg "\n")
                (when emagent-chat--assistant-marker
                  (set-marker emagent-chat--assistant-marker (point))))
              (setq emagent-chat--last-compact-hint (current-time))
              (when (fboundp 'emagent-tools-age-clear-bytes-hint)
                (emagent-tools-age-clear-bytes-hint)))))))))

(defun emagent-chat--close-finished-response ()
  "Reset response markers and insert the next user heading stub.

Called once the ACP finish render has settled (no newer assistant text)."
  (emagent-chat--flush-response-pending t)
  (emagent-chat--maybe-insert-compact-hint)
  (when (fboundp 'emagent-archive-on-turn-end)
    (emagent-archive-on-turn-end))
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
            (when emagent-chat--finish-close
              (progn (emagent-chat--maybe-insert-compact-hint) (when (fboundp 'emagent-archive-on-turn-end) (emagent-archive-on-turn-end))))
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

(defun emagent-chat--lang-from-filename (file)
  "Return an org babel language tag for FILE, or nil when unknown."
  (pcase (downcase (or (file-name-extension file) ""))
    ("el" "elisp")
    ("elc" "elisp")
    ("org" "org")
    ("py" "python")
    ("js" "javascript")
    ("ts" "typescript")
    ("sh" "shell")
    ("bash" "shell")
    ("zsh" "shell")
    ("java" "java")
    ("go" "go")
    ("rs" "rust")
    ("rb" "ruby")
    ("json" "json")
    ("yaml" "yaml")
    ("yml" "yaml")
    ("md" "markdown")
    ("mermaid" "mermaid")
    (_ nil)))

(defun emagent-chat--lang-from-src-tag (tag)
  "Return a normalized org babel language tag for TAG."
  (cond
   ((string-match "\\`[0-9]+:[0-9]+:\\(.+\\)\\'" tag)
    (or (emagent-chat--lang-from-filename (match-string 1 tag)) "text"))
   ((member tag '("elisp" "emacs-lisp")) "elisp")
   (t tag)))

(defun emagent-chat--table-row-p (line)
  "Return non-nil when LINE resembles an org/markdown table row."
  (let ((trimmed (string-trim line)))
    ;; Require at least two chars so a lone "|" is not both prefix and suffix;
    ;; callers do (substring trimmed 1 -1), which signals on a 1-char string.
    (and (>= (length trimmed) 2)
         (string-prefix-p "|" trimmed)
         (string-suffix-p "|" trimmed))))

(defun emagent-chat--table-hline-p (line)
  "Return non-nil when LINE is a table separator row."
  (when (emagent-chat--table-row-p line)
    (let ((inner (substring (string-trim line) 1 -1)))
      (and (not (string-empty-p inner))
           ;; Markdown |---|---| and org |---+---| hlines; reject data rows.
           (not (string-match-p "[^-+:|[:space:]]" inner))))))

(defun emagent-chat--table-ncols (line)
  "Return the number of columns in table row LINE."
  (length (split-string (substring (string-trim line) 1 -1) "|" t)))

(defun emagent-chat--org-table-hline (ncols)
  "Return an org table separator row for NCOLS columns."
  (concat "|" (mapconcat (lambda (_) "---------")
                         (number-sequence 1 ncols) "+") "|"))

(defun emagent-chat--normalize-table-row (line)
  "Normalize spacing in a single table row.

Arguments: LINE."
  (let* ((trimmed (string-trim line))
         (cells (mapcar #'string-trim
                        (split-string (substring trimmed 1 -1) "|" t))))
    (concat "|" (mapconcat (lambda (cell) (format " %s " cell))
                           cells "|") "|")))

(defun emagent-chat--fix-table-block (rows)
  "Convert markdown table ROWS into a valid org table block."
  (let* ((body (if (and (> (length rows) 1)
                        (emagent-chat--table-hline-p (nth 1 rows)))
                   (append (list (car rows)) (nthcdr 2 rows))
                 rows))
         (normalized (mapcar #'emagent-chat--normalize-table-row body))
         (ncols (emagent-chat--table-ncols (car normalized)))
         (hline (emagent-chat--org-table-hline ncols)))
    (append (list (car normalized) hline) (cdr normalized))))

(defun emagent-chat--convert-markdown-tables (text)
  "Convert markdown-style pipe tables into org tables.

Arguments: TEXT."
  (let* ((lines (split-string text "\n"))
         (parts nil)
         (i 0)
         (n (length lines)))
    (while (< i n)
      (let ((line (nth i lines)))
        (if (emagent-chat--table-row-p line)
            (let ((start i))
              (while (and (< i n) (emagent-chat--table-row-p (nth i lines)))
                (setq i (1+ i)))
              (let* ((rows (seq-subseq lines start i))
                     (fixed (emagent-chat--fix-table-block rows))
                     (prev (car parts)))
                (when (and prev (not (string-empty-p prev))
                           (not (emagent-chat--table-row-p prev)))
                  (push "" parts))
                (push (mapconcat #'identity fixed "\n") parts)
                (when (< i n)
                  (push "" parts))))
          (progn
            (push line parts)
            (setq i (1+ i))))))
    (mapconcat #'identity (nreverse parts) "\n")))

(defun emagent-chat--convert-markdown-headings (text)
  "Convert markdown ### / ## headings in TEXT to org headings.

A prose-only transform: it is applied outside src blocks so a fenced
\"## Title\" is left as literal code."
  (replace-regexp-in-string
   "^## \\(.*\\)$" "** \\1"
   (replace-regexp-in-string "^###+ \\(.*\\)$" "* \\1" text)))

(defun emagent-chat--convert-markdown-prose (text)
  "Convert markdown links, bold, and glued sentences in prose TEXT.

Prose-only transforms applied outside src blocks, so a fenced `arr[i](fn)' or
`path.Join' is left as literal code.  Bold runs after inline-code conversion
in `emagent-chat--convert-agent-markup' so `**`code`**' becomes
`*=code=*' rather than leaving markdown stars around org verbatim."
  (let ((result
         ;; Markdown links [text](url) → [[url][text]] org links.
         (replace-regexp-in-string
          "\\[\\([^][\n]+\\)\\](\\([^)\n]+\\))"
          "[[\\2][\\1]]"
          text)))
    ;; Markdown bold **text** → org bold *text*.  Same pattern as the
    ;; streaming insert path; without this, finish rewrite reintroduces
    ;; markdown stars around already-converted =verbatim= spans.
    (setq result
          (replace-regexp-in-string
           "\\*\\*\\([^*\n]+\\)\\*\\*" "*\\1*" result))
    ;; Insert a space when sentence-ending punctuation is immediately followed
    ;; by a capital letter.  Bind case-fold-search=nil so [A-Z] only matches
    ;; true uppercase — without this, domain names like github.corp get spaces.
    ;; Require a lowercase letter right before the punctuation too, so an
    ;; ALL-CAPS token like VDUNGEON.DAT or a CONSTANT.EXT filename is left
    ;; alone — real glued sentences ("Done.Next step") end in lowercase.
    (let ((case-fold-search nil))
      (replace-regexp-in-string
       "\\([a-z]\\)\\([.?!]\\)\\([A-Z]\\)"
       "\\1\\2 \\3"
       result))))

(defun emagent-chat--url-like-p (text)
  "Return non-nil if TEXT is a clickable URL or Org link target."
  (string-match-p "\\`\\(?:https?://\\|file:\\|emagent://\\|mailto:\\)"
                  text))

(defun emagent-chat--convert-inline-code-spans (text)
  "Replace markdown `code` spans in TEXT with org markup.

URL-like spans become org links (`[[url]]') so they stay clickable; other
spans become org verbatim (`=code=').

Use a literal replacement: lambda return values are still scanned for
replacement escapes, so spans containing backslashes would error."
  (replace-regexp-in-string
   "`\\([^`\n]+\\)`"
   (lambda (match)
     (let ((code (substring match 1 -1)))
       (if (emagent-chat--url-like-p code)
           (format "[[%s]]" code)
         (format "=%s=" code))))
   text nil t))

(defun emagent-chat--normalize-response-spacing (text)
  "Normalize spacing around blocks and tables in TEXT.

Heading conversion lives in `emagent-chat--convert-markdown-headings' and
link/sentence conversion in `emagent-chat--convert-markdown-prose'
\(both applied outside src blocks); this handles only whole-text spacing
that must see the block and table markers."
  (let ((result (replace-regexp-in-string "\\`[\n\r]+" "" text)))
    (setq result
          (replace-regexp-in-string
           "\\(\n[ \t]*\\)+#\\+END_SRC"
           "\n#+END_SRC"
           result))
    (setq result
          (replace-regexp-in-string
           "#\\+END_SRC[ \t]*\n\\([^[:space:]\n]\\)"
           "#+END_SRC\n\n\\1"
           result))
    (setq result
          (replace-regexp-in-string
           "\\([^[:space:]\n]\\)\n#\\+BEGIN_SRC "
           "\\1\n\n#+BEGIN_SRC "
           result))
    (setq result
          (replace-regexp-in-string
           "\\([^[:space:]\n|]\\)\n\\(|\\)"
           "\\1\n\n\\2"
           result))
    (setq result
          (replace-regexp-in-string
           "\\(|[^\n]*|\n\\)\\([^|\n#]\\)"
           "\\1\n\n\\2"
           result))
    result))

(defun emagent-chat--escape-reasoning-line (line)
  "Escape LINE so Org will not parse it as a headline or keyword.

Reasoning is rendered as the body of the `** Thinking' subsection (not inside
a block), so a leading `*' or `#' must be neutralized with a leading space.
Exception: `#+begin_src'/`#+end_src' markers inserted by our fence conversion
must not be escaped — they need to remain valid org src block delimiters."
  (cond
   ;; Preserve org src block markers produced by emagent-chat--split-fences.
   ((string-match-p "\\`#\\+\\(?:begin_src\\|end_src\\)\\b" (downcase line))
    line)
   ((string-match-p "\\`[ \t]*[*#]" line)
    (concat " " line))
   (t line)))

(defun emagent-chat--escape-reasoning-text (text &optional mid-line)
  "Convert markdown markup in reasoning TEXT to org before inserting it.
Applied once per flush (text is already outside any code fence at this point).
Order matters: heading and bold conversions run before escape-reasoning-line
so the escape pass never sees raw # / ** markers.

With MID-LINE non-nil, TEXT continues an existing buffer line rather than
starting one, so the first line's leading `*'/`#' is left unescaped: org only
mis-parses such a marker at a real line start, and escaping it here would
inject a spurious leading space after the resumed text (e.g. a `**bold**' span
held across a streaming boundary)."
  (if (string-empty-p (or text ""))
      ""
    (let* (;; Markdown links [text](url) → [[url][text]]
           (text (replace-regexp-in-string
                  "\\[\\([^][\n]+\\)\\](\\([^)\n]+\\))"
                  "[[\\2][\\1]]" text))
           ;; Markdown headings → bold text
           (text (replace-regexp-in-string
                  "^#\\{1,6\\} \\(.*\\)$" "*\\1*" text))
           ;; Markdown bold **text** → org bold *text*
           (text (replace-regexp-in-string
                  "\\*\\*\\([^*\n]+\\)\\*\\*" "*\\1*" text))
           ;; Markdown inline code `code` → org verbatim =code=
           (text (emagent-chat--convert-inline-code-spans text))
           (lines (split-string text "\n")))
      ;; Finally escape any remaining # / * at line starts so org
      ;; does not mis-parse them as keywords or headings.
      (mapconcat #'identity
                 (cl-loop for line in lines
                          for first = t then nil
                          collect (if (and mid-line first)
                                      line
                                    (emagent-chat--escape-reasoning-line line)))
                 "\n"))))

(defconst emagent-chat--src-block-re
  "^[ \t]*#\\+BEGIN_SRC\\(?:.*\n\\)*?[ \t]*#\\+END_SRC[ \t]*$"
  "Match a complete org src block from BEGIN_SRC line to END_SRC line.")

(defun emagent-chat--escape-src-body (body)
  "Comma-escape lines in BODY that Org would misread as src-block delimiters.

A code block that documents Org can contain a literal `#+END_SRC' (or
`#+BEGIN_SRC') line; left as-is it closes the generated block early for both
Org's own parser and `emagent-chat--src-block-re', mangling everything after it.
Prefixing the delimiter with a comma is Org's escape convention (stripped on
export/edit), so the line stays part of the block body."
  (let ((case-fold-search t))
    (replace-regexp-in-string
     "^\\([ \t]*\\)\\(#\\+\\(?:BEGIN\\|END\\)_SRC\\)"
     "\\1,\\2"
     body)))

(defun emagent-chat--map-outside-src-blocks (fn text)
  "Return TEXT with FN applied to every span outside org src blocks.

Src blocks (`#+BEGIN_SRC'…`#+END_SRC') are emitted verbatim, so markdown
transforms run over prose only and never rewrite code-block interiors (a fenced
\"## Title\" or backtick span must survive untouched)."
  (let ((case-fold-search t) (pos 0) (len (length text)) (out nil))
    (while (and (< pos len)
                (string-match emagent-chat--src-block-re text pos))
      (let ((mb (match-beginning 0)) (me (match-end 0)))
        (push (funcall fn (substring text pos mb)) out)
        (push (substring text mb me) out)
        (setq pos me)))
    (push (funcall fn (substring text pos)) out)
    (apply #'concat (nreverse out))))

(defun emagent-chat--demote-response-headings (text)
  "Demote every Org headline in TEXT to level >= 3.

The assistant answer is rendered under the level-2 `** Response' subsection, so
its own headings must nest beneath it rather than starting new turns.  Headings
inside src blocks are left alone."
  (emagent-chat--map-outside-src-blocks
   (lambda (s)
     (replace-regexp-in-string
      "^\\*+ "
      (lambda (m)
        (if (< (1- (length m)) 3) "*** " m))
      s))
   text))

(defun emagent-chat--convert-code-fences (text)
  "Convert markdown ``` fences in TEXT to org src blocks."
  (let ((pos 0)
        (parts nil))
    (while (string-match "```" text pos)
      (let ((fence-start (match-beginning 0))
            (after-fence (match-end 0)))
        (push (substring text pos fence-start) parts)
        (if (not (string-match "\n" text after-fence))
            (progn (push "```" parts) (setq pos after-fence))
          (let* ((tag-end (match-beginning 0))
                 (body-start (match-end 0))
                 (tag (string-trim (substring text after-fence tag-end))))
            (if (not (string-match "```" text body-start))
                (let ((body (substring text body-start)))
                  (push (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                                (emagent-chat--lang-from-src-tag tag)
                                (emagent-chat--escape-src-body body))
                        parts)
                  (setq pos (length text)))
              (let* ((body-end (match-beginning 0))
                     (close-end (match-end 0))
                     (body (substring text body-start body-end)))
                (push (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                              (emagent-chat--lang-from-src-tag tag)
                              (emagent-chat--escape-src-body body))
                      parts)
                (setq pos close-end)))))))
    (push (substring text pos) parts)
    (apply #'concat (nreverse parts))))

(defun emagent-chat--close-unclosed-org-src (text)
  "Append missing #+END_SRC lines for unclosed org src blocks in TEXT."
  (let ((lines (split-string text "\n"))
        (open nil)
        (result nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^#\\+BEGIN_SRC\\b" line)
        (when open (push "#+END_SRC" result))
        (setq open t)
        (push line result))
       ((string-match-p "^#\\+END_SRC\\b" line)
        (setq open nil)
        (push line result))
       (t (push line result))))
    (when open (push "#+END_SRC" result))
    (mapconcat #'identity (nreverse result) "\n")))

(defun emagent-chat--fix-org-src-citations (text)
  "Rewrite file-citation language tags in org src block headers.

Arguments: TEXT."
  (let ((start 0)
        (parts nil))
    (while (string-match
            "^#\\+BEGIN_SRC +\\([0-9]+:[0-9]+:\\([^ \t\n]+\\)\\)\n"
            text start)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (file (match-string 2 text)))
        (push (substring text start match-start) parts)
        (push (format "#+BEGIN_SRC %s\n"
                      (or (emagent-chat--lang-from-filename file) "text"))
              parts)
        (setq start match-end)))
    (push (substring text start) parts)
    (apply #'concat (nreverse parts))))

(defun emagent-chat--normalize-elisp-src-tags (text)
  "Rewrite emacs-lisp org src headers to elisp for font-lock.

Arguments: TEXT."
  (replace-regexp-in-string
   "#\\+BEGIN_SRC emacs-lisp\\b"
   "#+BEGIN_SRC elisp"
   text))

(defun emagent-chat--unwrap-outer-org-src (text)
  "Remove a single outer #+BEGIN_SRC org wrapper around all of TEXT."
  (let ((trimmed (string-trim text)))
    (if (string-match (concat "\\`#\\+BEGIN_SRC +org\\s-*\n"
                              "\\(\\(?:.\\|\n\\)*\\)"
                              "\n#\\+END_SRC\\s-*\\'")
                      trimmed)
        (match-string 1 trimmed)
      text)))

(defun emagent-chat--convert-agent-markup (text)
  "Convert leftover markdown markup in agent responses to org.

Code fences are converted to src blocks first; the remaining prose transforms
\(inline backticks, bold, tables, heading/spacing normalization) then run only
outside those blocks, so a fenced backtick span, `## heading', or table row
is never rewritten inside code.

Arguments: TEXT."
  (let* ((text (replace-regexp-in-string "\r\n?" "\n" text))
         (fenced (emagent-chat--normalize-elisp-src-tags
                  (emagent-chat--convert-code-fences
                   (emagent-chat--fix-org-src-citations
                    (emagent-chat--unwrap-outer-org-src text)))))
         ;; Prose-corrupting transforms (inline code, bold, headings, tables,
         ;; links, sentence spacing) run only outside src blocks so code survives.
         (prose (emagent-chat--map-outside-src-blocks
                 (lambda (s)
                   (emagent-chat--convert-markdown-prose
                    (emagent-chat--convert-markdown-tables
                     (emagent-chat--convert-markdown-headings
                      (emagent-chat--convert-inline-code-spans s)))))
                 fenced)))
    ;; Whole-text spacing normalization needs to see the block/table markers.
    (emagent-chat--close-unclosed-org-src
     (emagent-chat--normalize-response-spacing prose))))

(defvar-local emagent-chat--font-lock-deferred-p nil
  "When non-nil, defer org font-lock until the emagent buffer is active.")

(defun emagent-chat--align-org-tables-in-region (start end)
  "Align every org table between START and END.

Uses a line-shape check instead of `org-at-table-p' so we never invoke
`org-element-at-point' on every `|' match (that pegged Emacs at 100% CPU
when deferred align ran from redisplay hooks on large chat buffers)."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "^|" nil t)
        (beginning-of-line)
        ;; Cheap shape check — never `org-at-table-p' (org-element).
        (if (looking-at-p "^|.*|")
            (condition-case nil
                (progn
                  (org-table-align)
                  (goto-char (or (org-table-end nil) (point-max))))
              (error (forward-line 1)))
          (forward-line 1))))))

(defun emagent-chat--font-lock-region-start ()
  "Return a start position for incremental font-lock in the current buffer.

When a response is open, only re-fontify a trailing window of that response.
Long tool-heavy turns accumulate large `#+begin_src' blocks; re-fontifying
them all on every stream/tool update blocked the event loop (the \"hang\" on
large sessions).  Between turns, fall back to the user zone — never the
whole session log."
  (let* ((response-start
          (and (boundp 'emagent-chat--response-body-start)
               emagent-chat--response-body-start
               (marker-position emagent-chat--response-body-start)))
         (user-start
          (and (fboundp 'emagent-chat--user-zone-start)
               (emagent-chat--user-zone-start)))
         (floor (or response-start user-start (point-min)))
         ;; ~12k chars covers recent thought/tool/response chunks without
         ;; redoing earlier src blocks in the same turn.
         (window 12000))
    (if response-start
        (max floor (- (point-max) window))
      floor)))

(defun emagent-chat--font-lock-response-tail ()
  "Re-fontify the response tail without flushing the entire session buffer."
  (when font-lock-mode
    (save-excursion
      (let ((start (emagent-chat--font-lock-region-start))
            (end (point-max)))
        (when (< start end)
          (condition-case nil
              (font-lock-fontify-region start end)
            (error nil)))))))

(defun emagent-chat--maybe-font-lock-flush ()
  "Run org font-lock on the response tail when safe; defer otherwise.

Defer when the buffer is not selected, and also while an ACP turn is
busy or finishing — fontifying large Thinking/tool regions on every
chunk pegs the command loop for both Cursor and Claude."
  (if (and (emagent-chat--buffer-active-p)
           (not (emagent-acp-turn-in-flight-p)))
      (progn
        (setq emagent-chat--font-lock-deferred-p nil)
        (emagent-chat--font-lock-response-tail))
    (setq emagent-chat--font-lock-deferred-p t)))

(defun emagent-chat--flush-deferred-font-lock ()
  "Font-lock the response tail when a deferred flush was requested.

Skipped while an ACP turn is still in flight so settle happens once."
  (when (and emagent-chat--font-lock-deferred-p
             (emagent-chat--buffer-active-p)
             (not (emagent-acp-turn-in-flight-p)))
    (setq emagent-chat--font-lock-deferred-p nil)
    (emagent-chat--font-lock-response-tail)))

(defvar-local emagent-chat--table-align-start nil
  "Marker for the start of a pending idle org-table align region, or nil.")

(defvar-local emagent-chat--table-align-end nil
  "Marker for the end of a pending idle org-table align region, or nil.")

(defvar-local emagent-chat--table-align-timer nil
  "Idle timer that aligns `emagent-chat--table-align-start'..end, or nil.")

(defun emagent-chat--cancel-scheduled-table-align ()
  "Cancel any pending idle org-table alignment for this buffer."
  (when emagent-chat--table-align-timer
    (cancel-timer emagent-chat--table-align-timer)
    (setq emagent-chat--table-align-timer nil))
  (when (markerp emagent-chat--table-align-start)
    (set-marker emagent-chat--table-align-start nil))
  (when (markerp emagent-chat--table-align-end)
    (set-marker emagent-chat--table-align-end nil))
  (setq emagent-chat--table-align-start nil
        emagent-chat--table-align-end nil))

(defun emagent-chat--run-scheduled-table-align ()
  "Align the pending org-table region, if any, without dirtying the buffer."
  (setq emagent-chat--table-align-timer nil)
  (when-let* ((start emagent-chat--table-align-start)
              (end emagent-chat--table-align-end)
              (s (marker-position start))
              (e (marker-position end))
              ((< s e)))
    (setq emagent-chat--table-align-start nil
          emagent-chat--table-align-end nil)
    (set-marker start nil)
    (set-marker end nil)
    (let ((was-modified (buffer-modified-p))
          (inhibit-read-only t))
      (unwind-protect
          (progn
            (when (fboundp 'emagent-chat--writable)
              (emagent-chat--writable))
            (save-excursion
              (emagent-chat--align-org-tables-in-region s e)))
        (set-buffer-modified-p was-modified)))))

(defun emagent-chat--schedule-align-org-tables (start end)
  "Schedule alignment of org tables between START and END.

Uses `run-at-time' (not an idle timer): continuous ACP/MCP process I/O
resets Emacs idle, so `run-with-idle-timer' deferred aligns often never
fired.  Finish prefers a synchronous align; this helper remains for
callers that must not block the current filter.  Never runs from
redisplay/`window-configuration-change-hook' — that path hung Emacs on
large chats via `org-element' parses."
  (when (and (integer-or-marker-p start)
             (integer-or-marker-p end)
             (< (if (markerp start) (marker-position start) start)
                (if (markerp end) (marker-position end) end)))
    (emagent-chat--cancel-scheduled-table-align)
    (setq emagent-chat--table-align-start (copy-marker start t)
          emagent-chat--table-align-end (copy-marker end nil))
    (let ((buf (current-buffer)))
      (setq emagent-chat--table-align-timer
            (run-at-time
             0 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (emagent-chat--run-scheduled-table-align)))))))))

(defconst emagent-chat--model-link-re
  "\\[\\[emagent://\\([^][]+\\)\\]\\(?:\\[\\([^][]*\\)\\]\\)?\\]"
  "Matches a `/model' override link `[[emagent://AGENT/MODEL][short]]'.
Group 1 is the link target `AGENT/MODEL' (shown on hover); group 2 the
short model label shown as the link text.  Being an org link, the
marker is fontified by org, survives saving the session to disk, and
reveals the full agent/model id on hover.  The `emagent://' scheme
tags this as the model marker so unrelated links a user pastes are not
mistaken for it.")

(defun emagent-chat--model-link-escape (text)
  "Percent-encode TEXT for use in an emagent:// model-link path."
  (url-hexify-string (or text "")))

(defun emagent-chat--model-link-unescape (text)
  "Decode a percent-encoded model-link path segment TEXT."
  (url-unhex-string (or text "")))

(defun emagent-chat--model-link-description (label model-id)
  "Return Org-safe visible link text for LABEL or MODEL-ID.

Org link descriptions cannot contain square brackets, so picker
bracket suffixes are rewritten as parentheses."
  (let ((text (substring-no-properties
               (or label (emagent-model-choice-label model-id) model-id))))
    (replace-regexp-in-string
     "\\[\\([^]]*\\)\\]" "(\\1)" text)))

(defun emagent-chat--model-link-parse-path (path)
  "Return (MODEL-ID . SPEC) parsed from emagent:// PATH.

SPEC is ((CONFIG-ID . VALUE) ...) including the model pair.  Query
pairs encode non-model config options selected in the variant picker."
  (let* ((path (string-remove-prefix "//" (or path "")))
         (qpos (string-search "?" path))
         (base (if qpos (substring path 0 qpos) path))
         (query (when qpos (substring path (1+ qpos))))
         (slash (string-search "/" base))
         (model-id (emagent-chat--model-link-unescape
                    (if slash (substring base (1+ slash)) base)))
         (pairs nil))
    (when (and query (not (string-empty-p query)))
      (dolist (cell (split-string query "&" t))
        (let* ((eqpos (string-search "=" cell))
               (key (emagent-chat--model-link-unescape
                     (if eqpos (substring cell 0 eqpos) cell)))
               (val (emagent-chat--model-link-unescape
                     (if eqpos (substring cell (1+ eqpos)) ""))))
          (when (and (not (string-empty-p key))
                     (not (equal key "model")))
            (push (cons key val) pairs)))))
    (cons model-id
          (cons (cons "model" model-id) (nreverse pairs)))))

(defun emagent-chat--model-link-query (spec _model-id)
  "Return `?k=v&...' for non-model pairs in SPEC, or empty string."
  (let ((pairs (cl-remove-if
                (lambda (pair) (equal (car pair) "model"))
                spec)))
    (if (null pairs)
        ""
      (concat "?"
              (mapconcat
               (lambda (pair)
                 (concat (emagent-chat--model-link-escape (car pair))
                         "="
                         (emagent-chat--model-link-escape (cdr pair))))
               pairs
               "&")))))

(defun emagent-chat--model-link-path-id (path)
  "Return the model id from a link PATH `AGENT/MODEL' (or bare MODEL).

PATH may carry a leading `//' authority slash from the raw link, and an
optional `?cfg=value' query for non-model config pairs.  The agent is
the first segment; the model id is the rest before any query."
  (car (emagent-chat--model-link-parse-path path)))

(defun emagent-chat--region-turn-model (start end)
  "Return the model id of the first `/model' link between START and END."
  (car (emagent-chat--region-turn-model-info start end)))

(defun emagent-chat--region-turn-apply-spec (start end)
  "Return apply-spec from the first `/model' link between START and END."
  (cdr (emagent-chat--region-turn-model-info start end)))

(defun emagent-chat--region-turn-model-info (start end)
  "Return (MODEL-ID . SPEC) from the first `/model' link in START..END.

SPEC may be nil when the link has no query (model-only override)."
  (save-excursion
    (goto-char start)
    (when (re-search-forward emagent-chat--model-link-re end t)
      (let* ((parsed (emagent-chat--model-link-parse-path
                      (match-string-no-properties 1)))
             (model-id (car parsed))
             (spec (cdr parsed)))
        (cons model-id
              (and spec
                   (> (length spec) 1)
                   spec))))))

(defun emagent-chat--strip-model-links (text)
  "Remove `/model' override links from outgoing TEXT.
The marker is client UI — the slash command is documented as never sent
to the agent."
  (string-trim
   (replace-regexp-in-string
    (concat "[ \t]*" emagent-chat--model-link-re) "" text)))

(defun emagent-chat--model-link (model-id &optional label spec)
  "Return the `/model' marker link for MODEL-ID.

Optional LABEL is the picker selection text (brackets rewritten as
parentheses for Org).  Optional SPEC is ((CONFIG-ID . VALUE) ...);
non-model pairs are stored in the link query so send can reapply them."
  (let* ((agent (emagent-session-agent))
         (short (emagent-chat--model-link-description label model-id))
         (model-esc (emagent-chat--model-link-escape model-id))
         (base (if agent (format "%s/%s" agent model-esc) model-esc))
         (path (concat base (emagent-chat--model-link-query spec model-id))))
    (format "[[emagent://%s][%s]]" path short)))

(defun emagent-chat--follow-model-link (path &optional _prefix)
  "Describe the `/model' override link PATH when activated."
  (message "Model for this turn: %s (delete the link to cancel)"
           (string-remove-prefix "//" path)))

(defun emagent-chat--model-link-help-echo (_window object position)
  "Tooltip for a `/model' link: the `agent/model' target.

Arguments: OBJECT, POSITION."
  (with-current-buffer (if (bufferp object) object (current-buffer))
    (save-excursion
      (goto-char position)
      (when (or (looking-at emagent-chat--model-link-re)
                (and (search-backward "[[" (max (point-min) (- position 200)) t)
                     (looking-at emagent-chat--model-link-re)))
        (format "Model for this turn: %s" (match-string-no-properties 1))))))

(defun emagent-chat--display-path (path &optional project-dir)
  "Return PATH formatted for display in the chat UI.
Under the session project root: ./projectname/relative/path.
Under user home but outside the project: ~/….
Otherwise: the absolute PATH.

Relative paths resolve against the project directory, not
`default-directory' — saving the session file moves `default-directory'
to the session file's directory, which is unrelated to the project.

Arguments: PROJECT-DIR."
  (let* ((project (when-let ((raw (or project-dir
                                      (and (boundp 'emagent-chat-project-directory)
                                           emagent-chat-project-directory)
                                      (emagent-session-store-read-project-property))))
                    (file-truename
                     (file-name-as-directory (expand-file-name raw)))))
         (expanded (file-truename (expand-file-name path project)))
         (home (file-truename (expand-file-name "~")))
         (home-prefix (concat home "/")))
    (cond
     ((and project
           (string-prefix-p project expanded)
           (not (string= expanded (directory-file-name project))))
      (concat "./"
              (file-name-nondirectory (directory-file-name project))
              "/"
              (file-relative-name expanded project)))
     ((string-prefix-p home-prefix expanded)
      (abbreviate-file-name expanded))
     ((string= expanded home)
      "~")
     (t expanded))))

(defun emagent-chat--session-directory ()
  "Return the ACP working directory for the current emagent buffer.
Reads #+EMAGENT_PROJECT from the buffer header if set, falling back to
variable `buffer-file-name', `project-current' or `user-emacs-directory'."
  (expand-file-name
   (or (emagent-session-store-read-project-property)
       (and buffer-file-name (file-name-directory buffer-file-name))
       (if (boundp 'emagent-default-directory) emagent-default-directory)
       (and (fboundp 'project-current)
            (when-let ((proj (project-current nil default-directory)))
              (project-root proj)))
       user-emacs-directory)))

(defun emagent-tools--apply-button-line-keymap (beg end keymap)
  "Attach KEYMAP to the button line spanning BEG through END (exclusive).
Shortcuts then work anywhere on that line, including at line beginning."
  (when (and beg end keymap (< beg end))
    (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position))))
      (put-text-property line-beg (1- end) 'keymap keymap))))

(defun emagent-tools--goto-first-button (pos)
  "Move point to the first button at or after POS; return non-nil on success."
  (when pos
    (goto-char pos)
    (or (button-at (point))
        (when-let ((btn (next-button (max (1- pos) (point-min)))))
          (goto-char (button-start btn))
          t))))

(defun emagent-tools--focus-inline-buttons (chat-buffer button-pos)
  "Move point to BUTTON-POS in CHAT-BUFFER so button keymaps accept shortcuts."
  (when (and chat-buffer (buffer-live-p chat-buffer) button-pos)
    (when-let ((pos (if (markerp button-pos)
                        (marker-position button-pos)
                      button-pos)))
      (if-let ((win (get-buffer-window chat-buffer)))
          (progn
            (select-window win)
            (with-current-buffer chat-buffer
              (emagent-tools--goto-first-button pos)
              (recenter -3)))
        (with-current-buffer chat-buffer
          (emagent-tools--goto-first-button pos))))))

(defun emagent-tools--choice-shortcut (value)
  "Return a single-character keyboard shortcut for VALUE, or nil."
  (cond
   ((memq value '(yes :allow-once :accept)) "y")
   ((memq value '(no :deny :reject)) "n")
   ((eq value :allow-session) "s")
   ((eq value :allow-always) "w")
   ((memq value '(all :allow-all)) "a")
   (t nil)))

(defun emagent-tools--buttons-prompt (prompt choices chat-buffer callback &optional preamble)
  "Insert optional PREAMBLE, PROMPT, and CHOICES as buttons in CHAT-BUFFER.
CHOICES is a list of (LABEL . VALUE) pairs.  Non-blocking: inserts the dialog
and returns immediately.  CALLBACK is called with the VALUE when a button is
clicked.  Falls back to `completing-read' (synchronous) when CHAT-BUFFER is
nil or dead, calling CALLBACK with the chosen value.

Accept/reject choices bind both lower- and upper-case Y/N.  Labels show the
shortcut in parentheses.  When a trailing `* user>' stub is present, the
dialog is inserted above it rather than after it."
  (if (not (and chat-buffer (buffer-live-p chat-buffer)))
      (let* ((labels (mapcar #'car choices))
             (label (completing-read (concat prompt " ") labels nil t)))
        (funcall callback (cdr (assoc label choices))))
    (let (start-mark end-mark first-button (responded nil))
      (let ((do-respond
             (lambda (v)
               (unless responded
                 (setq responded t)
                 (when (and start-mark end-mark
                            (marker-buffer start-mark)
                            (marker-buffer end-mark))
                   (with-current-buffer chat-buffer
                     (let ((inhibit-read-only t))
                       (when (fboundp 'emagent-chat--writable)
                         (funcall #'emagent-chat--writable))
                       (delete-region (marker-position start-mark)
                                      (marker-position end-mark)))))
                 (funcall callback v)))))
        (with-current-buffer chat-buffer
          (let ((inhibit-read-only t))
            (when (fboundp 'emagent-chat--writable)
              (funcall #'emagent-chat--writable))
            (goto-char
             ;; Only park above a real trailing * user> stub.  Bare
             ;; user-zone-start can be point-min when no response exists
             ;; yet, which would put the dialog at the buffer head.
             (let ((zone (and (fboundp 'emagent-chat--user-zone-start)
                              (emagent-chat--user-zone-start))))
               (if (and zone
                        (fboundp 'emagent-chat--user-heading-at-point-p)
                        (save-excursion
                          (goto-char zone)
                          (emagent-chat--user-heading-at-point-p)))
                   zone
                 (point-max))))
            (unless (bolp) (insert "\n"))
            (setq start-mark (copy-marker (point) nil))
            (when preamble (insert preamble))
            (insert "\n" prompt "\n")
            ;; Build keymap with all shortcuts BEFORE inserting buttons,
            ;; then pass it to each insert-button so the button's own
            ;; overlay keymap contains our shortcuts (higher priority than
            ;; any external overlay we add afterward).
            (let ((btn-keymap (make-sparse-keymap)))
              (set-keymap-parent btn-keymap button-map)
              ;; First pass: define all shortcuts in btn-keymap
              (dolist (choice choices)
                (when-let ((key (emagent-tools--choice-shortcut (cdr choice))))
                  (let ((handler
                         (let ((vv (cdr choice)))
                           (lambda ()
                             (interactive)
                             (funcall do-respond vv)))))
                    (define-key btn-keymap (kbd key) handler)
                    (define-key btn-keymap (kbd (upcase key)) handler))))
              ;; Second pass: insert buttons with btn-keymap as their keymap
              (dolist (choice choices)
                (let* ((v (cdr choice))
                       (key (emagent-tools--choice-shortcut v))
                       (label (if key
                                  (format "[%s (%s)]" (car choice) key)
                                (concat "[" (car choice) "]"))))
                  (unless first-button
                    (setq first-button (copy-marker (point) nil)))
                  (insert-button
                   label
                   'keymap btn-keymap
                   'action (lambda (_b) (funcall do-respond v))
                   'follow-link t)
                  (insert "  ")))
              (insert "\n")
              (setq end-mark (copy-marker (point) nil))
              (when first-button
                (emagent-tools--apply-button-line-keymap
                 (marker-position first-button)
                 (marker-position end-mark)
                 btn-keymap))
              ;; Stop sticky follow so later tool/stream inserts do not
              ;; yank point off the dialog (Y/N keymap needs point here).
              (when (boundp 'emagent-chat--follow-output)
                (setq emagent-chat--follow-output nil)))))
        (emagent-tools--focus-inline-buttons chat-buffer first-button)))))

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

(defun emagent-chat--buffer-active-p (&optional buffer)
  "Return non-nil when BUFFER is displayed in the selected window."
  (let ((buf (or buffer (current-buffer))))
    (and (window-live-p (selected-window))
         (eq buf (window-buffer (selected-window))))))

(defalias 'emagent-chat--buffer-visible-p 'emagent-chat--buffer-active-p)

(defun emagent-chat--buffer-displayed-p (&optional buffer)
  "Return non-nil when BUFFER is shown in a window on a visible frame.

Unlike `emagent-chat--buffer-active-p', this is true even when the buffer is
not in the selected window (e.g. side-by-side with another buffer, or while
Emacs itself is unfocused).  It is nil only when no visible frame displays
the buffer (every window hidden or the frame iconified)."
  (and (get-buffer-window-list (or buffer (current-buffer)) nil 'visible) t))

(defun emagent-chat--follow-output-pos ()
  "Return the buffer position streaming output should keep in view."
  (or (when (fboundp 'emagent-chat--open-response-body-bounds)
        (when-let ((bounds (emagent-chat--open-response-body-bounds)))
          (cdr bounds)))
      (point-max)))

(defun emagent-chat--ensure-follow-window (&optional buffer)
  "Arm sticky follow and scroll BUFFER's window to the live output end.

Call after opening a response (send or quiet Build).  Preparing/Thinking
are inserted without `emagent-chat--with-streaming-view', so without this
the first stream chunk can see the follow position off-screen and clear
sticky follow before any recenter runs."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (setq emagent-chat--follow-output t)
      (let ((pos (emagent-chat--follow-output-pos)))
        (goto-char pos)
        (when-let ((win (get-buffer-window buf 'visible)))
          (set-window-point win pos)
          (when (eq win (selected-window))
            (recenter -1)))))))

(defun emagent-chat--live-tail-start ()
  "Return start of the live exchange (prompt + open response), or nil."
  (when (and (fboundp 'emagent-chat--open-response-begin)
             (fboundp 'emagent-chat--user-heading-re))
    (when-let ((begin (emagent-chat--open-response-begin)))
      (save-excursion
        (goto-char begin)
        (if (re-search-backward (emagent-chat--user-heading-re) nil t)
            (line-beginning-position)
          begin)))))

(defun emagent-chat--window-at-bottom-p (window)
  "Return non-nil when WINDOW should follow newly inserted chat output.

Follow when point is on the live prompt/response and either sticky follow
is armed or the window sits on the live end.  Exact `point-max' alone is
not enough: after send, point often remains on the prompt while the
Preparing/Thinking scaffold grows past it.

Sticky follow survives the end briefly leaving the window (Preparing is
inserted outside streaming-view).  It clears when point leaves the live
exchange.  Mid-buffer points without sticky do not re-arm follow."
  (and window (window-live-p window)
       (eq (window-buffer window) (current-buffer))
       (let* ((wp (window-point window))
              (follow-pos (emagent-chat--follow-output-pos))
              (tail (emagent-chat--live-tail-start))
              (end-visible (or noninteractive
                              (pos-visible-in-window-p follow-pos window)))
              (in-live-tail (if tail (>= wp tail) (= wp (point-max)))))
         (cond
          ((not in-live-tail)
           (when (eq window (selected-window))
             (setq emagent-chat--follow-output nil))
           nil)
          ;; Sticky send/Build follow: keep tracking even if the end left
          ;; the window before the first recenter could run.
          (emagent-chat--follow-output t)
          ((not end-visible) nil)
          ((= wp follow-pos)
           (setq emagent-chat--follow-output t)
           t)
          ((= wp (point-max))
           (setq emagent-chat--follow-output t)
           t)
          (t nil)))))

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
  "Restore scroll state from VIEWS returned by `emagent-chat--save-window-views'.

Windows marked for follow keep newly inserted text in view by moving
their `window-point' to `emagent-chat--follow-output-pos'."
  (dolist (view views)
    (let ((win (plist-get view :window)))
      (when (window-live-p win)
        (if (plist-get view :at-bottom)
            (let ((pos (emagent-chat--follow-output-pos)))
              (set-window-point win pos)
              (with-selected-window win
                (goto-char pos)
                (recenter -1)))
          (set-window-start win (plist-get view :start) t))))))

(defun emagent-chat--with-stable-view (fn)
  "Run FN while preserving window scroll unless already at buffer end."
  (let* ((saved-point (point-marker))
         (saved-windows (emagent-chat--save-window-views))
         (selected (selected-window))
         (follow (cl-some (lambda (v)
                            (and (eq (plist-get v :window) selected)
                                 (plist-get v :at-bottom)))
                          saved-windows)))
    (unwind-protect
        (funcall fn)
      (emagent-chat--restore-window-views saved-windows)
      (unless follow
        (when (marker-position saved-point)
          (goto-char saved-point)))
      (set-marker saved-point nil))))

(defun emagent-chat--with-streaming-view (fn)
  "Run FN during live streaming, following windows already at buffer end.

Windows scrolled away from the end keep their `window-start'; windows that
were showing `point-max' are scrolled to the new end after FN returns.
Inserts use `save-excursion', so this explicit follow is required — Emacs
does not auto-scroll when `window-point' is not at the insertion point."
  (let ((views (emagent-chat--save-window-views)))
    (unwind-protect
        (funcall fn)
      (emagent-chat--restore-window-views views))))

(provide 'emagent-chat-ui)
;;; emagent-chat-ui.el ends here
