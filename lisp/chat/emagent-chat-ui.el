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
;; Shared chat UI helpers, headers, model UI, send/buffer state, and view.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-model)
(require 'emagent-session)
(require 'project)

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
\(never shown) tags this as the model marker so unrelated links a user
pastes are not mistaken for it."
  (let* ((agent (emagent-session-agent))
         (short (or (emagent-model-normalize-id model-id) model-id))
         (path (if agent (format "%s/%s" agent model-id) model-id)))
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
