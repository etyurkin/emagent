;;; emagent-archive.el --- Roll large session buffers into archive chunks -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; When a saved emagent session buffer grows past a size threshold, older
;; closed turns can be moved into numbered sibling files under
;; `NAME-archive/NNN.org'.  The hot buffer keeps a short TOC with links;
;; each chunk links back to the main session file.  Full transcript text
;; is preserved (moved, not deleted).
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emagent-session)

(defgroup emagent-archive nil
  "Archive oversized emagent session buffers."
  :group 'emagent)

(defcustom emagent-archive-auto t
  "When non-nil, roll older turns after a response when over threshold."
  :type 'boolean
  :group 'emagent-archive)

(defcustom emagent-archive-threshold-bytes 1048576
  "Roll archive when the saved session buffer exceeds this many bytes.

Ignored for unsaved (scratch) buffers.  `/archive force' skips this gate."
  :type 'integer
  :group 'emagent-archive)

(defcustom emagent-archive-keep-turns 3
  "Number of trailing user turns to keep in the hot session buffer."
  :type 'integer
  :group 'emagent-archive)

(defcustom emagent-archive-hint-cooldown 600
  "Seconds between unsaved-buffer archive hints in one session buffer."
  :type 'integer
  :group 'emagent-archive)

(defconst emagent-archive--toc-heading "* Archive"
  "Org headline for the hot-buffer archive table of contents.")

(defvar-local emagent-archive--last-unsaved-hint nil
  "Time of the last unsaved-archive hint, or nil.")

(defun emagent-archive--user-heading-re ()
  "Return regexp matching the emagent user heading at line start."
  (format "^\\* %s> ?" (regexp-quote (user-login-name))))

(defun emagent-archive--session-file ()
  "Return the absolute path of the current session file, or nil."
  (when buffer-file-name
    (expand-file-name buffer-file-name)))

(defun emagent-archive--dir (&optional session-file)
  "Return the archive directory path for SESSION-FILE (or current)."
  (when-let ((file (or session-file (emagent-archive--session-file))))
    (expand-file-name
     (concat (file-name-base file) "-archive")
     (file-name-directory file))))

(defun emagent-archive--next-chunk-path (&optional session-file)
  "Return the path of the next unused archive chunk for SESSION-FILE."
  (when-let ((dir (emagent-archive--dir session-file)))
    (let ((n 1)
          path)
      (while (progn
               (setq path (expand-file-name (format "%03d.org" n) dir))
               (file-exists-p path))
        (setq n (1+ n)))
      path)))

(defun emagent-archive--buffer-bytes ()
  "Return the current buffer size in bytes."
  (buffer-size))

(defun emagent-archive--over-threshold-p ()
  "Return non-nil when the buffer exceeds `emagent-archive-threshold-bytes'."
  (and (integerp emagent-archive-threshold-bytes)
       (> emagent-archive-threshold-bytes 0)
       (>= (emagent-archive--buffer-bytes) emagent-archive-threshold-bytes)))

(defun emagent-archive--user-heading-positions (zone)
  "Return buffer positions of user headings at/after ZONE, oldest first."
  (let (pos)
    (save-excursion
      (goto-char zone)
      (while (re-search-forward (emagent-archive--user-heading-re) nil t)
        (push (line-beginning-position) pos)))
    (nreverse pos)))

(defun emagent-archive--blurb (body)
  "Return a one-line TOC blurb from moved BODY text."
  (let* ((re (emagent-archive--user-heading-re))
         (line
          (or (and (string-match re body)
                   (let* ((rest (substring body (match-end 0)))
                          (nl (string-match "\n" rest)))
                     (string-trim
                      (if nl (substring rest 0 nl) rest))))
              (car (split-string (string-trim body) "\n" t))))
         (clean (replace-regexp-in-string "[ \t\n]+" " " (or line ""))))
    (if (> (length clean) 80)
        (concat (substring clean 0 77) "...")
      clean)))

(defun emagent-archive--plan-move ()
  "Return a plist for an archiveable region, or nil when none."
  (let* ((zone (emagent-session-store-metadata-end))
         (heads (emagent-archive--user-heading-positions zone))
         (keep (max 1 (or emagent-archive-keep-turns 1)))
         (n (length heads)))
    (when (> n keep)
      (let* ((cut (nth (- n keep) heads))
             (beg (car heads))
             (end cut)
             (body (and beg end (< beg end)
                        (buffer-substring-no-properties beg end))))
        (when (and body (string-match-p "[^ \t\n]" body))
          (list :beg beg
                :end end
                :turns (- n keep)
                :blurb (emagent-archive--blurb body)
                :body body))))))

(defun emagent-archive--relative-chunk (chunk-path session-file)
  "Return CHUNK-PATH relative to SESSION-FILE's directory."
  (file-relative-name chunk-path (file-name-directory session-file)))

(defun emagent-archive--write-chunk (chunk-path body session-file)
  "Write BODY into CHUNK-PATH with a backlink to SESSION-FILE."
  (let* ((dir (file-name-directory chunk-path))
         (main (file-name-nondirectory session-file))
         (back (concat "../" main))
         (n (file-name-base chunk-path))
         (title (format "%s archive %s" (file-name-base session-file) n)))
    (make-directory dir t)
    (with-temp-file chunk-path
      (insert (format "#+TITLE: %s\n" title))
      (insert (format "Back to [[file:%s][%s]].\n\n" back main))
      (insert body)
      (unless (string-suffix-p "\n" body)
        (insert "\n")))))

(defun emagent-archive--append-toc (chunk-rel blurb)
  "Append a TOC line for CHUNK-REL with BLURB under `* Archive'."
  (let* ((inhibit-read-only t)
         (date (format-time-string "%Y-%m-%d"))
         (line (format "- [[file:%s][%s]] — %s\n"
                       chunk-rel date
                       (if (string-empty-p blurb) "(no summary)" blurb)))
         (zone (emagent-session-store-metadata-end)))
    (when (fboundp 'emagent-chat--writable)
      (emagent-chat--writable))
    (save-excursion
      (goto-char zone)
      (unless (re-search-forward
               (concat "^" (regexp-quote emagent-archive--toc-heading) "\\s-*$")
               nil t)
        (goto-char zone)
        (unless (bolp) (insert "\n"))
        (insert emagent-archive--toc-heading "\n")
        (goto-char zone)
        (re-search-forward
         (concat "^" (regexp-quote emagent-archive--toc-heading) "\\s-*$")
         nil t))
      (forward-line 1)
      (while (looking-at "^[ \t]*$")
        (delete-region (point) (line-beginning-position 2)))
      (while (looking-at "^- ")
        (forward-line 1))
      (insert line))))

(defun emagent-archive--last-response-insert-point ()
  "Return a buffer position at the end of the latest Response body, or nil."
  (cond
   ((and (fboundp 'emagent-chat--open-response-p)
         (emagent-chat--open-response-p)
         (fboundp 'emagent-chat--response-body-bounds)
         (emagent-chat--response-body-bounds))
    (cdr (emagent-chat--response-body-bounds)))
   (t
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^\\*\\* Response\\s-*$" nil t)
        (forward-line 1)
        (let ((user-re (emagent-archive--user-heading-re)))
          (if (re-search-forward user-re nil t)
              (line-beginning-position)
            (point-max))))))))

(defun emagent-archive--insert-hint (msg)
  "Append MSG under the latest Response body, or `message' it."
  (if-let ((end (emagent-archive--last-response-insert-point)))
      (let ((inhibit-read-only t))
        (when (fboundp 'emagent-chat--writable)
          (emagent-chat--writable))
        (save-excursion
          (goto-char end)
          (skip-chars-backward " \t\n")
          (insert "\n\n" msg "\n")
          (when (and (boundp 'emagent-chat--assistant-marker)
                     emagent-chat--assistant-marker)
            (set-marker emagent-chat--assistant-marker (point)))))
    (message "%s" msg)))

(defun emagent-archive--unsaved-hint-due-p ()
  "Return non-nil when an unsaved-archive hint may be shown."
  (let ((last emagent-archive--last-unsaved-hint)
        (cooldown emagent-archive-hint-cooldown))
    (or (null last)
        (null cooldown)
        (<= cooldown 0)
        (>= (float-time (time-subtract (current-time) last))
            cooldown))))

(defun emagent-archive-maybe-hint-unsaved ()
  "Hint under Response when buffer is large but unsaved."
  (when (and (null buffer-file-name)
             (emagent-archive--over-threshold-p)
             (emagent-archive--unsaved-hint-due-p))
    (emagent-archive--insert-hint
     (concat
      "/This session buffer is large and unsaved. "
      "Save it (C-x C-s) anywhere you like so emagent can move "
      "older turns into a sibling archive directory./"))
    (setq emagent-archive--last-unsaved-hint (current-time))
    t))

(defun emagent-archive--execute (plan)
  "Apply PLAN by writing its body into the next archive chunk.

Return the chunk path."
  (let* ((session (emagent-archive--session-file))
         (chunk (and session (emagent-archive--next-chunk-path session)))
         (beg (plist-get plan :beg))
         (end (plist-get plan :end))
         (body (plist-get plan :body))
         (turns (plist-get plan :turns))
         (blurb (plist-get plan :blurb))
         (inhibit-read-only t))
    (unless (and session chunk body)
      (error "Missing session file or plan body"))
    (when (fboundp 'emagent-chat--writable)
      (emagent-chat--writable))
    (emagent-archive--write-chunk chunk body session)
    (delete-region beg end)
    (let ((rel (emagent-archive--relative-chunk chunk session)))
      (emagent-archive--append-toc rel blurb)
      (emagent-archive--insert-hint
       (format
        "/Moved %d turns out of this buffer → ~%s~ (full text; not deleted)./"
        turns
        (abbreviate-file-name chunk)))
      (message "emagent: moved %d turns → %s" turns
               (abbreviate-file-name chunk))
      chunk)))

(defun emagent-archive-try (&optional force)
  "Archive older conversation history when appropriate.

With FORCE non-nil, skip the size threshold.  Always refuses unsaved
buffers and empty moves (never creates an empty chunk file)."
  (cond
   ((not (emagent-archive--session-file))
    (message "emagent: save the session buffer before archiving")
    nil)
   ((and (not force) (not (emagent-archive--over-threshold-p)))
    (message "emagent: nothing to move (buffer under archive threshold)")
    nil)
   (t
    (let ((plan (emagent-archive--plan-move)))
      (if (null plan)
          (progn
            (message "emagent: nothing to move (only the live turn remains)")
            nil)
        (emagent-archive--execute plan))))))

(defun emagent-archive-on-turn-end ()
  "After a finished response: auto-roll or hint to save when large."
  (cond
   ((null buffer-file-name)
    (emagent-archive-maybe-hint-unsaved))
   ((and emagent-archive-auto
         (emagent-archive--over-threshold-p))
    (emagent-archive-try nil))))

(defun emagent-archive-command-p (text)
  "Return non-nil when TEXT is a `/archive' client command."
  (let ((trimmed (string-trim text)))
    (when (string-prefix-p "/" trimmed)
      (let* ((body (substring trimmed 1))
             (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
             (cmd (if space (substring body 0 space) body)))
        (string= cmd "archive")))))

(defun emagent-archive-apply (&optional text)
  "Handle `/archive' and `/archive force' from TEXT without the agent."
  (when (and (fboundp 'emagent-chat--slash-token-bounds)
             (emagent-chat--slash-token-bounds))
    (let ((bounds (emagent-chat--slash-token-bounds)))
      (delete-region (car bounds) (cdr bounds))))
  (let* ((trimmed (string-trim (or text "")))
         (rest (string-trim
                (substring trimmed
                           (min (length trimmed) (length "/archive")))))
         (force (string-match-p "\\`force\\>" rest)))
    (emagent-archive-try force)))

(provide 'emagent-archive)

;;; emagent-archive.el ends here
