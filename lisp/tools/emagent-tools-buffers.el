;;; emagent-tools-buffers.el --- Wide buffer records for MCP  -*- lexical-binding: t; -*-

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
;; Filterable wide alist records for Emacs session UI state: buffers,
;; windows, frames, marks, registers, diagnostics, and bookmarks.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)

;; cl-defstruct accessors are invisible to check-declare; load at compile time.
(eval-when-compile (require 'flymake))
(declare-function flymake--lookup-type-property "flymake" (type prop &optional default))
(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))
(declare-function bookmark-get-position "bookmark" (bookmark-name-or-record))
(declare-function bookmark-get-front-context-string "bookmark" (bookmark-name-or-record))
(declare-function bookmark-get-rear-context-string "bookmark" (bookmark-name-or-record))
(declare-function bookmark-name-from-full-record "bookmark" (bookmark-record))
(declare-function bookmark-prop-get "bookmark" (bookmark-record prop))

(require 'register)

(defun emagent-tools--buffer-type (buffer)
  "Return coarse type of BUFFER as a symbol: `file', `internal', or `non-file'.

`file' visits a file.  `internal' has no file and a name that looks like an
Emacs internal buffer (leading space or wrapping asterisks).  Everything else
is `non-file'."
  (with-current-buffer buffer
    (cond
     (buffer-file-name 'file)
     ((or (string-prefix-p " " (buffer-name))
          (and (string-prefix-p "*" (buffer-name))
               (string-suffix-p "*" (buffer-name))))
      'internal)
     (t 'non-file))))

(defun emagent-tools--buffer-file-mtime (buffer)
  "Return BUFFER's visited-file mtime as a float, or nil."
  (when-let* ((path (buffer-local-value 'buffer-file-name buffer))
              (attrs (file-attributes path)))
    (float-time (file-attribute-modification-time attrs))))

(defun emagent-tools--buffer-record (buffer mru-index selected-buffer other-buf
                                            current-buf)
  "Return a wide alist describing BUFFER for MCP/JSON consumers.

MRU-INDEX is BUFFER's 0-based position in `buffer-list' at call time.
SELECTED-BUFFER is `(window-buffer (selected-window))', OTHER-BUF is
`(other-buffer SELECTED-BUFFER t)', and CURRENT-BUF is `current-buffer' at
the start of the tool call—passed in so every row shares one snapshot.
Keys are underscore-separated strings so they match wire naming elsewhere.
Boolean fields use t/:false so JSON encoding keeps false distinct from null."
  (with-current-buffer buffer
    (let* ((windows (get-buffer-window-list buffer nil t))
           (frames (delete-dups (mapcar #'window-frame windows)))
           (path buffer-file-name))
      `(("name" . ,(buffer-name))
        ("mru_index" . ,mru-index)
        ("selected" . ,(if (eq buffer selected-buffer) t :false))
        ("current" . ,(if (eq buffer current-buf) t :false))
        ("other" . ,(if (eq buffer other-buf) t :false))
        ("path" . ,path)
        ("file_mtime" . ,(emagent-tools--buffer-file-mtime buffer))
        ("char_count" . ,(buffer-size))
        ("line_count" . ,(line-number-at-pos (point-max) t))
        ("mode" . ,(symbol-name major-mode))
        ("read_only" . ,(if buffer-read-only t :false))
        ("modified" . ,(if (buffer-modified-p) t :false))
        ("modified_tick" . ,(buffer-modified-tick))
        ("chars_modified_tick" . ,(buffer-chars-modified-tick))
        ("visible" . ,(if windows t :false))
        ("frame_names" . ,(mapcar (lambda (frame)
                                    (or (frame-parameter frame 'name)
                                        (prin1-to-string frame)))
                                  frames))
        ("type" . ,(symbol-name (emagent-tools--buffer-type buffer)))))))

(defun emagent-tools--list-buffers-frame-match-p (buffer frame-filter-regex)
  "Return non-nil if BUFFER is shown on a frame whose name matches REGEX.

When FRAME-FILTER-REGEX is nil, return non-nil (no frame constraint)."
  (or (null frame-filter-regex)
      (cl-some (lambda (frame)
                 (let ((fname (frame-parameter frame 'name)))
                   (and fname
                        (string-match-p frame-filter-regex fname)
                        (get-buffer-window buffer frame))))
               (frame-list))))

(defun emagent-tools--list-buffers-match-p (buffer type-filter window-filter
                                                   frame-filter-regex
                                                   name-filter-regex
                                                   path-filter-glob
                                                   mode-filter-regex)
  "Return non-nil if BUFFER matches TYPE-FILTER and the other filters.

WINDOW-FILTER, FRAME-FILTER-REGEX, NAME-FILTER-REGEX, PATH-FILTER-GLOB,
and MODE-FILTER-REGEX are optional list_buffers constraints."
  (let* ((type (symbol-name (emagent-tools--buffer-type buffer)))
         (name (buffer-name buffer))
         (path (buffer-local-value 'buffer-file-name buffer))
         (mode (symbol-name (buffer-local-value 'major-mode buffer)))
         (visible (get-buffer-window buffer t)))
    (when (and type-filter
               (not (member type-filter '("file" "non-file" "internal"))))
      (error "List_buffers: invalid type_filter %S (use file, non-file, or internal)"
             type-filter))
    (when (and window-filter
               (not (member window-filter '("visible" "hidden"))))
      (error "List_buffers: invalid window_filter %S (use visible or hidden)"
             window-filter))
    (and (or (null type-filter) (string= type-filter type))
         (or (null window-filter)
             (if (string= window-filter "visible") visible (null visible)))
         (emagent-tools--list-buffers-frame-match-p buffer frame-filter-regex)
         (or (null name-filter-regex)
             (string-match-p name-filter-regex name))
         (or (null path-filter-glob)
             (and path (string-match-p (wildcard-to-regexp path-filter-glob)
                                       path)))
         (or (null mode-filter-regex)
             (string-match-p mode-filter-regex mode)))))

(defun emagent-tools--list-buffers-row-get (row key)
  "Return string KEY from alist ROW."
  (alist-get key row nil nil #'equal))

(defun emagent-tools--list-buffers-compare (a b sort-by descending)
  "Return non-nil if row A should sort before row B for SORT-BY.

SORT-BY is one of \"mru\", \"name\", \"mode\", or \"file_mtime\".  Null
`file_mtime' values sort after non-null ones.  DESCENDING is ignored for
\"mru\"."
  (let ((key (or sort-by "mru")))
    (cond
     ((string= key "mru")
      (< (emagent-tools--list-buffers-row-get a "mru_index")
         (emagent-tools--list-buffers-row-get b "mru_index")))
     ((string= key "file_mtime")
      (let ((ta (emagent-tools--list-buffers-row-get a "file_mtime"))
            (tb (emagent-tools--list-buffers-row-get b "file_mtime")))
        (cond
         ((and ta tb)
          (let ((less (< ta tb)))
            (if descending (not less) less)))
         (ta t)
         (tb nil)
         (t (< (emagent-tools--list-buffers-row-get a "mru_index")
               (emagent-tools--list-buffers-row-get b "mru_index"))))))
     ((member key '("name" "mode"))
      (let* ((sa (emagent-tools--list-buffers-row-get a key))
             (sb (emagent-tools--list-buffers-row-get b key))
             (less (string-lessp sa sb)))
        (if descending (not less) less)))
     (t
      (error "List_buffers: invalid sort_by %S (use mru, name, mode, or file_mtime)"
             sort-by)))))

(defun emagent-tool-list-buffers (&optional type-filter window-filter
                                            frame-filter-regex
                                            name-filter-regex
                                            path-filter-glob
                                            mode-filter-regex
                                            sort-by sort-descending limit)
  "List Emacs buffers as wide alist records.

TYPE-FILTER, WINDOW-FILTER, FRAME-FILTER-REGEX, NAME-FILTER-REGEX,
PATH-FILTER-GLOB, and MODE-FILTER-REGEX combine as AND.  Omit any filter
to leave that dimension unconstrained.  Matching buffers are sorted by
SORT-BY (default mru), optionally SORT-DESCENDING, then truncated to
LIMIT.

Each element is an alist of string keys (see MCP tool description)."
  (let* ((selected-buffer (window-buffer (selected-window)))
         (current-buf (current-buffer))
         (other-buf (other-buffer selected-buffer t))
         (rows nil)
         (mru-index 0))
    (dolist (buffer (buffer-list))
      (when (emagent-tools--list-buffers-match-p
             buffer type-filter window-filter frame-filter-regex
             name-filter-regex path-filter-glob mode-filter-regex)
        (push (emagent-tools--buffer-record buffer mru-index selected-buffer
                                            other-buf current-buf)
              rows))
      (setq mru-index (1+ mru-index)))
    (setq rows (nreverse rows))
    (setq rows (sort rows (lambda (a b)
                            (emagent-tools--list-buffers-compare
                             a b sort-by sort-descending))))
    (if limit
        (seq-take rows limit)
      rows)))

(defun emagent-tools--buffer-mru-index (buffer)
  "Return BUFFER's 0-based index in `buffer-list', or nil if absent."
  (cl-position buffer (buffer-list) :test #'eq))

(defun emagent-tools--selector-flag-p (value)
  "Return non-nil when VALUE is an active boolean selector (Elisp/JSON true)."
  (eq value t))

(defun emagent-tools--resolve-buffer (name mru selected other current)
  "Resolve a buffer from mutually exclusive selectors.

NAME is a buffer name string, MRU a 0-based `buffer-list' index, and
SELECTED / OTHER / CURRENT are flags (true to use that buffer).  With no
selector, use the `selected-window' buffer.  Signal an error when more
than one selector is set or when the chosen selector matches no buffer."
  (let* ((selected-buffer (window-buffer (selected-window)))
         (current-buf (current-buffer))
         (other-buf (other-buffer selected-buffer t))
         (flags (append (when name (list 'name))
                        (when (integerp mru) (list 'mru))
                        (when (emagent-tools--selector-flag-p selected)
                          (list 'selected))
                        (when (emagent-tools--selector-flag-p other)
                          (list 'other))
                        (when (emagent-tools--selector-flag-p current)
                          (list 'current))))
         (count (length flags)))
    (when (> count 1)
      (error "Buffer_info: multiple selectors %S; use exactly one of name, mru, selected, other, current"
             flags))
    (cond
     ((zerop count) selected-buffer)
     (name
      (or (get-buffer name)
          (error "Buffer_info: no buffer named %S" name)))
     ((integerp mru)
      (let ((buffers (buffer-list)))
        (if (and (>= mru 0) (< mru (length buffers)))
            (nth mru buffers)
          (error "Buffer_info: mru %S out of range (0..%d)"
                 mru (1- (length buffers))))))
     ((emagent-tools--selector-flag-p selected)
      (or selected-buffer
          (error "Buffer_info: no selected buffer")))
     ((emagent-tools--selector-flag-p other)
      (or other-buf
          (error "Buffer_info: no other buffer")))
     ((emagent-tools--selector-flag-p current)
      (or current-buf
          (error "Buffer_info: no current buffer")))
     (t
      (error "Buffer_info: internal selector error")))))

(defun emagent-tools--buffer-pos-fields (prefix position)
  "Return alist fields for POSITION using keys prefixed by PREFIX.

PREFIX is \"point\" or \"mark\".  When POSITION is nil, value fields are nil."
  (if (null position)
      `((,(concat prefix) . nil)
        (,(concat prefix "_line") . nil)
        (,(concat prefix "_column") . nil))
    (save-excursion
      (goto-char position)
      `((,(concat prefix) . ,position)
        (,(concat prefix "_line") . ,(line-number-at-pos position t))
        (,(concat prefix "_column") . ,(current-column))))))

(defun emagent-tools--buffer-info-extras (buffer)
  "Return alist extras for BUFFER: point, mark, region, narrow, view state."
  (with-current-buffer buffer
    (let* ((mark-pos (mark t))
           (region-beg (and mark-pos (min (point) mark-pos)))
           (region-end (and mark-pos (max (point) mark-pos)))
           (win (get-buffer-window buffer 'visible))
           (coding buffer-file-coding-system))
      (append
       (emagent-tools--buffer-pos-fields "point" (point))
       (emagent-tools--buffer-pos-fields "mark" mark-pos)
       `(("region_active" . ,(if (and mark-pos (region-active-p)) t :false))
         ("region_beginning" . ,region-beg)
         ("region_end" . ,region-end)
         ("region_char_count" . ,(and mark-pos (- region-end region-beg)))
         ("narrowed" . ,(if (buffer-narrowed-p) t :false))
         ("narrow_beginning" . ,(point-min))
         ("narrow_end" . ,(point-max))
         ("directory" . ,default-directory)
         ("coding_system" . ,(and coding (symbol-name coding)))
         ("window_start" . ,(and win (window-start win)))
         ("window_end" . ,(and win (window-end win t))))))))

(defun emagent-tools--buffer-info-record (buffer)
  "Return full buffer_info alist for BUFFER (list_buffers row plus extras)."
  (let* ((selected-buffer (window-buffer (selected-window)))
         (current-buf (current-buffer))
         (other-buf (other-buffer selected-buffer t))
         (mru (or (emagent-tools--buffer-mru-index buffer)
                  (error "Buffer_info: buffer %S not in buffer-list"
                         (buffer-name buffer))))
         (base (emagent-tools--buffer-record buffer mru selected-buffer other-buf
                                             current-buf)))
    (append base (emagent-tools--buffer-info-extras buffer))))

(defun emagent-tool-buffer-info (&optional name mru selected other current)
  "Describe one Emacs buffer as a wide alist record.

Resolve with exactly one of NAME, MRU, SELECTED, OTHER, or CURRENT.  With no
selector, defaults to selected.  Multiple selectors or no matching buffer is
an error (not null)."
  (emagent-tools--buffer-info-record
   (emagent-tools--resolve-buffer name mru selected other current)))

(defun emagent-tools--window-record (window selected-window)
  "Return a wide alist describing WINDOW for MCP/JSON consumers.

SELECTED-WINDOW is `(selected-window)' at call time so every row shares
one snapshot.  Boolean fields use t/:false."
  (let* ((buffer (window-buffer window))
         (frame (window-frame window))
         (edges (window-edges window))
         (dedicated (window-dedicated-p window)))
    (with-current-buffer buffer
      `(("buffer_name" . ,(buffer-name))
        ("buffer_path" . ,buffer-file-name)
        ("frame_name" . ,(or (frame-parameter frame 'name)
                             (prin1-to-string frame)))
        ("selected" . ,(if (eq window selected-window) t :false))
        ("dedicated" . ,(if dedicated t :false))
        ("width" . ,(window-width window))
        ("height" . ,(window-height window))
        ("left" . ,(nth 0 edges))
        ("top" . ,(nth 1 edges))
        ("point" . ,(window-point window))
        ("window_start" . ,(window-start window))
        ("window_end" . ,(window-end window t))))))

(defun emagent-tools--list-windows-match-p (window buffer-name-regex
                                                   frame-filter-regex
                                                   selected-only
                                                   selected-window)
  "Return non-nil if WINDOW matches BUFFER-NAME-REGEX and other filters.

FRAME-FILTER-REGEX, SELECTED-ONLY, and SELECTED-WINDOW further constrain
the match for list_windows."
  (let* ((buffer (window-buffer window))
         (name (buffer-name buffer))
         (frame (window-frame window))
         (fname (frame-parameter frame 'name)))
    (and (or (null buffer-name-regex)
             (string-match-p buffer-name-regex name))
         (or (null frame-filter-regex)
             (and fname (string-match-p frame-filter-regex fname)))
         (or (not selected-only)
             (eq window selected-window)))))

(defun emagent-tool-list-windows (&optional buffer-name-regex
                                            frame-filter-regex
                                            selected limit)
  "List live windows as wide alist records.

BUFFER-NAME-REGEX and FRAME-FILTER-REGEX are optional filters.  When
SELECTED is t, keep only `(selected-window)'.  Matching rows are truncated
to LIMIT when set."
  (let* ((selected-window (selected-window))
         (selected-only (eq selected t))
         (rows nil))
    (dolist (window (window-list-1 nil nil t))
      (when (emagent-tools--list-windows-match-p
             window buffer-name-regex frame-filter-regex
             selected-only selected-window)
        (push (emagent-tools--window-record window selected-window) rows)))
    (setq rows (nreverse rows))
    (if limit
        (seq-take rows limit)
      rows)))


(defun emagent-tools--frame-record (frame selected-frame)
  "Return a wide alist describing FRAME for MCP/JSON consumers.

SELECTED-FRAME is `(selected-frame)' at call time so every row shares one
snapshot.  Boolean fields use t/:false."
  (let* ((windows (window-list frame))
         (buffers (delete-dups (mapcar #'window-buffer windows))))
    `(("name" . ,(or (frame-parameter frame 'name)
                     (prin1-to-string frame)))
      ("selected" . ,(if (eq frame selected-frame) t :false))
      ("visible" . ,(if (frame-visible-p frame) t :false))
      ("width" . ,(frame-width frame))
      ("height" . ,(frame-height frame))
      ("left" . ,(frame-parameter frame 'left))
      ("top" . ,(frame-parameter frame 'top))
      ("window_count" . ,(length windows))
      ("buffer_names" . ,(mapcar #'buffer-name buffers)))))

(defun emagent-tools--list-frames-match-p (frame name-filter-regex
                                                selected-only
                                                selected-frame)
  "Return non-nil if FRAME matches NAME-FILTER-REGEX and selected filter.

SELECTED-ONLY non-nil keeps only SELECTED-FRAME."
  (let ((fname (frame-parameter frame 'name)))
    (and (or (null name-filter-regex)
             (and fname (string-match-p name-filter-regex fname)))
         (or (not selected-only)
             (eq frame selected-frame)))))

(defun emagent-tool-list-frames (&optional name-filter-regex selected limit)
  "List live frames as wide alist records.

NAME-FILTER-REGEX matches `frame-parameter' name.  When SELECTED is t, keep
only `(selected-frame)'.  Matching rows are truncated to LIMIT when set."
  (let* ((selected-frame (selected-frame))
         (selected-only (eq selected t))
         (rows nil))
    (dolist (frame (frame-list))
      (when (emagent-tools--list-frames-match-p
             frame name-filter-regex selected-only selected-frame)
        (push (emagent-tools--frame-record frame selected-frame) rows)))
    (setq rows (nreverse rows))
    (if limit
        (seq-take rows limit)
      rows)))

(defun emagent-tools--mark-record (buffer position current-p)
  "Return a wide alist for POSITION in BUFFER.

CURRENT-P non-nil marks the buffer's current mark (`mark')."
  (with-current-buffer buffer
    (save-excursion
      (goto-char position)
      `(("buffer_name" . ,(buffer-name))
        ("position" . ,position)
        ("line" . ,(line-number-at-pos position t))
        ("column" . ,(current-column))
        ("current" . ,(if current-p t :false))))))

(defun emagent-tools--buffer-mark-positions (buffer)
  "Return \((POSITION . CURRENT-P)...\) for BUFFER's mark and `mark-ring'."
  (with-current-buffer buffer
    (let* ((current (mark t))
           (seen (make-hash-table :test 'eql))
           (rows nil))
      (when current
        (puthash current t seen)
        (push (cons current t) rows))
      (dolist (pos mark-ring)
        (let ((n (cond ((markerp pos) (marker-position pos))
                       ((integerp pos) pos)
                       (t nil))))
          (when (and n (not (gethash n seen)))
            (puthash n t seen)
            (push (cons n nil) rows))))
      (nreverse rows))))

(defun emagent-tool-list-marks (&optional buffer buffer-name-regex limit)
  "List mark positions as wide alist records.

BUFFER is an optional buffer name; when nil, use the selected window's
buffer.  BUFFER-NAME-REGEX filters by buffer name.  Matching rows are
truncated to LIMIT when set."
  (let* ((target
          (if buffer
              (or (get-buffer buffer)
                  (error "List_marks: no buffer named %S" buffer))
            (window-buffer (selected-window))))
         (name (buffer-name target))
         (rows nil))
    (when (or (null buffer-name-regex)
              (string-match-p buffer-name-regex name))
      (dolist (cell (emagent-tools--buffer-mark-positions target))
        (push (emagent-tools--mark-record target (car cell) (cdr cell))
              rows)))
    (setq rows (nreverse rows))
    (if limit
        (seq-take rows limit)
      rows)))

(defun emagent-tools--register-name (key)
  "Return a string wire name for register KEY."
  (cond
   ((characterp key) (string key))
   ((stringp key) key)
   (t (format "%s" key))))

(defun emagent-tools--register-type (value)
  "Return a coarse type string for register VALUE."
  (cond
   ((stringp value) "string")
   ((numberp value) "number")
   ((markerp value) "marker")
   ((and (consp value) (window-configuration-p (car value))) "window")
   ((window-configuration-p value) "window")
   ((and (consp value) (frame-configuration-p (car value))) "frame")
   ((frame-configuration-p value) "frame")
   (t "other")))

(defun emagent-tools--register-preview (value)
  "Return a short preview string for register VALUE."
  (truncate-string-to-width
   (cond
    ((stringp value) value)
    ((numberp value) (number-to-string value))
    ((markerp value)
     (format "marker:%s@%s"
             (or (and (marker-buffer value)
                      (buffer-name (marker-buffer value)))
                 "?")
             (marker-position value)))
    (t (prin1-to-string value)))
   80 nil nil t))

(defun emagent-tools--register-record (key value)
  "Return a wide alist describing register KEY with VALUE."
  (let* ((type (emagent-tools--register-type value))
         (marker (and (markerp value) value))
         (buf (and marker (marker-buffer marker))))
    `(("name" . ,(emagent-tools--register-name key))
      ("type" . ,type)
      ("preview" . ,(emagent-tools--register-preview value))
      ("buffer_name" . ,(and buf (buffer-name buf)))
      ("position" . ,(and marker (marker-position marker))))))

(defun emagent-tool-list-registers (&optional type-filter limit)
  "List `register-alist' entries as wide alist records.

TYPE-FILTER keeps rows whose type string equals it (string, number, marker,
window, frame, or other).  Matching rows are truncated to LIMIT when set."
  (let ((rows nil))
    (dolist (cell register-alist)
      (let* ((row (emagent-tools--register-record (car cell) (cdr cell)))
             (type (alist-get "type" row nil nil #'equal)))
        (when (or (null type-filter)
                  (equal type-filter type))
          (push row rows))))
    (setq rows (nreverse rows))
    (if limit
        (seq-take rows limit)
      rows)))



(defun emagent-tools--flymake-severity-string (type)
  "Return a coarse severity string for Flymake diagnostic TYPE."
  (require 'flymake)
  (let* ((name (and (fboundp 'flymake--lookup-type-property)
                    (flymake--lookup-type-property type 'flymake-type-name)))
         (sev (and (fboundp 'flymake--severity)
                   (flymake--severity type))))
    (cond
     ((and (stringp name) (not (string-empty-p name))) name)
     ((and (numberp sev) (>= sev (warning-numeric-level :error))) "error")
     ((and (numberp sev) (>= sev (warning-numeric-level :warning)))
      "warning")
     (t "note"))))

(defun emagent-tools--severity-rank (severity)
  "Return a sort/filter rank for SEVERITY string (higher is worse)."
  (pcase severity
    ("error" 3)
    ("warning" 2)
    ("note" 1)
    (_ 0)))

(defun emagent-tools--diagnostic-record (buffer diag)
  "Return a wide alist for Flymake DIAG in BUFFER."
  (require 'flymake)
  (with-current-buffer buffer
    (let* ((beg (flymake-diagnostic-beg diag))
           (end (flymake-diagnostic-end diag))
           (type (flymake-diagnostic-type diag))
           (backend (and (fboundp 'flymake-diagnostic-backend)
                         (flymake-diagnostic-backend diag)))
           (path buffer-file-name)
           (beg-ok (and (integer-or-marker-p beg)
                        (>= beg (point-min))
                        (<= beg (point-max))))
           (end-ok (and (integer-or-marker-p end)
                        (>= end (point-min))
                        (<= end (point-max)))))
      (save-excursion
        `(("buffer_name" . ,(buffer-name))
          ("path" . ,path)
          ("line" . ,(and beg-ok (line-number-at-pos beg t)))
          ("column" . ,(and beg-ok
                            (progn (goto-char beg) (current-column))))
          ("end_line" . ,(and end-ok (line-number-at-pos end t)))
          ("end_column" . ,(and end-ok
                                (progn (goto-char end) (current-column))))
          ("severity" . ,(emagent-tools--flymake-severity-string type))
          ("type" . ,(and type (format "%s" type)))
          ("message" . ,(flymake-diagnostic-text diag))
          ("backend" . ,(and backend (format "%s" backend))))))))

(defun emagent-tools--list-diagnostics-match-p (row buffer-name-regex
                                                    path-filter-glob
                                                    severity-filter)
  "Return non-nil when diagnostic ROW matches list_diagnostics filters.

BUFFER-NAME-REGEX, PATH-FILTER-GLOB, and SEVERITY-FILTER are optional
constraints on buffer name, path, and minimum severity."
  (let ((name (alist-get "buffer_name" row nil nil #'equal))
        (path (alist-get "path" row nil nil #'equal))
        (severity (alist-get "severity" row nil nil #'equal)))
    (and (or (null buffer-name-regex)
             (and name (string-match-p buffer-name-regex name)))
         (or (null path-filter-glob)
             (and path (string-match-p (wildcard-to-regexp path-filter-glob)
                                       path)))
         (or (null severity-filter)
             (>= (emagent-tools--severity-rank severity)
                 (emagent-tools--severity-rank severity-filter))))))

(defun emagent-tool-list-diagnostics (&optional buffer buffer-name-regex
                                                 path-filter-glob
                                                 severity limit)
  "List Flymake diagnostics as wide alist records.

BUFFER is an optional buffer name; when nil, scan file-visiting buffers
with `flymake-mode'.  BUFFER-NAME-REGEX, PATH-FILTER-GLOB, and SEVERITY
\(min level: note < warning < error\) further filter.  Truncate to LIMIT."
  (require 'flymake nil t)
  (unless (fboundp 'flymake-diagnostics)
    (error "List_diagnostics: flymake is not available"))
  (when (and severity
             (not (member severity '("error" "warning" "note"))))
    (error "List_diagnostics: invalid severity %S (use error, warning, or note)"
           severity))
  (let* ((buffers
          (if buffer
              (list (or (get-buffer buffer)
                        (error "List_diagnostics: no buffer named %S" buffer)))
            (seq-filter
             (lambda (buf)
               (and (buffer-file-name buf)
                    (buffer-local-value 'flymake-mode buf)))
             (buffer-list))))
         (rows nil))
    (dolist (buf buffers)
      (dolist (diag (with-current-buffer buf (flymake-diagnostics)))
        (let ((row (emagent-tools--diagnostic-record buf diag)))
          (when (emagent-tools--list-diagnostics-match-p
                 row buffer-name-regex path-filter-glob severity)
            (push row rows)))))
    (setq rows (nreverse rows))
    (if limit
        (seq-take rows limit)
      rows)))

(defun emagent-tools--bookmark-type (record)
  "Return a coarse type string for bookmark RECORD."
  (cond
   ((bookmark-get-filename record) "file")
   ((bookmark-prop-get record 'buffer-name) "buffer")
   ((bookmark-prop-get record 'handler) "other")
   (t "other")))

(defun emagent-tools--bookmark-record (name record)
  "Return a wide alist for bookmark NAME with RECORD."
  (require 'bookmark)
  (let* ((path (bookmark-get-filename record))
         (pos (bookmark-get-position record))
         (front (bookmark-get-front-context-string record))
         (rear (bookmark-get-rear-context-string record))
         (type (emagent-tools--bookmark-type record))
         (visiting (and path (find-buffer-visiting path)))
         (line (and visiting pos
                    (with-current-buffer visiting
                      (line-number-at-pos pos t)))))
    `(("name" . ,name)
      ("type" . ,type)
      ("path" . ,path)
      ("position" . ,pos)
      ("line" . ,line)
      ("front_context" . ,(and front
                               (truncate-string-to-width front 80 nil nil t)))
      ("rear_context" . ,(and rear
                              (truncate-string-to-width rear 80 nil nil t))))))

(defun emagent-tools--list-bookmarks-match-p (row name-filter-regex
                                                  path-filter-glob
                                                  type-filter)
  "Return non-nil when bookmark ROW matches list_bookmarks filters.

NAME-FILTER-REGEX, PATH-FILTER-GLOB, and TYPE-FILTER are optional
constraints on bookmark name, path, and type."
  (let ((name (alist-get "name" row nil nil #'equal))
        (path (alist-get "path" row nil nil #'equal))
        (type (alist-get "type" row nil nil #'equal)))
    (and (or (null name-filter-regex)
             (and name (string-match-p name-filter-regex name)))
         (or (null path-filter-glob)
             (and path (string-match-p (wildcard-to-regexp path-filter-glob)
                                       path)))
         (or (null type-filter)
             (equal type-filter type)))))

(defun emagent-tool-list-bookmarks (&optional name-filter-regex
                                              path-filter-glob
                                              type-filter limit)
  "List `bookmark-alist' entries as wide alist records.

NAME-FILTER-REGEX, PATH-FILTER-GLOB, and TYPE-FILTER (file, buffer, or
other) constrain the result.  Truncate to LIMIT when set."
  (require 'bookmark)
  (when (and type-filter
             (not (member type-filter '("file" "buffer" "other"))))
    (error "List_bookmarks: invalid type_filter %S (use file, buffer, or other)"
           type-filter))
  (let ((rows nil))
    (dolist (cell bookmark-alist)
      (let* ((name (bookmark-name-from-full-record cell))
             (record cell)
             (row (emagent-tools--bookmark-record name record)))
        (when (emagent-tools--list-bookmarks-match-p
               row name-filter-regex path-filter-glob type-filter)
          (push row rows))))
    (setq rows (nreverse rows))
    (if limit
        (seq-take rows limit)
      rows)))


(provide 'emagent-tools-buffers)

;;; emagent-tools-buffers.el ends here
