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
;; Filterable wide alist records for Emacs buffers (list_buffers / buffer_info).
;;
;;; Code:

(require 'cl-lib)
(require 'seq)

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

(provide 'emagent-tools-buffers)

;;; emagent-tools-buffers.el ends here
