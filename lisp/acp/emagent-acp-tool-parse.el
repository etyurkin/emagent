;;; emagent-acp-tool-parse.el --- ACP tool-call detail parsing  -*- lexical-binding: t; -*-

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

;; Pure helpers that extract paths, commands, and display details
;; from ACP tool-call payloads.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'map)
(require 'emagent-acp-state)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-provider)

(defconst emagent-acp--tool-call-detail-limit 120
  "Maximum detail length shown in Executing tool-call lines.")

(defun emagent-acp--update-put (update key value)
  "Return UPDATE alist with KEY bound to VALUE, replacing any prior binding."
  (cons (cons key value) (assoc-delete-all key update)))

(defun emagent-acp--tool-call-elisp-prin1-p (value)
  "Return non-nil when VALUE resembles a printed Elisp object."
  (and (stringp value)
       (string-match-p "\\`#s(" (string-trim value))))

(defun emagent-acp--tool-call-prin1-hash-detail (raw)
  "Extract a display string from a printed hash-table RAW, or nil."
  (when (emagent-acp--tool-call-elisp-prin1-p raw)
    (cl-loop for key in '(command path file pattern form directory args)
             when (string-match (format "(%s \\([^)]*\\))" key) raw)
             return (string-trim (match-string 1 raw)))))

(defun emagent-acp--tool-call-raw-input-empty-p (raw)
  "Return non-nil when tool-call rawInput carries no usable parameters.

Arguments: RAW."
  (or (null raw)
      (emagent-acp--tool-call-elisp-prin1-p raw)
      (and (stringp raw)
           (let ((trimmed (string-trim raw)))
             (or (string-empty-p trimmed)
                 (member trimmed '("{}" "[]" "null")))))
      (and (listp raw)
           (or (null raw)
               ;; `(())' reads as `(nil)' — a status tick with no args.
               (null (delq nil (copy-sequence raw)))
               (and (cl-every #'consp raw)
                    (null (emagent-acp--tool-call-raw-input-detail-from-data
                           raw))
                    (null (emagent-acp--tool-call-compact-arg-summary raw)))))
      (and (hash-table-p raw) (zerop (hash-table-count raw)))))

(defun emagent-acp--tool-call-update-from-request (tool-call)
  "Return an ACP tool-call UPDATE alist from a permission TOOL-CALL object."
  (when-let ((id (map-elt tool-call 'toolCallId)))
    (append `((toolCallId . ,id))
            (cl-remove nil
                       (list (when-let ((v (map-elt tool-call 'title)))
                               (cons 'title v))
                             (when-let ((v (map-elt tool-call 'rawInput)))
                               (cons 'rawInput v))
                             (when-let ((v (map-elt tool-call 'arguments)))
                               (cons 'arguments v))
                             (when-let ((v (map-elt tool-call 'subtitle)))
                               (cons 'subtitle v))
                             (when-let ((v (map-elt tool-call 'kind)))
                               (cons 'kind v))
                             (when-let ((v (map-elt tool-call 'status)))
                               (cons 'status v))
                             (when-let ((v (map-elt tool-call 'content)))
                               (cons 'content v)))))))

(defun emagent-acp--tool-call-truncate (string)
  "Return STRING truncated for tool-call display."
  (when string
    (if (> (length string) emagent-acp--tool-call-detail-limit)
        (concat (substring string 0 emagent-acp--tool-call-detail-limit) "…")
      string)))

(defun emagent-acp--tool-call-data-get (data key)
  "Return KEY from ACP tool-call DATA alist or hash-table."
  (cond
   ((hash-table-p data)
    (or (gethash key data)
        (gethash (symbol-name key) data)
        (gethash (downcase (symbol-name key)) data)))
   ((listp data)
    (or (alist-get key data)
        (alist-get (symbol-name key) data)
        (cdr (assoc key data))
        (cdr (assoc (symbol-name key) data))
        (cdr (assoc (downcase (symbol-name key)) data))))
   (t nil)))

(defun emagent-acp--tool-call-value-string (value)
  "Return a display string for tool-call VALUE, or nil."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((null value) nil)
   ((hash-table-p value)
    (cl-loop for key in '(command path file file_path target_file filename
                               relativeWorkspacePath url query q search
                               searchTerm search_term searchQuery input
                               text pattern glob form directory dir name args)
             for v = (emagent-acp--tool-call-data-get value key)
             when (and (stringp v) (not (string-empty-p (string-trim v))))
             return (string-trim v)))
   (t (let ((text (prin1-to-string value)))
        (unless (string-empty-p text) text)))))

(defun emagent-acp--tool-call-normalize-data (raw)
  "Return RAW tool input as an alist/hash-table, parsing JSON strings."
  (cond
   ((or (hash-table-p raw) (listp raw)) raw)
   ((stringp raw)
    (condition-case nil
        (json-parse-string raw
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object nil)
      (error nil)))
   (t nil)))

(defun emagent-acp--tool-call-edits-detail (raw)
  "Extract a file-path summary from tool-call edit lists in RAW."
  (when-let ((data (emagent-acp--tool-call-normalize-data raw))
             (edits (emagent-acp--tool-call-data-get data 'edits)))
    (let* ((items (cond
                   ((vectorp edits) (append edits nil))
                   ((listp edits) edits)
                   (t nil)))
           (paths
            (delq nil
                  (mapcar
                   (lambda (item)
                     (emagent-acp--tool-call-value-string
                      (or (emagent-acp--tool-call-data-get item 'path)
                          (emagent-acp--tool-call-data-get item 'file_path)
                          (emagent-acp--tool-call-data-get item 'target_file)
                          (emagent-acp--tool-call-data-get item 'relativeWorkspacePath))))
                   items))))
      (when paths
        (if (= (length paths) 1)
            (car paths)
          (format "%s (+%d more)" (car paths) (1- (length paths))))))))

(defun emagent-acp--tool-call-nested-raw-input (data)
  "Return nested arguments/input object from tool-call DATA, or nil."
  (when (or (hash-table-p data) (listp data))
    (cl-loop for key in '(arguments params input args payload data)
             for raw = (emagent-acp--tool-call-data-get data key)
             for parsed = (or (emagent-acp--tool-call-normalize-data raw) raw)
             when (and parsed (or (hash-table-p parsed) (listp parsed)))
             return parsed)))

(defun emagent-acp--tool-call-raw-input-detail-from-data (data)
  "Extract a concise detail string from normalized tool-call DATA."
  (when-let* ((key (seq-find (lambda (k)
                               (emagent-acp--tool-call-value-string
                                (emagent-acp--tool-call-data-get data k)))
                             '(path file file_path target_file filename
                                   relativeWorkspacePath url command query q
                                   search searchTerm search_term searchQuery
                                   input text pattern glob form
                                   directory dir args description))))
    (emagent-acp--tool-call-value-string
     (emagent-acp--tool-call-data-get data key))))

(defun emagent-acp--tool-call-compact-arg-summary (data)
  "Return a compact \"k=v\" summary for scalar args in DATA, or nil."
  (let ((skip-keys '("name" "tool" "toolname" "type" "title" "kind" "status"
                     "arguments" "params" "input" "args" "payload" "data"
                     "content" "edits" "locations"))
        pairs)
    (cond
     ((listp data)
      (dolist (entry data)
        (when (consp entry)
          (let* ((key (downcase (format "%s" (car entry))))
                 (value (cdr entry))
                 (text (and (not (member key skip-keys))
                            (not (or (listp value) (vectorp value) (hash-table-p value)))
                            (emagent-acp--tool-call-value-string value))))
            (when (and text (not (string-empty-p (string-trim text))))
              (push (format "%s=%s" key (string-trim text)) pairs))))))
     ((hash-table-p data)
      (maphash
       (lambda (k value)
         (let* ((key (downcase (format "%s" k)))
                (text (and (not (member key skip-keys))
                           (not (or (listp value) (vectorp value) (hash-table-p value)))
                           (emagent-acp--tool-call-value-string value))))
           (when (and text (not (string-empty-p (string-trim text))))
             (push (format "%s=%s" key (string-trim text)) pairs))))
       data)))
    (when pairs
      (let ((parts (seq-take (nreverse pairs) 2)))
        (if (> (length pairs) 2)
            (format "%s (+%d more)"
                    (string-join parts " ")
                    (- (length pairs) 2))
          (string-join parts " "))))))

(defun emagent-acp--tool-call-raw-input-detail (raw)
  "Extract a concise detail string from tool-call rawInput RAW."
  (or
   (when-let ((data (emagent-acp--tool-call-normalize-data raw)))
     (or (when-let ((nested (emagent-acp--tool-call-nested-raw-input data)))
           (or (emagent-acp--tool-call-raw-input-detail-from-data nested)
               (emagent-acp--tool-call-compact-arg-summary nested)))
         (emagent-acp--tool-call-raw-input-detail-from-data data)
         (emagent-acp--tool-call-compact-arg-summary data)))
   (emagent-acp--tool-call-prin1-hash-detail raw)))

(defun emagent-acp--tool-call-locations-detail (locations)
  "Extract a file-path summary from tool-call locations LOCATIONS."
  (when locations
    (let ((paths (delq nil
                       (mapcar (lambda (loc)
                                 (cond
                                  ((stringp loc) loc)
                                  ((listp loc) (map-elt loc 'path))
                                  ((hash-table-p loc)
                                   (or (gethash "path" loc) (gethash 'path loc)))))
                               (append locations nil)))))
      (when paths
        (if (= (length paths) 1)
            (car paths)
          (format "%s (+%d more)" (car paths) (1- (length paths))))))))

(defun emagent-acp--tool-call-content-detail (content)
  "Extract a concise detail string from tool-call content CONTENT."
  (when content
    (let* ((item (cond
                  ((vectorp content) (and (> (length content) 0) (aref content 0)))
                  ((listp content) (car content))
                  (t nil)))
           (text (or (map-nested-elt item '(content text))
                     (map-nested-elt item '(text))
                     (and (stringp item) item))))
      (when (and (stringp text) (not (string-empty-p (string-trim text))))
        (string-trim text)))))

(defconst emagent-acp--tool-call-path-keys
  '(path file_path filePath target_file relativeWorkspacePath file filename)
  "JSON keys that carry a file path in ACP tool-call rawInput.")

(defun emagent-acp--tool-call-eval-form (tool-call)
  "Return an eval form string from permission TOOL-CALL, or nil."
  (when tool-call
    (let ((raw (or (map-elt tool-call 'arguments)
                   (map-elt tool-call 'rawInput))))
      (when-let ((data (emagent-acp--tool-call-normalize-data raw)))
        (or (emagent-acp--tool-call-data-get data 'form)
            (emagent-acp--tool-call-data-get data 'code))))))

(defun emagent-acp--tool-call-command-text (tool-call)
  "Extract the command string from TOOL-CALL."
  (or (when-let* ((raw (or (map-elt tool-call 'rawInput)
                           (map-elt tool-call 'arguments)))
                  (data (emagent-acp--tool-call-normalize-data raw)))
        (or (emagent-acp--tool-call-data-get data 'command)
            (emagent-acp--tool-call-data-get data 'text)
            (emagent-acp--tool-call-data-get data 'cmd)))
      (map-elt tool-call 'subtitle)
      (map-elt tool-call 'title)))

(defun emagent-acp--tool-call-write-kind-p (kind)
  "Return non-nil when KIND is a file write/edit tool call."
  (and kind (member kind '("write" "edit"))))

(defun emagent-acp--tool-call-data-path (data)
  "Return the first file path string from tool-call DATA, or nil."
  (when data
    (cl-loop for key in emagent-acp--tool-call-path-keys
             for val = (emagent-acp--tool-call-data-get data key)
             when (and (stringp val) (not (string-empty-p val)))
             return val)))

(defun emagent-acp--tool-call-locations-path (tool-call)
  "Return a file path from TOOL-CALL locations, or nil."
  (when tool-call
    (emagent-acp--tool-call-locations-detail (map-elt tool-call 'locations))))

(defun emagent-acp--tool-call-path (tool-call)
  "Return a file path from permission TOOL-CALL, or nil."
  (when tool-call
    (or (emagent-acp--tool-call-locations-path tool-call)
        (let ((raw (or (map-elt tool-call 'arguments)
                       (map-elt tool-call 'rawInput))))
          (when-let ((data (emagent-acp--tool-call-normalize-data raw)))
            (emagent-acp--tool-call-data-path data))))))

(defun emagent-acp--tool-call-input (update)
  "Return raw tool input from ACP UPDATE.

Cursor often sends a useless `#s(hash-table …)' string in rawInput while the
real parameters live in arguments; prefer arguments when both are present."
  (let ((args (map-elt update 'arguments))
        (raw (map-elt update 'rawInput)))
    (cond
     ((and args (not (emagent-acp--tool-call-raw-input-empty-p args))) args)
     ((and raw (not (emagent-acp--tool-call-raw-input-empty-p raw))) raw)
     (t (or args raw)))))

(defun emagent-acp--tool-call-detail (update)
  "Return a concise detail string from ACP tool-call UPDATE, or nil."
  (let ((input (emagent-acp--tool-call-input update)))
    (or (when-let ((desc (map-elt update 'description)))
          (when (and (stringp desc) (not (string-empty-p (string-trim desc))))
            (string-trim desc)))
        (emagent-acp--tool-call-raw-input-detail input)
        (emagent-acp--tool-call-edits-detail input)
        (emagent-acp--tool-call-locations-detail (map-elt update 'locations))
        (let ((subtitle (map-elt update 'subtitle)))
          (when (emagent-acp--human-tool-detail-p subtitle)
            subtitle))
        (emagent-acp--tool-call-content-detail (map-elt update 'content)))))

(defconst emagent-acp--tool-call-weak-details
  '("tool" "Tool" "running" "pending" "unknown" "Unknown")
  "ACP tool-call detail strings too generic to display without store.db lookup.")

(defun emagent-acp--human-tool-detail-p (detail)
  "Return non-nil when DETAIL is safe to show in a permission prompt."
  (and (stringp detail)
       (let ((trimmed (string-trim detail)))
         (and (not (string-empty-p trimmed))
              (not (member trimmed emagent-acp--tool-call-weak-details))
              (not (emagent-acp--tool-call-elisp-prin1-p trimmed))))))

(defun emagent-acp--tool-call-detail-from-tool-call (tool-call)
  "Return a human-readable detail string from permission TOOL-CALL."
  (when tool-call
    (let ((update (emagent-acp--tool-call-update-from-request tool-call)))
      (or (and update (emagent-acp--tool-call-detail update))
          (emagent-acp--tool-call-raw-input-detail (map-elt tool-call 'arguments))
          (emagent-acp--tool-call-raw-input-detail (map-elt tool-call 'rawInput))))))

(defun emagent-acp--tool-call-meaningful-detail-p (update)
  "Return non-nil when UPDATE has useful path, command, or similar detail."
  (when-let ((detail (emagent-acp--tool-call-detail update)))
    (let ((trimmed (string-trim detail)))
      (and (not (string-empty-p trimmed))
           (not (member trimmed emagent-acp--tool-call-weak-details))))))

(defun emagent-acp--tool-call-generic-title-p (state title)
  "Return non-nil when TITLE is too generic to show without detail.

Arguments: STATE."
  (emagent-acp--provider-generic-title-p state title))

(defun emagent-acp--tool-call-redundant-detail-p (title detail)
  "Return non-nil when DETAIL provides nothing beyond generic TITLE."
  (when (and (stringp title) (stringp detail))
    (let* ((t0 (downcase (string-trim title)))
           (d0 (downcase (string-trim detail)))
           (t1 (replace-regexp-in-string "^emagent-" "" t0))
           (t2 (replace-regexp-in-string "^mcp_" "" t1))
           (basename (when (string-match-p "/" d0)
                       (car (last (split-string d0 "/"))))))
      (or (string= t0 d0)
          (string= t1 d0)
          (string= t2 d0)
          (and (string-match-p ":" t0)
               (string= (car (split-string t0 ":")) d0))
          ;; Detail is an absolute path whose filename is already in the title
          ;; (title carries a relative path; absolute path adds no new info).
          (and basename
               (not (string-empty-p basename))
               (string-match-p (regexp-quote basename) t0))))))

(provide 'emagent-acp-tool-parse)
;;; emagent-acp-tool-parse.el ends here
