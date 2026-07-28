;;; emagent-acp-permit.el --- Permission helpers and cursor tool-resolve  -*- lexical-binding: t; -*-

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
;; Tool-call handling, permission prompts, and the permission queue.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'json)
(require 'subr-x)
(require 'emagent-acp-protocol)
(require 'emagent-acp-usage)
(require 'emagent-chat)
(require 'emagent-chat-ui)
(require 'emagent-cursor)
(require 'emagent-log)
(require 'emagent-policy)
(require 'emagent-session)
(require 'emagent-tools)

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

(defun emagent-acp--tool-call-edit-field (item &rest keys)
  "Return the first non-empty string value of KEYS from ITEM."
  (when item
    (cl-loop for key in keys
             for val = (emagent-acp--tool-call-data-get item key)
             when (and (stringp val) (not (string-empty-p val)))
             return val)))

(defun emagent-acp--tool-call-edit-items (data)
  "Return the normalized list of patch-style edits in DATA, or nil."
  (when data
    (when-let ((edits (emagent-acp--tool-call-data-get data 'edits)))
      (cond
       ((vectorp edits) (append edits nil))
       ((listp edits) edits)
       (t nil)))))

(defun emagent-acp--tool-call-edit-path (item)
  "Return the file path named by edit ITEM, or nil."
  (emagent-acp--tool-call-edit-field
   item 'path 'file_path 'target_file 'relativeWorkspacePath 'file 'filename))

(defun emagent-acp--tool-call-write-path (tool-call raw detail)
  "Return the file path for a write/edit TOOL-CALL, falling back to DETAIL.

Arguments: RAW."
  (or (emagent-acp--tool-call-path tool-call)
      (when-let ((data (and raw (emagent-acp--tool-call-normalize-data raw))))
        (or (emagent-acp--tool-call-data-path data)
            (when-let ((items (emagent-acp--tool-call-edit-items data)))
              (emagent-acp--tool-call-edit-path (car items)))))
      detail))

(defun emagent-acp--tool-call-infer-kind (tool-call)
  "Guess TOOL-CALL kind when ACP omits it (common with Cursor permissions)."
  (when tool-call
    (let* ((explicit (map-elt tool-call 'kind))
           (title (downcase (or (map-elt tool-call 'title) "")))
           (raw (or (map-elt tool-call 'rawInput) (map-elt tool-call 'arguments)))
           (data (when raw (emagent-acp--tool-call-normalize-data raw)))
           (content (when data (emagent-acp--tool-call-data-get data 'content)))
           (edits (when data (emagent-acp--tool-call-data-get data 'edits)))
           (command (emagent-acp--tool-call-command-text tool-call)))
      (cond
       ((and explicit (not (string-empty-p explicit)))
        (downcase explicit))
       ((or (and content (not (string-empty-p content))) edits) "write")
       ((emagent-acp--tool-call-edit-field data 'new_string 'newText 'new_text
                                           'newString 'after 'replace 'content 'text)
        "write")
       ((string-match-p "\\(?:edit\\|write\\|apply\\|replace\\|patch\\)" title) "write")
       ((string-match-p "\\`read" title) "read")
       ((emagent-acp--tool-call-eval-form tool-call) "eval")
       (command "execute")
       (t nil)))))

(defun emagent-acp--tool-call-apply-edit (text old new)
  "Apply a single OLD/NEW patch-style edit to TEXT."
  (cond
   ((and (stringp old) (stringp new))
    (if (string-empty-p old)
        (concat (or text "") new)
      (replace-regexp-in-string (regexp-quote old) new (or text "") t t)))
   ((stringp new) new)
   (t text)))

(defun emagent-acp--tool-call-proposed-content (path data)
  "Return PATH's content after applying the edits in DATA, or nil."
  (when (and path data)
    (let ((content (emagent-acp--tool-call-data-get data 'content))
          (items (emagent-acp--tool-call-edit-items data)))
      (cond
       ((and (stringp content) (not (string-empty-p content))) content)
       (items
        (let ((current (condition-case nil
                          (emagent-tools--read-file-content path)
                        (error ""))))
          (cl-loop for item in items
                   with text = current
                   for old = (emagent-acp--tool-call-edit-field
                              item 'old_string 'oldText 'old_text 'oldString
                              'before 'search)
                   for new = (emagent-acp--tool-call-edit-field
                              item 'new_string 'newText 'new_text 'newString
                              'after 'replace 'content 'text)
                   when (stringp new)
                   do (setq text (emagent-acp--tool-call-apply-edit text old new))
                   finally return (if (string-empty-p text) nil text))))
       ((emagent-acp--tool-call-edit-field
         data 'new_string 'newText 'new_text 'newString 'after 'replace 'content 'text)
        (let* ((current (condition-case nil
                           (emagent-tools--read-file-content path)
                         (error "")))
               (old (emagent-acp--tool-call-edit-field
                     data 'old_string 'oldText 'old_text 'oldString 'before 'search))
               (new (emagent-acp--tool-call-edit-field
                     data 'new_string 'newText 'new_text 'newString
                     'after 'replace 'content 'text)))
          (emagent-acp--tool-call-apply-edit current old new)))
       (t nil)))))

(defun emagent-acp--tool-call-edit-patch-string (path old new)
  "Build a diff-shaped preview of an edit from its raw OLD/NEW strings.
Empty lines must survive: with OMIT-NULLS the preview would silently
drop the blank lines separating functions in NEW.  Only a single
trailing newline is trimmed so content ending in \\n doesn't grow a
spurious empty +/- line.

Arguments: PATH."
  (when (and (stringp new) (not (string-empty-p new)))
    (let* ((name (file-name-nondirectory path))
           (old-lines (when (and old (not (string-empty-p old)))
                        (split-string (string-remove-suffix "\n" old) "\n")))
           (new-lines (split-string (string-remove-suffix "\n" new) "\n")))
      (concat "--- " name " (current)\n+++ " name " (proposed)\n"
              (if old-lines
                  (concat "@@ edit @@\n"
                          (mapconcat (lambda (line) (concat "-" line))
                                     old-lines "\n")
                          "\n"
                          (mapconcat (lambda (line) (concat "+" line))
                                     new-lines "\n"))
                (mapconcat (lambda (line) (concat "+" line)) new-lines "\n"))))))

(defvar emagent-acp--edit-diff-cache (make-hash-table :test 'equal)
  "Map toolCallId to a real pre-edit diff for later tool-call re-renders.
The agent writes the file right after permission is granted, but the same
tool call re-renders on later status updates (in_progress, completed) — by
then the on-disk file equals the proposed content and diffing yields
nothing.  The first render's diff is kept here so re-renders show it.")

(defvar emagent-acp--edit-diff-cache-order nil
  "List of toolCallIds in `emagent-acp--edit-diff-cache', most recent first.")

(defconst emagent-acp--edit-diff-cache-max 200
  "Entries kept in `emagent-acp--edit-diff-cache' before evicting the oldest.
Sized for display re-renders within a turn; completed tool calls stop
re-rendering once the turn ends, so evicted entries are rarely missed.")

(defun emagent-acp--edit-diff-cache-put (id diff)
  "Remember DIFF for toolCallId ID, evicting the oldest entry over the cap."
  (when (and id diff)
    (unless (gethash id emagent-acp--edit-diff-cache)
      (push id emagent-acp--edit-diff-cache-order)
      (when (> (length emagent-acp--edit-diff-cache-order)
               emagent-acp--edit-diff-cache-max)
        (remhash (car (last emagent-acp--edit-diff-cache-order))
                 emagent-acp--edit-diff-cache)
        (setq emagent-acp--edit-diff-cache-order
              (butlast emagent-acp--edit-diff-cache-order))))
    (puthash id diff emagent-acp--edit-diff-cache))
  diff)

(defun emagent-acp--tool-call-reversed-diff-string (resolved data proposed)
  "Real diff recovered by reverse-applying DATA's edits to PROPOSED.
When the file already contains PROPOSED (the render happened after the
write) the pre-edit content can still be reconstructed for old/new-string
edits: substitute each new string back to its old string, newest edit
first.  Returns nil when DATA has no reversible edits or reversal changes
nothing (e.g. a pure whole-content write).

Arguments: RESOLVED."
  (when-let ((items (emagent-acp--tool-call-edit-items data)))
    (let ((old-content proposed))
      (dolist (item (reverse items))
        (let ((old (emagent-acp--tool-call-edit-field
                    item 'old_string 'oldText 'old_text 'oldString
                    'before 'search))
              (new (emagent-acp--tool-call-edit-field
                    item 'new_string 'newText 'new_text 'newString
                    'after 'replace 'content 'text)))
          (when (and (stringp old) (not (string-empty-p old))
                     (stringp new) (not (string-empty-p new)))
            (setq old-content
                  (emagent-acp--tool-call-apply-edit old-content new old)))))
      (unless (string= old-content proposed)
        (emagent-tools--diff-strings (file-name-nondirectory resolved)
                                     old-content proposed)))))

(defun emagent-acp--tool-call-edit-diff-string (path data &optional id)
  "Return a diff rendering the edit in DATA against PATH, or nil.
Prefers a real diff; the hand-built patch preview is the last resort:
1. `diff' against the on-disk file (renders before the write).
2. The diff cached under toolCallId ID by an earlier pre-write render.
3. `diff' against pre-edit content reconstructed by reversing the edits.
4. A patch-shaped preview built from the raw old/new strings."
  (when-let* ((proposed (emagent-acp--tool-call-proposed-content path data))
              (resolved (emagent-tools--root-directory path)))
    (or (emagent-acp--edit-diff-cache-put
         id (emagent-tools--write-diff-string resolved proposed))
        (and id (gethash id emagent-acp--edit-diff-cache))
        (emagent-acp--edit-diff-cache-put
         id (emagent-acp--tool-call-reversed-diff-string resolved data proposed))
        (when-let* ((items (emagent-acp--tool-call-edit-items data))
                    (item (car items))
                    (new (emagent-acp--tool-call-edit-field
                          item 'new_string 'newText 'new_text 'newString
                          'after 'replace 'content 'text)))
          (let ((old (emagent-acp--tool-call-edit-field
                      item 'old_string 'oldText 'old_text 'oldString
                      'before 'search)))
            (emagent-acp--tool-call-edit-patch-string resolved old new)))
        (when-let ((new (emagent-acp--tool-call-edit-field
                         data 'new_string 'newText 'new_text 'newString
                         'after 'replace 'content 'text)))
          (let ((old (emagent-acp--tool-call-edit-field
                      data 'old_string 'oldText 'old_text 'oldString
                      'before 'search)))
            (emagent-acp--tool-call-edit-patch-string resolved old new))))))

(defun emagent-acp--tool-call-edit-block-spec (update)
  "Return a diff block spec when UPDATE is a write/edit change.
The value is (\"diff\" . DIFF) when the change can be reconstructed;
otherwise nil.  Lets an auto-allowed edit render the same diff a
permission prompt would, instead of a bare arrow line."
  (when-let* ((kind (emagent-acp--tool-call-infer-kind update))
              ((emagent-acp--tool-call-write-kind-p kind))
              (raw (or (map-elt update 'rawInput) (map-elt update 'arguments)))
              (path (emagent-acp--tool-call-write-path
                     update raw (emagent-acp--tool-call-detail update)))
              (data (emagent-acp--tool-call-normalize-data raw))
              (diff (emagent-acp--tool-call-edit-diff-string
                     path data (map-elt update 'toolCallId))))
    (cons "diff" diff)))

(defun emagent-acp--tool-call-shell-command (update)
  "Return an explicit shell command string from UPDATE, or nil.
Unlike `emagent-acp--tool-call-command-text', this never falls back to the
tool title/subtitle, so non-shell tools (grep, read) are not misread as
commands."
  (when-let* ((raw (emagent-acp--tool-call-input update))
              (data (emagent-acp--tool-call-normalize-data raw)))
    (cl-flet ((cmd (d) (or (emagent-acp--tool-call-value-string
                            (emagent-acp--tool-call-data-get d 'command))
                           (emagent-acp--tool-call-value-string
                            (emagent-acp--tool-call-data-get d 'cmd)))))
      (or (cmd data)
          (when-let ((nested (emagent-acp--tool-call-nested-raw-input data)))
            (cmd nested))))))

(defconst emagent-acp--heredoc-lang-alist
  '(("python3" . "python") ("python2" . "python") ("python" . "python")
    ("node" . "js") ("nodejs" . "js")
    ("ruby" . "ruby") ("perl" . "perl") ("php" . "php") ("lua" . "lua")
    ("bash" . "sh") ("sh" . "sh") ("zsh" . "sh"))
  "Map heredoc interpreter names to org-babel source block languages.")

(defun emagent-acp--tool-call-heredoc-script (command)
  "Return (LANG . BODY) when COMMAND is an interpreter heredoc, else nil."
  (when (and (stringp command)
             (string-match
              (concat
               "\\`[ \t]*\\([^ \t\n]+\\)\\(?:[ \t]+-\\)?[ \t]*<<-?[ \t]*"
               "\\(?:'\\(?2:[^'\n]+\\)'\\|\"\\(?2:[^\"\n]+\\)\"\\|\\(?2:[A-Za-z_][A-Za-z0-9_]*\\)\\)"
               "[ \t]*\n\\(?3:\\(?:.\\|\n\\)*?\\)\n[ \t]*\\2[ \t\n]*\\'")
              command))
    (let* ((interpreter (file-name-nondirectory (match-string 1 command)))
           (lang (cdr (assoc interpreter emagent-acp--heredoc-lang-alist))))
      (when lang
        (cons lang (match-string 3 command))))))

(defconst emagent-acp--shell-tool-names
  '("grep" "rg" "ripgrep" "ag" "cat" "ls" "find" "fd" "sed" "awk"
    "head" "tail")
  "List CLI utility names treated as structured shell tools.
When used as a structured tool title, render as a reconstructed `sh'
command (e.g. grep PATTERN).  Kept deliberately narrow so structured file
read/write/search tools are never mistaken for shell commands.")

(defun emagent-acp--tool-call-cli-tool (update)
  "Return the CLI command word (grep, cat, ...) named by UPDATE's title, or nil.
Only a curated set of shell utilities is recognized, so structured
read/write/search tools are not treated as shell commands."
  (let* ((title (downcase (string-trim (or (map-elt update 'title) ""))))
         (title (replace-regexp-in-string
                 "\\`\\(?:mcp_[^_]+_\\|mcp:? *\\|emagent-\\)" "" title))
         (word (car (split-string title "[^a-z0-9.+-]+" t))))
    (and word (member word emagent-acp--shell-tool-names) word)))

(defconst emagent-acp--cli-tool-arg-order
  '(("grep" pattern path glob)
    ("rg" pattern path glob)
    ("ripgrep" pattern path glob)
    ("ag" pattern path glob)
    ("find" path name glob pattern)
    ("fd" pattern path)
    ("sed" pattern file path)
    ("awk" pattern file path)
    ("cat" path file)
    ("head" path file)
    ("tail" path file)
    ("ls" path directory dir))
  "Ordered rawInput fields used to reconstruct a CLI tool command line.
The car of each entry is the tool word; the rest name structured arguments
in the order they should appear after it, so e.g. grep renders both its
pattern and path rather than a single field.")

(defun emagent-acp--tool-call-display-quote (value)
  "Wrap VALUE in double quotes for `shell-command' display when needed.
Only embedded double quotes are escaped; regexp backslashes are preserved so
the displayed pattern matches what the agent actually searched for."
  (if (and (stringp value)
           (not (string-empty-p value))
           (string-match-p "[][[:space:]\"'`$|&;<>()*?{}]" value))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" value) "\"")
    value))

(defun emagent-acp--cli-command-args (data order)
  "Return quoted argument strings for the ORDER fields present in DATA."
  (delq nil
        (mapcar
         (lambda (key)
           (when-let ((v (emagent-acp--tool-call-value-string
                          (emagent-acp--tool-call-data-get data key))))
             (let ((s (string-trim v)))
               (unless (string-empty-p s)
                 (emagent-acp--tool-call-display-quote s)))))
         order)))

(defun emagent-acp--tool-call-cli-command (update cli)
  "Reconstruct a full command line for CLI tool word CLI from UPDATE, or nil.
Unlike a single-field detail, this includes every recognized structured
argument (e.g. grep's pattern and path) in a natural order so the rendered
command is complete."
  (when-let* ((raw (emagent-acp--tool-call-input update))
              (data (emagent-acp--tool-call-normalize-data raw))
              (order (cdr (assoc cli emagent-acp--cli-tool-arg-order))))
    (let* ((nested (emagent-acp--tool-call-nested-raw-input data))
           (args (or (and nested (emagent-acp--cli-command-args nested order))
                     (emagent-acp--cli-command-args data order))))
      (when args
        (string-join (cons cli args) " ")))))

(defun emagent-acp--tool-call-block-spec (update)
  "Return (LANG . CODE) when UPDATE should render as an Org src block, else nil.
Explicit shell commands render as `sh' and eval forms as `elisp' (their detail
is real code).  A command that heredocs a script into a known interpreter
\(e.g. `python3 - <<EOF ... EOF') renders as that interpreter's language with
just the script body, not the shell wrapper.  A structured CLI tool (grep,
cat, ...) renders as `sh' with its detail reconstructed into a command line
\(e.g. grep PATTERN).  A write/edit tool renders as a `diff' block so an
auto-allowed edit shows the same change a permission prompt would.  Any other
tool renders as a block when its detail spans multiple lines or exceeds
`emagent-acp--tool-call-detail-limit' characters; shorter single-line details
such as file paths stay as compact arrow lines."
  (let* ((command (emagent-acp--tool-call-shell-command update))
         (heredoc (and command (emagent-acp--tool-call-heredoc-script command)))
         (form (unless command (emagent-acp--tool-call-eval-form update)))
         (detail (unless (or command form)
                   (emagent-acp--tool-call-detail update)))
         (cli (and detail (emagent-acp--tool-call-cli-tool update)))
         (edit (unless (or command form heredoc cli)
                 (emagent-acp--tool-call-edit-block-spec update))))
    (cond
     (heredoc heredoc)
     (command (cons "sh" command))
     (form (cons "elisp" form))
     (cli (cons "sh" (or (emagent-acp--tool-call-cli-command update cli)
                         (format "%s %s" cli detail))))
     (edit edit)
     ((and detail (or (string-match-p "\n" detail)
                      (> (length detail) emagent-acp--tool-call-detail-limit))
           ;; Don't create a text block for an absolute path when the title
           ;; already carries the user-friendly relative path — the arrow
           ;; line from the label is cleaner in that case.
           (not (emagent-acp--tool-call-redundant-detail-p
                 (string-trim (or (map-elt update 'title) ""))
                 detail)))
      (cons "text" detail))
     (t nil))))

(defun emagent-acp--tool-call-emagent-tool-p (update)
  "Return non-nil when UPDATE names a tool from emagent's own MCP server.

Such tools run inside Emacs; once one's permission is granted (it is
running or finished) without a recorded ACP permission decision, its line
is tagged (Allow: Emacs) instead of the inferred (Allow: Agent) used for
agent-native tools.  A pending call stays untagged — it may still be
awaiting a permission prompt.

Detection relies on the emagent MCP namespace (e.g. `mcp_emagent_read_file'):
an explicit `emagent-tool' flag set by provider enrichment, or the word
`emagent' surviving in the title.  Bare tool names are deliberately not matched
because generic names like `grep' collide with agent-native tools."
  (or (map-elt update 'emagent-tool)
      (let ((title (downcase (string-trim (or (map-elt update 'title) "")))))
        (and (not (string-empty-p title))
             (string-match-p "\\bemagent\\b" title)
             t))))

(defun emagent-acp--switch-mode-tool-p (tool-call)
  "Return non-nil when TOOL-CALL is an ACP mode-switch permission/tool."
  (when tool-call
    (let ((kind (downcase (or (map-elt tool-call 'kind) "")))
          (title (string-trim (or (map-elt tool-call 'title) ""))))
      (or (string= kind "switch_mode")
          (string-match-p "\\`switch[_-]?mode\\'" kind)
          (string-match-p "\\`switch\\s-+mode\\b" title)
          (string-match-p "\\`ExitPlanMode\\'" title)
          (string-match-p "\\`Ready to code[?]\\'" title)))))

(defun emagent-acp--switch-mode-target-id (tool-call)
  "Return target mode id from TOOL-CALL rawInput, or nil."
  (when-let* ((raw (or (map-elt tool-call 'rawInput)
                       (map-elt tool-call 'arguments)))
              (data (emagent-acp--tool-call-normalize-data raw)))
    (let ((target (or (emagent-acp--tool-call-data-get data 'targetModeId)
                      (emagent-acp--tool-call-data-get data 'target_mode_id))))
      (when (and (stringp target) (not (string-empty-p (string-trim target))))
        (string-trim target)))))

(defun emagent-acp--switch-mode-display-title (tool-call)
  "Return a user-facing title for switch_mode TOOL-CALL.

Never leaves bare `unknown' (Cursor titles SwitchMode that way when
targetModeId is missing)."
  (let* ((title (string-trim (or (map-elt tool-call 'title) "")))
         (raw (or (map-elt tool-call 'rawInput) (map-elt tool-call 'arguments)))
         (data (and raw (emagent-acp--tool-call-normalize-data raw)))
         (target (emagent-acp--switch-mode-target-id tool-call))
         (explanation (and data (emagent-acp--tool-call-data-get data 'explanation)))
         (explanation (and (stringp explanation)
                           (let ((e (string-trim explanation)))
                             (unless (string-empty-p e) e))))
         (bad (or (string-empty-p title)
                  (string-match-p "\\`unknown\\'" title)
                  (string-match-p ":\\s-*unknown\\s-*\\'" title)
                  (string-match-p "\\`switch\\s-+mode\\'" title))))
    (cond
     ((and (not bad) (not (string-empty-p title))) title)
     (target (format "Switch to %s" target))
     (explanation
      (format "Switch mode: %s"
              (truncate-string-to-width explanation 60 nil nil "...")))
     (t "Switch mode"))))

(defun emagent-acp--ingest-tool-call-request (state tool-call)
  "Merge TOOL-CALL from session/request_permission and refresh display.

Arguments: STATE."
  (when-let ((update (emagent-acp--tool-call-update-from-request tool-call)))
    (emagent-acp--on-tool-call state update)))

(defun emagent-acp--tool-call-base-label (label)
  "Return LABEL without a trailing Allow/Denied annotation."
  (if (and (stringp label)
           (string-match " ?\\((Allow: [^)]+)\\|(Allow)\\|(Denied)\\)\\'" label))
      (string-trim (substring label 0 (match-beginning 0)))
    label))

(defun emagent-acp--tool-call-prefer-label (prev next)
  "Return PREV when it is a richer tool label than NEXT, else NEXT.

Keeps query/path detail when a later status tick only carries the tool title."
  (let* ((prev-base (emagent-acp--tool-call-base-label prev))
         (next-base (emagent-acp--tool-call-base-label next))
         (prev-l (and (stringp prev-base) (downcase prev-base)))
         (next-l (and (stringp next-base) (downcase next-base))))
    (if (and prev-l next-l
             (not (string-empty-p prev-l))
             (not (string-empty-p next-l))
             (> (length prev-l) (length next-l))
             (or (string-prefix-p next-l prev-l)
                 (string-prefix-p (concat next-l ":") prev-l)))
        (let ((ann (and (stringp next)
                        (string-match
                         " ?\\((Allow: [^)]+)\\|(Allow)\\|(Denied)\\)\\'" next)
                        (match-string 0 next))))
          (if ann (concat prev-base ann) prev-base))
      next)))

(defun emagent-acp--emit-tool-call-display (state id kind merged label status)
  "Push TOOL-CALL LABEL to the chat buffer and update session UI.

Arguments: STATE, ID, KIND, MERGED, STATUS."
  (let* ((labels (emagent-acp-state-tool-call-labels state))
         (prev (and id labels (gethash id labels)))
         (decision (and id (when-let ((d (emagent-acp-state-tool-call-decisions state)))
                             (gethash id d))))
         (completed (member status '("completed" "failed")))
         ;; A running or finished call already had its permission granted;
         ;; a pending call may still be awaiting a permission prompt.
         (granted (or completed (equal status "in_progress")))
         (display (emagent-acp--tool-call-prefer-label
                   prev
                   (cond
                    ((or (null label) (string-empty-p label)) label)
                    (decision (emagent-acp--permission-decision-label label decision))
                    ((and granted (emagent-acp--tool-call-emagent-tool-p merged))
                     (format "%s (Allow: Emacs)" label))
                    ;; Tool runs without ACP permission: the agent's own
                    ;; allow-list permitted it directly — infer the decision.
                    (granted (format "%s (Allow: Agent)" label))
                    (t label))))
         (label-changed (and display (not (string-empty-p display))
                             (or (null prev) (not (string= prev display))))))
    (when label
      (emagent-acp--detect-external-refusal-in-text state label))
    (when label-changed
      (when id (puthash id display labels))
      (unless completed
        (emagent-acp--notify-user state (format "emagent: tool %s" label)))
      (when-let ((buf (emagent-acp--chat-buffer state))
                 (cb (emagent-acp-state-cb-tool-call state)))
        (let ((spec (emagent-acp--tool-call-block-spec merged)))
          (with-current-buffer buf
            (funcall cb id display (car spec) (cdr spec))))))
    (if completed
        (progn
          (setf (emagent-acp-state-current-tool state) nil)
          (setf (emagent-acp-state-current-tool-kind state) nil))
      (when label-changed
        (setf (emagent-acp-state-current-tool state) label)
        (when kind (setf (emagent-acp-state-current-tool-kind state) kind))
        (emagent-acp--schedule-prompt-watchdog state)))
    (when (or label-changed completed)
      (emagent-acp--refresh-mode-line state))))

(defun emagent-acp--permission-choice-label (choice)
  "Return a short display label for permission CHOICE, or nil."
  (pcase choice
    (:allow-once "Once")
    (:allow-session "Session")
    (:allow-always "Always")
    (:allow-all "All")
    (:deny "Denied")
    (_ nil)))

(defun emagent-acp--permission-decision-label (base-label choice)
  "Return BASE-LABEL with permission CHOICE appended in parentheses when known.

A scoped approval (`:allow-session' etc.) renders as `(Allow: Session)'; a
generic approval (`:allow', used for policy/auto-trust) renders as `(Allow)';
`:deny' renders as `(Denied)'.  A string CHOICE (switch_mode optionId) is
shown as-is."
  (pcase choice
    ('nil base-label)
    (:deny (format "%s (Denied)" base-label))
    ((pred stringp) (format "%s (%s)" base-label choice))
    (_ (if-let ((suffix (emagent-acp--permission-choice-label choice)))
           (format "%s (Allow: %s)" base-label suffix)
         (format "%s (Allow)" base-label)))))

(defun emagent-acp--tool-call-displayable-p (state update)
  "Return non-nil when UPDATE should appear in the Thinking block.

Arguments: STATE."
  (let* ((title (string-trim (or (map-elt update 'title) "")))
         (detail (emagent-acp--tool-call-detail update)))
    (cond
     ;; Show when detail is meaningful — redundancy check belongs only in
     ;; label-building/block-spec, not in the visibility decision.
     ((and detail (emagent-acp--tool-call-meaningful-detail-p update)) t)
     ((and (not (string-empty-p title))
           (not (emagent-acp--tool-call-generic-title-p state title))
           (or (null detail) (string-empty-p detail)))
      t)
     (t nil))))

(defun emagent-acp--tool-call-label (update)
  "Return a display label for ACP tool-call UPDATE."
  (let* ((title (string-trim (or (map-elt update 'title) "tool")))
         (title (if (string-match-p "\\`MCP:? *tool\\'" title) "MCP" title))
         (switch-mode (emagent-acp--switch-mode-tool-p update))
         (title (if switch-mode
                    (emagent-acp--switch-mode-display-title update)
                  title))
         (detail (and (not switch-mode)
                      (emagent-acp--tool-call-detail update))))
    (cond
     ((and detail (not (string-empty-p detail))
           (not (string-match-p (regexp-quote detail) title))
           (not (emagent-acp--tool-call-redundant-detail-p title detail)))
      (format "%s: %s" title (emagent-acp--tool-call-truncate detail)))
     ;; Detail is redundant (basename already in title) or equals title: when
     ;; it is an absolute path, the title carries a user-friendly relative path
     ;; — prefer the title so the operation name and relative path stay visible.
     ((and detail (not (string-empty-p detail))
           (string-match-p "\\`/" (string-trim detail)))
      title)
     ((and detail (not (string-empty-p detail))) detail)
     (t title))))

(defun emagent-acp--merged-tool-call-update (state update)
  "Return UPDATE merged with stored title/rawInput for STATE.

Empty or placeholder rawInput on a later status tick must not wipe a
previously stored query/path: Cursor often sends the args once, then
`in_progress' updates with an empty rawInput, which used to erase the
detail from the Thinking arrow line."
  (let* ((id (map-elt update 'toolCallId))
         (titles (emagent-acp-state-tool-call-titles state))
         (inputs (emagent-acp-state-tool-call-inputs state))
         (stored-title (and id titles (gethash id titles)))
         (stored-input (and id inputs (gethash id inputs)))
         (title (or (map-elt update 'title) stored-title))
         (incoming (or (map-elt update 'rawInput)
                       (map-elt update 'arguments)))
         (raw-input
          (cond
           ((and incoming
                 (not (emagent-acp--tool-call-raw-input-empty-p incoming)))
            incoming)
           ((and stored-input
                 (not (emagent-acp--tool-call-raw-input-empty-p stored-input)))
            stored-input)
           (t incoming)))
         (merged update))
    (when (and id title)
      (puthash id title titles))
    (when title
      (setq merged (emagent-acp--update-put merged 'title title)))
    (when (and raw-input
               (not (emagent-acp--tool-call-raw-input-empty-p raw-input)))
      (setq merged (emagent-acp--update-put merged 'rawInput raw-input))
      (when id (puthash id raw-input inputs)))
    merged))

(defun emagent-acp--wakeup-tool-p (title)
  "Return non-nil when TITLE names the ScheduleWakeup harness tool."
  (and (stringp title)
       (string-match-p "\\(?:\\`\\|__\\)ScheduleWakeup\\'" (string-trim title))))

(defun emagent-acp--capture-schedule-wakeup (state update)
  "Record a ScheduleWakeup request from tool-call UPDATE in STATE.

The agent ends its turn after calling ScheduleWakeup and expects the
client to send the wakeup prompt after the delay.  Only recorded here;
the timer is armed when the turn completes (`emagent-acp--arm-wakeup'),
and a `stop' call cancels a pending wakeup immediately."
  (when (and emagent-acp-honor-schedule-wakeup
             (emagent-acp--wakeup-tool-p (map-elt update 'title)))
    (when-let* ((raw (map-elt update 'rawInput))
                (data (emagent-acp--tool-call-normalize-data raw)))
      (let ((stop (emagent-acp--tool-call-data-get data 'stop))
            (delay (emagent-acp--tool-call-data-get data 'delaySeconds))
            (prompt (emagent-acp--tool-call-data-get data 'prompt))
            (reason (emagent-acp--tool-call-data-get data 'reason)))
        (when (stringp delay)
          (setq delay (string-to-number delay)))
        (cond
         ((and stop (not (memq stop '(:false :json-false))))
          (emagent-acp--cancel-wakeup state)
          (emagent-log "wakeup: loop stopped by agent"))
         ((and (numberp delay) (> delay 0))
          (setf (emagent-acp-state-wakeup-request state)
                (list :delay (max 10 (min (round delay) 3600))
                      :prompt (and (stringp prompt)
                                   (not (string-empty-p prompt))
                                   prompt)
                      :reason (and (stringp reason)
                                   (not (string-empty-p reason))
                                   reason)))))))))

(defun emagent-acp--on-tool-call (state update)
  "Display or refresh a tool-call line from ACP UPDATE.

Arguments: STATE."
  (unless (or (emagent-acp-state-replaying-history state)
              (emagent-acp-state-quiet-prompt state))
    (let* ((update (emagent-acp--provider-enrich-tool-call state update))
           (id (map-elt update 'toolCallId))
           (status (map-elt update 'status))
           (kind (map-elt update 'kind))
           (merged (emagent-acp--merged-tool-call-update state update))
           (label (emagent-acp--tool-call-label merged))
           (pending-table (emagent-acp-state-tool-call-pending state))
           (defer (emagent-acp--provider-defer-tool-call-p state merged))
           (show (and label (not (string-empty-p label)) (not defer)
                        (emagent-acp--tool-call-displayable-p state merged))))
      (emagent-acp--capture-schedule-wakeup state merged)
      (when defer
        (puthash id merged pending-table)
        (emagent-acp--provider-enqueue-tool-resolve state id))
      (when show
        (emagent-acp--emit-tool-call-display state id kind merged label status)
        (when id (remhash id pending-table)))
      (when (emagent-acp-state-permission-queue state)
        (unless (fboundp 'emagent-acp--drain-permission-queue)
          (require 'emagent-acp-permit))
        (emagent-acp--drain-permission-queue state)))))

(defun emagent-acp--permission-question-line (emagent-acp-request)
  "Return the command or path to show on the permission ? line.

Arguments: EMAGENT-ACP-REQUEST."
  (let* ((tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (detail (and tool-call (emagent-acp--tool-call-detail-from-tool-call tool-call)))
         (title (emagent-acp--permission-prompt-title emagent-acp-request))
         (name (and title (replace-regexp-in-string "\\`Allow \\(.*\\)[?]\\'" "\\1" title))))
    (cond
     ((emagent-acp--human-tool-detail-p detail)
      ;; A shell detail is self-explanatory ("make test"), but an MCP
      ;; tool's raw-input fragment ("--oneline -10") is meaningless
      ;; without the tool's name — prepend it.
      (if (and name (string-match-p "\\`mcp__" name)
               (not (string-match-p (regexp-quote name) detail)))
          (format "%s %s" name detail)
        detail))
     (name name)
     (t "Permission request"))))

(defun emagent-acp--permission-prompt-title (emagent-acp-request)
  "Return the primary permission question line from EMAGENT-ACP-REQUEST."
  (let* ((tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (raw (or (map-nested-elt emagent-acp-request '(params title))
                  (map-nested-elt emagent-acp-request '(params toolCall title))
                  "Permission request"))
         (title (car (split-string raw "\n" t))))
    (if (and tool-call (emagent-acp--switch-mode-tool-p tool-call))
        (emagent-acp--switch-mode-display-title
         (if (map-elt tool-call 'title)
             tool-call
           (cons (cons 'title title) tool-call)))
      title)))

(defun emagent-acp--permission-prompt-text (emagent-acp-request)
  "Return user-facing permission prompt text for EMAGENT-ACP-REQUEST."
  (let* ((title (emagent-acp--permission-prompt-title emagent-acp-request))
         (tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (detail (emagent-acp--tool-call-detail-from-tool-call tool-call)))
    (if (and (emagent-acp--human-tool-detail-p detail)
             (not (string-match-p (regexp-quote detail) title)))
        (format "%s\n%s" title (emagent-acp--tool-call-truncate detail))
      title)))

(cl-defun emagent-acp--handle-one-permission (&key state emagent-acp-request on-complete)
  "Show permission dialog for EMAGENT-ACP-REQUEST in STATE's chat buffer.

For auto-deny/auto-approve: sends the ACP response synchronously and calls
ON-COMPLETE immediately.  For interactive prompts: inserts the dialog
non-blockingly and returns; ON-COMPLETE is called after the user responds.

`switch_mode' permissions (Claude ExitPlanMode, Cursor SwitchMode) never
auto-approve: the agent's optionIds are mode ids and must be returned
unchanged."
  (let* ((raw-tool-call (map-nested-elt emagent-acp-request '(params toolCall)))
         (tool-call (and raw-tool-call
                         (emagent-acp--permission-tool-call state raw-tool-call)))
         (options (map-nested-elt emagent-acp-request '(params options)))
         (request-id (map-elt emagent-acp-request 'id))
         (question (emagent-acp--permission-question-line emagent-acp-request))
         (fingerprint (and tool-call (emagent-acp--permission-fingerprint tool-call)))
         (validation (and tool-call (emagent-acp--permission-validate tool-call)))
         (buf (emagent-acp--chat-buffer state))
         (allow-id (emagent-acp--permission-acp-allow-id options))
         (deny-id (emagent-acp--permission-acp-deny-id options))
         (switch-mode (emagent-acp--switch-mode-tool-p tool-call))
         (switch-choices (and switch-mode
                              (emagent-acp--switch-mode-choices options))))
    (when raw-tool-call
      (emagent-acp--ingest-tool-call-request state raw-tool-call))
    (let ((respond
           (lambda (choice)
             (when (and (not switch-mode)
                        (emagent-acp--permission-approved-choice-p choice))
               (emagent-acp--permission-apply-choice state fingerprint buf choice))
             (let* ((response
                     (cond
                      ((and (stringp choice) (not (string-empty-p choice)))
                       (emagent-acp-make-session-request-permission-response
                        :request-id request-id :option-id choice))
                      ((and (not switch-mode)
                            validation (eq (car validation) :deny))
                       (if deny-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id deny-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      ((and (not switch-mode)
                            (emagent-acp--permission-approved-choice-p choice))
                       (if allow-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id allow-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      ((and (not switch-mode) (eq choice :deny))
                       (if deny-id
                           (emagent-acp-make-session-request-permission-response
                            :request-id request-id :option-id deny-id)
                         (emagent-acp-make-session-request-permission-response
                          :request-id request-id :cancelled t)))
                      (t
                       (emagent-acp-make-session-request-permission-response
                        :request-id request-id :cancelled t))))
                    (outcome (map-nested-elt response '(:result outcome))))
               (emagent-log "permission response: question=%s outcome=%s choice=%s"
                            question (or outcome "?") choice)
               (emagent-acp-send-response :client (emagent-acp-state-client state) :response response))
             (when on-complete (funcall on-complete)))))
      (cond
       (switch-mode
        (unless (fboundp 'emagent-acp--prepare-interactive-context)
          (require 'emagent-acp))
        (emagent-acp--prepare-interactive-context state)
        (emagent-acp--clear-prompt-watchdog state)
        (let ((after-response
               (lambda (choice)
                 (when choice
                   (emagent-acp--show-permission-decision state tool-call choice))
                 (when (emagent-acp-state-busy state)
                   (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
                     (require 'emagent-acp))
                   (emagent-acp--schedule-prompt-watchdog state))
                 (emagent-acp--refresh-mode-line state)
                 (funcall respond (or choice :cancel))))
              (choices (or switch-choices
                           '(("Cancel" . :cancel))))
              (prompt (or (and tool-call
                               (emagent-acp--switch-mode-display-title tool-call))
                          question))
              (preamble (emagent-acp--switch-mode-preamble
                         (or raw-tool-call tool-call))))
          (emagent-tools--buttons-prompt
           prompt choices buf after-response preamble)))
       ((and validation (eq (car validation) :deny))
        (emagent-log "permission denied by emagent gate: %s — %s" question (cdr validation))
        (emagent-acp--show-permission-decision state tool-call :deny)
        (funcall respond :deny))
       ((emagent-acp--permission-gate-auto-approve-p state tool-call validation fingerprint buf)
        (let ((stored (emagent-acp--permission-stored-auto-choice state fingerprint buf)))
          (emagent-log "permission auto-approve: %s (fingerprint %s)" question (or fingerprint "none"))
          (emagent-acp--show-permission-decision state tool-call (or stored :allow))
          (funcall respond :allow-once)))
       (t
        (unless (fboundp 'emagent-acp--prepare-interactive-context)
          (require 'emagent-acp))
        (emagent-acp--prepare-interactive-context state)
        (emagent-acp--clear-prompt-watchdog state)
        (let ((after-response
               (lambda (choice)
                 (when choice
                   (emagent-acp--show-permission-decision state tool-call choice))
                 (when (emagent-acp-state-busy state)
                   (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
                     (require 'emagent-acp))
                   (emagent-acp--schedule-prompt-watchdog state))
                 (emagent-acp--refresh-mode-line state)
                 (funcall respond (or choice :cancel)))))
          (if (and buf (buffer-live-p buf)
                   (emagent-acp-state-cb-permission state)
                   (with-current-buffer buf (emagent-chat--open-response-p)))
              (with-current-buffer buf
                (funcall (emagent-acp-state-cb-permission state)
                         question emagent-acp--permission-emagent-choices
                         after-response tool-call))
            (emagent-tools--buttons-prompt
             question emagent-acp--permission-emagent-choices buf after-response))))))))

(defun emagent-acp--permission-interactive-p (state)
  "Return non-nil when ACP permission dialogue may need user input.

Arguments: STATE."
  (and (not (emagent-acp-state-session-auto-approve state))
       (not (eq emagent-acp-auto-approve-permissions t))))

(defun emagent-acp--cancel-permission-request (state request)
  "Reply `cancelled' to permission REQUEST so the waiting agent does not hang.
Used when a request is abandoned by an error, interrupt, or teardown rather
than by a user decision.

Arguments: STATE."
  (when-let ((request-id (map-elt request 'id)))
    (ignore-errors
      (emagent-acp-send-response
       :client (emagent-acp-state-client state)
       :response (emagent-acp-make-session-request-permission-response
                  :request-id request-id :cancelled t)))))

(defun emagent-acp--cancel-outstanding-permissions (state)
  "Reply `cancelled' to every queued permission request, then clear the queue.
Leaves `:permission-busy' untouched: an in-flight interactive prompt owns its
own response.  Call from interrupt/teardown so abandoned requests never leave
the agent blocked.

Arguments: STATE."
  (dolist (request (emagent-acp-state-permission-queue state))
    (emagent-acp--cancel-permission-request state request))
  (setf (emagent-acp-state-permission-queue state) nil))

(defun emagent-acp--schedule-permission-drain (state)
  "Run `emagent-acp--drain-permission-queue-now' outside the ACP process filter.

Arguments: STATE."
  (unless (or (emagent-acp-state-permission-drain-timer state)
              (emagent-acp-state-permission-busy state))
    (setf (emagent-acp-state-permission-drain-timer state)
              (run-at-time 0 nil
                           (lambda ()
                             (setf (emagent-acp-state-permission-drain-timer state) nil)
                             (emagent-acp--drain-permission-queue-now state))))))

(defun emagent-acp--drain-permission-queue-now (state)
  "Process queued permission requests without recursive auto-approve nesting.

For auto-deny/auto-approve: handle at most
`emagent-acp-permission-drain-batch-size' requests, then reschedule so a flood
of MCP permissions cannot peg the Emacs command loop.  For interactive
prompts: insert one dialog and return; the button callback schedules the
next drain.

Arguments: STATE."
  (if (and (emagent-acp-state-permission-queue state)
           (active-minibuffer-window))
      ;; Minibuffer is active — inserting a dialog would conflict.  Poll.
      (unless (or (emagent-acp-state-permission-drain-timer state)
                  (emagent-acp-state-permission-busy state))
        (setf (emagent-acp-state-permission-drain-timer state)
              (run-at-time 0.3 nil
                           (lambda ()
                             (setf (emagent-acp-state-permission-drain-timer state) nil)
                             (emagent-acp--drain-permission-queue-now state)))))
    (let ((batch 0)
          (limit (max 1 emagent-acp-permission-drain-batch-size))
          (interactive (emagent-acp--permission-interactive-p state)))
      (while (and (emagent-acp-state-permission-queue state)
                  (not (emagent-acp-state-permission-busy state))
                  (or interactive (< batch limit)))
        (setq batch (1+ batch))
        (let ((request (car (emagent-acp-state-permission-queue state))))
          (setf (emagent-acp-state-permission-queue state)
                (cdr (emagent-acp-state-permission-queue state)))
          (setf (emagent-acp-state-permission-busy state) t)
          (emagent-acp--refresh-mode-line state)
          (condition-case err
              (emagent-acp--handle-one-permission
               :state state
               :emagent-acp-request request
               :on-complete
               (lambda ()
                 ;; Auto-approve runs this before handle-one returns (busy
                 ;; clears; the while loop continues).  Interactive prompts
                 ;; run this later from the button callback.
                 (setf (emagent-acp-state-permission-busy state) nil)
                 (emagent-acp--refresh-mode-line state)
                 (condition-case cont-err
                     (progn
                       (unless (fboundp 'emagent-acp--maybe-complete-deferred-prompt)
                         (require 'emagent-acp))
                       (emagent-acp--maybe-complete-deferred-prompt state)
                       (when (and (emagent-acp-state-permission-queue state)
                                  (emagent-acp--permission-interactive-p state))
                         (emagent-acp--schedule-permission-drain state)))
                   ((error quit)
                    (emagent-log "permission on-complete error: %s"
                                 (error-message-string cont-err))))))
            ((error quit)
             ;; Request was popped but not answered: cancel so the agent is
             ;; not left blocked, then continue the iterative drain.
             (emagent-log "permission handler error: %s" (error-message-string err))
             (emagent-acp--cancel-permission-request state request)
             (setf (emagent-acp-state-permission-busy state) nil)
             (emagent-acp--refresh-mode-line state)
             (when (and (emagent-acp-state-permission-queue state)
                        (emagent-acp--permission-interactive-p state))
               (emagent-acp--schedule-permission-drain state))))))
      ;; Yield after an auto-approve batch so redisplay can run.
      (when (and (not interactive)
                 (emagent-acp-state-permission-queue state)
                 (not (emagent-acp-state-permission-busy state)))
        (emagent-acp--schedule-permission-drain state)))))

(defun emagent-acp--drain-permission-queue (state)
  "Process queued permission requests one at a time.

Interactive prompts are deferred to the next event cycle so
`recursive-edit' never runs inside the ACP process filter.

Arguments: STATE."
  (when (emagent-acp-state-permission-queue state)
    (if (emagent-acp--permission-interactive-p state)
        (emagent-acp--schedule-permission-drain state)
      (emagent-acp--drain-permission-queue-now state))))

(defun emagent-acp--hydrate-session-permissions (state session-id)
  "Load ~/.emagent session permissions for SESSION-ID into STATE."
  (when (and session-id (not (string-empty-p session-id)))
    (setf (emagent-acp-state-permission-auto-allow state)
              (copy-sequence (emagent-permissions-session-fingerprints session-id)))
    (when (emagent-permissions-session-auto-approve-p session-id)
      (setf (emagent-acp-state-session-auto-approve state) t))))

(defun emagent-acp--permission-option-deny-p (opt)
  "Return non-nil when OPT is a deny-type ACP permission option."
  (let ((kind (downcase (or (map-elt opt 'kind) "")))
        (id (downcase (or (map-elt opt 'optionId) "")))
        (name (downcase (or (map-elt opt 'name) ""))))
    (or (member kind '("deny" "deny_once" "deny-once" "reject"))
        (member id '("deny" "deny_once" "deny-once" "no" "reject"))
        (string-match-p "deny\\|reject" id)
        (string-match-p "\\`\\(?:deny\\|no\\|reject\\)" name))))

(defun emagent-acp--permission-option-always-id (options)
  "Return allow_always/allow-always optionId from OPTIONS, or nil."
  (when options
    (or (map-elt (seq-find (lambda (opt)
                             (let ((id (downcase (or (map-elt opt 'optionId) ""))))
                               (member id '("allow_always" "allow-always"))))
                   options)
                'optionId)
        (map-elt (seq-find (lambda (opt)
                             (let ((kind (downcase (or (map-elt opt 'kind) ""))))
                               (member kind '("allow_always" "allow-always"))))
                   options)
                'optionId))))

(defconst emagent-acp--permission-acp-allow-prefer
  '("allow_once" "allow-once" "run_once" "yes" "run" "allow")
  "ACP optionIds emagent may return to the agent (never allow_always).")

(defconst emagent-acp--permission-emagent-choices
  '(("Allow" . :allow-once)
    ("Allow for session" . :allow-session)
    ("Allow always" . :allow-always)
    ("Allow all (session)" . :allow-all)
    ("Deny" . :deny))
  "User-facing permission choices handled by emagent, not the external agent.")

(defun emagent-acp--permission-acp-allow-id (options)
  "Return a one-shot allow optionId from OPTIONS, or nil; never allow_always.

emagent always answers the agent one-shot and remembers durable grants on its
own side (~/.emagent), so a user's \"Allow once\" can never become a permanent
agent-side whitelist.  If the agent offers no one-shot allow option, return nil
so the request is cancelled (fail-closed) rather than escalated to allow_always."
  (or (map-elt (seq-find (lambda (opt)
                           (let ((id (downcase (or (map-elt opt 'optionId) ""))))
                             (and id (member id emagent-acp--permission-acp-allow-prefer))))
                 options)
              'optionId)
      (let ((fallback (map-elt (seq-find #'emagent-acp--permission-option-allow-p options)
                               'optionId)))
        (when (and fallback
                   (not (member (downcase fallback) '("allow_always" "allow-always"))))
          fallback))
      (progn
        (when (emagent-acp--permission-option-always-id options)
          (emagent-log "permission: agent offers only allow_always; refusing to escalate a one-shot grant, cancelling"))
        nil)))

(defun emagent-acp--permission-acp-deny-id (options)
  "Return a deny optionId from OPTIONS, or nil."
  (map-elt (seq-find #'emagent-acp--permission-option-deny-p options) 'optionId))

(defun emagent-acp--switch-mode-choices (options)
  "Return (NAME . OPTION-ID) pairs from switch_mode OPTIONS."
  (let (choices)
    (dolist (opt (append options nil))
      (when-let* ((id (map-elt opt 'optionId))
                  ((and (stringp id) (not (string-empty-p id))))
                  (name (or (map-elt opt 'name) id))
                  ((stringp name)))
        (push (cons name id) choices)))
    (nreverse choices)))

(defun emagent-acp--switch-mode-plan-text (tool-call)
  "Return plan text from switch_mode TOOL-CALL content blocks, or nil."
  (when-let ((blocks (append (map-elt tool-call 'content) nil)))
    (let (parts)
      (dolist (block blocks)
        (let* ((inner (or (map-elt block 'content) block))
               (text (or (map-elt inner 'text)
                         (map-elt block 'text)
                         (and (stringp inner) inner))))
          (when (and (stringp text) (not (string-empty-p (string-trim text))))
            (push (string-trim text) parts))))
      (when parts
        (string-join (nreverse parts) "\n\n")))))

(defun emagent-acp--switch-mode-preamble (tool-call)
  "Return an org quote preamble for switch_mode TOOL-CALL plan text."
  (when-let ((text (emagent-acp--switch-mode-plan-text tool-call)))
    (concat "#+begin_quote\n" text "\n#+end_quote\n")))

(defconst emagent-acp--subcommand-programs
  '("git" "npm" "npx" "pnpm" "yarn" "docker" "docker-compose" "kubectl"
    "cargo" "go" "pip" "pip3" "gh" "brew" "apt" "apt-get" "systemctl"
    "make" "gradle" "mvn" "terraform" "helm" "dotnet" "rustup")
  "Programs whose first sub-verb changes what the command does.
For these, an execute fingerprint includes the sub-verb so a grant for e.g.
`git status' does not also auto-approve `git push --force'.")

(defun emagent-acp--execute-subverb (args)
  "Return the sub-verb in ARGS (a command's arguments), or nil.

Skips flags and the value a single-dash short flag consumes, so a global option
with a value (`git -C DIR', `kubectl -n NS', `docker -H HOST') does not make its
value masquerade as the subcommand.  Best-effort: a `-X' short flag is assumed
to take the next word as its value; a `--long' flag is assumed self-contained."
  (let ((consume nil) result)
    (cl-loop for w in args do
             (cond
              ((string-prefix-p "--" w) (setq consume nil))
              ((string-prefix-p "-" w) (setq consume t))
              (consume (setq consume nil))
              (t (setq result w) (cl-return))))
    result))

(defun emagent-acp--leaf-fingerprint (leaf)
  "Return the execute-fingerprint token for one leaf shell command LEAF, or nil.

The token is the program name, plus the sub-verb for
`emagent-acp--subcommand-programs' (so `git status' and `git push' differ).
Comments and empty leaves yield nil.  The program is the first
whitespace-delimited word after dropping leading VAR=VALUE assignments; a plain
whitespace split (rather than a shell tokenizer) is used because patterns like
`grep \"a\\|b\"' confuse `split-string-shell-command'."
  (let* ((words (emagent-policy-match--strip-leading-assignments
                 (split-string (string-trim leaf) "[[:space:]]+" t)))
         (program (car words)))
    (cond
     ((null program) nil)
     ((string-prefix-p "#" program) nil)
     ((member program emagent-acp--subcommand-programs)
      (if-let ((verb (emagent-acp--execute-subverb (cdr words))))
          (format "%s:%s" program verb)
        program))
     (t program))))

(defun emagent-acp--execute-fingerprint (command)
  "Return the execute fingerprint for shell COMMAND.

Keyed on the sorted set of leaf program names within COMMAND (and the sub-verb
for `emagent-acp--subcommand-programs'), so a grant is scoped to the operations
involved rather than the exact argv.  Two commands that differ only in their
path/glob arguments — or in how a pipeline or `VAR=$(...)' assignment is
composed — share one fingerprint, so a single \"Allow for session\" covers
both.  A plain single command keeps its former `execute:PROGRAM[:VERB]' key.
Policy rules still block dangerous commands regardless of any grant."
  (let* ((leaves (or (emagent-policy-shell-commands command)
                     (list (string-trim command))))
         (parts (delete-dups
                 (delq nil (mapcar #'emagent-acp--leaf-fingerprint leaves)))))
    (if parts
        (concat "execute:" (mapconcat #'identity (sort parts #'string<) ","))
      (format "execute:%s"
              (car (split-string (string-trim command) "[[:space:]]+" t))))))

(defun emagent-acp--permission-fingerprint (tool-call)
  "Return a stable fingerprint string for auto-allowing similar TOOL-CALLs.

Execute commands are keyed on the program name (and sub-verb for tools like
git/npm/docker, see `emagent-acp--subcommand-programs').  Policy rules still
block dangerous commands regardless.

Arguments: TOOL-CALL."
  (when tool-call
    (let* ((kind    (downcase (or (emagent-acp--tool-call-infer-kind tool-call) "")))
           (command (emagent-acp--tool-call-command-text tool-call))
           (form    (emagent-acp--tool-call-eval-form tool-call))
           (path    (emagent-acp--tool-call-path tool-call))
           (title   (or (map-elt tool-call 'title) "")))
      (cond
       ((and (string= kind "execute") (stringp command) (not (string-empty-p command)))
        (emagent-acp--execute-fingerprint command))
       (form
        (format "eval:%s" (secure-hash 'sha1 form)))
       ((and (member kind '("read" "write")) path)
        (format "%s:%s" kind path))
       ((not (string-empty-p title))
        (format "%s:%s" (if (string-empty-p kind) "tool" kind) title))
       (command
        (emagent-acp--execute-fingerprint command))
       (t "unknown")))))

(defun emagent-acp--tool-call-execute-p (tool-call)
  "Return non-nil when TOOL-CALL is an execute (shell) request."
  (let ((kind (emagent-acp--tool-call-infer-kind tool-call)))
    (and kind (member kind '("execute")))))

(defun emagent-acp--permission-validate (tool-call)
  "Return nil when TOOL-CALL passes emagent validation.
Otherwise (:deny . REASON) or (:confirm . REASON)."
  (or       (when-let ((form (emagent-acp--tool-call-eval-form tool-call)))
        (emagent-policy-check-elisp form))
      (when-let* ((command (and (emagent-acp--tool-call-execute-p tool-call)
                                (emagent-acp--tool-call-command-text tool-call))))
        (emagent-policy-check-shell command))))

(defun emagent-acp--permission-auto-allowed-p (state fingerprint chat-buffer)
  "Return non-nil when FINGERPRINT is auto-approved for STATE or CHAT-BUFFER."
  (or (emagent-acp-state-session-auto-approve state)
      (and fingerprint
           (or (member fingerprint (emagent-acp-state-permission-auto-allow state))
               (member fingerprint (emagent-permissions-global-fingerprints))
               (member fingerprint
                       (emagent-permissions-session-fingerprints
                        (emagent-acp-state-session-id state)))
               (and chat-buffer (buffer-live-p chat-buffer)
                    (with-current-buffer chat-buffer
                      (or (member fingerprint (emagent-session-allowed-permissions))
                          (member fingerprint
                                  (emagent-permissions-project-fingerprints
                                   (emagent-session-project-directory))))))))))

(defun emagent-acp--permission-stored-auto-choice (state fingerprint chat-buffer)
  "Return the stored user CHOICE that auto-approves FINGERPRINT, or nil.

Arguments: STATE, CHAT-BUFFER."
  (cond
   ((emagent-acp-state-session-auto-approve state) :allow-all)
   ((and fingerprint (member fingerprint (emagent-permissions-global-fingerprints)))
    :allow-always)
   ((and fingerprint
         (member fingerprint (emagent-acp-state-permission-auto-allow state)))
    :allow-session)
   ((and fingerprint
         (member fingerprint
                 (emagent-permissions-session-fingerprints
                  (emagent-acp-state-session-id state))))
    :allow-session)
   ((and fingerprint chat-buffer (buffer-live-p chat-buffer)
         (with-current-buffer chat-buffer
           (or (member fingerprint (emagent-session-allowed-permissions))
               (member fingerprint
                       (emagent-permissions-project-fingerprints
                        (emagent-session-project-directory))))))
    :allow-session)
   (t nil)))

(defun emagent-acp--show-permission-decision (state tool-call choice)
  "Update the permission tool-call line for TOOL-CALL with CHOICE.

Records CHOICE in STATE's :tool-call-decisions table so later tool_call_update
renders of the same line keep the decision suffix instead of dropping it."
  (when (and tool-call choice)
    (when-let* ((id (map-elt tool-call 'toolCallId))
                (update (emagent-acp--tool-call-update-from-request tool-call))
                (merged (emagent-acp--merged-tool-call-update state update))
                (base (emagent-acp--tool-call-label merged))
                (label (emagent-acp--permission-decision-label base choice))
                (buf (emagent-acp--chat-buffer state)))
      (when-let ((decisions (emagent-acp-state-tool-call-decisions state)))
        (puthash id choice decisions))
      (when-let ((cb (emagent-acp-state-cb-tool-call state)))
        (let ((spec (emagent-acp--tool-call-block-spec merged)))
          (with-current-buffer buf
            (funcall cb id label (car spec) (cdr spec))))))))

(defun emagent-acp--permission-gate-auto-approve-p (state tool-call validation fingerprint chat-buffer)
  "Return non-nil when emagent should approve without prompting.

FINGERPRINT identifies the request.  A policy :deny is never auto-approved.
A policy :confirm is auto-approved only
under \"Allow all (session)\" — the explicit user opt-out of prompting.  A
stored fingerprint grant (or the t/safe auto-approve modes) removes the prompt
only for policy-clean requests: it must not silence a :confirm, so e.g. an
`execute:rm' grant made for `rm foo.log' cannot auto-run `rm -rf ~'.

The `safe' mode auto-approves only `read'/`write' tool kinds; `execute', `eval',
and unknown/MCP tools always prompt under `safe' (an eval or MCP call is never
\"safe\" merely because it is not a shell command).

Arguments: STATE, TOOL-CALL, VALIDATION, CHAT-BUFFER."
  (let ((deny (and validation (eq (car validation) :deny)))
        (confirm (and validation (eq (car validation) :confirm))))
    (and (not deny)
         (or (emagent-acp-state-session-auto-approve state)
             (and (not confirm)
                  (or (emagent-acp--permission-auto-allowed-p state fingerprint chat-buffer)
                      (eq emagent-acp-auto-approve-permissions t)
                      (and (eq emagent-acp-auto-approve-permissions 'safe)
                           (member (emagent-acp--tool-call-infer-kind tool-call)
                                   '("read" "write")))))))))

(defun emagent-acp--permission-apply-choice (state fingerprint _chat-buffer choice)
  "Record user CHOICE for FINGERPRINT in STATE."
  (pcase choice
    (:allow-all
     (setf (emagent-acp-state-session-auto-approve state) t)
     (when-let ((session-id (emagent-acp-state-session-id state)))
       (emagent-permissions-set-session-auto-approve session-id))
     (emagent-log "permission: allow all (session)"))
    (:allow-session
     (when fingerprint
       (setf (emagent-acp-state-permission-auto-allow state)
                 (append (emagent-acp-state-permission-auto-allow state)
                         (list fingerprint)))
       (when-let ((session-id (emagent-acp-state-session-id state)))
         (emagent-permissions-add-session-fingerprint session-id fingerprint))))
    (:allow-always
     (when fingerprint
       (emagent-permissions-add-global-fingerprint fingerprint)))
    (_ nil)))

(defun emagent-acp--permission-approved-choice-p (choice)
  "Return non-nil when user CHOICE approves the request."
  (memq choice '(:allow-once :allow-session :allow-always :allow-all)))

(defun emagent-acp--permission-option-allow-p (opt)
  "Return non-nil when OPT is an allow-type ACP permission option."
  (let ((kind (downcase (or (map-elt opt 'kind) "")))
        (id (downcase (or (map-elt opt 'optionId) "")))
        (name (downcase (or (map-elt opt 'name) ""))))
    (or (member kind '("allow" "allow_once" "allow_always" "allow-once" "allow-always"))
        (member id '("allow_once" "allow-once" "allow_always" "allow-always" "allow" "yes" "run" "run_once"))
        (string-match-p "allow" id)
        (string-match-p "\\`\\(?:allow\\|yes\\|run\\)" name))))

(defun emagent-acp--tool-call-shell-needs-confirm-p (tool-call)
  "Return non-nil when TOOL-CALL is execute and policy requires confirmation."
  (when-let ((command (and (emagent-acp--tool-call-execute-p tool-call)
                            (emagent-acp--tool-call-command-text tool-call))))
    (emagent-policy-shell-needs-confirm-p command)))

(defun emagent-acp--permission-tool-call (state tool-call)
  "Return TOOL-CALL merged with session inputs and provider enrichment.

Cursor tool-call notifications skip sync store.db lookups (they freeze
Emacs); permission prompts still enrich once here because the user is
already waiting on the dialog.

Arguments: STATE, TOOL-CALL."
  (when tool-call
    (let* ((update (or (emagent-acp--tool-call-update-from-request tool-call)
                       tool-call))
           (merged (emagent-acp--merged-tool-call-update state update))
           (enriched (emagent-acp--provider-enrich-tool-call state merged)))
      ;; Cursor's provider enrich is intentionally a no-op on the hot path.
      ;; A single sync store.db read is acceptable when showing a permission
      ;; dialog so the user sees the real command/path.
      (if (and (eq (emagent-acp--provider-symbol state) 'cursor)
               (fboundp 'emagent-cursor-enrich-tool-call-update)
               (emagent-acp-state-session-id state))
          (emagent-cursor-enrich-tool-call-update
           (emagent-acp-state-session-id state) enriched)
        enriched))))

(defun emagent-acp--tool-call-write-content-block (tool-call raw _detail path)
  "Return an Allow-edit org block for TOOL-CALL's write to PATH from RAW."
  (when (and path (not (string-empty-p path)))
    (let* ((data (emagent-acp--tool-call-normalize-data raw))
           (resolved (emagent-tools--root-directory path))
           (heading (format "Allow edit: %s" (file-name-nondirectory resolved))))
      (if-let ((diff (when data
                       (emagent-acp--tool-call-edit-diff-string
                        path data (map-elt tool-call 'toolCallId)))))
          (format "** %s\n#+BEGIN_SRC diff\n%s\n#+END_SRC" heading diff)
        (if-let ((proposed (emagent-acp--tool-call-proposed-content path data)))
            (let ((lang (or (file-name-extension resolved) "text")))
              (format "** %s\n#+BEGIN_SRC %s\n%s\n#+END_SRC"
                      heading lang
                      (substring proposed 0 (min (length proposed) 4000))))
          (format "** Allow edit\n= %s =" resolved))))))

(defun emagent-acp--tool-call-content-block (tool-call)
  "Return an org subsection string for the permission prompt, or nil.
For eval, shell, and edit tool calls, return a code block with the payload.
Edit prompts prefer a unified diff; patch edits fall back to a hunk preview.

Arguments: TOOL-CALL."
  (when tool-call
    (or (when-let ((form (emagent-acp--tool-call-eval-form tool-call)))
          (format "** Allow eval\n#+BEGIN_SRC elisp\n%s\n#+END_SRC" form))
        (let* ((kind (emagent-acp--tool-call-infer-kind tool-call))
               (raw (or (map-elt tool-call 'rawInput)
                        (map-elt tool-call 'arguments)))
               (command (emagent-acp--tool-call-command-text tool-call))
               (detail (emagent-acp--tool-call-detail-from-tool-call tool-call)))
          (when kind
            (cond
             ((member kind '("execute" ""))
              (when command
                (if-let ((heredoc (emagent-acp--tool-call-heredoc-script command)))
                    (format "** Allow execute\n#+BEGIN_SRC %s\n%s\n#+END_SRC"
                            (car heredoc) (cdr heredoc))
                  (format "** Allow execute\n#+BEGIN_SRC sh\n%s\n#+END_SRC" command))))
             ((emagent-acp--tool-call-write-kind-p kind)
              (or (when-let ((path (emagent-acp--tool-call-write-path
                                    tool-call raw detail)))
                    (emagent-acp--tool-call-write-content-block
                     tool-call raw detail path))
                  (when command
                    (format "** Allow edit\n#+BEGIN_SRC sh\n%s\n#+END_SRC"
                            command))))
             (t nil)))))))

(provide 'emagent-acp-permit)
;;; emagent-acp-permit.el ends here
