;;; emagent-acp.el --- ACP wire-up for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.3.1
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
;; ACP facade: Cursor provider, permissions/tools, and session control.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'json)
(require 'subr-x)
(require 'emagent-acp-protocol)
(require 'emagent-chat)
(require 'emagent-chat-ui)
(require 'emagent-log)
(require 'emagent-session)
(require 'emagent-tools)

(eval-when-compile
  (require 'cl-lib))

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

(defun emagent-acp--edit-diff-cache-forget (id)
  "Drop the cached edit diff for toolCallId ID, if any."
  (when id
    (remhash id emagent-acp--edit-diff-cache)
    (setq emagent-acp--edit-diff-cache-order
          (delete id emagent-acp--edit-diff-cache-order))))

(defun emagent-acp--edit-diff-cache-clear ()
  "Drop all cached edit diffs.
Called at turn start; the org transcript already holds rendered diffs."
  (clrhash emagent-acp--edit-diff-cache)
  (setq emagent-acp--edit-diff-cache-order nil))

(defun emagent-acp--release-tool-call-payloads (state id)
  "Drop large in-flight payloads for toolCallId ID in STATE.
The chat buffer already holds the rendered tool line; keep title/label/
decision maps for arrow-line updates."
  (when (and state id)
    (when-let ((inputs (emagent-acp-state-tool-call-inputs state)))
      (remhash id inputs))
    (when-let ((pending (emagent-acp-state-tool-call-pending state)))
      (remhash id pending))
    (emagent-acp--edit-diff-cache-forget id)))

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

Detection relies on the emagent MCP namespace (e.g. `mcp_emagent_fs'):
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
         (completed (member status '("completed" "failed" "cancelled")))
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
          (emagent-acp--release-tool-call-payloads state id)
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
and a `stop' call cancels a pending wakeup immediately.
Compress turns ignore schedule requests (stop still cancels)."
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
         ((emagent-acp-state-compress-pending state)
          (emagent-log "wakeup: ignored during compress"))
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
      (unless (emagent-acp-state-compress-pending state)
        (emagent-acp--explore-note-tool-kind
         (or kind (map-elt merged 'kind)))
        (when defer
          (puthash id merged pending-table)
          (emagent-acp--provider-enqueue-tool-resolve state id))
        (when show
          (emagent-acp--emit-tool-call-display state id kind merged label status)
          (when id (remhash id pending-table)))
        (when (emagent-acp-state-permission-queue state)
          (unless (fboundp 'emagent-acp--drain-permission-queue)
            (require 'emagent-acp))
          (emagent-acp--drain-permission-queue state))))))

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
       ((emagent-acp-state-compress-pending state)
        (emagent-log "permission cancelled during compress: %s" question)
        (funcall respond :deny))
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

(defun emagent-guard--path-verdict (path)
  "Return an authorization verdict for file PATH.
Resolves and confines PATH via `emagent-tools--root-directory', turning its
boundary/protected-tree signal into a `:deny' verdict."
  (condition-case err
      (cons :allow (emagent-tools--root-directory path))
    (error (cons :deny (error-message-string err)))))

(defun emagent-guard-check (op payload)
  "Return the authorization verdict for OP applied to PAYLOAD.

OP is one of:
  `read' `write' `delete' — PAYLOAD is a file path; a `:allow' verdict carries
                            the resolved canonical path.
  `shell'                 — PAYLOAD is a command string.
  `eval'                  — PAYLOAD is an elisp form string.

See the commentary for the verdict shape."
  (pcase op
    ((or 'read 'write 'delete) (emagent-guard--path-verdict payload))
    ('shell (or (emagent-policy-check-shell payload) '(:allow . t)))
    ('eval  (or (emagent-policy-check-elisp payload) '(:allow . t)))
    (_ (cons :deny (format "unknown guarded operation: %S" op)))))

(defun emagent-guard-allow-p (verdict)
  "Return non-nil when VERDICT authorizes the effect."
  (eq (car-safe verdict) :allow))

(defun emagent-guard-deny-p (verdict)
  "Return non-nil when VERDICT refuses the effect outright."
  (eq (car-safe verdict) :deny))

(defun emagent-guard-resolved (verdict)
  "Return the resolved value of an allowing VERDICT, or nil."
  (and (eq (car-safe verdict) :allow) (cdr verdict)))

(defun emagent-guard-reason (verdict)
  "Return the human-readable reason string of VERDICT, or nil."
  (and (memq (car-safe verdict) '(:deny :confirm)) (cdr verdict)))

(defvar emagent-tools--root-boundary)
(defvar emagent-tools-age--session-key)
(defvar emagent-usage--session-key)

(defvar emagent-tools--project-directory)

(defun emagent-acp--fs-session-root (state)
  "Return the project root ACP fs/* operations must stay within, or nil.

Mirrors the boundary the MCP dispatcher binds for its tools; without it the
fs/* handlers would resolve agent-supplied paths with no project confinement.

Arguments: STATE."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (ignore-errors (emagent-session-project-directory))))))

(defun emagent-acp--fs-unavailable-response (method)
  
  "Internal helper for METHOD."
  (emagent-acp-make-error
   :code -32601
   :message (format "%s disabled; use the external agent's project file tools"
                    method)))

(defun emagent-acp--fs-send (client make request-id &rest response-args)
  "Send the fs response built by MAKE for REQUEST-ID over CLIENT.
MAKE is a `*-text-file-response' constructor; RESPONSE-ARGS are its remaining
keyword arguments (`:content' or `:error')."
  (emagent-acp-send-response
   :client client
   :response (apply make :request-id request-id response-args)))

(defun emagent-acp--fs-send-error (client make request-id code message)
  "Send an fs error response with CODE and MESSAGE (see `emagent-acp--fs-send').

Arguments: CLIENT, MAKE, REQUEST-ID."
  (emagent-acp--fs-send client make request-id
                        :error (emagent-acp-make-error :code code :message message)))

(cl-defun emagent-acp--on-fs-read (&key state emagent-acp-request)
  
  "Internal helper for STATE and EMAGENT-ACP-REQUEST."
  (let ((client (emagent-acp-state-client state))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path)))
        (make #'emagent-acp-make-fs-read-text-file-response))
    (if (not emagent-acp-file-access)
        (emagent-acp--fs-send client make request-id
                              :error (emagent-acp--fs-unavailable-response
                                      "fs/read_text_file"))
      ;; Confine to the session root; fall back to the ambient project directory
      ;; rather than nil (no confinement) when the chat buffer is gone, so a
      ;; missing session root cannot open up unconfined filesystem access.
      (let* ((emagent-tools--root-boundary
              (or (emagent-acp--fs-session-root state)
                  emagent-tools--project-directory))
             (emagent-tools--project-directory
              (or emagent-tools--root-boundary emagent-tools--project-directory))
             (verdict (emagent-guard-check 'read path)))
        (if (not (emagent-guard-allow-p verdict))
            (emagent-acp--fs-send-error client make request-id -32603
                                        (emagent-guard-reason verdict))
          (condition-case err
              (let* ((canonical (emagent-guard-resolved verdict))
                     (line (or (map-nested-elt emagent-acp-request '(params line)) 1))
                     (limit (map-nested-elt emagent-acp-request '(params limit)))
                     (content (emagent-tools--read-file-content canonical line limit)))
                (emagent-acp--fs-send client make request-id :content content))
            (file-missing
             (emagent-acp--fs-send-error client make request-id -32002
                                         "Resource not found"))
            (error
             (emagent-acp--fs-send-error client make request-id -32603
                                         (error-message-string err)))))))))

(cl-defun emagent-acp--on-fs-write (&key state emagent-acp-request)
  
  "Internal helper for STATE and EMAGENT-ACP-REQUEST."
  (let ((client (emagent-acp-state-client state))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path)))
        (content (or (map-nested-elt emagent-acp-request '(params content)) ""))
        (make #'emagent-acp-make-fs-write-text-file-response))
    (if (not emagent-acp-file-access)
        (emagent-acp--fs-send client make request-id
                              :error (emagent-acp--fs-unavailable-response
                                      "fs/write_text_file"))
      ;; Confine to the session root; fall back to the ambient project directory
      ;; rather than nil (no confinement) when the chat buffer is gone, so a
      ;; missing session root cannot open up unconfined filesystem access.
      (let* ((emagent-tools--root-boundary
              (or (emagent-acp--fs-session-root state)
                  emagent-tools--project-directory))
             (emagent-tools--project-directory
              (or emagent-tools--root-boundary emagent-tools--project-directory))
             (verdict (emagent-guard-check 'write path)))
        (if (not (emagent-guard-allow-p verdict))
            (emagent-acp--fs-send-error client make request-id -32603
                                        (emagent-guard-reason verdict))
          (let ((resolved (emagent-guard-resolved verdict)))
            (when emagent-acp-confirm-fs-writes
              (emagent-acp--prepare-interactive-context state))
            (condition-case err
                (if (and emagent-acp-confirm-fs-writes
                         (not (emagent-tools--confirm-write
                               'emagent-tool-write-file resolved content
                               (emagent-acp--chat-buffer state))))
                    (emagent-acp--fs-send-error client make request-id -32603
                                                "Write denied by user")
                  (let ((written (emagent-tools--write-file-content resolved content)))
                    (emagent-acp--notify-user
                     state (format "emagent: wrote %s (C-/ to undo in that buffer)"
                                   written))
                    (emagent-acp--fs-send client make request-id)))
              (error
               (emagent-acp--fs-send-error client make request-id -32603
                                           (error-message-string err))))))))))

(cl-defun emagent-acp--on-permission (&key state emagent-acp-request)
  
  "Internal helper for STATE and EMAGENT-ACP-REQUEST."
  (setf (emagent-acp-state-permission-queue state)
            (append (emagent-acp-state-permission-queue state) (list emagent-acp-request)))
  (emagent-acp--drain-permission-queue state))

(cl-defun emagent-acp--on-request (&key state emagent-acp-request)
  "Dispatch an inbound ACP request for STATE.

Handles fs/*, session/request_permission, and Cursor extension methods
cursor/create_plan and cursor/ask_question (blocking).

Arguments: EMAGENT-ACP-REQUEST."
  (pcase (map-elt emagent-acp-request 'method)
    ("fs/read_text_file"
     (emagent-acp--on-fs-read :state state :emagent-acp-request emagent-acp-request))
    ("fs/write_text_file"
     (emagent-acp--on-fs-write :state state :emagent-acp-request emagent-acp-request))
    ("session/request_permission"
     (emagent-acp--on-permission :state state :emagent-acp-request emagent-acp-request))
    ("cursor/create_plan"
     (emagent-acp--on-create-plan :state state :emagent-acp-request emagent-acp-request))
    ("cursor/ask_question"
     (emagent-acp--on-ask-question :state state :emagent-acp-request emagent-acp-request))
    (_
     (emagent-acp-send-response
      :client (emagent-acp-state-client state)
      :response `((:request-id . ,(map-elt emagent-acp-request 'id))
                  (:error . ,(emagent-acp-make-error
                              :code -32601
                              :message (format "Unsupported method: %s"
                                               (map-elt emagent-acp-request 'method)))))))))

(defun emagent-acp--cursor-ext-params (request)
  "Return params alist from Cursor extension REQUEST."
  (or (map-elt request 'params) (map-elt request :params)))

(defun emagent-acp--cursor-auto-accept-plan-p (state)
  "Return non-nil when STATE should accept `cursor/create_plan' without prompting.

Noninteractive sessions always auto-accept.  Interactively, honor
`emagent-acp-auto-accept-plans' (default nil = prompt)."
  (or noninteractive
      (pcase emagent-acp-auto-accept-plans
        ('t t)
        ('nil nil)
        (_
         (or (emagent-acp-state-session-auto-approve state)
             (eq emagent-acp-auto-approve-permissions t))))))

(defun emagent-acp--format-create-plan-text (params)
  "Return display text for a `cursor/create_plan' PARAMS alist."
  (let* ((name (map-elt params 'name))
         (overview (map-elt params 'overview))
         (plan (map-elt params 'plan))
         (todos (map-elt params 'todos))
         (parts nil))
    (when (and (stringp name) (not (string-empty-p (string-trim name))))
      (push (format "Plan: %s" (string-trim name)) parts))
    (when (and (stringp overview) (not (string-empty-p (string-trim overview))))
      (push (string-trim overview) parts))
    (when (and (stringp plan) (not (string-empty-p (string-trim plan))))
      (push (string-trim plan) parts))
    (when todos
      (let ((lines
             (delq nil
                   (mapcar
                    (lambda (todo)
                      (let ((content (map-elt todo 'content))
                            (status (or (map-elt todo 'status) "pending")))
                        (when (and (stringp content)
                                   (not (string-empty-p (string-trim content))))
                          (format "- [%s] %s" status (string-trim content)))))
                    (append todos nil)))))
        (when lines
          (push (concat "Todos:\n" (string-join lines "\n")) parts))))
    (string-join (nreverse parts) "\n\n")))

(defun emagent-acp--create-plan-preamble (text)
  "Return an org quote block wrapping plan TEXT for approval."
  (concat "#+begin_quote\n"
          (string-trim text)
          "\n#+end_quote\n"))

(defun emagent-acp--insert-create-plan-thought (state text)
  "Append plan TEXT into STATE's chat Thinking section when possible."
  (when-let* ((buf (emagent-acp--chat-buffer state))
              (trimmed (string-trim (or text ""))))
    (unless (string-empty-p trimmed)
      (with-current-buffer buf
        (when (and (fboundp 'emagent-chat-append-thought)
                   (fboundp 'emagent-chat--flush-thought-pending)
                   (or (not (fboundp 'emagent-chat--open-response-p))
                       (emagent-chat--open-response-p)))
          (emagent-chat-append-thought (concat "\n\n" trimmed "\n"))
          (emagent-chat--flush-thought-pending t))))))

(defun emagent-acp--persist-create-plan (state params)
  "Write create_plan PARAMS under ~/.cursor/plans/; return file:// URI.

Arguments: STATE."
  (let* ((dir (expand-file-name "~/.cursor/plans"))
         (raw-name (or (map-elt params 'name) "Plan"))
         (slug (replace-regexp-in-string
                "[^a-zA-Z0-9-_ ]" "" (format "%s" raw-name)))
         (slug (string-trim (substring slug 0 (min 50 (length slug)))))
         (slug (if (string-empty-p slug) "Plan" slug))
         (sid (or (emagent-acp-state-session-id state) "session"))
         (suffix (substring sid 0 (min 8 (length sid))))
         (file (expand-file-name
                (format "%s-%s.plan.md" slug suffix) dir))
         (plan (or (map-elt params 'plan) ""))
         (overview (map-elt params 'overview))
         (parts (list (format "<!-- %s -->" sid)))
         (body nil))
    (when (and (stringp overview)
               (not (string-empty-p (string-trim overview))))
      (setq parts (append parts (list (string-trim overview)))))
    (when (and (stringp plan) (not (string-empty-p plan)))
      (setq parts (append parts (list plan))))
    (setq body (concat (string-join parts "\n\n") "\n"))
    (make-directory dir t)
    (with-temp-file file (insert body))
    (concat "file://" (expand-file-name file))))

(defun emagent-acp--plan-build-prompt (plan-uri params)
  "Return the follow-up Build prompt for PLAN-URI and PARAMS."
  (let ((name (or (map-elt params 'name) "the approved plan")))
    (format
     (concat "Build the approved plan %S (%s). Execute its todos now; "
             "do not stop after planning - implement and verify.")
     name plan-uri)))

(defun emagent-acp--queue-plan-build (state plan-uri params)
  "Queue a Build follow-up on STATE after create_plan accept.

PLAN-URI and PARAMS feed the execute prompt text.  Defers the chat
user-heading stub until Build starts so Accept does not leave an empty
`* user>' between the plan dialog and agent work."
  (when emagent-acp-auto-build-plans
    (setf (emagent-acp-state-plan-build-prompt state)
          (emagent-acp--plan-build-prompt plan-uri params))
    (when-let ((buf (emagent-acp--chat-buffer state)))
      (with-current-buffer buf
        (setq emagent-chat--defer-user-stub t)))
    (emagent-log "cursor/create_plan: queued Build turn")))

(defun emagent-acp--send-create-plan-outcome (state request-id outcome
                                                     &optional reason
                                                     plan-uri)
  "Reply to `cursor/create_plan' REQUEST-ID for STATE with OUTCOME.
OUTCOME is a string: accepted, rejected, or cancelled.  REASON is
optional.  PLAN-URI is sent when accepting.

Arguments: STATE, REQUEST-ID."
  (emagent-log "cursor/create_plan response: %s%s"
               outcome
               (if reason (format " (%s)" reason) ""))
  (emagent-acp-send-response
   :client (emagent-acp-state-client state)
   :response (emagent-acp-make-cursor-create-plan-response
              :request-id request-id
              :outcome outcome
              :reason reason
              :plan-uri plan-uri)))

(defun emagent-acp--accept-create-plan (state request-id params)
  "Accept create_plan for STATE: persist plan, queue Build, reply.

Arguments: REQUEST-ID, PARAMS."
  (let ((plan-uri (emagent-acp--persist-create-plan state params)))
    (emagent-acp--queue-plan-build state plan-uri params)
    (emagent-acp--send-create-plan-outcome
     state request-id "accepted" nil plan-uri)))

(cl-defun emagent-acp--on-create-plan (&key state emagent-acp-request)
  "Handle blocking Cursor `cursor/create_plan' for STATE.

Shows the plan for approval (default) or auto-accepts when configured.
Accept persists a plan file, returns planUri, and queues a Build
follow-up turn (see `emagent-acp-auto-build-plans').

Arguments: EMAGENT-ACP-REQUEST."
  (let* ((request-id (map-elt emagent-acp-request 'id))
         (params (emagent-acp--cursor-ext-params emagent-acp-request))
         (text (emagent-acp--format-create-plan-text params))
         (buf (emagent-acp--chat-buffer state)))
    (emagent-log "cursor/create_plan: name=%s plan-chars=%d"
                 (or (map-elt params 'name) "?")
                 (length (or (map-elt params 'plan) "")))
    (emagent-acp--insert-create-plan-thought state text)
    (cond
     ((emagent-acp--cursor-auto-accept-plan-p state)
      (emagent-acp--accept-create-plan state request-id params))
     (t
      (unless (fboundp 'emagent-acp--prepare-interactive-context)
        (require 'emagent-acp))
      (emagent-acp--prepare-interactive-context state)
      (emagent-acp--clear-prompt-watchdog state)
      (emagent-tools--buttons-prompt
       (if emagent-acp-auto-build-plans
           "Accept and build this plan?"
         "Accept this plan?")
       (if emagent-acp-auto-build-plans
           '(("Accept & Build" . :accept) ("Reject" . :reject))
         '(("Accept" . :accept) ("Reject" . :reject)))
       buf
       (lambda (choice)
         (when (emagent-acp-state-busy state)
           (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
             (require 'emagent-acp))
           (emagent-acp--schedule-prompt-watchdog state))
         (emagent-acp--refresh-mode-line state)
         (pcase choice
           (:accept
            (emagent-acp--accept-create-plan state request-id params))
           (:reject
            (emagent-acp--cancel-plan-build state)
            (emagent-acp--send-create-plan-outcome
             state request-id "rejected" "User rejected the plan"))
           (_
            (emagent-acp--cancel-plan-build state)
            (emagent-acp--send-create-plan-outcome
             state request-id "cancelled"))))
       (emagent-acp--create-plan-preamble text))))))

(defun emagent-acp--ask-question-default-answers (params)
  "Return default answered outcome payload for ask_question PARAMS."
  (let ((questions (append (map-elt params 'questions) nil))
        answers)
    (dolist (q questions)
      (let* ((qid (map-elt q 'id))
             (options (append (map-elt q 'options) nil))
             (first (car options))
             (oid (and first (map-elt first 'id))))
        (when (and qid oid)
          (push `((questionId . ,qid)
                  (selectedOptionIds . ,(vector oid)))
                answers))))
    `((outcome . "answered")
      (answers . ,(apply #'vector (nreverse answers))))))

(defun emagent-acp--send-ask-question-result (state request-id result)
  "Send ask_question RESULT for REQUEST-ID on STATE."
  (emagent-log "cursor/ask_question response: %s"
               (or (map-elt result 'outcome) "?"))
  (emagent-acp-send-response
   :client (emagent-acp-state-client state)
   :response `((:request-id . ,request-id)
               (:result . ((outcome . ,result))))))

(cl-defun emagent-acp--on-ask-question (&key state emagent-acp-request)
  "Handle blocking Cursor `cursor/ask_question' for STATE.

Arguments: EMAGENT-ACP-REQUEST."
  (let* ((request-id (map-elt emagent-acp-request 'id))
         (params (emagent-acp--cursor-ext-params emagent-acp-request))
         (questions (append (map-elt params 'questions) nil))
         (title (or (map-elt params 'title) "Question"))
         (buf (emagent-acp--chat-buffer state)))
    (emagent-log "cursor/ask_question: title=%s questions=%d"
                 title (length questions))
    (cond
     ((or noninteractive (null questions))
      (emagent-acp--send-ask-question-result
       state request-id
       (if questions
           (emagent-acp--ask-question-default-answers params)
         '((outcome . "skipped") (reason . "No questions")))))
     (t
      ;; Interactive: answer questions sequentially, then respond once.
      (unless (fboundp 'emagent-acp--prepare-interactive-context)
        (require 'emagent-acp))
      (emagent-acp--prepare-interactive-context state)
      (emagent-acp--clear-prompt-watchdog state)
      (let* ((remaining questions)
             (answers nil)
             (finish
              (lambda (result)
                (when (emagent-acp-state-busy state)
                  (unless (fboundp 'emagent-acp--schedule-prompt-watchdog)
                    (require 'emagent-acp))
                  (emagent-acp--schedule-prompt-watchdog state))
                (emagent-acp--refresh-mode-line state)
                (emagent-acp--send-ask-question-result state request-id result)))
             (ask-next nil))
        (setq ask-next
              (lambda ()
                (if (null remaining)
                    (funcall finish
                             `((outcome . "answered")
                               (answers . ,(apply #'vector (nreverse answers)))))
                  (let* ((q (car remaining))
                         (qid (map-elt q 'id))
                         (prompt (or (map-elt q 'prompt) title))
                         (options (append (map-elt q 'options) nil))
                         (choices
                          (mapcar (lambda (opt)
                                    (cons (or (map-elt opt 'label)
                                              (map-elt opt 'id)
                                              "?")
                                          (map-elt opt 'id)))
                                  options)))
                    (setq remaining (cdr remaining))
                    (if (null choices)
                        (funcall ask-next)
                      (emagent-tools--buttons-prompt
                       prompt
                       (append choices '(("Skip" . :skip)))
                       buf
                       (lambda (choice)
                         (cond
                          ((eq choice :skip)
                           (funcall finish
                                    '((outcome . "skipped")
                                      (reason . "User skipped"))))
                          (t
                           (when (and qid choice)
                             (push `((questionId . ,qid)
                                     (selectedOptionIds . ,(vector choice)))
                                   answers))
                           (funcall ask-next))))))))))
        (funcall ask-next))))))

(defun emagent-acp-attach-context (text)
  "Attach TEXT to the next prompt in the current buffer."
  (let ((state (emagent-acp--session)))
    (setf (emagent-acp-state-extra-context state)
              (append (emagent-acp-state-extra-context state) (list text)))))

(defun emagent-acp--image-media-type (ext)
  "Return the MIME type string for image extension EXT, or nil if not an image."
  (pcase (downcase (or ext ""))
    ("png"  "image/png")
    ("jpg"  "image/jpeg")
    ("jpeg" "image/jpeg")
    ("gif"  "image/gif")
    ("webp" "image/webp")
    (_      nil)))

(defcustom emagent-acp-attach-max-images 4
  "Maximum image attachments extracted from one prompt."
  :type 'integer
  :group 'emagent)

(defcustom emagent-acp-attach-max-bytes 2097152
  "Maximum bytes per attached image (pre-base64)."
  :type 'integer
  :group 'emagent)

(defun emagent-acp--extract-image-links (text)
  "Extract [[file:...]] image links from TEXT.

Scans for org file links whose paths end in PNG/JPEG/GIF/WebP, reads and
base64-encodes each file, and removes the link from the text.  Non-image
links and unreadable paths are left in place.  At most
`emagent-acp-attach-max-images' images are attached; files larger than
`emagent-acp-attach-max-bytes' stay as text links.

Returns (CLEANED-TEXT . IMAGES) where IMAGES is a list of
 ((media-type . TYPE) (data . BASE64)) plists."
  (let ((link-re "\\[\\[file:\\([^]
]+\\)\\]\\(?:\\[[^]]*\\]\\)?\\]")
        images parts (pos 0) skipped)
    (while (string-match link-re text pos)
      (let* ((link-beg (match-beginning 0))
             (link-end (match-end 0))
             (path (match-string 1 text))
             (expanded (expand-file-name path))
             (media-type (emagent-acp--image-media-type
                          (file-name-extension expanded))))
        (push (substring text pos link-beg) parts)
        (cond
         ((not (and media-type (file-readable-p expanded)))
          (push (substring text link-beg link-end) parts))
         ((>= (length images) emagent-acp-attach-max-images)
          (setq skipped (1+ (or skipped 0)))
          (push (substring text link-beg link-end) parts))
         (t
          (let ((nbytes (file-attribute-size (file-attributes expanded))))
            (if (and (integerp nbytes)
                     (> nbytes emagent-acp-attach-max-bytes))
                (progn
                  (setq skipped (1+ (or skipped 0)))
                  (push (substring text link-beg link-end) parts))
              (let ((data (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (insert-file-contents-literally expanded)
                            (base64-encode-region (point-min) (point-max) t)
                            (buffer-string))))
                (push `((media-type . ,media-type) (data . ,data)) images)
                (when (fboundp 'emagent-usage-tax-add)
                  (emagent-usage-tax-add 'images 1)))))))
        (setq pos link-end)))
    (push (substring text pos) parts)
    (when (and skipped (> skipped 0))
      (push (format
             "\n[emagent: skipped %d image attachment(s); max %d files / %d bytes each]"
             skipped
             emagent-acp-attach-max-images
             emagent-acp-attach-max-bytes)
            parts))
    (cons (string-trim (apply #'concat (nreverse parts)))
          (nreverse images))))


(defconst emagent-acp--materialize-prompt-text
  (concat "Acknowledge that this compacted session is ready. "
          "Reply with exactly: ready. Do not use tools.")
  "Quiet prompt text that forces the agent to persist a new session.

Cursor ACP creates only meta.json until the first session/prompt; without
this turn, compact then restart fails session/load.")

(defun emagent-acp--materialize-session (state)
  "Send a quiet prompt so STATE's new session is durable across restarts.

Called after /compact creates a fresh session/new.  The reply is not
rendered into the chat buffer."
  (when-let ((session-id (emagent-acp-state-session-id state)))
    (when (and (emagent-acp-state-ready state)
               (not (emagent-acp-state-busy state)))
      (emagent-log "materializing compacted session…")
      (emagent-acp--progress state "materializing compacted session…")
      (setf (emagent-acp-state-quiet-prompt state) t)
      (emagent-acp--turn-begin state)
      (emagent-acp--dispatch-prompt-request
       :state state
       :session-id session-id
       :blocks `[((type . "text")
                  (text . ,emagent-acp--materialize-prompt-text))]
       :images nil
       :gen (emagent-acp-state-prompt-generation state)
       :attempt 1))))

(defun emagent-acp--schedule-prompt-retry (state session-id blocks images gen attempt reason)
  "Re-dispatch the in-flight prompt after exponential backoff.

REASON is a short human-readable phrase describing why the retry fires; it is
shown to the user together with the attempt count.  The GEN guard prevents a
stale retry from firing after the prompt was superseded or interrupted.

Arguments: STATE, SESSION-ID, BLOCKS, IMAGES."
  (let* ((delay (emagent-acp--prompt-retry-delay attempt))
         (next (1+ attempt)))
    (setf (emagent-acp-state-prompt-retry-gen state) gen)
    (emagent-acp--notify-user
     state
     (format "emagent: %s; retrying prompt (%d/%d) in %.1fs"
             reason next emagent-acp-prompt-retry-attempts delay))
    (emagent-acp--schedule-prompt-watchdog state)
    (run-with-timer
     delay nil
     (lambda ()
       (setf (emagent-acp-state-prompt-retry-gen state) nil)
       (if (and (eq (emagent-acp-state-prompt-generation state) gen)
                (emagent-acp-state-busy state))
           (emagent-acp--dispatch-prompt-request
            :state state :session-id session-id
            :blocks blocks :images images
            :gen gen :attempt next)
         (emagent-log "emagent: prompt retry skipped (busy=%s gen=%s/%s)"
                      (if (emagent-acp-state-busy state) "yes" "no")
                      (emagent-acp-state-prompt-generation state)
                      gen))))))

(defun emagent-acp--log-transient-error (state &optional message)
  "Log MESSAGE and STATE's partial assistant output to `emagent-log-buffer-name'.

Used when a transient error ends an in-flight turn: the details are recorded in
the log instead of the chat buffer, and the turn is then resumed with
\"continue\" (see `emagent-acp--schedule-continue')."
  (when (and message (not (string-empty-p message)))
    (emagent-log "transient error: %s" message))
  (let ((text (string-trim (or (emagent-acp-state-assistant-text state) ""))))
    (unless (string-empty-p text)
      (emagent-log "partial output before auto-continue:\n%s" text))))

(defun emagent-acp--schedule-continue (state session-id images gen reason)
  "Resume an errored in-flight turn by re-dispatching a \"continue\" prompt.

Unlike `emagent-acp--schedule-prompt-retry' (which replays the ORIGINAL prompt
and is only safe when the turn did no work), this sends a fresh \"continue\"
turn so tool side effects such as commits or pushes are never repeated.  The
open response block is kept, so the continued output renders into it; the
transient error itself is only logged (see `emagent-acp--log-transient-error'),
never rendered into the chat buffer.  REASON is logged with the attempt count;
the `:continue-attempts' counter bounds the number of resumes and the GEN guard
cancels a stale resume after an interrupt or new prompt.

Arguments: STATE, SESSION-ID, IMAGES."
  (let* ((attempt (1+ (or (emagent-acp-state-continue-attempts state) 0)))
         (delay (emagent-acp--prompt-retry-delay attempt)))
    (setf (emagent-acp-state-continue-attempts state) attempt)
    (emagent-acp--notify-user
     state
     (format "emagent: %s; auto-continuing (%d/%d) in %.1fs"
             reason attempt emagent-acp-prompt-retry-attempts delay))
    (emagent-acp--schedule-prompt-watchdog state)
    (run-with-timer
     delay nil
     (lambda ()
       (when (and (eq (emagent-acp-state-prompt-generation state) gen)
                  (emagent-acp-state-busy state))
         (emagent-acp--dispatch-prompt-request
          :state state :session-id session-id
          :blocks [((type . "text") (text . "continue"))]
          :images images
          :gen gen :attempt 1))))))

(cl-defun emagent-acp--dispatch-prompt-request (&key state session-id blocks images gen attempt)
  "Send the session/prompt request, recovering from transient network failures.

ATTEMPT is the 1-based try count.  Recovery depends on how the failure arrives
and whether the turn already did work:

- Pure transient failure with no tool calls or content
  (`emagent-acp--agent-error-only-response-p' /
  `emagent-acp--turn-did-no-work-p') is replayed with exponential backoff up to
  `emagent-acp-prompt-retry-attempts' via `emagent-acp--schedule-prompt-retry'.

- A turn that already ran tool calls or produced content but ended on a
  transient error (`emagent-acp--turn-hit-transient-error-p') is resumed by
  auto-sending \"continue\" via `emagent-acp--schedule-continue', so side
  effects such as commits or pushes are never repeated.  The error is logged to
  `emagent-log-buffer-name' rather than rendered into the chat buffer.

GEN guards against a stale retry firing after the prompt was superseded or
interrupted.

Arguments: STATE, SESSION-ID, BLOCKS, IMAGES."
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-prompt-request
             :session-id session-id :prompt blocks :images images)
   :on-success
   (lambda (response)
     (when (eq (emagent-acp-state-prompt-generation state) gen)
       (cond
        ((and (emagent-acp-state-busy state)
              (< attempt emagent-acp-prompt-retry-attempts)
              (emagent-acp--agent-error-only-response-p state))
         (let ((message (string-trim (or (emagent-acp-state-assistant-text state) ""))))
           (setf (emagent-acp-state-assistant-text state) "")
           (setf (emagent-acp-state-thought-text state) "")
           (emagent-acp--clear-thought-buffer state)
           (emagent-acp--cancel-prompt-render state)
           (emagent-acp--schedule-prompt-retry
            state session-id blocks images gen attempt
            (format "agent returned a transient error (%s)" message))))
        ((and (emagent-acp-state-busy state)
              (< (or (emagent-acp-state-continue-attempts state) 0)
                 emagent-acp-prompt-retry-attempts)
              (emagent-acp--turn-hit-transient-error-p state))
         (emagent-acp--log-transient-error state)
         (setf (emagent-acp-state-assistant-text state) "")
         (setf (emagent-acp-state-thought-text state) "")
         (emagent-acp--clear-thought-buffer state)
         (emagent-acp--cancel-prompt-render state)
         (emagent-acp--schedule-continue
          state session-id images gen "agent turn ended on a transient error"))
        (t
         (emagent-acp--complete-prompt state response)))))
   :on-failure
   (lambda (error _raw)
     (when (eq (emagent-acp-state-prompt-generation state) gen)
       (let ((message (or (map-elt error 'message) (format "%s" error))))
         (cond
          ((and (emagent-acp-state-busy state)
                (< attempt emagent-acp-prompt-retry-attempts)
                (emagent-acp--retriable-prompt-error-p message)
                (emagent-acp--turn-did-no-work-p state))
           (emagent-acp--schedule-prompt-retry
            state session-id blocks images gen attempt
            (format "prompt failed (%s)" message)))
          ((and (emagent-acp-state-busy state)
                (emagent-acp--retriable-prompt-error-p message)
                (< (or (emagent-acp-state-continue-attempts state) 0)
                   emagent-acp-prompt-retry-attempts))
           (emagent-acp--log-transient-error state message)
           (setf (emagent-acp-state-assistant-text state) "")
           (setf (emagent-acp-state-thought-text state) "")
           (emagent-acp--clear-thought-buffer state)
           (emagent-acp--cancel-prompt-render state)
           (emagent-acp--schedule-continue
            state session-id images gen (format "prompt interrupted (%s)" message)))
          (t
           (emagent-acp--abort-prompt state (format "prompt failed: %s" message))
           (emagent-acp--notify-user
            state (format "emagent: prompt failed: %s" message)))))))))

(defun emagent-acp--reset-permission-gate (state)
  "Cancel STATE's pending permission drain and clear the permission gate.
Replies `cancelled' to any outstanding requests so the agent does not hang.
Shared by the two turn-boundary owners (`--turn-begin' and finalize)."
  (when-let ((timer (emagent-acp-state-permission-drain-timer state)))
    (cancel-timer timer)
    (setf (emagent-acp-state-permission-drain-timer state) nil))
  (emagent-acp--cancel-outstanding-permissions state)
  (setf (emagent-acp-state-permission-busy state) nil)
  (setf (emagent-acp-state-deferred-complete-response state) nil))

(defun emagent-acp--turn-begin (state)
  "Enter the streaming phase of a new turn for STATE.

Mints a fresh turn generation (so a late response from a previous turn fails
the GEN guard instead of finalizing this one) and resets all turn-scoped state:
resume budget, streamed text, finalize flags, the tool-call display tables, the
provider tool-resolve queue, and any outstanding permission requests.  This is
the single entry point for turn start; the terminal paths (`--complete-prompt',
`--abort-prompt', `--finalize-in-flight-prompt') own turn end."
  (setf (emagent-acp-state-busy state) t)
  (setf (emagent-acp-state-prompt-generation state) (1+ (or (emagent-acp-state-prompt-generation state) 0)))
  (setf (emagent-acp-state-continue-attempts state) 0)
  (setf (emagent-acp-state-assistant-text state) "")
  (setf (emagent-acp-state-thought-text state) "")
  (setf (emagent-acp-state-prompt-finalized state) nil)
  (setf (emagent-acp-state-prompt-finishing state) nil)
  (clrhash (emagent-acp-state-tool-call-titles state))
  (clrhash (emagent-acp-state-tool-call-inputs state))
  (clrhash (emagent-acp-state-tool-call-labels state))
  (clrhash (emagent-acp-state-tool-call-decisions state))
  (clrhash (emagent-acp-state-tool-call-pending state))
  (emagent-acp--edit-diff-cache-clear)
  ;; A new turn supersedes any agent-scheduled wakeup: a stale request must
  ;; not arm after an unrelated prompt, and a pending timer must not fire
  ;; into the middle of this turn's conversation.
  (emagent-acp--cancel-wakeup state)
  (emagent-acp--cancel-plan-build state)
  (emagent-acp--provider-reset-tool-resolve state)
  (emagent-acp--reset-permission-gate state)
  (emagent-acp--cancel-prompt-render state)
  (emagent-acp--clear-thought-buffer state)
  (progn
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--schedule-prompt-watchdog state))
  (unless (emagent-acp-state-quiet-prompt state)
    (when (fboundp 'emagent-chat--send-pending-end)
      (when-let ((buf (emagent-acp--chat-buffer state)))
        (with-current-buffer buf
          (emagent-chat--send-pending-end))))
    (when (fboundp 'emagent-chat--promote-transient-to-thinking)
      (when-let ((buf (emagent-acp--chat-buffer state)))
        (with-current-buffer buf
          (emagent-chat--promote-transient-to-thinking)))))
  ;; Push the now-busy status; the mode line starts the spinner from it.
  (emagent-acp--refresh-mode-line state))

(cl-defun emagent-acp-send-prompt (user-text &optional compress)
  "Send USER-TEXT to the current buffer's ACP session.

When COMPRESS is non-nil, USER-TEXT is already a compression summary prompt
assembled by `emagent-chat--dispatch-compress': context injection is skipped
and the turn is marked so `emagent-acp--render-prompt-response' resets the
session with the summary once it finishes.  MCP and /compress detection live
in the chat send path (`emagent-chat-send'); by the time a prompt reaches
here it is always the final text to dispatch."
  (let* ((state (emagent-acp--session))
         (session-id (emagent-acp-state-session-id state)))
    (unless (emagent-acp-state-ready state)
      (user-error "Emagent is still connecting"))
    (when (emagent-acp-state-busy state)
      (user-error "Emagent is busy"))
    (setq user-text (emagent-acp--provider-normalize-slash-prompt state user-text))
    (when compress
      (setf (emagent-acp-state-compress-pending state) t))
    (let* ((slash-command-p (and (not compress) (emagent-chat--bare-slash-command-p user-text)))
           (extra (emagent-acp-state-extra-context state))
           (full-prompt (if (or slash-command-p compress)
                            user-text
                          (emagent-context-build-prompt user-text extra)))
           (extracted (emagent-acp--extract-image-links
                       (substring-no-properties full-prompt)))
           (clean-text (car extracted))
           (images (cdr extracted))
           (blocks `[((type . "text") (text . ,clean-text))]))
      (setf (emagent-acp-state-extra-context state) nil)
      (cond
       (compress (emagent-log "compressing conversation"))
       (slash-command-p (emagent-log "send slash command: %s" user-text)))
      (emagent-log "dispatch prompt (%d chars)" (length clean-text))
      (emagent-acp--turn-begin state)
      (emagent-acp--dispatch-prompt-request
       :state state :session-id session-id
       :blocks blocks :images images
       :gen (emagent-acp-state-prompt-generation state) :attempt 1))))

(cl-defun emagent-acp--finalize-in-flight-prompt (&optional stop-notice)
  "Finalize the in-flight prompt and cancel it on the agent side.

When STOP-NOTICE is non-nil, append it to any partial assistant text
before closing the response block.  Returns non-nil when a prompt was
finalized.  A compress turn is cancelled instead of compacted: the
stop notice must not become the session SUMMARY."
  (let ((state emagent-acp--session))
    (unless (and state
                 (or (emagent-acp-state-busy state)
                     (emagent-acp-state-prompt-finishing state)))
      (cl-return-from emagent-acp--finalize-in-flight-prompt nil))
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    ;; Interrupt/stop must not leave a ScheduleWakeup to arm later.
    (emagent-acp--cancel-wakeup state)
    (emagent-acp--cancel-plan-build state)
    (emagent-acp--flush-thought-buffer state)
    (let ((was-compress (emagent-acp-state-compress-pending state)))
      (when was-compress
        (setf (emagent-acp-state-compress-pending state) nil)
        (setf (emagent-acp-state-assistant-text state) "")
        (setf (emagent-acp-state-thought-text state) "")
        (emagent-log "compression cancelled by interrupt/stop"))
      (when (and stop-notice (not (string-empty-p stop-notice)))
        (if was-compress
            (setf (emagent-acp-state-assistant-text state) stop-notice)
          (let* ((text (or (emagent-acp-state-assistant-text state) ""))
                 (full (if (string-empty-p text)
                           stop-notice
                         (concat text "\n\n" stop-notice))))
            (setf (emagent-acp-state-assistant-text state) full)))))
    (setf (emagent-acp-state-prompt-generation state) (1+ (or (emagent-acp-state-prompt-generation state) 0)))
    (when-let ((client (emagent-acp-state-client state))
               (session-id (emagent-acp-state-session-id state)))
      (ignore-errors
        (emagent-acp-send-notification
         :client client
         :notification (emagent-acp-make-session-cancel-notification
                        :session-id session-id))))
    (emagent-acp--reset-permission-gate state)
    (setf (emagent-acp-state-busy state) nil)
    (setf (emagent-acp-state-prompt-finishing state) t)
    (setf (emagent-acp-state-prompt-finalized state) nil)
    (let* ((chat (emagent-acp-state-chat-buffer state))
           (token (or (and (boundp 'emagent-mcp--token) emagent-mcp--token)
                      (and (buffer-live-p chat)
                           (buffer-local-value 'emagent-mcp--token chat)))))
      (when (and token (fboundp 'emagent-mcp-cancel-session-tools))
        (emagent-mcp-cancel-session-tools token)))
    (emagent-acp--render-prompt-response state)
    (emagent-acp--refresh-mode-line state)
    t))

(defun emagent-acp-interrupt ()
  "Interrupt the in-flight prompt and close the response block cleanly.

Appends a user-visible stop notice to whatever the agent has produced so far,
then finalizes the response as if it completed normally.  The pending ACP
request continues in the background but its result is ignored."
  (interactive)
  (if (emagent-acp--finalize-in-flight-prompt
       "/Stopped — awaiting new instructions./")
      (message "emagent: interrupted")
    (user-error "No active emagent prompt to interrupt")))

(defun emagent-acp-shutdown-buffer ()
  "Shut down the ACP session for the current buffer."
  (emagent-chat-clear-slash-commands)
  (when emagent-mcp--token
    (emagent-mcp-deregister-session emagent-mcp--token))
  (when-let ((state emagent-acp--session))
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--cancel-prompt-render state)
    (emagent-acp--cancel-state-timers state)
    (when-let ((client (emagent-acp-state-client state)))
      (emagent-acp-shutdown :client client))
    (setq emagent-acp--session nil)))

(defvar emagent-chat--finish-close)

(defun emagent-acp--clear-prompt-watchdog (state)
  "Cancel any pending prompt stall watchdog for STATE."
  (when-let ((timer (emagent-acp-state-prompt-watchdog-timer state)))
    (cancel-timer timer))
  (setf (emagent-acp-state-prompt-watchdog state) nil)
  (setf (emagent-acp-state-prompt-watchdog-timer state) nil)
  (setf (emagent-acp-state-prompt-watchdog-extensions state) 0))

(defun emagent-acp--watchdog-should-extend-p (state waiting)
  "Return non-nil when STATE's stall should extend rather than finalize.

WAITING is non-nil when ACP work is still outstanding.  Compress turns
never extend once assistant text exists (the SUMMARY is enough to reset
the session).  Open permission dialogs always extend (user wait, not an
agent wedge).  Other waiting turns extend at most
`emagent-acp-watchdog-max-extensions' times."
  (and waiting
       (not (and (emagent-acp-state-compress-pending state)
                 (let ((text (emagent-acp-state-assistant-text state)))
                   (and text (not (string-empty-p text))))))
       (or (emagent-acp--permission-pending-p state)
           (< (or (emagent-acp-state-prompt-watchdog-extensions state) 0)
              (or emagent-acp-watchdog-max-extensions 0)))))

(defun emagent-acp--schedule-prompt-watchdog (state)
  "Abort a prompt that stays busy without ACP progress.

Cancel any existing watchdog first: this is re-invoked on every displayed tool
call and permission answer, and without the cancel each call would leak a live
timer (token-guarded no-ops that still pin STATE for the whole timeout).

When ACP work is still outstanding (pending RPC, permission prompt, or
tool-resolve), extend the watchdog instead of finalizing — up to
`emagent-acp-watchdog-max-extensions' times — so the UI does not close the
Response while the agent keeps working.  Compress turns with buffered
SUMMARY text finalize on the first stall even if session/prompt is still
pending (Claude ACP can wedge without ever returning)."
  (when-let ((old (emagent-acp-state-prompt-watchdog-timer state)))
    (cancel-timer old))
  (let* ((token (cl-gensym "emagent-prompt-watchdog"))
         (timer (run-with-timer
                 emagent-acp-watchdog-timeout nil
                 (lambda ()
                   (when (and (eq (emagent-acp-state-prompt-watchdog state) token)
                              (emagent-acp-state-busy state))
                     (let* ((client (emagent-acp-state-client state))
                            (pending (and client (map-elt client :pending-requests)))
                            (waiting
                             (or pending
                                 (emagent-acp--permission-pending-p state)
                                 (and (fboundp 'emagent-acp--provider-tool-resolve-active-p)
                                      (emagent-acp--provider-tool-resolve-active-p state)))))
                       (emagent-log "emagent: prompt stalled (no ACP completion in %ds)"
                                    emagent-acp-watchdog-timeout)
                       (when pending
                         (emagent-log "emagent: pending ACP request count: %d"
                                      (length pending)))
                       (cond
                        ((emagent-acp--watchdog-should-extend-p state waiting)
                         (setf (emagent-acp-state-prompt-watchdog-extensions state)
                               (1+ (or (emagent-acp-state-prompt-watchdog-extensions state) 0)))
                         (emagent-log
                          "emagent: prompt still waiting on agent work; extending watchdog (%d/%d)"
                          (emagent-acp-state-prompt-watchdog-extensions state)
                          emagent-acp-watchdog-max-extensions)
                         (emagent-acp--schedule-prompt-watchdog state))
                        ((and (emagent-acp-state-assistant-text state)
                              (not (string-empty-p
                                    (emagent-acp-state-assistant-text state))))
                         (when waiting
                           (emagent-log
                            "emagent: pending ACP work abandoned after stall; finalizing partial"))
                         (emagent-log "emagent: prompt stalled; finalizing partial response")
                         (emagent-acp--complete-prompt state nil))
                        (t
                         (emagent-acp--abort-prompt
                          state
                          "prompt stalled — reconnect with M-x emagent-mode or kill and reopen the buffer")))))))))
    (setf (emagent-acp-state-prompt-watchdog state) token)
    (setf (emagent-acp-state-prompt-watchdog-timer state) timer)))



(defun emagent-acp--stream-to-buffer-p (state)
  "Return non-nil when agent chunks may update the chat buffer live.

Arguments: STATE.

Chunks may stream while the prompt is busy or while a finish render is
still settling (`prompt-finishing'), so late agent text is not stranded
after an early stub.  Once `prompt-finalized' is set, streaming stops."
  (and emagent-acp-stream-to-buffer
       (not (emagent-acp-state-compress-pending state))
       (not (emagent-acp-state-quiet-prompt state))
       (not (emagent-acp-state-prompt-finalized state))
       (or (emagent-acp-state-busy state)
           (emagent-acp-state-prompt-finishing state))))

(defun emagent-acp--stream-thought-to-buffer-p (state)
  "Return non-nil when reasoning may stream into the chat buffer live.

Arguments: STATE."
  (and (memq emagent-acp-thought-progress '(buffer both))
       (not (emagent-acp-state-compress-pending state))
       (not (emagent-acp-state-quiet-prompt state))
       (not (emagent-acp-state-prompt-finalized state))
       (or (emagent-acp-state-busy state)
           (emagent-acp-state-prompt-finishing state))))

(defun emagent-acp--cancel-prompt-render (state)
  "Cancel a pending debounced render for STATE."
  (when-let ((timer (emagent-acp-state-finish-timer state)))
    (cancel-timer timer))
  (setf (emagent-acp-state-finish-timer state) nil)
  (setf (emagent-acp-state-finish-token state) nil))

(defun emagent-acp--schedule-prompt-render (state)
  "Debounced render of the accumulated prompt into the chat buffer.

Arguments: STATE."
  (let ((token (cl-gensym "emagent-finish")))
    (emagent-acp--cancel-prompt-render state)
    (setf (emagent-acp-state-finish-token state) token)
    (setf (emagent-acp-state-finish-timer state)
              (run-with-timer
               emagent-acp-render-delay nil
               (lambda ()
                 (when (and (eq (emagent-acp-state-finish-token state) token)
                            (emagent-acp-state-prompt-finishing state))
                   (setf (emagent-acp-state-finish-timer state) nil)
                   (emagent-acp--render-prompt-response state)))))))

(defun emagent-acp--render-prompt-response (state)
  "Render accumulated prompt text into the chat buffer for STATE.

For a normal finish, rewrite the open response without closing it, then
close only when assistant/thought text is still the snapshot that was
rendered.  Late chunks that arrive during the debounce or the finish
callback update state and reschedule; an early stub must not land before
the final text is stable."
  (when (emagent-acp-state-prompt-finishing state)
    (when-let ((buffer (emagent-acp--chat-buffer state)))
      (cond
       ((emagent-acp-state-quiet-prompt state)
        (setf (emagent-acp-state-quiet-prompt state) nil)
        (setf (emagent-acp-state-assistant-text state) "")
        (setf (emagent-acp-state-thought-text state) "")
        (emagent-acp--clear-thought-buffer state)
        (emagent-acp--cancel-prompt-render state)
        (setf (emagent-acp-state-prompt-finishing state) nil)
        (setf (emagent-acp-state-prompt-finalized state) t)
        (emagent-log "compacted session materialized")
        (emagent-acp--progress state "connected")
        (emagent-acp--refresh-mode-line state))
       ((emagent-acp-state-compress-pending state)
        (let* ((raw (string-trim (or (emagent-acp-state-assistant-text state) "")))
               (summary (if (fboundp 'emagent-session-notes-strip-facts)
                            (emagent-session-notes-strip-facts raw)
                          raw))
               (display (if (string-empty-p summary)
                            "(Facts saved to session notes.)"
                          summary)))
          (setf (emagent-acp-state-compress-pending state) nil)
          (if (string-empty-p raw)
              (progn
                (emagent-log "compression aborted: empty summary")
                (with-current-buffer buffer
                  (when-let ((cb (emagent-acp-state-cb-fail state)))
                    (funcall cb "Compression produced no summary; conversation left intact"))))
            (with-current-buffer buffer
              (when-let ((cb (emagent-acp-state-cb-finish state)))
                (funcall cb
                         (format "*Context compacted.* Agent session reset; the summary below is its only memory of the prior conversation.\n\n%s"
                                 display))))
            (emagent-log "compressed session (%d chars)" (length summary))
            (with-current-buffer buffer
              (when (fboundp 'emagent-session-notes-merge-from-summary)
                (emagent-session-notes-merge-from-summary raw))
              (when (fboundp 'emagent-chat--reset-compact-hint-cooldown)
                (emagent-chat--reset-compact-hint-cooldown)))
            (unless (fboundp 'emagent-acp--new-session)
              (require 'emagent-acp))
            (emagent-acp--new-session
             :state state
             :compressed-context summary
             :on-ready
             (lambda ()
               (emagent-acp--materialize-session state))))
          (setf (emagent-acp-state-prompt-finishing state) nil)
          (setf (emagent-acp-state-prompt-finalized state) t)
          (with-current-buffer buffer
            (emagent-chat--flush-deferred-font-lock))
          (emagent-acp--refresh-mode-line state)))
       (t
        (let ((token (emagent-acp-state-finish-token state))
              (assistant (emagent-acp-state-assistant-text state))
              (thought (emagent-acp-state-thought-text state))
              (failed nil))
          (condition-case err
              (with-current-buffer buffer
                (when-let ((cb (emagent-acp-state-cb-finish state)))
                  (let ((emagent-chat--finish-close nil))
                    (funcall cb assistant thought))))
            (error
             (setq failed t)
             (emagent-log "emagent: finish failed: %s" (error-message-string err))
             (with-current-buffer buffer
               (when-let ((cb (emagent-acp-state-cb-fail state)))
                 (funcall cb (format "response finalize failed: %s"
                                     (error-message-string err)))))))
          (cond
           (failed
            (setf (emagent-acp-state-prompt-finishing state) nil)
            (setf (emagent-acp-state-prompt-finalized state) t)
            (with-current-buffer buffer
              (emagent-chat--flush-deferred-font-lock))
            (emagent-acp--refresh-mode-line state))
           ((and (emagent-acp-state-prompt-finishing state)
                 (eq (emagent-acp-state-finish-token state) token)
                 (eq (emagent-acp-state-assistant-text state) assistant)
                 (eq (emagent-acp-state-thought-text state) thought))
            ;; Finalize before close so a reentrant chunk cannot stream or
            ;; schedule another render against a half-closed response.
            (setf (emagent-acp-state-prompt-finalized state) t)
            (setf (emagent-acp-state-prompt-finishing state) nil)
            (emagent-acp--cancel-prompt-render state)
            (with-current-buffer buffer
              (emagent-chat--close-finished-response))
            (emagent-acp--refresh-mode-line state)
            (emagent-acp--schedule-auto-compact state))
           ((and (emagent-acp-state-prompt-finishing state)
                 (eq (emagent-acp-state-finish-token state) token))
            ;; Text changed during finish but no newer timer was scheduled.
            (emagent-acp--schedule-prompt-render state)))))))))

(defun emagent-acp--maybe-complete-deferred-prompt (state)
  "Run a deferred `emagent-acp--complete-prompt' when permissions are clear.

Arguments: STATE."
  (when-let ((response (emagent-acp-state-deferred-complete-response state)))
    (unless (emagent-acp--permission-pending-p state)
      (setf (emagent-acp-state-deferred-complete-response state) nil)
      (emagent-acp--complete-prompt state response))))

(defun emagent-acp--complete-prompt (state response)
  "Finalize the in-flight prompt for STATE using RESPONSE and close chat."
  (cond
   ((emagent-acp-state-prompt-finalized state)
    (when (emagent-acp-state-busy state)
      (setf (emagent-acp-state-busy state) nil)
      (emagent-acp--refresh-mode-line state)))
   ((not (emagent-acp-state-busy state))
    nil)
   ((emagent-acp--permission-pending-p state)
    (setf (emagent-acp-state-deferred-complete-response state) response))
   (t
    (setf (emagent-acp-state-prompt-retry-gen state) nil)
    (setf (emagent-acp-state-prompt-finishing state) t)
    (setf (emagent-acp-state-busy state) nil)
    (setf (emagent-acp-state-current-tool state) nil)
    (setf (emagent-acp-state-current-tool-kind state) nil)
    (emagent-acp--clear-prompt-watchdog state)
    (emagent-acp--trace "prompt done (%d chars, %d thought)"
                        (length (or (emagent-acp-state-assistant-text state) ""))
                        (length (or (emagent-acp-state-thought-text state) "")))
    (emagent-acp--flush-thought-buffer state)
    (when (and response (map-elt response 'usage))
      (emagent-acp--save-usage-from-response state (map-elt response 'usage)))
    (emagent-acp--refresh-mode-line state)
    (emagent-acp--schedule-prompt-render state)
    (emagent-acp--arm-wakeup state)
    (emagent-acp--arm-plan-build state))))

(defun emagent-acp--context-fill-percent (state)
  "Return context window fill percent for STATE, or nil when unknown."
  (when-let* ((usage (emagent-acp-state-usage state))
              (used (map-elt usage :context-used))
              (size (map-elt usage :context-size))
              ((and (numberp used) (numberp size) (> size 0))))
    (* 100.0 (/ (float used) size))))

(defvar-local emagent-chat--explore-sticky nil
  "Non-nil while explore-model routing should stick across turns.")

(defun emagent-acp--explore-prompt-p (text)
  "Return non-nil when TEXT resembles an explore-only turn."
  (let ((lower (downcase (string-trim (or text "")))))
    (and (not (string-empty-p lower))
         (not (string-match-p
               (concat "\\b\\(edit\\|fix\\|implement\\|refactor\\|commit\\|"
                       "push\\|write\\|create\\|delete\\|rename\\|patch\\|"
                       "apply\\|migrate\\)\\b")
               lower))
         (or (bound-and-true-p emagent-chat--explore-sticky)
             (string-match-p
              (concat "\\b\\(what\\|where\\|which\\|list\\|find\\|show\\|"
                      "explain\\|search\\|outline\\|status\\|grep\\|"
                      "read\\|inspect\\|locate\\|summarize\\|check\\|scan\\)\\b")
              lower)))))

(defun emagent-acp--explore-clear-sticky ()
  "Stop sticky explore-model routing after a write/execute turn."
  (setq emagent-chat--explore-sticky nil))

(defun emagent-acp--explore-note-tool-kind (kind)
  "Clear explore sticky when KIND is a mutating tool kind."
  (when (and (stringp kind)
             (member kind '("write" "execute" "edit" "delete")))
    (emagent-acp--explore-clear-sticky)))

(defun emagent-acp--resolve-explore-model (state)
  "Return an explore model id for STATE, or nil."
  (when (and emagent-acp-auto-explore-model state)
    (or emagent-acp-explore-model
        (cl-loop for entry across
                 (vconcat (emagent-acp--get-available-models state nil))
                 for id = (or (map-elt entry 'modelId)
                              (map-elt entry 'id)
                              (and (stringp entry) entry))
                 when (and (stringp id)
                           (string-match-p emagent-acp-explore-model-regexp id))
                 return id))))

(defun emagent-acp--auto-compact-ready-p (state)
  "Return non-nil when STATE should start an automatic /compress.

Skips when a post-create_plan Build turn is pending (Build wins).  A
pending ScheduleWakeup is cancelled by `emagent-acp--maybe-auto-compact'."
  (when-let* ((threshold emagent-acp-auto-compact-threshold)
              ((and (integerp threshold) (> threshold 0)))
              ((emagent-acp-state-ready state))
              ((not (emagent-acp-state-busy state)))
              ((not (emagent-acp-state-prompt-finishing state)))
              ((not (emagent-acp-state-compress-pending state)))
              ((not (emagent-acp-state-quiet-prompt state)))
              ((not (emagent-acp--permission-pending-p state)))
              ((null (emagent-acp-state-plan-build-timer state)))
              ((null (emagent-acp-state-plan-build-prompt state)))
              (pct (emagent-acp--context-fill-percent state))
              ((>= pct threshold)))
    (let ((last (and (boundp 'emagent-chat--last-auto-compact)
                     emagent-chat--last-auto-compact))
          (cooldown (or emagent-acp-auto-compact-cooldown 0)))
      (or (null last)
          (<= cooldown 0)
          (>= (float-time (time-subtract (current-time) last))
              cooldown)))))

(defun emagent-acp--schedule-auto-compact (state)
  "Arm a short timer to maybe auto-/compress STATE after a settled turn."
  (when (and (integerp emagent-acp-auto-compact-threshold)
             (> emagent-acp-auto-compact-threshold 0)
             (not (emagent-acp-state-compress-pending state))
             (not (emagent-acp-state-quiet-prompt state)))
    (run-with-timer 0.5 nil #'emagent-acp--maybe-auto-compact state)))

(defun emagent-acp--maybe-auto-compact (state)
  "Run automatic /compress for STATE when still idle and over threshold."
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (eq emagent-acp--session state)
                   (emagent-acp--auto-compact-ready-p state)
                   (fboundp 'emagent-chat--conversation-history-text)
                   (fboundp 'emagent-chat--run-auto-compress)
                   (not (string-empty-p
                         (emagent-chat--conversation-history-text))))
          (emagent-acp--cancel-wakeup state)
          (emagent-acp--cancel-plan-build state)
          (emagent-chat--run-auto-compress))))))

(defun emagent-acp--arm-wakeup (state)
  "Start the ScheduleWakeup timer for STATE after this turn completes.
Called when the turn completes: the agent has ended its reply and now
waits to be re-invoked.  The wakeup prompt is sent as a regular user
turn so the transcript records what re-started the agent.
Compress turns drop any captured request instead of arming."
  (when (emagent-acp-state-compress-pending state)
    (emagent-acp--cancel-wakeup state))
  (when-let ((request (and emagent-acp-honor-schedule-wakeup
                           (not (emagent-acp-state-compress-pending state))
                           (emagent-acp-state-wakeup-request state))))
    (emagent-acp--cancel-wakeup state)
    (let ((delay (plist-get request :delay))
          (text (or (plist-get request :prompt)
                    (if-let ((reason (plist-get request :reason)))
                        (format "Wake up: %s" reason)
                      "Wake up: continue the scheduled task."))))
      (emagent-acp--notify-user
       state (format "emagent: wakeup armed in %ds%s" delay
                     (if-let ((reason (plist-get request :reason)))
                         (format " — %s" reason)
                       "")))
      (setf (emagent-acp-state-wakeup-timer state)
            (run-with-timer delay nil #'emagent-acp--fire-wakeup state text)))))

(defun emagent-acp--fire-wakeup (state text)
  "Send TEXT as a new user turn for STATE's chat buffer.
Skips silently when the buffer is gone or a prompt is already running
\(a manual turn superseded the loop)."
  (setf (emagent-acp-state-wakeup-timer state) nil)
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (cond
       ((emagent-acp-state-busy state)
        (emagent-log "wakeup: skipped — a prompt is already running"))
       ((not (fboundp 'emagent-chat--insert-user-heading-with-text))
        (emagent-log "wakeup: skipped — chat send unavailable"))
       (t
        (emagent-log "wakeup: %s" (emagent-log-truncate-line text 80))
        (let ((response-pos (emagent-chat--insert-user-heading-with-text text)))
          (emagent-chat--begin-response response-pos))
        (emagent-chat--ensure-follow-window buffer)
        ;; emagent-acp-send drops the turn unless a send token is armed
        ;; (manual C-c C-c calls send-pending-begin; Build/wakeup must too).
        (emagent-chat--send-pending-begin)
        (unless (fboundp 'emagent-acp-send)
          (require 'emagent-acp))
        (emagent-acp-send text))))))

(defun emagent-acp--set-session-mode (state mode-id)
  "Best-effort `session/set_mode' to MODE-ID for STATE."
  (when-let ((session-id (emagent-acp-state-session-id state)))
    (unless (fboundp 'emagent-acp--send-request)
      (require 'emagent-acp-protocol))
    (emagent-acp--send-request
     :state state
     :request (emagent-acp-make-session-set-mode-request
               :session-id session-id
               :mode-id mode-id)
     :on-success
     (lambda (_response)
       (setf (emagent-acp-state-session-mode-id state) mode-id)
       (emagent-acp--refresh-mode-line state))
     :on-failure
     (lambda (err _raw)
       (emagent-log "session/set_mode %s failed: %s"
                    mode-id
                    (or (map-elt err 'message) err))))))

(defun emagent-acp--ensure-agent-mode (state)
  "Best-effort `session/set_mode' to agent for STATE before Build."
  (emagent-acp--set-session-mode state "agent"))

(defun emagent-acp--fire-plan-build (state text)
  "Send TEXT as the Build follow-up for STATE without a user heading.

Build instructions are agent-internal: open Thinking/Response for the
work, but do not invent a synthetic `* user>' line in the transcript."
  (setf (emagent-acp-state-plan-build-timer state) nil)
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (cond
       ((emagent-acp-state-busy state)
        (emagent-log "plan-build: skipped — a prompt is already running"))
       (t
        (emagent-log "plan-build: %s" (emagent-log-truncate-line text 80))
        ;; Build owns the next turn; allow a normal stub after it finishes.
        (setq emagent-chat--defer-user-stub nil)
        (emagent-chat--begin-response (emagent-chat--user-zone-start))
        (emagent-chat--ensure-follow-window buffer)
        (emagent-chat--send-pending-begin)
        (unless (fboundp 'emagent-acp-send)
          (require 'emagent-acp))
        (emagent-acp-send text))))))

(defun emagent-acp--arm-plan-build (state)
  "Arm a Build turn when create_plan queued one on STATE."
  (when-let ((text (emagent-acp-state-plan-build-prompt state)))
    (setf (emagent-acp-state-plan-build-prompt state) nil)
    (when-let ((timer (emagent-acp-state-plan-build-timer state)))
      (when (timerp timer) (cancel-timer timer))
      (setf (emagent-acp-state-plan-build-timer state) nil))
    (emagent-log "cursor/create_plan: arming Build turn")
    (emagent-acp--ensure-agent-mode state)
    (setf (emagent-acp-state-plan-build-timer state)
          (run-with-timer 0.35 nil
                          #'emagent-acp--fire-plan-build state text))))

(defun emagent-acp--log-thought-line (mode text)
  "Log one thought TEXT line according to MODE."
  (let ((line (string-trim text)))
    (unless (string-empty-p line)
      (pcase mode
        ('minimal
         (emagent-log "… %s" (emagent-log-truncate-line line 80)))
        ('trail
         (emagent-log "… %s" (emagent-log-truncate-line line 72 t)))
        (_ nil)))))

(defun emagent-acp--clear-thought-buffer (state)
  
  "Internal helper for STATE."
  (setf (emagent-acp-state-thought-buffer state) ""))

(defun emagent-acp--flush-thought-buffer (state)
  "Log any trailing thought text for STATE and clear the buffer."
  (when-let ((mode emagent-acp-thought-progress))
    (when-let ((tail (string-trim (or (emagent-acp-state-thought-buffer state) ""))))
      (unless (string-empty-p tail)
        (emagent-acp--log-thought-line mode tail)))
    (emagent-acp--clear-thought-buffer state)))

(defun emagent-acp--thought-chunk (state text)
  "Accumulate thought TEXT for display and optional logging.

Arguments: STATE.

Drops late thoughts after compress finalize (busy cleared), matching
agent_message_chunk handling so SUMMARY rendering is not disturbed."
  (unless (or (string-empty-p text)
              (and (emagent-acp-state-compress-pending state)
                   (not (emagent-acp-state-busy state))))
    (emagent-acp--detect-external-refusal-in-text state text)
    (setf (emagent-acp-state-thought-text state)
          (concat (or (emagent-acp-state-thought-text state) "") text))
    (when-let ((mode emagent-acp-thought-progress))
      (when (emagent-acp-state-prompt-finishing state)
        (emagent-acp--schedule-prompt-render state))
      (when (memq mode '(buffer both))
        (when-let ((buf (and (emagent-acp--stream-thought-to-buffer-p state)
                             (emagent-acp--chat-buffer state))))
          (with-current-buffer buf
            (when-let ((cb (emagent-acp-state-cb-thought state)))
              (funcall cb text)))))
      (when (memq mode '(minimal trail both))
        (let ((pending (concat (or (emagent-acp-state-thought-buffer state) "") text)))
          (while (string-match "\\`\\(.+?[.!?]\\)\\(?:[[:space:]]\\|\\'\\)" pending)
            (let ((end (match-end 0)))
              (emagent-acp--log-thought-line
               (if (eq mode 'both) 'minimal mode)
               (substring pending 0 end))
              (setq pending (substring pending end))))
          (setf (emagent-acp-state-thought-buffer state) pending))))))

(defun emagent-acp--run-reveal (reveal &optional now)
  
  "Internal helper for REVEAL and NOW."
  (when reveal
    (if now
        (funcall reveal)
      (run-with-idle-timer 0 nil reveal))))

(defun emagent-acp--reveal-buffer (state &optional now)
  "Run the buffer reveal callback for STATE, if any.

When NOW is non-nil, show the buffer immediately for interactive prompts."
  (when-let ((reveal (emagent-acp-state-on-reveal state)))
    (setf (emagent-acp-state-on-reveal state) nil)
    (emagent-acp--run-reveal reveal now)))

(defun emagent-acp--prepare-interactive-context (state)
  "Focus the chat buffer's window before a user prompt, without rearranging.

Selects the chat window only when it is already visible in the selected
frame, so permission shortcuts (y/n/…) work when the user is looking at
the session.  Never pops the buffer into a window or touches other
frames: with several sessions across frames, stealing a window would
flip an unrelated frame to this session's project.  Background prompts
are surfaced by `emagent-chat--notify-inactive-update' instead.

Arguments: STATE."
  (emagent-acp--reveal-buffer state t)
  (when-let* ((buffer (emagent-acp--chat-buffer state))
              (window (get-buffer-window buffer)))
    (select-window window)))

(defun emagent-acp--fail-connect (state message)
  "Show MESSAGE, reveal the chat buffer, and stop connecting.

Arguments: STATE."
  (setf (emagent-acp-state-ready state) nil)
  (emagent-acp--notify-user state message)
  (emagent-acp--reveal-buffer state))

(defun emagent-acp--quota-error-p (message)
  "Return non-nil when MESSAGE is a session/rate/usage quota error."
  (and (stringp message)
       (string-match-p
        (concat "session limit\\|rate limit\\|usage limit\\|spend limit"
                "\\|You've hit your\\|hit your limit\\|out of credits"
                "\\|quota exceeded\\|quota limit")
        message)))

(defun emagent-acp--fatal-agent-error-p (message)
  "Return non-nil when MESSAGE should abort the in-flight prompt.

RetriableError and other transient network failures are excluded: those are
retried by `emagent-acp--schedule-prompt-retry' and must not be double-handled
via stderr subscription (which would clear `:busy' before the retry fires).

Session/rate quota errors are fatal so they surface in the chat buffer even
when they arrive only on agent stderr."
  (and (stringp message)
       (not (string-match-p "RetriableError" message))
       (not (emagent-acp--retriable-prompt-error-p message))
       (or (emagent-acp--quota-error-p message)
           (string-match-p
            "timed out\\|timeout\\|failed with status\\|ApiError\\|API Error\\|\\[31merror"
            message))))

(defun emagent-acp--prompt-retry-pending-p (state)
  "Return non-nil when STATE is waiting to replay a failed prompt."
  (and state
       (emagent-acp-state-prompt-retry-gen state)
       (eq (emagent-acp-state-prompt-retry-gen state)
           (emagent-acp-state-prompt-generation state))
       (emagent-acp-state-busy state)))

(defun emagent-acp--retriable-prompt-error-p (message)
  "Return non-nil when a failed prompt MESSAGE is a transient network error.

Covers Cursor's own RetriableError wrapper and the common DNS/connection
failures underneath it (getaddrinfo ENOTFOUND api2.cursor.sh, connection
resets, timeouts).  These usually recover on a second attempt, so emagent
retries them before surfacing the error (`emagent-acp-prompt-retry-attempts')."
  (and (stringp message)
       (string-match-p
        (concat "RetriableError\\|getaddrinfo\\|ENOTFOUND\\|EAI_AGAIN"
                "\\|ECONNRESET\\|ECONNREFUSED\\|ConnectionRefused"
                "\\|ETIMEDOUT\\|EPIPE"
                "\\|\\[unavailable\\]\\|socket hang up\\|network error"
                "\\|Unable to connect to API")
        message)))

(defun emagent-acp--prompt-retry-delay (attempt)
  "Return backoff seconds to wait before the next retry after ATTEMPT (1-based)."
  (* emagent-acp-prompt-retry-base-delay (expt 2 (max 0 (1- attempt)))))

(defun emagent-acp--abort-prompt (state message)
  "Abort the in-flight prompt for STATE and show MESSAGE.

Quota/session-limit errors are always shown in the chat buffer, even when the
watchdog already finalized a partial Response (busy cleared) while the agent
was still working."
  (setf (emagent-acp-state-prompt-retry-gen state) nil)
  (let ((quiet (emagent-acp-state-quiet-prompt state))
        (in-flight (or (emagent-acp-state-busy state)
                       (emagent-acp-state-prompt-finishing state)))
        (force (and (not (emagent-acp-state-quiet-prompt state))
                    (emagent-acp--quota-error-p message))))
    (when (or in-flight force)
      ;; Do not arm a ScheduleWakeup captured during a failed/aborted turn.
      (emagent-acp--cancel-wakeup state)
      (emagent-acp--cancel-plan-build state)
      (when in-flight
        (emagent-acp--clear-prompt-watchdog state)
        (emagent-acp--cancel-prompt-render state)
        (setf (emagent-acp-state-busy state) nil)
        (setf (emagent-acp-state-prompt-finishing state) nil)
        (setf (emagent-acp-state-prompt-finalized state) nil)
        (setf (emagent-acp-state-assistant-text state) "")
        (setf (emagent-acp-state-compress-pending state) nil)
        (setf (emagent-acp-state-quiet-prompt state) nil)
        (emagent-acp--flush-thought-buffer state))
      (emagent-acp--trace "prompt aborted: %s" message)
      (cond
       (quiet
        (emagent-log "compacted session materialize failed: %s" message))
       (t
        (when-let ((buffer (emagent-acp--chat-buffer state)))
          (with-current-buffer buffer
            (when-let ((cb (emagent-acp-state-cb-fail state)))
              (funcall cb message))))))
      (emagent-acp--refresh-mode-line state))))

(defun emagent-acp--system-prompt ()
  "Return the system prompt for new ACP sessions."
  (concat emagent-acp-system-prompt
          (emagent-mcp-gateway-system-prompt)
          (when emagent-acp-prefer-emacs
            (emagent-prompts--prefer-emacs-prompt))
          (when emagent-acp-prefer-emacs
            (emagent-prompts--structural-policy))))

(defun emagent-acp--session-system-prompt (&optional compressed-context)
  "Return the system prompt for session/new, optionally with COMPRESSED-CONTEXT."
  (require 'emagent-usage nil t)
  (let* ((summary (string-trim (or compressed-context "")))
         (budget (and (boundp 'emagent-usage-budget-compressed)
                      emagent-usage-budget-compressed))
         (summary (if (and (fboundp 'emagent-usage--cap-string)
                           (not (string-empty-p summary)))
                      (emagent-usage--cap-string summary budget 'compressed)
                    summary))
         (notes
          (when-let ((buf (and emagent-acp--session
                               (emagent-acp--chat-buffer emagent-acp--session))))
            (with-current-buffer buf
              (when (fboundp 'emagent-session-notes-prompt-block)
                (emagent-session-notes-prompt-block)))))
         (base (concat (emagent-acp--system-prompt) (or notes ""))))
    (when (fboundp 'emagent-usage-tax-add)
      (emagent-usage-tax-add 'system (length base)))
    (if (string-empty-p summary)
        base
      (concat base
              (format "\n\n[Compressed prior conversation context]\n%s"
                      summary)))))

(defun emagent-acp--trace-update (update-type emagent-acp-notification)
  "Log UPDATE-TYPE and a short payload summary when tracing.

Arguments: EMAGENT-ACP-NOTIFICATION."
  (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) ""))
        (title (map-nested-elt emagent-acp-notification '(params update title))))
    (pcase update-type
      ((or "agent_message_chunk" "agent_thought_chunk")
       (emagent-acp--trace "recv %s +%d" update-type (length text)))
      ((or "tool_call" "tool_call_update")
       (let* ((update (map-nested-elt emagent-acp-notification '(params update)))
              (raw (or (map-elt update 'rawInput) (map-elt update 'arguments)))
              (subtitle (map-elt update 'subtitle))
              (locations (map-elt update 'locations))
              (id (map-elt update 'toolCallId))
              (raw-summary
               (cond
                ((or (null raw) (equal raw :null) (equal raw "")) nil)
                ((hash-table-p raw)
                 (format "keys(%s)"
                         (string-join (hash-table-keys raw) ",")))
                ((listp raw)
                 (format "keys(%s)"
                         (string-join (mapcar (lambda (p) (format "%s" (car p))) raw) ",")))
                ((stringp raw)
                 (format "str(%d)" (length raw)))
                (t "?")))
              (detail (or raw-summary
                          (when subtitle (format "sub=%s" (truncate-string-to-width subtitle 40 nil nil "…")))
                          (when locations (format "locs=%d" (length (append locations nil))))
                          "no-detail")))
         (emagent-acp--trace "recv %s %s [%s] %s"
                             update-type
                             (or title id "?")
                             (or (map-elt update 'status) "")
                             detail)))
      (_
       (emagent-acp--trace "recv %s" (or update-type "session/update"))))))

(cl-defun emagent-acp--on-notification (&key state emagent-acp-notification)
  
  "Internal helper for STATE and EMAGENT-ACP-NOTIFICATION."
  (when (equal (map-elt emagent-acp-notification 'method) "session/update")
    (let ((update-type (map-nested-elt emagent-acp-notification '(params update sessionUpdate))))
      (emagent-acp--trace-update update-type emagent-acp-notification)
      (pcase update-type
        ("agent_message_chunk"
         (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) "")))
           ;; Drop late chunks after compress finalize (busy cleared): they
           ;; must not rewrite the SUMMARY snapshot before render runs.
           (unless (or (emagent-acp-state-replaying-history state)
                       (and (emagent-acp-state-compress-pending state)
                            (not (emagent-acp-state-busy state))))
             (when (and (not (string-empty-p text))
                        (emagent-acp-state-tool-call-since-last-chunk state)
                        (not (string-empty-p (or (emagent-acp-state-assistant-text state) ""))))
               (setq text (concat "\n\n" text)))
             (setf (emagent-acp-state-tool-call-since-last-chunk state) nil)
             (emagent-acp--detect-external-refusal-in-text state text)
             (setf (emagent-acp-state-assistant-text state) (concat (emagent-acp-state-assistant-text state) text))
             (when (emagent-acp-state-prompt-finishing state)
               (emagent-acp--schedule-prompt-render state))
             (when-let ((buf (and (emagent-acp--stream-to-buffer-p state)
                                 (emagent-acp--chat-buffer state))))
               (with-current-buffer buf
                 (when-let ((cb (emagent-acp-state-cb-chunk state)))
                   (funcall cb text)))))))
        ("agent_thought_chunk"
         (let ((text (or (map-nested-elt emagent-acp-notification '(params update content text)) "")))
           (emagent-acp--thought-chunk state text)))
        ("tool_call"
         (setf (emagent-acp-state-tool-call-since-last-chunk state) t)
         (emagent-acp--on-tool-call state (map-nested-elt emagent-acp-notification '(params update))))
        ("tool_call_update"
         (emagent-acp--on-tool-call state (map-nested-elt emagent-acp-notification '(params update))))
        ("config_option_update"
         (emagent-acp--save-config-options
          state
          (map-nested-elt emagent-acp-notification '(params update configOptions)))
         (when-let ((model-id (emagent-acp--current-model-id state nil)))
           (emagent-acp--persist-model-id state model-id)))
        ("usage_update"
         (emagent-acp--update-usage-from-notification
          state
          (map-nested-elt emagent-acp-notification '(params update))))
        ("available_commands_update"
         (let ((commands (map-nested-elt emagent-acp-notification
                                         '(params update availableCommands))))
           (when-let* ((buffer (emagent-acp--chat-buffer state))
                       (cb (emagent-acp-state-cb-slash-commands state)))
             (with-current-buffer buffer
               (funcall cb commands)))))
        ("current_mode_update"
         (let* ((update (map-nested-elt emagent-acp-notification
                                        '(params update)))
                (mode-id (or (map-elt update 'currentModeId)
                             (map-elt update 'modeId)
                             (map-elt update :currentModeId)
                             (map-elt update :modeId))))
           (when (and (stringp mode-id) (not (string-empty-p mode-id)))
             (setf (emagent-acp-state-session-mode-id state) mode-id)
             (emagent-acp--refresh-mode-line state))))
        (_ nil)))))

(cl-defun emagent-acp--subscribe (&key state)
  "Subscribe STATE's client to ACP errors, notifications, and requests."
  (let ((buffer (emagent-acp--chat-buffer state)))
    (emagent-acp-subscribe-to-errors
     :client (emagent-acp-state-client state)
     :buffer buffer
     :on-error
     (lambda (emagent-acp-error)
       (let ((message (or (map-elt emagent-acp-error 'message)
                          (format "%s" emagent-acp-error))))
         (emagent-acp--log-agent-stderr message)
         (when (and (emagent-acp--fatal-agent-error-p message)
                    (not (emagent-acp--prompt-retry-pending-p state))
                    (or (emagent-acp-state-busy state)
                        (emagent-acp-state-prompt-finishing state)
                        (emagent-acp--quota-error-p message)))
           (emagent-acp--abort-prompt state message))
         (when (emagent-acp--stderr-notify-p emagent-acp-error)
           (emagent-acp--notify-user state (format "emagent error: %s" message))))))
    (emagent-acp-subscribe-to-notifications
     :client (emagent-acp-state-client state)
     :buffer buffer
     :on-notification
     (lambda (notification)
       (emagent-acp--on-notification :state state
                                     :emagent-acp-notification notification)))
    (emagent-acp-subscribe-to-requests
     :client (emagent-acp-state-client state)
     :buffer buffer
     :on-request
     (lambda (request)
       (emagent-acp--on-request :state state :emagent-acp-request request)))))

(cl-defun emagent-acp--authenticate (&key state method-id on-ready)
  "Send an authenticate request with METHOD-ID, then connect the session.

Called when `initialize' returns authMethods (e.g. cursor_login).
The authenticate call completes the credential handshake so the agent
grants full plan access (including Auto model) to this ACP session.

Arguments: STATE, ON-READY."
  (emagent-acp--progress state (format "authenticating (%s)…" method-id))
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-authenticate-request :method-id method-id)
   :on-success (lambda (_response)
                 (emagent-acp--connect-session :state state :on-ready on-ready))
   :on-failure (lambda (error _raw)
                 (emagent-log "authenticate %s failed: %s — proceeding anyway"
                              method-id
                              (or (map-elt error 'message) (format "%s" error)))
                 (emagent-acp--connect-session :state state :on-ready on-ready))))

(cl-defun emagent-acp--initialize (&key state on-ready)
  
  "Internal helper for STATE and ON-READY."
  (emagent-acp--progress state "initializing ACP…")
  (emagent-acp--send-request
   :state state
   :request (if emagent-acp-file-access
                (emagent-acp-make-initialize-request
                 :protocol-version 1
                 :client-info `((name . "emagent")
                                (title . "Emacs Emagent")
                                (version . "1.0.2"))
                 :read-text-file-capability t
                 :write-text-file-capability t)
              (emagent-acp-make-initialize-request
               :protocol-version 1
               :client-info `((name . "emagent")
                              (title . "Emacs Emagent")
                              (version . "1.0.2"))))
   :on-success (lambda (response)
                 (setf (emagent-acp-state-initialized state) t)
                 (setf (emagent-acp-state-mcp-http state) (emagent-acp--mcp-http-capable-p response))
                 (emagent-acp--infer-external-tool-gate-from-agent state)
                 (emagent-acp--infer-external-tool-gate-from-initialize-response state response)
                 (emagent-acp--maybe-log-external-tool-gate-proactive state)
                 (let ((auth-methods (append (map-elt response 'authMethods) nil)))
                   (if-let ((method-id (map-elt (seq-find
                                                 (lambda (m) (map-elt m 'id))
                                                 auth-methods)
                                                'id)))
                       (emagent-acp--authenticate
                        :state state :method-id method-id :on-ready on-ready)
                     (emagent-acp--connect-session :state state :on-ready on-ready))))
   :on-failure (lambda (error _raw)
                 (emagent-acp--fail-connect
                  state
                  (format "emagent: initialize failed: %s"
                          (or (map-elt error 'message) (format "%s" error)))))))

(defun emagent-acp--mcp-http-capable-p (initialize-response)
  "Return non-nil when INITIALIZE-RESPONSE advertises http MCP support."
  (let ((value (map-nested-elt initialize-response
                               '(agentCapabilities mcpCapabilities http))))
    (and value (not (eq value :false)) (not (eq value :json-false)))))

(cl-defun emagent-acp--session-ready (&key state session-id on-ready resumed)
  
  "Internal helper for STATE and SESSION-ID and ON-READY and RESUMED."
  (setf (emagent-acp-state-session-id state) session-id)
  (setf (emagent-acp-state-ready state) t)
  (emagent-acp--persist-session-id state session-id)
  (emagent-acp--hydrate-session-permissions state session-id)
  (emagent-acp--progress state (if resumed "resumed" "connected"))
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (emagent-tools-set-project-directory (emagent-acp--session-cwd state))
      (pcase emagent-chat-provider
        ('cursor (emagent-chat-seed-cursor-slash-commands))
        ('claude
         (when (null emagent-chat-slash-commands)
           (emagent-log "loading slash commands from agent…"))))))
  (emagent-acp--start-rss-timer state)
  (emagent-acp--reveal-buffer state)
  (when on-ready (funcall on-ready)))

(cl-defun emagent-acp--new-session (&key state on-ready compressed-context)
  
  "Internal helper for STATE and ON-READY and COMPRESSED-CONTEXT."
  (when (fboundp 'emagent-tools-age-reset)
    (let* ((chat (emagent-acp--chat-buffer state))
           (emagent-tools-age--session-key
            (or (and (buffer-live-p chat)
                     (buffer-local-value 'emagent-mcp--token chat))
                (and (buffer-live-p chat)
                     (format "buf:%s" (buffer-name chat)))
                'global)))
      (emagent-tools-age-reset)))
  (when (fboundp 'emagent-usage-tax-reset)
    (let* ((chat (emagent-acp--chat-buffer state))
           (emagent-usage--session-key
            (or (and (buffer-live-p chat)
                     (buffer-local-value 'emagent-mcp--token chat))
                (and (buffer-live-p chat)
                     (format "buf:%s" (buffer-name chat)))
                'global)))
      (emagent-usage-tax-reset)))
  (when-let ((chat (emagent-acp--chat-buffer state)))
    (with-current-buffer chat
      (setq emagent-chat--explore-sticky nil)))
  (emagent-acp--progress state "creating session…")
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-new-request
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (emagent-acp-state-mcp-http state)
                                                       (emagent-acp--chat-buffer state))
             :meta `((systemPrompt . ((append . ,(emagent-acp--session-system-prompt
                                                  compressed-context))))))
   :on-success (lambda (response)
                 (unless (fboundp 'emagent-acp--configure-model)
                   (require 'emagent-acp-protocol))
                 (emagent-acp--configure-model
                  :state state
                  :session-id (map-elt response 'sessionId)
                  :response response
                  :on-ready on-ready))
   :on-failure (lambda (error _raw)
                 (emagent-acp--fail-connect
                  state
                  (format "emagent: session/new failed: %s"
                          (or (map-elt error 'message) (format "%s" error)))))))

(cl-defun emagent-acp--load-session (&key state session-id on-ready)
  "Resume SESSION-ID for STATE, falling back to session/new on failure."
  (emagent-acp--progress state "resuming session…")
  (setf (emagent-acp-state-replaying-history state) t)
  (emagent-acp--set-suppress-history-updates
   (emagent-acp-state-client state) t)
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-load-request
             :session-id session-id
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (emagent-acp-state-mcp-http state)
                                                       (emagent-acp--chat-buffer state))
             :meta `((systemPrompt . ((append . ,(emagent-acp--system-prompt))))))
   :on-success (lambda (response)
                 (emagent-acp--set-suppress-history-updates
                  (emagent-acp-state-client state) nil)
                 (setf (emagent-acp-state-replaying-history state) nil)
                 (unless (fboundp 'emagent-acp--configure-model)
                   (require 'emagent-acp-protocol))
                 (emagent-acp--configure-model
                  :state state
                  :session-id session-id
                  :response response
                  :on-ready on-ready
                  :resumed t))
   :on-failure (lambda (error _raw)
                 (emagent-acp--set-suppress-history-updates
                  (emagent-acp-state-client state) nil)
                 (setf (emagent-acp-state-replaying-history state) nil)
                 (emagent-log "session/load failed for %s: %s"
                              session-id
                              (or (map-elt error 'message) (format "%s" error)))
                 (emagent-acp--progress state "resume failed, creating session…")
                 (when-let ((buf (emagent-acp--chat-buffer state)))
                   (with-current-buffer buf
                     (let ((was-modified (buffer-modified-p)))
                       (unwind-protect
                           (emagent-session-clear-id)
                         (set-buffer-modified-p was-modified)))))
                 (emagent-acp--new-session :state state :on-ready on-ready))))

(cl-defun emagent-acp--connect-session (&key state on-ready)
  
  "Internal helper for STATE and ON-READY."
  (emagent-acp--progress state "connecting session…")
  (let ((saved (emagent-acp--saved-session-id state)))
    (if (and saved (not (string-empty-p saved)))
        (emagent-acp--load-session :state state :session-id saved :on-ready on-ready)
      (emagent-acp--new-session :state state :on-ready on-ready))))

(cl-defun emagent-acp-start (&key client chat-buffer on-ready on-reveal callbacks)
  "Start an emagent ACP session in CHAT-BUFFER.

ON-REVEAL is called once when the chat buffer should be shown.
CALLBACKS is an alist of rendering callbacks keyed by:
  :cb-chunk, :cb-thought, :cb-finish, :cb-fail, :cb-slash-commands.

Arguments: CLIENT, ON-READY."
  (when (and emagent-acp-prefer-emacs (not emagent-acp-file-access))
    (emagent-log "prefer-Emacs mode works best with `emagent-acp-file-access'"))
  (when emagent-acp-trace
    (setq emagent-acp-logging-enabled t))
  (with-current-buffer chat-buffer
    (emagent-chat-clear-slash-commands)
    ;; Cursor built-ins are local; keep them available while the agent
    ;; connects so TAB completion does not go empty mid-reconnect.
    (emagent-chat-seed-cursor-slash-commands)
    (setq emagent-acp--session (emagent-acp--make-state :client client
                                                        :chat-buffer chat-buffer
                                                        :on-reveal on-reveal))
    (setf (emagent-acp-state-provider emagent-acp--session) (or emagent-chat-provider 'cursor))
    (dolist (cb callbacks)
      (emagent-acp--set-callback emagent-acp--session (car cb) (cdr cb)))
    (emagent-mcp-register-session :token (emagent-mcp-buffer-token)
                                  :cwd (emagent-chat--session-directory)
                                  :buffer chat-buffer
                                  :prefer-emacs emagent-acp-prefer-emacs
                                  :acp t)
    (emagent-acp--progress emagent-acp--session "starting agent…")
    (emagent-acp--subscribe :state emagent-acp--session)
    (emagent-acp--initialize :state emagent-acp--session :on-ready on-ready)
    emagent-acp--session))

(eval-when-compile
  (require 'cl-lib))

(defun emagent-acp--make-client (provider buffer)
  "Create an ACP client for PROVIDER using BUFFER as context.

Looks up `:make-client' on the provider registered via
`emagent-acp--register-provider'."
  (let* ((process-directory (and (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (emagent-chat--session-directory))))
         (spec (gethash provider emagent-acp--provider-specs))
         (maker (plist-get spec :make-client)))
    (unless maker
      (user-error "Unknown emagent provider: %s" provider))
    (funcall maker :context-buffer buffer
                   :process-directory process-directory)))

(defvar emagent-default-provider)

(cl-defun emagent-acp-ensure-connected (&key on-ready on-reveal)
  "Connect the current emagent buffer to its ACP provider if needed.

When the agent process died but buffer-local state remains, tear it down and
reconnect (resuming the saved session id when present).  Optional ON-READY runs
once the session is ready; ON-REVEAL runs when the chat buffer should be shown.
While a connection is already in flight, ON-READY is queued instead of tearing
the session down and starting over."
  (when on-ready (push on-ready emagent-acp--when-connected-queue))
  (cond
   ((emagent-acp--connected-p)
    (emagent-acp--run-when-connected-queue))
   ((emagent-acp--connecting-p)
    nil)
   (t
    (emagent-acp--teardown-stale-session)
    (let* ((provider (or emagent-chat-provider emagent-default-provider))
           (client (emagent-acp--make-client provider (current-buffer))))
      (emagent-acp-start :client client
                        :chat-buffer (current-buffer)
                        :on-ready #'emagent-acp--run-when-connected-queue
                        :on-reveal on-reveal
                        :callbacks
                        `((:cb-chunk          . ,#'emagent-chat-append-assistant)
                          (:cb-thought        . ,#'emagent-chat-append-thought)
                          (:cb-finish         . ,(lambda (&rest args)
                                                    (apply #'emagent-chat-finish-assistant args)
                                                    (emagent-acp--restore-turn-model)))
                          (:cb-fail           . ,(lambda (&rest args)
                                                    (apply #'emagent-chat-fail-assistant args)
                                                    (emagent-acp--turn-model-on-failure
                                                     (car args))))
                          (:cb-slash-commands . ,#'emagent-chat-set-slash-commands)
                          (:cb-tool-call      . ,#'emagent-chat-show-tool-call)
                          (:cb-permission     . ,#'emagent-chat-permission-prompt)
                          (:cb-status         . ,#'emagent-chat-set-status)))))))

(defun emagent-acp--send-prompt-safe (buffer user-text &optional compress)
  "Send USER-TEXT from BUFFER, logging and surfacing failures in the chat.
COMPRESS is forwarded to `emagent-acp-send-prompt'."
  (with-current-buffer buffer
    (condition-case err
        (emagent-acp-send-prompt user-text compress)
      (error
       (let ((msg (error-message-string err)))
         (when (fboundp 'emagent-chat--send-pending-end)
           (emagent-chat--send-pending-end))
         (emagent-log "emagent: send failed: %s" msg)
         (when (fboundp 'emagent-chat-fail-assistant)
           (emagent-chat-fail-assistant msg)))))))

(defun emagent-acp-send (user-text &optional compress)
  "Ensure connection and send USER-TEXT from the current buffer.

When a per-turn model override (`emagent-chat--turn-model', set by `/model') is
active and differs from the session model, switch to it transiently first, then
send; the buffer model is restored when the turn ends (see
`emagent-chat-finish-assistant' / `emagent-chat-fail-assistant').

COMPRESS is forwarded to `emagent-acp-send-prompt': set by
`emagent-chat--dispatch-compress' when USER-TEXT is already a compression
summary prompt rather than ordinary chat input."
  (let ((buf (current-buffer))
        (turn-model emagent-chat--turn-model)
        (turn-spec emagent-chat--turn-apply-spec)
        (token emagent-chat--send-token))
    (emagent-acp-ensure-connected
     :on-ready
     (lambda ()
       (with-current-buffer buf
         (when (emagent-chat--send-active-p token)
           (let* ((state emagent-acp--session)
                  (current (and state (emagent-acp-current-model-id)))
                  (explore
                   (and (not turn-model)
                        (not compress)
                        state
                        (emagent-acp--explore-prompt-p user-text)
                        (emagent-acp--resolve-explore-model state)))
                  (turn-model (or turn-model explore))
                  (target (and turn-model state
                               (emagent-acp--match-model-id turn-model state nil)))
                  (spec
                   (and turn-spec state target
                        (equal (emagent-acp--spec-model-value turn-spec state)
                               target)
                        turn-spec))
                  (need-switch
                   (or (and target current (not (string= target current)))
                       (and spec (> (length spec) 1)))))
             (when (and explore target)
               (setq emagent-chat--turn-model target
                     emagent-chat--explore-sticky t
                     emagent-chat--turn-apply-spec nil))
             (if need-switch
                 (progn
                   (unless emagent-chat--turn-model-base
                     (setq emagent-chat--turn-model-base current))
                   (unless emagent-chat--turn-config-base
                     (setq emagent-chat--turn-config-base
                           (and state spec
                                (emagent-acp--snapshot-config-values
                                 state spec))))
                   (when (fboundp 'emagent-acp--progress)
                     (emagent-acp--progress
                      state
                      (format "switching model to %s for this turn…"
                              (if (fboundp 'emagent-acp--model-display-name)
                                  (emagent-acp--model-display-name state nil target)
                                target))))
                   (emagent-acp-set-model-transient
                    target
                    (lambda ()
                      (when (emagent-chat--send-active-p token)
                        (emagent-acp--send-prompt-safe buf user-text compress)))
                    spec))
               (emagent-acp--send-prompt-safe buf user-text compress)))))))))

(defun emagent-acp--wire-chat-buffer ()
  "Install buffer teardown for the current emagent chat buffer.

Adds `emagent-acp-shutdown-buffer' on `kill-buffer-hook'.  Send/attach/quit
commands call ACP entry points directly (lazy `require'), so no buffer-local
function slots are needed."
  (add-hook 'kill-buffer-hook #'emagent-acp-shutdown-buffer nil t))

(add-hook 'emagent-mode-hook #'emagent-acp--wire-chat-buffer)

(defun emagent-acp--restore-turn-model ()
  "Restore the session model overridden by `/model' and clear the override.
Called on a successful turn: switches back to the captured base model and clears
`emagent-chat--turn-model' so the next prompt uses the buffer model again."
  (when emagent-chat--turn-model
    (cond
     (emagent-chat--turn-config-base
      (emagent-acp-set-model-transient
       emagent-chat--turn-model-base
       #'ignore
       emagent-chat--turn-config-base))
     (emagent-chat--turn-model-base
      (emagent-acp-set-model-transient emagent-chat--turn-model-base #'ignore)))
    (setq emagent-chat--turn-model nil
          emagent-chat--turn-model-base nil
          emagent-chat--turn-apply-spec nil
          emagent-chat--turn-config-base nil)))

(defun emagent-acp--turn-model-on-failure (&optional message)
  "After a failed `/model' turn, keep or restore the per-turn override.

MESSAGE is the failure text used to classify transient vs permanent errors.
Only transient network failures (after retries are exhausted) ask whether to
keep the override for a manual `retry'.  Permanent errors such as context
overflow restore the buffer model immediately."
  (when emagent-chat--turn-model
    (if (and message (fboundp 'emagent-acp--retriable-prompt-error-p)
         (emagent-acp--retriable-prompt-error-p message))
        (emagent-tools--buttons-prompt
         (format "Continue with %s for the next prompt?" emagent-chat--turn-model)
         '(("Yes, keep it" . keep) ("No, use the buffer model" . restore))
         (current-buffer)
         (lambda (choice)
           (when (eq choice 'restore)
             (emagent-acp--restore-turn-model))))
      (emagent-acp--restore-turn-model))))

(eval-when-compile
  (require 'cl-lib))

;; Register grouped lisp/ subdirectories on load-path so that
;; cross-directory requires (emagent-log from lisp/core/ etc.)
;; work during byte-compilation by Elpaca or other build tools.
;; Uses `byte-compile-current-file' when set (Elpaca compile).
(eval-and-compile
  (when-let ((file (or load-file-name
                       (and (boundp 'byte-compile-current-file)
                            byte-compile-current-file)))
             (lisp (expand-file-name ".." (file-name-directory file))))
    (when (file-directory-p lisp)
      (dolist (dir (directory-files lisp nil "^[^.]"))
        (let ((path (expand-file-name dir lisp)))
          (when (file-directory-p path)
            (add-to-list 'load-path path)))))))

(defun emagent-acp-prefer-emacs-p ()
  "Return non-nil when emagent instructs the agent to prefer Emacs tools."
  emagent-acp-prefer-emacs)

(defun emagent-reset-permissions ()
  "Reset stored emagent permissions via a minibuffer menu.

Choices:
  project: all      — clears fingerprints and allowed tools for the current
                      project directory
  project: session  — clears fingerprints and auto-approve for the current
                      ACP session
  global: all       — clears all globally approved fingerprints."
  (interactive)
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Not in an emagent buffer"))
  (let* ((session-id (emagent-session-id))
         (project-dir (emagent-session-project-directory))
         (choices
          (delq nil
                (list
                 (when project-dir  "project: all")
                 (when session-id   "project: session")
                 "global: all")))
         (choice (completing-read "Reset permissions: " choices nil t)))
    (pcase choice
      ("project: all"
       (unless project-dir (user-error "No project directory for this buffer"))
       (emagent-permissions-reset-project project-dir)
       (message "emagent: cleared project permissions for %s" project-dir))
      ("project: session"
       (unless session-id (user-error "No active session for this buffer"))
       (emagent-permissions-reset-session session-id)
       (message "emagent: cleared session permissions for session %s" session-id))
      ("global: all"
       (emagent-permissions-reset-global)
       (message "emagent: cleared all global permissions")))))

(defun emagent-acp-current-model-id ()
  "Return the ACP session model id for the current buffer, or nil."
  (when-let ((state (emagent-acp--session)))
    (emagent-acp--current-model-id state nil)))

(defun emagent-acp-set-model-transient (model-id on-done &optional spec)
  "Switch this buffer's ACP session model to MODEL-ID without persisting it.
The buffer model (`emagent-session-model') is left unchanged, so this is a
per-turn override.  ON-DONE is called once the switch resolves (success or
failure) so the caller can proceed to send the prompt.

Optional SPEC is ((CONFIG-ID . VALUE) ...) applied instead of MODEL-ID alone
when non-nil (composed model_config / thought_level rows)."
  (let ((state (emagent-acp--session)))
    (if state
        (if spec
            (emagent-acp--config-option-set-spec
             :state state
             :session-id (emagent-acp-state-session-id state)
             :spec spec
             :persist nil
             :on-success on-done
             :on-failure (lambda (&rest _) (when on-done (funcall on-done))))
          (emagent-acp--config-option-set-model-id
           :state state
           :session-id (emagent-acp-state-session-id state)
           :model-id model-id
           :persist nil
           :on-success on-done
           :on-failure (lambda (&rest _) (when on-done (funcall on-done)))))
      (when on-done (funcall on-done)))))

(defun emagent-set-model ()
  "Set the ACP model for the current emagent session.

Offers a flat, filterable list of advertised model variants (including
bracketed Cursor ids and composed model_config / thought_level rows)."
  (interactive)
  (let ((state (emagent-acp--session))
        (buf (current-buffer)))
    (unless state
      (user-error "No active session"))
    (emagent-acp--with-model-variant-choices
     state nil
     (lambda (choices)
       (with-current-buffer buf
         (let* ((session-id (emagent-acp-state-session-id state))
                (labels (mapcar (function car) choices))
                (selection (emagent-acp--read-labeled-choice
                            "Set emagent model: "
                            labels))
                (spec (emagent-acp--choice-by-label selection choices))
                (model-id (and spec (emagent-acp--spec-model-value spec state))))
           (unless session-id
             (user-error "No active session"))
           (unless choices
             (user-error "No models available"))
           (unless spec
             (user-error "Unknown model: %s" selection))
           (when-let ((current (emagent-acp--current-model-id state nil)))
             (when (and model-id (string= model-id current)
                        (= (length spec) 1))
               (user-error "Model already %s"
                           (emagent-acp--model-display-name state nil model-id))))
           (emagent-acp--config-option-set-spec
            :state state
            :session-id session-id
            :spec spec)))))))

(provide 'emagent-acp)
;;; emagent-acp.el ends here
