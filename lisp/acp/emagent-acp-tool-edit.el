;;; emagent-acp-tool-edit.el --- ACP tool-call edit and diff helpers  -*- lexical-binding: t; -*-

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

;; Infer write/edit kinds and build diff block specs for tool calls.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'emagent-tools)
(require 'emagent-acp-tool-parse)

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

(provide 'emagent-acp-tool-edit)
;;; emagent-acp-tool-edit.el ends here
