;;; emagent-trust.el --- Common workspace trust helpers -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.8
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
;; Workspace trust helpers for Claude and Cursor providers: detect and
;; record on-disk trust markers so ACP agents can operate in the project.
;;
;;; Code:

(require 'json)

(defgroup emagent-trust nil
  "Workspace trust integration for emagent."
  :group 'emagent
  :prefix "emagent-trust-")

(defcustom emagent-trust-enabled t
  "When non-nil, record workspace trust on disk before a new Claude/Cursor session.

Remote `default-directory' values (Tramp) skip file writes."
  :type 'boolean
  :group 'emagent-trust)

(defun emagent-trust--normalize-dir (directory)
  "Return DIRECTORY as an absolute, `directory-file-name' path."
  (let ((dir (expand-file-name directory)))
    (if (string= dir "/")
        "/"
      (directory-file-name dir))))

(defun emagent-trust--remote-p (directory)
  "Return non-nil when DIRECTORY is under Tramp or another remote handler."
  (file-remote-p (emagent-trust--normalize-dir directory)))

(defun emagent-trust--iso8601-utc-now ()
  "Return an ISO-8601 UTC timestamp string with millisecond field."
  (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" nil t))

(defun emagent-trust--json-read-file (path)
  "Read the first JSON value from PATH using `json-read'.

Return nil for a missing file or an empty buffer (treat as an empty JSON
object when paired with `emagent-trust--json-write-file').  Objects are alists
with string keys (`json-key-type' is `string').

Uses `json-read' instead of `json-parse-buffer' so large Claude Code state
files round-trip via `json-encode'.  Arrays use `json-array-type' `vector' and
JSON null uses the `:json-null' symbol so empty `[]' / `{}' do not become
Elisp nil and then serialize as JSON null (which breaks Claude Code
`projects' entries such as `allowedTools': [])."
  (condition-case nil
      (if (not (file-readable-p path))
          nil
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path)
          (if (zerop (buffer-size))
              nil
            (goto-char (point-min))
            (let ((json-object-type 'alist)
                  (json-array-type 'vector)
                  (json-key-type 'string)
                  (json-null :json-null)
                  (json-false :json-false))
              (json-read)))))
    (json-error
     (error "Invalid JSON in %s" path))))

(defun emagent-trust--json-write-file (object path)
  "Write OBJECT as pretty-printed JSON to PATH atomically.

OBJECT is either an alist/plist/list tree from `json-read' (written with
`json-encode'), a hash table (written with `json-serialize'), or nil (written
as `{}').  Optional pretty-print uses `json-pretty-print-buffer'."
  (let* ((dir (file-name-directory path))
         (tmp (make-temp-file "emagent-trust-" nil ".json")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (cond
       ((null object)
        (insert "{}"))
       ((hash-table-p object)
        (insert (json-serialize object
                                :null-object nil
                                :false-object :json-false)))
       (t
        (let ((json-object-type 'alist)
              (json-array-type 'vector)
              (json-key-type 'string)
              (json-null :json-null)
              (json-false :json-false))
          (insert (json-encode object)))))
      (when (fboundp 'json-pretty-print-buffer)
        (goto-char (point-min))
        (condition-case nil
            ;; `json-pretty-print' binds `json-null' / object / key but not
            ;; `json-array-type'.  With the default `list', JSON [] reads as
            ;; nil and re-encodes as null — corrupting Claude `projects' keys
            ;; like `allowedTools' and empty objects like `mcpServers'.
            (let ((json-array-type 'vector)
                  (json-null :json-null)
                  (json-false :json-false))
              (json-pretty-print-buffer))
          (json-error nil)))
      (write-region (point-min) (point-max) tmp nil 'silent))
    (rename-file tmp path t)))

(defun emagent-trust--prompt-ynq (directory agent-name)
  "Ask y/n/q for DIRECTORY trust with AGENT-NAME; return `y, `n, or `q."
  (let ((msg (format
              "Trust this workspace for %s?\n%s\n(y=yes write trust config, n=no keep restricted, q=quit) "
              agent-name directory)))
    (pcase (read-char-choice msg '(?y ?n ?q))
      (?y 'y)
      (?n 'n)
      (?q 'q))))

(defcustom emagent-trust-claude-json-file
  (expand-file-name "~/.claude.json")
  "Claude Code user state file (`projects' map per absolute directory).

Each project value may include `hasTrustDialogAccepted'.  Trust applies only
when that field is JSON true; a missing entry, a missing field, or JSON false
means not trusted at that path (parent directories are still checked, as in
the `claude' CLI)."
  :type 'file
  :group 'emagent-trust)

(defun emagent-trust-claude--has-trust-accepted-p (cell)
  "Return non-nil when project CELL records explicit trust.

In the JSON file the flag is the boolean true; after `json-read' it is
symbol t.  Absent key, JSON false (`:json-false'), or any other value is not
accepted at this path."
  (and cell
       (json-alist-p cell)
       (eq (alist-get "hasTrustDialogAccepted" cell nil nil #'equal) t)))

(defun emagent-trust-claude--projects-table (path)
  "Return (ROOT . PROJECTS) for PATH, creating `projects' if missing."
  (let ((root (emagent-trust--json-read-file path)))
    (unless (or (null root) (json-alist-p root))
      (error "Expected JSON object at top level: %s" path))
    (unless root (setq root '()))
    (let* ((entry (assoc "projects" root #'equal))
           (projects nil))
      (unless entry
        (setq entry (cons "projects" '()))
        (setq root (cons entry root)))
      (setq projects (cdr entry))
      (when (and projects (not (json-alist-p projects)))
        (user-error
         "Key `projects' must be a JSON object in %s (repair the file, then retry)"
         path))
      (cons root projects))))

(defun emagent-trust-claude-trusted-p (directory)
  "Return non-nil when DIRECTORY is trusted per Claude Code.

Walks upward like the CLI; see `emagent-trust-claude--has-trust-accepted-p'."
  (let* ((dir (emagent-trust--normalize-dir directory))
         (path emagent-trust-claude-json-file)
         (projects (cdr (emagent-trust-claude--projects-table path)))
         (p dir)
         (trusted nil))
    (while (and p (not trusted))
      (when-let ((cell (alist-get p projects nil nil #'equal)))
        (when (emagent-trust-claude--has-trust-accepted-p cell)
          (setq trusted t)))
      (unless trusted
        (setq p (unless (string= p "/")
                  (file-name-directory (directory-file-name p))))
        (when p
          (setq p (directory-file-name (expand-file-name p))))))
    trusted))

(defun emagent-trust-claude-record-trust (directory)
  "Set `hasTrustDialogAccepted' true for DIRECTORY in the Claude JSON file.

Preserves other keys on the same `projects' entry (e.g. fixes explicit false)."
  (let* ((dir (emagent-trust--normalize-dir directory))
         (path emagent-trust-claude-json-file)
         (pair (emagent-trust-claude--projects-table path))
         (root (car pair))
         (entry (assoc "projects" root #'equal))
         (projects (cdr entry)))
    (let ((cell (alist-get dir projects nil nil #'equal)))
      (cond
       ((and cell (json-alist-p cell))
        (setf (alist-get "hasTrustDialogAccepted" cell nil nil #'equal) t))
       (t
        (setf (alist-get dir projects nil nil #'equal)
              (list (cons "hasTrustDialogAccepted" t))))))
    ;; setf on an empty `projects' alist can replace the local list without
    ;; updating the cons on ROOT; sync the entry before writing.
    (setcdr entry projects)
    (emagent-trust--json-write-file root path)))

(defcustom emagent-trust-cursor-config-dir
  (expand-file-name "~/.cursor")
  "Cursor configuration directory (contains `projects/' trust markers)."
  :type 'directory
  :group 'emagent-trust)

(defun emagent-trust-cursor--project-slug (directory)
  "Return Cursor project slug for DIRECTORY (under projects/ in config dir)."
  (let ((abs (emagent-trust--normalize-dir directory)))
    (if (string= abs "/")
        ""
      (replace-regexp-in-string "/" "-" (string-remove-prefix "/" abs)))))

(defun emagent-trust-cursor--trust-file (directory)
  "Return the path to Cursor's `.workspace-trusted' marker for DIRECTORY."
  (expand-file-name
   (format "projects/%s/.workspace-trusted"
           (emagent-trust-cursor--project-slug directory))
   emagent-trust-cursor-config-dir))

(defun emagent-trust-cursor-trusted-p (directory)
  "Return non-nil when Cursor's trust marker exists and matches DIRECTORY."
  (let ((tf (emagent-trust-cursor--trust-file directory)))
    (and (file-readable-p tf)
         (condition-case nil
             (let* ((data (emagent-trust--json-read-file tf))
                    (wp (alist-get "workspacePath" data nil nil #'equal)))
               (and (stringp wp)
                    (string= (emagent-trust--normalize-dir wp)
                             (emagent-trust--normalize-dir directory))))
           (json-error nil)))))

(defun emagent-trust-cursor-record-trust (directory)
  "Write Cursor's `.workspace-trusted' marker for DIRECTORY."
  (let* ((dir (emagent-trust--normalize-dir directory))
         (tf (emagent-trust-cursor--trust-file directory))
         (obj (list (cons "trustedAt" (emagent-trust--iso8601-utc-now))
                    (cons "workspacePath" dir))))
    (emagent-trust--json-write-file obj tf)))

(defun emagent-trust--already-trusted-p (provider directory)
  "Return non-nil when PROVIDER considers DIRECTORY trusted on disk."
  (pcase provider
    ('claude (emagent-trust-claude-trusted-p directory))
    ('cursor (emagent-trust-cursor-trusted-p directory))
    (_ t)))

(defun emagent-trust--record-trust-if-needed (provider directory)
  "Write on-disk trust for PROVIDER at DIRECTORY when not already trusted."
  (unless (emagent-trust--already-trusted-p provider directory)
    (let ((dir (emagent-trust--normalize-dir directory)))
      (condition-case err
          (pcase provider
            ('claude (emagent-trust-claude-record-trust dir))
            ('cursor (emagent-trust-cursor-record-trust dir)))
        (error (signal (car err) (cdr err)))))))

(defun emagent-trust--configure (provider project-dir)
  "Handle trust setup for PROVIDER at PROJECT-DIR.

When trust is missing on disk, writes Claude (~/.claude.json) or Cursor
\(~/.cursor/projects/.../.workspace-trusted) markers automatically."
  (cond
   ((not (and emagent-trust-enabled (memq provider '(claude cursor))))
    nil)
   ((emagent-trust--remote-p project-dir)
    (message "emagent: skipping workspace trust (remote directory)")
    nil)
   (t
    (emagent-trust--record-trust-if-needed provider project-dir)
    nil)))

(provide 'emagent-trust)
;;; emagent-trust.el ends here
