;;; emagent-session.el --- Per-buffer emagent session identity  -*- lexical-binding: t; -*-

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
;; Session state, model/context helpers, and workspace trust.
;;
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'json)
(require 'subr-x)
(require 'org)
(require 'org-element)

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

(defgroup emagent-permissions nil
  "Persistent permission storage for emagent."
  :group 'emagent
  :prefix "emagent-permissions-")

(defcustom emagent-permissions-directory
  (expand-file-name ".emagent" "~")
  "Directory for emagent permission files and durable project notes.

Layout:
- global.json — fingerprints from \"Allow always\"
- sessions/SESSION.json — per-ACP-session fingerprints and allow-all flag
- projects/HASH.json — per-project MCP tools and permission fingerprints
- projects/HASH.notes.org — per-project durable notes (outside the repo)"
  :type 'directory
  :group 'emagent-permissions)

(defvar emagent-permissions--cache (make-hash-table :test 'equal)
  "Cache of (FILE-MTIME . DATA) keyed by absolute file path.")

(defun emagent-permissions--ensure-directory (subdir)
  "Return SUBDIR under `emagent-permissions-directory', creating it."
  (let ((dir (expand-file-name subdir emagent-permissions-directory)))
    (make-directory dir t)
    dir))

(defun emagent-permissions--safe-filename (name)
  "Return a filesystem-safe form of NAME."
  (replace-regexp-in-string "[^a-zA-Z0-9._-]+" "_" name))

(defun emagent-permissions--global-file ()
  
  "Internal helper."
  (expand-file-name "global.json" emagent-permissions-directory))

(defun emagent-permissions--session-file (session-id)
  
  "Internal helper for SESSION-ID."
  (expand-file-name
   (format "%s.json" (emagent-permissions--safe-filename session-id))
   (emagent-permissions--ensure-directory "sessions")))

(defun emagent-permissions--project-file (directory)
  
  "Internal helper for DIRECTORY."
  (when directory
    (let ((norm (emagent-trust--normalize-dir directory)))
      (expand-file-name
       (format "%s.json" (secure-hash 'sha1 norm))
       (emagent-permissions--ensure-directory "projects")))))

(defun emagent-permissions--invalidate (path)
  
  "Internal helper for PATH."
  (when path (remhash path emagent-permissions--cache)))

(defun emagent-permissions--file-mtime (path)
  
  "Internal helper for PATH."
  (when (file-readable-p path)
    (file-attribute-modification-time (file-attributes path))))

(defun emagent-permissions--read-json (path)
  
  "Internal helper for PATH."
  (or (emagent-trust--json-read-file path) nil))

(defun emagent-permissions--write-json (path object)
  
  "Internal helper for PATH and OBJECT."
  (make-directory emagent-permissions-directory t)
  (emagent-trust--json-write-file object path)
  (emagent-permissions--invalidate path))

(defun emagent-permissions--cached (path reader)
  
  "Internal helper for PATH and READER."
  (let ((mtime (emagent-permissions--file-mtime path)))
    (if-let ((entry (gethash path emagent-permissions--cache)))
        (if (equal (car entry) mtime)
            (cdr entry)
          (let ((data (funcall reader path)))
            (puthash path (cons mtime data) emagent-permissions--cache)
            data))
      (let ((data (funcall reader path)))
        (puthash path (cons mtime data) emagent-permissions--cache)
        data))))

(defun emagent-permissions--vector-to-list (value)
  
  "Internal helper for VALUE."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun emagent-permissions--fingerprints (data)
  
  "Internal helper for DATA."
  (emagent-permissions--vector-to-list (cdr (assoc "fingerprints" data))))

(defun emagent-permissions--tools (data)
  
  "Internal helper for DATA."
  (mapcar #'intern (emagent-permissions--vector-to-list (cdr (assoc "tools" data)))))

(defun emagent-permissions--auto-approve-p (data)
  
  "Internal helper for DATA."
  (let ((value (cdr (assoc "autoApprove" data))))
    (and value (not (eq value :json-false)) (not (eq value :false)))))

(defun emagent-permissions--maybe-migrate-legacy-global ()
  
  "Internal helper."
  (let* ((legacy (expand-file-name "allowed-permissions" emagent-permissions-directory))
         (global (emagent-permissions--global-file)))
    (when (and (file-readable-p legacy) (not (file-readable-p global)))
      (let (fingerprints)
        (with-temp-buffer
          (insert-file-contents legacy)
          (dolist (line (split-string (buffer-string) "\n" t))
            (setq fingerprints
                  (append fingerprints (split-string line "[ \t]+" t)))))
        (emagent-permissions--write-json
         global `((fingerprints . ,(vconcat (delete-dups fingerprints)))))))))

(defun emagent-permissions--read-global-data (path)
  
  "Internal helper for PATH."
  (emagent-permissions--maybe-migrate-legacy-global)
  (or (emagent-permissions--read-json path)
      '((fingerprints . []))))

(defun emagent-permissions--read-session-data (session-id path)
  
  "Internal helper for SESSION-ID and PATH."
  (or (emagent-permissions--read-json path)
      `((sessionId . ,session-id)
        (fingerprints . [])
        (autoApprove . :json-false))))

(defun emagent-permissions--read-project-data (directory path)
  
  "Internal helper for DIRECTORY and PATH."
  (or (emagent-permissions--read-json path)
      `((directory . ,(emagent-trust--normalize-dir directory))
        (fingerprints . [])
        (tools . []))))

(defun emagent-permissions-global-fingerprints ()
  "Return globally allowed ACP permission fingerprints."
  (emagent-permissions--fingerprints
   (emagent-permissions--cached (emagent-permissions--global-file)
                                #'emagent-permissions--read-global-data)))

(defun emagent-permissions-add-global-fingerprint (fingerprint)
  "Persist FINGERPRINT as globally allowed."
  (when fingerprint
    (let* ((path (emagent-permissions--global-file))
           (data (emagent-permissions--read-global-data path))
           (merged (append (emagent-permissions--fingerprints data)
                           (list fingerprint))))
      (unless (member fingerprint (emagent-permissions--fingerprints data))
        (emagent-permissions--write-json
         path `((fingerprints . ,(vconcat (delete-dups merged)))))))))

(defun emagent-permissions-session-fingerprints (session-id)
  "Return session-scoped fingerprints for SESSION-ID."
  (when (and session-id (not (string-empty-p session-id)))
    (emagent-permissions--fingerprints
     (emagent-permissions--cached (emagent-permissions--session-file session-id)
                                  (lambda (path)
                                    (emagent-permissions--read-session-data session-id path))))))

(defun emagent-permissions-add-session-fingerprint (session-id fingerprint)
  "Persist FINGERPRINT for SESSION-ID."
  (when (and session-id fingerprint (not (string-empty-p session-id)))
    (let* ((path (emagent-permissions--session-file session-id))
           (data (emagent-permissions--read-session-data session-id path))
           (merged (append (emagent-permissions--fingerprints data)
                           (list fingerprint))))
      (unless (member fingerprint (emagent-permissions--fingerprints data))
        (emagent-permissions--write-json
         path `((sessionId . ,session-id)
                (fingerprints . ,(vconcat (delete-dups merged)))
                (autoApprove . ,(if (emagent-permissions--auto-approve-p data) t :json-false))))))))

(defun emagent-permissions-session-auto-approve-p (session-id)
  "Return non-nil when SESSION-ID has allow-all enabled."
  (when (and session-id (not (string-empty-p session-id)))
    (emagent-permissions--auto-approve-p
     (emagent-permissions--cached (emagent-permissions--session-file session-id)
                                  (lambda (path)
                                    (emagent-permissions--read-session-data session-id path))))))

(defun emagent-permissions-set-session-auto-approve (session-id)
  "Enable allow-all for SESSION-ID."
  (when (and session-id (not (string-empty-p session-id)))
    (let* ((path (emagent-permissions--session-file session-id))
           (data (emagent-permissions--read-session-data session-id path)))
      (unless (emagent-permissions--auto-approve-p data)
        (emagent-permissions--write-json
         path `((sessionId . ,session-id)
                (fingerprints . ,(vconcat (emagent-permissions--fingerprints data)))
                (autoApprove . t)))))))

(defun emagent-permissions-project-fingerprints (directory)
  "Return project-scoped fingerprints for DIRECTORY."
  (when-let ((path (emagent-permissions--project-file directory)))
    (emagent-permissions--fingerprints
     (emagent-permissions--cached path
                                  (lambda (_path)
                                    (emagent-permissions--read-project-data directory path))))))

(defun emagent-permissions-project-tools (directory)
  "Return project-allowed MCP tool symbols for DIRECTORY."
  (when-let ((path (emagent-permissions--project-file directory)))
    (emagent-permissions--tools
     (emagent-permissions--cached path
                                  (lambda (_path)
                                    (emagent-permissions--read-project-data directory path))))))

(defun emagent-permissions-add-project-tool (directory tool)
  "Persist TOOL as allowed for DIRECTORY."
  (when (and directory tool)
    (let* ((sym (if (stringp tool) (intern tool) tool))
           (path (emagent-permissions--project-file directory))
           (data (emagent-permissions--read-project-data directory path))
           (merged (append (emagent-permissions--tools data) (list sym))))
      (unless (memq sym (emagent-permissions--tools data))
        (emagent-permissions--write-json
         path `((directory . ,(emagent-trust--normalize-dir directory))
                (fingerprints . ,(vconcat (emagent-permissions--fingerprints data)))
                (tools . ,(vconcat (mapcar #'symbol-name (delete-dups merged))))))))))

(defun emagent-permissions-reset-global ()
  "Clear all globally allowed permission fingerprints."
  (let ((path (emagent-permissions--global-file)))
    (emagent-permissions--write-json path '((fingerprints . [])))))

(defun emagent-permissions-reset-session (session-id)
  "Clear all permissions stored for SESSION-ID."
  (when (and session-id (not (string-empty-p session-id)))
    (let ((path (emagent-permissions--session-file session-id)))
      (emagent-permissions--write-json
       path `((sessionId . ,session-id)
               (fingerprints . [])
               (autoApprove . :json-false))))))

(defun emagent-permissions-reset-project (directory)
  "Clear all permissions stored for DIRECTORY."
  (when-let ((path (emagent-permissions--project-file directory)))
    (emagent-permissions--write-json
     path `((directory . ,(emagent-trust--normalize-dir directory))
             (fingerprints . [])
             (tools . [])))))

(defun emagent-model-canonical-id (model)
  "Return MODEL id in the form Cursor ACP expects (keep bracket suffixes)."
  (when model
    (if (member model '("auto" "default"))
        "default[]"
      model)))

(defun emagent-model-normalize-id (model)
  "Return a short user-facing label for MODEL.
Strips key=value annotations (e.g. [thinking=true]) and empty brackets ([]).
Maps Cursor default[] to auto."
  (when model
    (let ((stripped (replace-regexp-in-string
                     "\\[\\([^]]*=[^]]*\\)?\\]" "" model)))
      (if (member stripped '("default" "auto")) "auto" stripped))))

(defun emagent-model-choice-label-parts (id &optional name)
  "Return (PRIMARY . SUFFIX) for model ID.
PRIMARY is the base id without bracket annotations; SUFFIX is brackets
plus an optional parenthetical alias when NAME differs from the normalized id."
  (when id
    (let* ((bracket (and (string-match "\\[" id) (match-beginning 0)))
           (base (if bracket (substring id 0 bracket) id))
           (brackets (if bracket (substring id bracket) ""))
           (short-name (and name
                            (not (string= name (emagent-model-normalize-id id)))
                            name)))
      (cons base (concat brackets (if short-name (format " (%s)" short-name) ""))))))

(defun emagent-model-choice-label (id &optional name)
  "Return a `completing-read' label for model ID with the full canonical id.
When NAME differs from the normalized ID (e.g. Auto vs default[]), append it."
  (let ((parts (emagent-model-choice-label-parts id name)))
    (when parts (concat (car parts) (cdr parts)))))

(defun emagent-model-choice-label-display (id &optional name)
  "Like `emagent-model-choice-label', with theme faces for model and details.
The faces `emagent-model-choice-model'/`emagent-model-choice-detail' are
referenced by symbol and resolved at render time, so this stays a leaf.

Arguments: ID, NAME."
  (let ((parts (emagent-model-choice-label-parts id name)))
    (when parts
      (concat (propertize (car parts) 'face 'emagent-model-choice-model)
              (if (string-empty-p (cdr parts))
                  ""
                (propertize (cdr parts) 'face 'emagent-model-choice-detail))))))

(eval-when-compile
  (require 'cl-lib))

(eval-when-compile
  (require 'flymake)
  (require 'which-func))

(defun emagent-context--point-info ()
  "Return point line and column as an alist."
  (save-excursion
    (list (cons :line (line-number-at-pos))
          (cons :column (current-column)))))

(defun emagent-context--region-info ()
  "Return region bounds and text when active."
  (when (region-active-p)
    (list (cons :begin (region-beginning))
          (cons :end (region-end))
          (cons :text (buffer-substring-no-properties (region-beginning) (region-end))))))

(defun emagent-context--org-info ()
  "Return org headline info at point when in `org-mode'.
Skipped in `emagent-mode' buffers: `org-element-at-point' on large session
files is slow and the chat structure is not a normal org document."
  (when (and (derived-mode-p 'org-mode)
             (not (derived-mode-p 'emagent-mode)))
    (ignore-errors
      (let ((element (org-element-at-point)))
        (when (eq (org-element-type element) 'headline)
          (list (cons :title (org-element-property :raw-value element))
                (cons :level (org-element-property :level element))
                (cons :tags (org-element-property :tags element))))))))

(defun emagent-context--which-function ()
  "Return the enclosing function/method name at point, or nil."
  (when (fboundp 'which-function)
    (require 'which-func)
    (ignore-errors (which-function))))

(defun emagent-context--treesit-node ()
  "Return the treesit node type at point, or nil."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p)
             (fboundp 'treesit-node-at))
    (ignore-errors
      (treesit-node-type (treesit-node-at (point))))))

(defun emagent-context--flymake-diagnostics ()
  "Return flymake diagnostics at point as a list of (TYPE . TEXT) pairs."
  (when (and (bound-and-true-p flymake-mode)
             (require 'flymake nil t)
             (fboundp 'flymake-diagnostics))
    (ignore-errors
      (mapcar (lambda (d)
                (cons (format "%s" (flymake-diagnostic-type d))
                      (flymake-diagnostic-text d)))
              (flymake-diagnostics (point))))))

(defcustom emagent-context-level 'minimal
  "How much automatic Emacs context to attach to each prompt.

`minimal' (default): buffer, file, mode, directory, point, enclosing
function; region only when active; org headline in `org-mode'.
`full': also treesit node and flymake diagnostics."
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Full" full))
  :group 'emagent)

(defcustom emagent-context-skip-unchanged t
  "When non-nil, reuse a short unchanged marker if context matches the last turn."
  :type 'boolean
  :group 'emagent)

(defvar-local emagent-context--last-fingerprint nil
  "Fingerprint of the last injected Emacs context block.")

(defun emagent-context-auto ()
  "Build automatic Emacs context for the current buffer."
  (let ((full (eq emagent-context-level 'full))
        (ctx (list (cons :buffer (buffer-name))
                   (cons :file (or (buffer-file-name) nil))
                   (cons :major-mode (symbol-name major-mode))
                   (cons :default-directory default-directory)
                   (cons :point (emagent-context--point-info))
                   (cons :enclosing-function (emagent-context--which-function)))))
    (when (region-active-p)
      (setq ctx (append ctx (list (cons :region (emagent-context--region-info))))))
    (when (derived-mode-p 'org-mode)
      (setq ctx (append ctx (list (cons :org (emagent-context--org-info))))))
    (when full
      (setq ctx (append ctx
                        (list (cons :treesit-node (emagent-context--treesit-node))
                              (cons :flymake (emagent-context--flymake-diagnostics))))))
    ctx))

(defun emagent-context-format (context)
  "Format CONTEXT alist as a readable string block."
  (let ((lines
         (list "[Emacs context]"
               (format "buffer: %s" (map-elt context :buffer))
               (format "file: %s" (or (map-elt context :file) "<none>"))
               (format "major-mode: %s" (map-elt context :major-mode))
               (format "default-directory: %s" (map-elt context :default-directory)))))
    (when-let* ((point (map-elt context :point)))
      (setq lines (append lines
                          (list (format "point: line %s, column %s"
                                        (map-elt point :line)
                                        (map-elt point :column))))))
    (when-let* ((fn (map-elt context :enclosing-function)))
      (setq lines (append lines (list (format "enclosing-function: %s" fn)))))
    (when-let* ((node (map-elt context :treesit-node)))
      (setq lines (append lines (list (format "treesit-node: %s" node)))))
    (when-let* ((region (map-elt context :region)))
      (let ((rt (or (map-elt region :text) "")))
        (when (> (length rt) 2000)
          (setq rt (concat (substring rt 0 2000) "\n… (region truncated)")))
        (setq lines (append lines
                            (list (format "region: %s-%s"
                                          (map-elt region :begin)
                                          (map-elt region :end))
                                  (format "region-text:\n%s" rt))))))
    (when-let* ((diags (map-elt context :flymake)))
      (let ((kept (cl-subseq diags 0 (min 8 (length diags))))
            (extra (max 0 (- (length diags) 8))))
        (setq lines
              (append lines
                      (list (format "flymake-diagnostics: %s%s"
                                    (mapconcat (lambda (d)
                                                 (format "[%s] %s" (car d) (cdr d)))
                                               kept "; ")
                                    (if (> extra 0)
                                        (format " (+%d more)" extra)
                                      "")))))))
    (when-let* ((org (map-elt context :org)))
      (setq lines (append lines
                          (list (format "org-headline: level %s, title %s"
                                        (map-elt org :level)
                                        (map-elt org :title))))))
    (string-join lines "\n")))

(defun emagent-context-buffer-summary ()
  "Return a short summary of the current buffer."
  (let ((lines (count-lines (point-min) (point-max)))
        (chars (- (point-max) (point-min))))
    (format "[Buffer summary]\nname: %s\nlines: %s\nchars: %s\nmode: %s"
            (buffer-name) lines chars (symbol-name major-mode))))

(defun emagent-context-region ()
  "Return the active region text or signal an error."
  (unless (region-active-p)
    (user-error "No active region"))
  (format "[Region]\n%s"
          (buffer-substring-no-properties (region-beginning) (region-end))))

(defun emagent-context-build-prompt (user-text &optional extra-blocks)
  "Combine USER-TEXT with auto context and EXTRA-BLOCKS."
  (require 'emagent-usage nil t)
  (let* ((ctx (emagent-context-auto))
         (auto (emagent-context-format ctx))
         (fp (secure-hash 'sha1 auto))
         (block
          (if (and emagent-context-skip-unchanged
                   (equal fp emagent-context--last-fingerprint))
              "[Emacs context: unchanged]"
            (progn
              (setq emagent-context--last-fingerprint fp)
              auto)))
         (budget (and (boundp 'emagent-usage-budget-context)
                      emagent-usage-budget-context))
         (block (if (fboundp 'emagent-usage--cap-string)
                    (emagent-usage--cap-string block budget 'context)
                  block))
         (user (if (fboundp 'emagent-usage--cap-string)
                   (emagent-usage--cap-string (or user-text "") nil 'user)
                 (or user-text "")))
         (blocks (cons block (or extra-blocks nil))))
    (string-join (cons user blocks) "\n\n")))

(defun emagent-session-store-read-top-property (name)
  "Return the value of #+NAME at the top of the buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)" name) nil t)
      (string-trim (match-string 1)))))

(defun emagent-session-store-metadata-end ()
  "Return point after emagent comment and metadata header lines."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (or (looking-at "#\\+")
                    (looking-at "# ")
                    (looking-at "#$")))
      (forward-line 1))
    (point)))

(defun emagent-session-store-write-top-property (name value)
  "Insert or update #+NAME in the emagent metadata header.
No-op when #+NAME already holds VALUE, so re-running `emagent-mode' (e.g. on
desktop restore) does not mark the session buffer modified."
  (let* ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (value (format "%s" value))
         (line (format "#+%s: %s" name value))
         (pattern (format "^#\\+%s:[ \t]*.*\n?" name)))
    (unless (equal (emagent-session-store-read-top-property name) value)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (delete-region (match-beginning 0) (match-end 0)))
          (goto-char (emagent-session-store-metadata-end))
          (unless (bolp) (insert "\n"))
          (insert line "\n"))))))

(defun emagent-session-store-delete-top-property (name)
  "Delete #+NAME from the top of the buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "^#\\+%s:.*\n?" name) nil t)
        (replace-match "")))))

(defconst emagent-session-notes-property "EMAGENT_NOTES"
  "Top-property name for durable session notes.")

(defconst emagent-session-notes-max-chars 2000
  "Maximum characters kept in session notes.")

(defun emagent-session-notes-read ()
  "Return decoded session notes text, or \"\"."
  (let ((raw (emagent-session-store-read-top-property
              emagent-session-notes-property)))
    (if (not raw)
        ""
      (replace-regexp-in-string "\\\\n" "\n" raw t t))))

(defun emagent-session-notes-write (text)
  "Store TEXT as session notes (newline-escaped, capped)."
  (let* ((trimmed (string-trim (or text "")))
         (capped (if (> (length trimmed) emagent-session-notes-max-chars)
                     (substring trimmed 0 emagent-session-notes-max-chars)
                   trimmed))
         (encoded (replace-regexp-in-string "\n" "\\\\n" capped t t)))
    (if (string-empty-p capped)
        (emagent-session-store-delete-top-property emagent-session-notes-property)
      (emagent-session-store-write-top-property
       emagent-session-notes-property encoded))))

(defun emagent-session-notes--extract-facts (summary)
  "Return FACTS bullets from SUMMARY, or nil."
  (when (and (stringp summary)
             (string-match "FACTS:\\s-*\n\\(\\(.\\|\n\\)*\\)\\'" summary))
    (string-trim (match-string 1 summary))))

(defun emagent-session-notes-strip-facts (summary)
  "Return SUMMARY without a trailing FACTS: section."
  (if (and (stringp summary)
           (string-match "\n*FACTS:\\s-*\n\\(.\\|\n\\)*\\'" summary))
      (string-trim (substring summary 0 (match-beginning 0)))
    (or summary "")))

(defun emagent-session-notes-merge-from-summary (summary)
  "Merge FACTS from SUMMARY into durable session notes."
  (when-let ((facts (emagent-session-notes--extract-facts summary)))
    (let* ((old (emagent-session-notes-read))
           (combined (if (string-empty-p old)
                         facts
                       (concat old "\n" facts))))
      (emagent-session-notes-write combined))))

(defcustom emagent-project-notes-filename-suffix ".notes.org"
  "Suffix for durable project notes under `emagent-permissions-directory'.

Notes live at projects/HASH.notes.org next to projects/HASH.json, not
inside the project tree."
  :type 'string
  :group 'emagent)

(defun emagent-session-project-notes-file ()
  "Return absolute path of durable project notes under ~/.emagent, or nil.

Path is `emagent-permissions-directory'/projects/HASH.notes.org, using the
same project HASH as `emagent-permissions--project-file'."
  (when-let* ((root (emagent-session-project-directory))
              (norm (emagent-trust--normalize-dir root))
              (hash (secure-hash 'sha1 norm))
              (dir (emagent-permissions--ensure-directory "projects")))
    (expand-file-name
     (concat hash emagent-project-notes-filename-suffix)
     dir)))

(defun emagent-session-project-notes-read ()
  "Return project notes text, or \"\"."
  (when-let* ((path (emagent-session-project-notes-file))
              ((file-readable-p path)))
    (with-temp-buffer
      (insert-file-contents path)
      (string-trim (buffer-string)))))

(defun emagent-session-notes-prompt-block ()
  "Return a system-prompt block for session and project notes, or \"\"."
  (require 'emagent-usage nil t)
  (let* ((session (emagent-session-notes-read))
         (project (or (emagent-session-project-notes-read) ""))
         (parts (delq nil
                      (list (and (not (string-empty-p session))
                                 (format "session:\n%s" session))
                            (and (not (string-empty-p project))
                                 (format "project:\n%s" project)))))
         (raw (string-join parts "\n\n"))
         (budget (and (boundp 'emagent-usage-budget-notes)
                      emagent-usage-budget-notes))
         (notes (if (fboundp 'emagent-usage--cap-string)
                    (emagent-usage--cap-string raw budget 'notes)
                  raw)))
    (if (string-empty-p notes)
        ""
      (format "\n\n[Session notes]\n%s" notes))))

(defun emagent-session-store-read-project-property ()
  "Return the #+EMAGENT_PROJECT value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_PROJECT"))

(defun emagent-session-store-read-model-property ()
  "Return the #+EMAGENT_MODEL value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_MODEL"))

(defun emagent-session-store-read-session-property ()
  "Return the #+EMAGENT_SESSION value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_SESSION"))

(defun emagent-session-store-read-agent-property ()
  "Return the #+EMAGENT_AGENT value at the top of the buffer."
  (emagent-session-store-read-top-property "EMAGENT_AGENT"))

(defconst emagent-session-store--allowed-tools-property "EMAGENT_ALLOWED_TOOLS")

(defconst emagent-session-store--allowed-permissions-property "EMAGENT_ALLOWED_PERMISSIONS")

(defun emagent-session-store-read-allowed-tools-property ()
  "Return the #+EMAGENT_ALLOWED_TOOLS value as a list of tool symbols."
  (when-let* ((value (emagent-session-store-read-top-property
                      emagent-session-store--allowed-tools-property))
              ((not (string-empty-p value))))
    (mapcar #'intern (split-string value "[ ,]+" t))))

(defun emagent-session-store-read-allowed-permissions-property ()
  "Return #+EMAGENT_ALLOWED_PERMISSIONS as a list of permission fingerprints."
  (when-let* ((value (emagent-session-store-read-top-property
                      emagent-session-store--allowed-permissions-property))
              ((not (string-empty-p value))))
    (split-string value "[ ,]+" t)))

(defun emagent-session-store-display-project-directory (directory)
  "Return DIRECTORY as written in #+EMAGENT_PROJECT."
  (file-name-as-directory
   (abbreviate-file-name (expand-file-name directory))))

;; Names retain the `emagent-chat-' prefix: they are referenced widely and are
;; effectively the persisted-session field names.  Their home is here.

(defvar-local emagent-chat-project-directory nil
  "Project directory for the current emagent buffer.")

(defvar-local emagent-chat-model nil
  "ACP model id for the current emagent buffer.")

(defvar-local emagent-chat-session-id nil
  "ACP session id for the current emagent buffer.")

(defvar-local emagent-chat-provider nil
  "ACP provider symbol (`cursor' or `claude') for the current emagent buffer.")

(defvar-local emagent-chat-cursor-acp-extra-args nil
  "When non-nil, replaces `emagent-cursor-acp-extra-args' for this buffer only.")

(defvar-local emagent-chat-allowed-tools nil
  "Extra MCP tools allowed without confirmation for this buffer session.

Project-wide choices persist under `emagent-permissions-directory'.")

(defvar-local emagent-chat-allowed-permissions nil
  "Legacy buffer-local permission fingerprints from #+EMAGENT_ALLOWED_PERMISSIONS.

New choices persist under `emagent-permissions-directory'.")

(defun emagent-session-id ()
  "Return the persisted ACP session id for the current buffer."
  (or emagent-chat-session-id (emagent-session-store-read-session-property)))

(defun emagent-session-set-id (session-id)
  "Store ACP SESSION-ID in the current buffer."
  (unless (equal emagent-chat-session-id session-id)
    (setq emagent-chat-session-id session-id)
    (emagent-session-store-write-top-property "EMAGENT_SESSION" session-id)))

(defun emagent-session-clear-id ()
  "Remove the ACP session id from the current buffer."
  (setq emagent-chat-session-id nil)
  (emagent-session-store-delete-top-property "EMAGENT_SESSION"))

(defun emagent-session-set-project-directory (directory)
  "Store DIRECTORY as the project directory in the current buffer."
  (let ((dir (expand-file-name directory)))
    (setq emagent-chat-project-directory dir)
    (setq-local default-directory dir)
    (emagent-session-store-write-top-property
     "EMAGENT_PROJECT" (emagent-session-store-display-project-directory dir))))

(defun emagent-session-project-directory ()
  "Return the project directory for the current emagent buffer."
  (or emagent-chat-project-directory (emagent-session-store-read-project-property)))

(defun emagent-session-set-model (model)
  "Store ACP MODEL id in the current buffer.
No UI side effects — callers that need a mode-line refresh add it themselves."
  (setq model (emagent-model-canonical-id model))
  (unless (equal emagent-chat-model model)
    (setq emagent-chat-model model)
    (emagent-session-store-write-top-property "EMAGENT_MODEL" model))
  (setq emagent-chat-model (or emagent-chat-model model)))

(defun emagent-session-model ()
  "Return the ACP model id for the current emagent buffer."
  (emagent-model-canonical-id
   (or emagent-chat-model (emagent-session-store-read-model-property))))

(defun emagent-session-model-display (&optional model)
  "Return MODEL as a short label for the mode line."
  (emagent-model-normalize-id
   (or model (emagent-session-model))))

(defun emagent-session-set-agent (agent)
  "Store the ACP provider AGENT symbol in the current buffer."
  (when agent
    (setq emagent-chat-provider agent)
    (emagent-session-store-write-top-property "EMAGENT_AGENT" (symbol-name agent))))

(defun emagent-session-agent ()
  "Return the ACP provider symbol for the current emagent buffer, or nil."
  (or emagent-chat-provider
      (when-let* ((value (emagent-session-store-read-agent-property))
                  ((not (string-empty-p value))))
        (intern value))))

(defun emagent-session-allowed-tools ()
  "Return MCP tools allowed without confirmation for this buffer's project."
  (let* ((legacy (or emagent-chat-allowed-tools
                     (emagent-session-store-read-allowed-tools-property)))
         (stored (when-let ((dir (emagent-session-project-directory)))
                   (emagent-permissions-project-tools dir))))
    (cl-delete-duplicates (append legacy stored))))

(defun emagent-session-add-allowed-tool (tool)
  "Allow TOOL for this project without confirmation and persist it."
  (let* ((sym (if (stringp tool) (intern tool) tool))
         (dir (emagent-session-project-directory)))
    (unless (memq sym (emagent-session-allowed-tools))
      (setq emagent-chat-allowed-tools (append (or emagent-chat-allowed-tools nil)
                                               (list sym)))
      (when dir
        (emagent-permissions-add-project-tool dir sym)))))

(defun emagent-session-allowed-permissions ()
  "Return legacy buffer permission fingerprints still honored at the gate."
  (or emagent-chat-allowed-permissions
      (emagent-session-store-read-allowed-permissions-property)))

(defun emagent-session-add-allowed-permission (fingerprint)
  "Persist FINGERPRINT as globally allowed for ACP permission requests."
  (emagent-permissions-add-global-fingerprint fingerprint))

(defun emagent-session-allowed-permissions-for (session-id)
  "Return session-scoped permission fingerprints for SESSION-ID."
  (emagent-permissions-session-fingerprints session-id))

(defun emagent-session-add-session-permission (session-id fingerprint)
  "Record FINGERPRINT as session-scoped for SESSION-ID."
  (emagent-permissions-add-session-fingerprint session-id fingerprint))

(defun emagent-session-auto-approve-p (session-id)
  "Return non-nil when SESSION-ID has allow-all enabled."
  (emagent-permissions-session-auto-approve-p session-id))

(defun emagent-session-set-auto-approve (session-id)
  "Enable allow-all for SESSION-ID."
  (emagent-permissions-set-session-auto-approve session-id))

(provide 'emagent-session)
;;; emagent-session.el ends here
