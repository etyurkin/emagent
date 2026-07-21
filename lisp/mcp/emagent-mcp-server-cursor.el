;;; emagent-mcp-server-cursor.el --- Cursor MCP config helpers  -*- lexical-binding: t; -*-

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

;; Cursor mcp.json merge and per-project mcp-approvals helpers.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-mcp-server-lifecycle)

;;;; Cursor configuration

(defun emagent-mcp--lists-to-vectors (object)
  "Recursively convert JSON arrays (lists) to vectors for `json-serialize'.

`json-parse-buffer' with `:array-type \\='list\\=' yields lists, but
`json-serialize' treats lists as alists and requires symbol keys.

Arguments: OBJECT."
  (cond
   ((hash-table-p object)
    (maphash (lambda (key value)
               (puthash key (emagent-mcp--lists-to-vectors value) object))
             object)
    object)
   ((and (listp object) (not (stringp object)))
    (apply #'vector (mapcar #'emagent-mcp--lists-to-vectors object)))
   (t object)))

(defun emagent-mcp--read-json-file (file)
  "Return the parsed JSON object (hash-table) in FILE, or an empty one."
  (if (file-exists-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (json-parse-buffer :object-type 'hash-table
                               :array-type 'list
                               :null-object :null
                               :false-object :false))
        (error (make-hash-table :test 'equal)))
    (make-hash-table :test 'equal)))

(defun emagent-mcp-ensure-cursor-config ()
  "Merge an `emagent' http MCP entry into the global cursor-agent config.

The url uses ${env:EMAGENT_SESSION_TOKEN} so a single static file routes each
cursor-agent invocation to its own session.  Existing servers are preserved.
Only writes the file when the entry is absent or points to a different port."
  (let* ((port (emagent-mcp-ensure-server))
         (file emagent-mcp-cursor-config-file)
         (expected-url (format "http://127.0.0.1:%d/mcp/${env:EMAGENT_SESSION_TOKEN}" port))
         (data (emagent-mcp--read-json-file file))
         (servers (let ((value (gethash "mcpServers" data)))
                    (if (hash-table-p value) value (make-hash-table :test 'equal))))
         (current-entry (gethash emagent-mcp-server-name servers)))
    (unless (and (hash-table-p current-entry)
                 (equal (gethash "url" current-entry) expected-url))
      (let ((entry (make-hash-table :test 'equal)))
        (puthash "url" expected-url entry)
        (puthash emagent-mcp-server-name entry servers)
        (puthash "mcpServers" servers data)
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (insert (emagent-mcp--json-encode (emagent-mcp--lists-to-vectors data))))))
    file))

(defun emagent-cursor-project-slug (cwd)
  "Return Cursor's ~/.cursor/projects/ slug for absolute CWD."
  (let* ((abs (directory-file-name (expand-file-name cwd)))
         (raw (replace-regexp-in-string "\\`/" "" abs))
         (slug (replace-regexp-in-string "[^A-Za-z0-9]+" "-" raw)))
    (replace-regexp-in-string "-+" "-" slug)))

(defun emagent-cursor-mcp-approvals-file (cwd)
  "Return path to Cursor mcp-approvals.json for CWD."
  (expand-file-name
   "mcp-approvals.json"
   (expand-file-name (emagent-cursor-project-slug cwd)
                     (expand-file-name "projects"
                                       (expand-file-name ".cursor" "~")))))

(defun emagent-cursor--mcp-approval-key (name cwd url)
  "Return Cursor approval id for server NAME at CWD with http URL."
  (let* ((payload (format "{\"path\":%s,\"server\":{\"url\":%s}}"
                          (json-serialize cwd)
                          (json-serialize url)))
         (digest (substring (secure-hash 'sha256 payload) 0 16)))
    (format "%s-%s" name digest)))

(defun emagent-cursor--mcp-server-url (cfg)
  "Return http/sse URL from MCP CFG alist/hash, or nil."
  (or (map-elt cfg 'url)
      (and (hash-table-p cfg) (gethash "url" cfg))))

(defun emagent-mcp--cursor-extra-servers-p ()
  "Return non-nil when ~/.cursor/mcp.json has a non-emagent server."
  (when-let* ((file (bound-and-true-p emagent-mcp-cursor-config-file))
              ((file-readable-p file))
              (data (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'alist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :false))))
              (servers (map-elt data 'mcpServers)))
    (cl-some (lambda (pair)
               (let ((name (if (symbolp (car pair))
                               (symbol-name (car pair))
                             (format "%s" (car pair)))))
                 (not (equal name emagent-mcp-server-name))))
             servers)))

(defun emagent-cursor-write-mcp-approvals (&optional cwd)
  "Approve non-emagent http servers from ~/.cursor/mcp.json for CWD.

Writes ~/.cursor/projects/<slug>/mcp-approvals.json using Cursor's
`name-sha256prefix' key format.  `cursor-agent mcp enable' alone is not
enough: ACP only loads servers listed in that file for the session cwd.
Returns the approvals file path, or nil when there is nothing to write."
  (let* ((cwd (directory-file-name
               (expand-file-name
                (or cwd default-directory))))
         (file (bound-and-true-p emagent-mcp-cursor-config-file))
         (data (and file (file-readable-p file)
                    (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'alist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :false)))))
         (servers (map-elt data 'mcpServers))
         keys)
    (dolist (pair servers)
      (let* ((name (if (symbolp (car pair))
                       (symbol-name (car pair))
                     (format "%s" (car pair))))
             (url (emagent-cursor--mcp-server-url (cdr pair))))
        (unless (or (equal name emagent-mcp-server-name)
                    (not (stringp url))
                    (string-empty-p url))
          (push (emagent-cursor--mcp-approval-key name cwd url) keys))))
    (setq keys (nreverse (delete-dups keys)))
    (when keys
      (let ((approvals (emagent-cursor-mcp-approvals-file cwd)))
        (make-directory (file-name-directory approvals) t)
        (with-temp-file approvals
          (insert (emagent-mcp--json-encode (vconcat keys))))
        (emagent-log "wrote Cursor mcp approvals (%s): %s"
                     (length keys) approvals)
        approvals))))

(provide 'emagent-mcp-server-cursor)
;;; emagent-mcp-server-cursor.el ends here
