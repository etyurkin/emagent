;;; emagent-chat-header.el --- Buffer metadata header R/W for emagent  -*- lexical-binding: t; -*-

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

;; Chat-facing header helpers: displaying paths relative to the session
;; project, and resolving the ACP working directory.  Reading and writing
;; the underlying #+EMAGENT_* org properties is owned by
;; `emagent-session-store' (below the chat UI); this module consumes those
;; readers rather than duplicating them.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-model)
(require 'emagent-session-store)
(require 'project)

(defun emagent-chat--display-path (path &optional project-dir)
  "Return PATH formatted for display in the chat UI.
Under the session project root: ./projectname/relative/path.
Under user home but outside the project: ~/….
Otherwise: the absolute PATH.

Relative paths resolve against the project directory, not
`default-directory' — saving the session file moves `default-directory'
to the session file's directory, which is unrelated to the project.

Arguments: PROJECT-DIR."
  (let* ((project (when-let ((raw (or project-dir
                                      (and (boundp 'emagent-chat-project-directory)
                                           emagent-chat-project-directory)
                                      (emagent-session-store-read-project-property))))
                    (file-truename
                     (file-name-as-directory (expand-file-name raw)))))
         (expanded (file-truename (expand-file-name path project)))
         (home (file-truename (expand-file-name "~")))
         (home-prefix (concat home "/")))
    (cond
     ((and project
           (string-prefix-p project expanded)
           (not (string= expanded (directory-file-name project))))
      (concat "./"
              (file-name-nondirectory (directory-file-name project))
              "/"
              (file-relative-name expanded project)))
     ((string-prefix-p home-prefix expanded)
      (abbreviate-file-name expanded))
     ((string= expanded home)
      "~")
     (t expanded))))

(defun emagent-chat--session-directory ()
  "Return the ACP working directory for the current emagent buffer.
Reads #+EMAGENT_PROJECT from the buffer header if set, falling back to
variable `buffer-file-name', `project-current' or `user-emacs-directory'."
  (expand-file-name
   (or (emagent-session-store-read-project-property)
       (and buffer-file-name (file-name-directory buffer-file-name))
       (if (boundp 'emagent-default-directory) emagent-default-directory)
       (and (fboundp 'project-current)
            (when-let ((proj (project-current nil default-directory)))
              (project-root proj)))
       user-emacs-directory)))

(provide 'emagent-chat-header)
;;; emagent-chat-header.el ends here
