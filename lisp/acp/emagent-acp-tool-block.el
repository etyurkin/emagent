;;; emagent-acp-tool-block.el --- ACP tool-call shell and CLI block specs  -*- lexical-binding: t; -*-

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

;; Build Org src-block specs for shell, CLI, eval, and text tool calls.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'emagent-acp-tool-parse)
(require 'emagent-acp-tool-edit)

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

(provide 'emagent-acp-tool-block)
;;; emagent-acp-tool-block.el ends here
