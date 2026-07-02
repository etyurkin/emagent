;;; emagent-acp-tool-call.el --- Tool call detail extraction  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tool call detail extraction, normalization, display helpers, and
;; display filtering. Pure data processing with no session mutation.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-provider)

(declare-function emagent-chat-show-tool-call "emagent-chat")
(declare-function emagent-acp--permission-decision-label "emagent-acp-permit")
(declare-function emagent-acp--tool-call-eval-form "emagent-acp-permit")

(defun emagent-acp--tool-call-emagent-tool-p (update)
  "Return non-nil when UPDATE names a tool from emagent's own MCP server.

Such tools run inside Emacs and never reach the ACP permission path, so their
Thinking lines are tagged (Emacs) instead of carrying an (Allow) decision.

Detection relies on the emagent MCP namespace (e.g. `mcp_emagent_read_file'):
an explicit `emagent-tool' flag set by provider enrichment, or the word
`emagent' surviving in the title.  Bare tool names are deliberately not matched
because generic names like `grep' collide with agent-native tools."
  (or (map-elt update 'emagent-tool)
      (let ((title (downcase (string-trim (or (map-elt update 'title) "")))))
        (and (not (string-empty-p title))
             (string-match-p "\\bemagent\\b" title)
             t))))

(defun emagent-acp--tool-call-elisp-prin1-p (value)
  "Return non-nil when VALUE looks like a printed Elisp object."
  (and (stringp value)
       (string-match-p "\\`#s(" (string-trim value))))

(defun emagent-acp--tool-call-prin1-hash-detail (raw)
  "Extract a display string from a printed hash-table RAW, or nil."
  (when (emagent-acp--tool-call-elisp-prin1-p raw)
    (cl-loop for key in '(command path file pattern form directory args)
             when (string-match (format "(%s \\([^)]*\\))" key) raw)
             return (string-trim (match-string 1 raw)))))

(defun emagent-acp--tool-call-raw-input-empty-p (raw)
  "Return non-nil when tool-call rawInput carries no usable parameters."
  (or (null raw)
      (emagent-acp--tool-call-elisp-prin1-p raw)
      (and (stringp raw)
           (let ((trimmed (string-trim raw)))
             (or (string-empty-p trimmed)
                 (member trimmed '("{}" "[]" "null")))))
      (and (listp raw) (null raw))
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
                               (cons 'status v)))))))

(defun emagent-acp--ingest-tool-call-request (state tool-call)
  "Merge TOOL-CALL from session/request_permission and refresh display."
  (when-let ((update (emagent-acp--tool-call-update-from-request tool-call)))
    (emagent-acp--on-tool-call state update)))

(defun emagent-acp--emit-tool-call-display (state id kind merged label status)
  "Push TOOL-CALL LABEL to the chat buffer and update session UI."
  (let* ((labels (map-elt state :tool-call-labels))
         (prev (and id labels (gethash id labels)))
         (decision (and id (when-let ((d (map-elt state :tool-call-decisions)))
                             (gethash id d))))
         (display (cond
                   ((or (null label) (string-empty-p label)) label)
                   (decision (emagent-acp--permission-decision-label label decision))
                   ((emagent-acp--tool-call-emagent-tool-p merged)
                    (format "%s (Emacs)" label))
                   (t label)))
         (completed (member status '("completed" "failed")))
         (label-changed (and display (not (string-empty-p display))
                             (or (null prev) (not (string= prev display))))))
    (when label
      (emagent-acp--detect-external-refusal-in-text state label))
    (when label-changed
      (when id (puthash id display labels))
      (unless completed
        (emagent-acp--notify-user state (format "emagent: tool %s" label)))
      (when-let ((buf (emagent-acp--chat-buffer state)))
        (let ((spec (emagent-acp--tool-call-block-spec merged)))
          (with-current-buffer buf
            (emagent-chat-show-tool-call id display (car spec) (cdr spec))))))
    (if completed
        (progn
          (map-put! state :current-tool nil)
          (map-put! state :current-tool-kind nil))
      (when label-changed
        (map-put! state :current-tool label)
        (when kind (map-put! state :current-tool-kind kind))
        (emagent-acp--schedule-prompt-watchdog state)))
    (when (or label-changed completed)
      (emagent-acp--refresh-mode-line state))))

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
                               relativeWorkspacePath url query q search input
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
                                   search input text pattern glob form
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

(defconst emagent-acp--shell-tool-names
  '("grep" "rg" "ripgrep" "ag" "cat" "ls" "find" "fd" "sed" "awk"
    "head" "tail")
  "CLI utility names that, when used as a structured tool title, render as a
reconstructed `sh' command (e.g. grep PATTERN).  Kept deliberately narrow so
structured file read/write/search tools are never mistaken for shell commands.")

(defun emagent-acp--tool-call-cli-tool (update)
  "Return the CLI command word (grep, cat, ...) named by UPDATE's title, or nil.
Only a curated set of shell utilities is recognized, so structured
read/write/search tools are not treated as shell commands."
  (let* ((title (downcase (string-trim (or (map-elt update 'title) ""))))
         (title (replace-regexp-in-string
                 "\\`\\(?:mcp_[^_]+_\\|mcp:? *\\|emagent-\\)" "" title))
         (word (car (split-string title "[^a-z0-9.+-]+" t))))
    (and word (member word emagent-acp--shell-tool-names) word)))


(defun emagent-acp--tool-call-block-spec (update)
  "Return (LANG . CODE) when UPDATE should render as an Org src block, else nil.
Explicit shell commands render as `sh' and eval forms as `elisp' (their detail
is real code).  A structured CLI tool (grep, cat, ...) renders as `sh' with its
detail reconstructed into a command line (e.g. grep PATTERN).  Any other tool
renders as a block when its detail spans multiple lines or exceeds
`emagent-acp--tool-call-detail-limit' characters; shorter single-line
details such as file paths stay as compact arrow lines."
  (let* ((command (emagent-acp--tool-call-shell-command update))
         (form (unless command (emagent-acp--tool-call-eval-form update)))
         (detail (unless (or command form)
                   (emagent-acp--tool-call-detail update)))
         (cli (and detail (emagent-acp--tool-call-cli-tool update))))
    (cond
     (command (cons "sh" command))
     (form (cons "elisp" form))
     (cli (cons "sh" (format "%s %s" cli detail)))
     ((and detail (or (string-match-p "\n" detail)
                      (> (length detail) emagent-acp--tool-call-detail-limit)))
      (cons "text" detail))
     (t nil))))

(defconst emagent-acp--tool-call-weak-details
  '("tool" "Tool" "running" "pending")
  "ACP tool-call detail strings too generic to display without store.db lookup.")

(defun emagent-acp--tool-call-meaningful-detail-p (update)
  "Return non-nil when UPDATE has useful path, command, or similar detail."
  (when-let ((detail (emagent-acp--tool-call-detail update)))
    (let ((trimmed (string-trim detail)))
      (and (not (string-empty-p trimmed))
           (not (member trimmed emagent-acp--tool-call-weak-details))))))

(defun emagent-acp--tool-call-generic-title-p (state title)
  "Return non-nil when TITLE is too generic to show without detail."
  (emagent-acp--provider-generic-title-p state title))

(defun emagent-acp--tool-call-redundant-detail-p (title detail)
  "Return non-nil when DETAIL adds nothing beyond generic TITLE."
  (when (and (stringp title) (stringp detail))
    (let* ((t0 (downcase (string-trim title)))
           (d0 (downcase (string-trim detail)))
           (t1 (replace-regexp-in-string "^emagent-" "" t0))
           (t2 (replace-regexp-in-string "^mcp_" "" t1)))
      (or (string= t0 d0)
          (string= t1 d0)
          (string= t2 d0)
          (and (string-match-p ":" t0)
               (string= (car (split-string t0 ":")) d0))))))

(defun emagent-acp--tool-call-displayable-p (state update)
  "Return non-nil when UPDATE should appear in the Thinking block."
  (let* ((title (string-trim (or (map-elt update 'title) "")))
         (detail (emagent-acp--tool-call-detail update)))
    (cond
     ((and detail
           (emagent-acp--tool-call-meaningful-detail-p update)
           (not (emagent-acp--tool-call-redundant-detail-p title detail)))
      t)
     ((and (not (string-empty-p title))
           (not (emagent-acp--tool-call-generic-title-p state title))
           (or (null detail) (string-empty-p detail)))
      t)
     (t nil))))

(defun emagent-acp--tool-call-label (update)
  "Return a display label for ACP tool-call UPDATE."
  (let* ((title (string-trim (or (map-elt update 'title) "tool")))
         (title (if (string-match-p "\\`MCP:? *tool\\'" title) "MCP" title))
         (detail (emagent-acp--tool-call-detail update)))
    (cond
     ((and detail (not (string-empty-p detail))
           (not (string-match-p (regexp-quote detail) title)))
      (format "%s: %s" title (emagent-acp--tool-call-truncate detail)))
     ((and detail (not (string-empty-p detail))) detail)
     (t title))))

(defun emagent-acp--merged-tool-call-update (state update)
  "Return UPDATE merged with stored title/rawInput for STATE."
  (let* ((id (map-elt update 'toolCallId))
         (titles (map-elt state :tool-call-titles))
         (inputs (map-elt state :tool-call-inputs))
         (stored-title (and id titles (gethash id titles)))
         (stored-input (and id inputs (gethash id inputs)))
         (title (or (map-elt update 'title) stored-title))
         (raw-input (or (map-elt update 'rawInput)
                        (map-elt update 'arguments)
                        stored-input))
         (merged update))
    (when (and id title)
      (puthash id title titles))
    (when (and id raw-input)
      (puthash id raw-input inputs))
    (when title
      (setq merged (emagent-acp--update-put merged 'title title)))
    (when (and raw-input (not (emagent-acp--tool-call-raw-input-empty-p raw-input)))
      (setq merged (emagent-acp--update-put merged 'rawInput raw-input)))
    (when (and id (map-elt merged 'rawInput))
      (puthash id (map-elt merged 'rawInput) inputs))
    merged))

(defun emagent-acp--on-tool-call (state update)
  "Display or refresh a tool-call line from ACP UPDATE."
  (unless (map-elt state :replaying-history)
    (let* ((update (emagent-acp--provider-enrich-tool-call state update))
           (id (map-elt update 'toolCallId))
           (status (map-elt update 'status))
           (kind (map-elt update 'kind))
           (merged (emagent-acp--merged-tool-call-update state update))
           (label (emagent-acp--tool-call-label merged))
           (pending-table (map-elt state :tool-call-pending))
           (defer (emagent-acp--provider-defer-tool-call-p state merged))
           (show (and label (not (string-empty-p label)) (not defer)
                        (emagent-acp--tool-call-displayable-p state merged))))
      (when defer
        (puthash id merged pending-table)
        (emagent-acp--provider-enqueue-tool-resolve state id))
      (when show
        (emagent-acp--emit-tool-call-display state id kind merged label status)
        (when id (remhash id pending-table)))
      (when (map-elt state :permission-queue)
        (emagent-acp--drain-permission-queue state)))))

(provide 'emagent-acp-tool-call)
;;; emagent-acp-tool-call.el ends here
