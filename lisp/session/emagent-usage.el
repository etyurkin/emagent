;;; emagent-usage.el --- Persist and report ACP token usage -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Append ACP usage samples to a local JSONL log and present daily/session
;; summaries (ccusage-inspired, Claude-first).
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'subr-x)

(defgroup emagent-usage nil
  "Token usage logging and reports."
  :group 'emagent)

(defcustom emagent-usage-enabled t
  "When non-nil, append ACP usage samples to `emagent-usage-file'."
  :type 'boolean
  :group 'emagent-usage)

(defcustom emagent-usage-file
  (expand-file-name "emagent/usage.jsonl"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "JSONL file where per-turn usage samples are appended."
  :type 'file
  :group 'emagent-usage)

(defcustom emagent-usage-show-mode-line t
  "When non-nil, show session token totals on the mode line when known."
  :type 'boolean
  :group 'emagent-usage)

(defun emagent-usage--ensure-dir ()
  "Create the directory for `emagent-usage-file' when needed."
  (let ((dir (file-name-directory (expand-file-name emagent-usage-file))))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun emagent-usage--iso8601-now ()
  "Return current UTC time as an ISO-8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))

(defun emagent-usage--day-key (&optional time)
  "Return YYYY-MM-DD for TIME (default now, local)."
  (format-time-string "%Y-%m-%d" (or time (current-time))))

(defun emagent-usage--json-object (alist)
  "Convert ALIST to a hash table suitable for `json-serialize'."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (let* ((k (car pair))
             (key (cond
                   ((keywordp k) (substring (symbol-name k) 1))
                   ((symbolp k) (symbol-name k))
                   (t (format "%s" k)))))
        (puthash key (cdr pair) table)))
    table))

(defun emagent-usage-record (event)
  "Append EVENT alist to `emagent-usage-file' when enabled.

EVENT keys may include day, time, provider, model, session,
total-tokens, context-used, context-size, input-tokens,
output-tokens, cache-read-tokens, cache-creation-tokens, cost-usd."
  (when (and emagent-usage-enabled event)
    (condition-case err
        (progn
          (emagent-usage--ensure-dir)
          (let* ((path (expand-file-name emagent-usage-file))
                 (json (json-serialize (emagent-usage--json-object event))))
            (with-temp-buffer
              (insert json "\n")
              (write-region (point-min) (point-max) path t 'silent))))
      (error
       (when (fboundp 'emagent-log)
         (emagent-log "usage record failed: %s"
                      (error-message-string err)))))))

(defun emagent-usage--read-events (&optional days)
  "Return usage events from the log, optionally limited to last DAYS."
  (let* ((path (expand-file-name emagent-usage-file))
         (cutoff
          (when (and (integerp days) (> days 0))
            (time-subtract (current-time) (days-to-time days))))
         (events nil))
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (string-empty-p (string-trim line))
              (condition-case nil
                  (let* ((obj (json-parse-string line :object-type 'alist
                                                 :null-object nil
                                                 :false-object nil))
                         (day (map-elt obj 'day)))
                    (when (or (null cutoff)
                              (and (stringp day)
                                   (not (time-less-p
                                         (date-to-time
                                          (concat day "T12:00:00"))
                                         cutoff))))
                      (push obj events)))
                (error nil))))
          (forward-line 1))))
    (nreverse events)))

(defun emagent-usage--sum-field (events field)
  "Sum numeric FIELD across EVENTS."
  (cl-loop for ev in events
           for v = (map-elt ev field)
           when (numberp v) sum v))

(defun emagent-usage--format-tokens (n)
  "Format token count N for display."
  (cond
   ((not (numberp n)) "n/a")
   ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
   (t (format "%d" n))))

(defun emagent-usage-report-string (&optional days)
  "Return a human-readable usage report for the last DAYS (default 7)."
  (let* ((days (or days 7))
         (events (emagent-usage--read-events days))
         (by-day (make-hash-table :test 'equal))
         (total (emagent-usage--sum-field events 'total-tokens))
         (cost (emagent-usage--sum-field events 'cost-usd)))
    (dolist (ev events)
      (let* ((day (or (map-elt ev 'day) "unknown"))
             (cell (gethash day by-day)))
        (unless cell
          (setq cell (list 0 0.0 0))
          (puthash day cell by-day))
        (setcar cell (+ (car cell) (or (map-elt ev 'total-tokens) 0)))
        (setcar (cdr cell)
                (+ (cadr cell) (or (map-elt ev 'cost-usd) 0.0)))
        (setcar (cddr cell) (1+ (caddr cell)))))
    (with-temp-buffer
      (if (null events)
          (insert "emagent: no usage samples recorded yet\n")
        (insert (format "emagent usage (last %d day(s))\n" days))
        (insert (format "events: %d  tokens: %s"
                        (length events)
                        (emagent-usage--format-tokens total)))
        (when (and (numberp cost) (> cost 0))
          (insert (format "  cost: $%.4f" cost)))
        (insert "\n\n")
        (dolist (day (sort (hash-table-keys by-day) #'string<))
          (let ((cell (gethash day by-day)))
            (insert (format "%s  tok=%s  events=%d"
                            day
                            (emagent-usage--format-tokens (car cell))
                            (caddr cell)))
            (when (> (cadr cell) 0)
              (insert (format "  $%.4f" (cadr cell))))
            (insert "\n"))))
      (insert "\n" (emagent-usage-tax-report-string) "\n")
      (when-let ((delta (emagent-usage-tax-delta-string)))
        (insert delta "\n"))
      (buffer-string))))

;;;###autoload
(defun emagent-usage (&optional days)
  "Show token usage report for the last DAYS (default 7)."
  (interactive "P")
  (let ((days (cond
               ((integerp days) days)
               ((and days (not (consp days))) 30)
               (t 7))))
    (message "%s" (string-trim (emagent-usage-report-string days)))))

(defun emagent-usage--get (usage key &rest alts)
  "Return USAGE value for KEY or one of ALTS (keyword/symbol/string)."
  (let ((keys (cons key alts)))
    (cl-loop for k in keys
             for v = (cond
                      ((hash-table-p usage) (gethash k usage))
                      (t (map-elt usage k)))
             when v return v)))

(defun emagent-usage--event-from-usage (usage &optional meta)
  "Build a log event from USAGE hash/alist and optional META alist."
  (let ((total (emagent-usage--get usage :total-tokens
                                   'totalTokens 'total-tokens))
        (used (emagent-usage--get usage :context-used
                                  'contextUsed 'context-used 'used))
        (size (emagent-usage--get usage :context-size
                                  'contextSize 'context-size 'size))
        (input (emagent-usage--get usage :input-tokens
                                   'inputTokens 'input-tokens))
        (output (emagent-usage--get usage :output-tokens
                                    'outputTokens 'output-tokens))
        (cache-read (emagent-usage--get usage :cache-read-tokens
                                        'cacheReadTokens))
        (cache-create (emagent-usage--get usage :cache-creation-tokens
                                          'cacheCreationTokens))
        (cost (emagent-usage--get usage :cost-usd 'costUSD 'cost-usd)))
    (append
     `((day . ,(emagent-usage--day-key))
       (time . ,(emagent-usage--iso8601-now)))
     (when meta meta)
     (when (numberp total) `((total-tokens . ,total)))
     (when (numberp used) `((context-used . ,used)))
     (when (numberp size) `((context-size . ,size)))
     (when (numberp input) `((input-tokens . ,input)))
     (when (numberp output) `((output-tokens . ,output)))
     (when (numberp cache-read) `((cache-read-tokens . ,cache-read)))
     (when (numberp cache-create) `((cache-creation-tokens . ,cache-create)))
     (when (numberp cost) `((cost-usd . ,cost))))))

(defun emagent-usage-record-usage (usage &optional meta)
  "Record USAGE sample with optional META when it carries token fields."
  (let ((event (emagent-usage--event-from-usage usage meta)))
    (when (or (map-elt event 'total-tokens)
              (map-elt event 'context-used)
              (map-elt event 'input-tokens)
              (map-elt event 'cost-usd))
      (emagent-usage-record event))))

(defvar emagent-usage--tax-by-session (make-hash-table :test 'equal)
  "Map session id to system-tax counter tables for concurrent chats.")

(defvar emagent-usage--baseline-by-session (make-hash-table :test 'equal)
  "Map session id to usage baseline alists.")

(defvar emagent-usage--session-key nil
  "Dynamic session id for tax lookups; mirrors age session keys.")

(defvar emagent-mcp--token)
(defvar emagent-mcp--current-session-token)
(defvar emagent-tools--chat-buffer)
(defvar emagent-tools-age--session-key)

(defun emagent-usage--empty-tax ()
  "Return a fresh zeroed tax counter table."
  (let ((h (make-hash-table :test 'eq)))
    (puthash 'system 0 h)
    (puthash 'context 0 h)
    (puthash 'notes 0 h)
    (puthash 'compressed 0 h)
    (puthash 'user 0 h)
    (puthash 'images 0 h)
    (puthash 'mcp-bytes 0 h)
    h))

(defun emagent-usage--session-id ()
  "Return the tax-ledger key for the active ACP/MCP session."
  (or emagent-usage--session-key
      (and (boundp 'emagent-tools-age--session-key)
           emagent-tools-age--session-key)
      (and (boundp 'emagent-mcp--current-session-token)
           emagent-mcp--current-session-token)
      (and (boundp 'emagent-mcp--token)
           (local-variable-p 'emagent-mcp--token (current-buffer))
           (buffer-local-value 'emagent-mcp--token (current-buffer)))
      (and (boundp 'emagent-tools--chat-buffer)
           emagent-tools--chat-buffer
           (buffer-live-p emagent-tools--chat-buffer)
           (or (buffer-local-value 'emagent-mcp--token
                                   emagent-tools--chat-buffer)
               (format "buf:%s"
                       (buffer-name emagent-tools--chat-buffer))))
      'global))

(defun emagent-usage--tax-table ()
  "Return the tax counter table for `emagent-usage--session-id'."
  (let ((id (emagent-usage--session-id)))
    (or (gethash id emagent-usage--tax-by-session)
        (let ((h (emagent-usage--empty-tax)))
          (puthash id h emagent-usage--tax-by-session)
          h))))

(defun emagent-usage-tax-reset (&optional session-id)
  "Reset system-tax counters for SESSION-ID or the active session.

When SESSION-ID is the symbol `all', clear every session (tests)."
  (cond
   ((eq session-id 'all)
    (clrhash emagent-usage--tax-by-session)
    (clrhash emagent-usage--baseline-by-session))
   (t
    (let ((id (or session-id (emagent-usage--session-id))))
      (remhash id emagent-usage--tax-by-session)
      (remhash id emagent-usage--baseline-by-session)))))

(defun emagent-usage-tax-add (kind n)
  "Add N to tax counter KIND (symbol) for the active session."
  (when (and (symbolp kind) (numberp n) (> n 0))
    (let ((tax (emagent-usage--tax-table)))
      (puthash kind (+ (or (gethash kind tax) 0) n) tax))))

(defun emagent-usage-tax-get (kind)
  "Return tax counter KIND for the active session."
  (or (gethash kind (emagent-usage--tax-table)) 0))

(defun emagent-usage-baseline-alist ()
  "Return the current usage baseline alist, or nil."
  (gethash (emagent-usage--session-id) emagent-usage--baseline-by-session))

(defun emagent-usage-baseline-set ()
  "Snapshot current tax counters as the before/after baseline."
  (let ((baseline
         (mapcar (lambda (k) (cons k (emagent-usage-tax-get k)))
                 '(context notes compressed user images mcp-bytes system))))
    (puthash (emagent-usage--session-id) baseline
             emagent-usage--baseline-by-session)
    baseline))

(defun emagent-usage-baseline-clear ()
  "Clear the before/after usage baseline for the active session."
  (remhash (emagent-usage--session-id) emagent-usage--baseline-by-session))

(defun emagent-usage--tax-delta (kind)
  "Return current minus baseline for KIND (0 baseline when unset)."
  (- (emagent-usage-tax-get kind)
     (or (alist-get kind (emagent-usage-baseline-alist)) 0)))

(defcustom emagent-usage-budget-context 1500
  "Max chars of Emacs context injected per prompt (0 = unlimited)."
  :type 'integer
  :group 'emagent-usage)

(defcustom emagent-usage-budget-notes 2000
  "Max chars of session/project notes in the system prompt (0 = unlimited)."
  :type 'integer
  :group 'emagent-usage)

(defcustom emagent-usage-budget-compressed 8000
  "Max chars of compressed prior context in session/new (0 = unlimited)."
  :type 'integer
  :group 'emagent-usage)

(defun emagent-usage--cap-string (text budget kind)
  "Return TEXT capped to BUDGET chars, accounting KIND in the tax meter."
  (let* ((text (or text ""))
         (budget (and (integerp budget) (> budget 0) budget))
         (out (if (and budget (> (length text) budget))
                  (concat (substring text 0 budget) "\n… (truncated)")
                text)))
    (emagent-usage-tax-add kind (length out))
    out))

(defun emagent-usage-tax-delta-string ()
  "Return tax delta since baseline, or nil when no baseline is set."
  (when (emagent-usage-baseline-alist)
    (format
     (concat "since baseline: ctx=%s notes=%s compressed=%s user=%s "
             "images=%s mcp=%s system=%s")
     (emagent-usage--format-tokens (emagent-usage--tax-delta 'context))
     (emagent-usage--format-tokens (emagent-usage--tax-delta 'notes))
     (emagent-usage--format-tokens (emagent-usage--tax-delta 'compressed))
     (emagent-usage--format-tokens (emagent-usage--tax-delta 'user))
     (emagent-usage--tax-delta 'images)
     (emagent-usage--format-tokens (emagent-usage--tax-delta 'mcp-bytes))
     (emagent-usage--format-tokens (emagent-usage--tax-delta 'system)))))

(defun emagent-usage-tax-report-string ()
  "Return a short system-tax breakdown for the current session."
  (format
   (concat "session tax: ctx=%s notes=%s compressed=%s user=%s "
           "images=%s mcp=%s system=%s")
   (emagent-usage--format-tokens (emagent-usage-tax-get 'context))
   (emagent-usage--format-tokens (emagent-usage-tax-get 'notes))
   (emagent-usage--format-tokens (emagent-usage-tax-get 'compressed))
   (emagent-usage--format-tokens (emagent-usage-tax-get 'user))
   (emagent-usage-tax-get 'images)
   (emagent-usage--format-tokens (emagent-usage-tax-get 'mcp-bytes))
   (emagent-usage--format-tokens (emagent-usage-tax-get 'system))))

(provide 'emagent-usage)

;;; emagent-usage.el ends here
