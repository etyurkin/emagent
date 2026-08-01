;;; emagent-tools-age.el --- Age/stub repeated MCP tool payloads -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Track MCP tool returns per ACP session.  Identical large repeats return a
;; short stub.  File reads at an already-delivered emagent-tick return
;; "unchanged since tick N" across turns.  Cumulative bytes can trigger the
;; chat /compact hint (cooldown-gated in chat UI).
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup emagent-tools-age nil
  "Age and stub repeated tool outputs."
  :group 'emagent-tools)

(defcustom emagent-tools-age t
  "When non-nil, stub repeated identical large tool results."
  :type 'boolean
  :group 'emagent-tools-age)

(defcustom emagent-tools-age-tick-cache t
  "When non-nil, stub file reads already delivered at the same tick."
  :type 'boolean
  :group 'emagent-tools-age)

(defcustom emagent-tools-age-min-bytes 4000
  "Minimum payload size before a result is eligible for aging stubs."
  :type 'integer
  :group 'emagent-tools-age)

(defcustom emagent-tools-age-bytes-threshold 200000
  "Cumulative MCP payload bytes that request a /compact hint.

Nil or 0 disables the byte-based hint signal."
  :type '(choice (const :tag "Off" nil)
                 (integer :tag "Bytes"))
  :group 'emagent-tools-age)

(defvar emagent-tools-age--by-session (make-hash-table :test 'equal)
  "Map session id to age state plists for concurrent MCP agents.")

(defvar emagent-tools-age--session-key nil
  "Dynamic session id for age ledger lookups; set by MCP tool context.")

(defvar emagent-mcp--current-session-token)
(defvar emagent-tools--chat-buffer)
(defvar emagent-mcp--token)

(defun emagent-tools-age--session-id ()
  "Return the age-ledger key for the active MCP/ACP session.

Prefers an explicit MCP tool binding, then the current chat buffer's
`emagent-mcp--token' (mode-line / compact hints), then a chat-buffer
fallback from tool context."
  (or emagent-tools-age--session-key
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

(defun emagent-tools-age--state ()
  "Return the age state plist for `emagent-tools-age--session-id'."
  (let ((id (emagent-tools-age--session-id)))
    (or (gethash id emagent-tools-age--by-session)
        (let ((state (list :ledger (make-hash-table :test 'equal)
                           :ticks (make-hash-table :test 'equal)
                           :outlined (make-hash-table :test 'equal)
                           :bytes 0
                           :hint nil)))
          (puthash id state emagent-tools-age--by-session)
          state))))

(defun emagent-tools-age-reset (&optional session-id)
  "Clear aging ledger for SESSION-ID or the active session.

When SESSION-ID is the symbol `all', clear every session (tests).
With no argument, clear only the active session so concurrent MCP
agents keep their own ledgers across another chat's /compact."
  (cond
   ((eq session-id 'all)
    (clrhash emagent-tools-age--by-session))
   (t
    (remhash (or session-id (emagent-tools-age--session-id))
             emagent-tools-age--by-session))))

(defun emagent-tools-age--key (tool path args)
  "Return a ledger key for TOOL PATH ARGS."
  (format "%s|%s|%s"
          (or tool "?")
          (or path "")
          (or args "")))

(defun emagent-tools-age--tick-key (path args)
  "Return a tick-cache key for PATH ARGS."
  (format "%s|%s" (or path "") (or args "")))

(defun emagent-tools-age-mark-outlined (path)
  "Record that PATH already received an outline this session."
  (when (and path (not (string-empty-p path)))
    (puthash (expand-file-name path) t
             (plist-get (emagent-tools-age--state) :outlined))))

(defun emagent-tools-age-outlined-p (path)
  "Return non-nil when PATH was outlined earlier in this session."
  (and path
       (gethash (expand-file-name path)
                (plist-get (emagent-tools-age--state) :outlined))))

(defun emagent-tools-age--account-bytes (nbytes)
  "Add NBYTES to the session total and maybe arm the compact hint."
  (let* ((state (emagent-tools-age--state))
         (total (+ (or (plist-get state :bytes) 0) nbytes)))
    (plist-put state :bytes total)
    (when (and (integerp emagent-tools-age-bytes-threshold)
               (> emagent-tools-age-bytes-threshold 0)
               (>= total emagent-tools-age-bytes-threshold))
      (plist-put state :hint t))
    (when (fboundp 'emagent-usage-tax-add)
      (emagent-usage-tax-add 'mcp-bytes nbytes))))

(defun emagent-tools-age-tick-note (path args tick text &optional refresh)
  "Return TEXT, or an unchanged stub when TICK was already delivered.

PATH/ARGS identify the read window.  REFRESH forces a full body."
  (let* ((text (or text ""))
         (tick (and (stringp tick) (not (string-empty-p tick)) tick))
         (key (emagent-tools-age--tick-key path args))
         (ticks (plist-get (emagent-tools-age--state) :ticks)))
    (cond
     ((or (not emagent-tools-age-tick-cache) (not tick))
      text)
     (refresh
      (puthash key tick ticks)
      text)
     ((equal tick (gethash key ticks))
      (format
       "[unchanged since tick %s — pass refresh=1 to re-fetch]"
       tick))
     (t
      (puthash key tick ticks)
      text))))

(defun emagent-tools-age-note (tool path args text &optional refresh)
  "Record TEXT for TOOL/PATH/ARGS and maybe return an aged stub.

When REFRESH is non-nil, always return TEXT and refresh the ledger entry.
Otherwise, a repeat of the same hash for a large payload returns a stub."
  (let* ((text (or text ""))
         (nbytes (string-bytes text))
         (ledger (plist-get (emagent-tools-age--state) :ledger)))
    (emagent-tools-age--account-bytes nbytes)
    (if (or (not emagent-tools-age)
            refresh
            (< nbytes emagent-tools-age-min-bytes))
        (progn
          (when (and emagent-tools-age (>= nbytes emagent-tools-age-min-bytes))
            (puthash (emagent-tools-age--key tool path args)
                     (list :bytes nbytes
                           :hash (secure-hash 'sha1 text)
                           :count 1)
                     ledger))
          text)
      (let* ((key (emagent-tools-age--key tool path args))
             (hash (secure-hash 'sha1 text))
             (prev (gethash key ledger)))
        (if (and prev (equal hash (plist-get prev :hash)))
            (progn
              (puthash key
                       (list :bytes nbytes
                             :hash hash
                             :count (1+ (or (plist-get prev :count) 1)))
                       ledger)
              (format
               (concat "[aged: %s %s — %d chars earlier; pass refresh=1 "
                       "or change args to re-fetch]")
               tool
               (if (and path (not (string-empty-p path))) path "")
               nbytes))
          (puthash key (list :bytes nbytes :hash hash :count 1) ledger)
          text)))))

(defun emagent-tools-age-bytes ()
  "Return cumulative MCP tool payload bytes for this session."
  (or (plist-get (emagent-tools-age--state) :bytes) 0))

(defun emagent-tools-age-bytes-hint-p ()
  "Return non-nil when cumulative MCP bytes request a compact hint."
  (plist-get (emagent-tools-age--state) :hint))

(defun emagent-tools-age-clear-bytes-hint ()
  "Clear the pending byte-based compact hint flag."
  (plist-put (emagent-tools-age--state) :hint nil))

(provide 'emagent-tools-age)

;;; emagent-tools-age.el ends here
