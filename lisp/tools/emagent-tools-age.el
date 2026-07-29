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

(defvar emagent-tools-age--ledger (make-hash-table :test 'equal)
  "Map of age keys to plists (:bytes :hash :count).")

(defvar emagent-tools-age--ticks (make-hash-table :test 'equal)
  "Map of path|args keys to last delivered emagent-tick.")

(defvar emagent-tools-age--bytes 0
  "Cumulative tool payload bytes for the current ACP session.")

(defvar emagent-tools-age--bytes-hint-pending nil
  "Non-nil when cumulative bytes crossed the hint threshold.")

(defvar emagent-tools-age--outlined (make-hash-table :test 'equal)
  "Set of resolved paths that already received an outline this session.")

(defun emagent-tools-age-reset ()
  "Clear aging ledger and byte counters for a new/compressed session."
  (clrhash emagent-tools-age--ledger)
  (clrhash emagent-tools-age--ticks)
  (clrhash emagent-tools-age--outlined)
  (setq emagent-tools-age--bytes 0
        emagent-tools-age--bytes-hint-pending nil))

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
    (puthash (expand-file-name path) t emagent-tools-age--outlined)))

(defun emagent-tools-age-outlined-p (path)
  "Return non-nil when PATH was outlined earlier in this session."
  (and path (gethash (expand-file-name path) emagent-tools-age--outlined)))

(defun emagent-tools-age--account-bytes (nbytes)
  "Add NBYTES to the session total and maybe arm the compact hint."
  (setq emagent-tools-age--bytes (+ emagent-tools-age--bytes nbytes))
  (when (and (integerp emagent-tools-age-bytes-threshold)
             (> emagent-tools-age-bytes-threshold 0)
             (>= emagent-tools-age--bytes emagent-tools-age-bytes-threshold))
    (setq emagent-tools-age--bytes-hint-pending t))
  (when (fboundp 'emagent-usage-tax-add)
    (emagent-usage-tax-add 'mcp-bytes nbytes)))

(defun emagent-tools-age-tick-note (path args tick text &optional refresh)
  "Return TEXT, or an unchanged stub when TICK was already delivered.

PATH/ARGS identify the read window.  REFRESH forces a full body."
  (let* ((text (or text ""))
         (tick (and (stringp tick) (not (string-empty-p tick)) tick))
         (key (emagent-tools-age--tick-key path args)))
    (cond
     ((or (not emagent-tools-age-tick-cache) (not tick))
      text)
     (refresh
      (puthash key tick emagent-tools-age--ticks)
      text)
     ((equal tick (gethash key emagent-tools-age--ticks))
      (format
       "[unchanged since tick %s — pass refresh=1 to re-fetch]"
       tick))
     (t
      (puthash key tick emagent-tools-age--ticks)
      text))))

(defun emagent-tools-age-note (tool path args text &optional refresh)
  "Record TEXT for TOOL/PATH/ARGS and maybe return an aged stub.

When REFRESH is non-nil, always return TEXT and refresh the ledger entry.
Otherwise, a repeat of the same hash for a large payload returns a stub."
  (let* ((text (or text ""))
         (nbytes (string-bytes text)))
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
                     emagent-tools-age--ledger))
          text)
      (let* ((key (emagent-tools-age--key tool path args))
             (hash (secure-hash 'sha1 text))
             (prev (gethash key emagent-tools-age--ledger)))
        (if (and prev (equal hash (plist-get prev :hash)))
            (progn
              (puthash key
                       (list :bytes nbytes
                             :hash hash
                             :count (1+ (or (plist-get prev :count) 1)))
                       emagent-tools-age--ledger)
              (format
               "[aged: %s %s — %d chars earlier; pass refresh=1 or change args to re-fetch]"
               tool
               (if (and path (not (string-empty-p path))) path "")
               nbytes))
          (puthash key (list :bytes nbytes :hash hash :count 1)
                   emagent-tools-age--ledger)
          text)))))

(defun emagent-tools-age-bytes ()
  "Return cumulative MCP tool payload bytes for this session."
  emagent-tools-age--bytes)

(defun emagent-tools-age-bytes-hint-p ()
  "Return non-nil when cumulative MCP bytes request a compact hint."
  emagent-tools-age--bytes-hint-pending)

(defun emagent-tools-age-clear-bytes-hint ()
  "Clear the pending byte-based compact hint flag."
  (setq emagent-tools-age--bytes-hint-pending nil))

(provide 'emagent-tools-age)

;;; emagent-tools-age.el ends here
