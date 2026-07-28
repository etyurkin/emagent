;;; emagent-tools-compact.el --- Compact tool outputs for fewer tokens -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; RTK-style post-processors for agent-facing tool results: strip ANSI,
;; collapse blanks, head+tail budgets, failures-focused test output, and
;; tighter git/list/grep/read payloads.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup emagent-tools-compact nil
  "Compact tool outputs before they reach the agent."
  :group 'emagent-tools)

(defcustom emagent-tools-compact t
  "When non-nil, compact tool outputs returned to the agent."
  :type 'boolean
  :group 'emagent-tools-compact)

(defcustom emagent-tools-compact-budget 24000
  "Default max characters kept after compacting tool output."
  :type 'integer
  :group 'emagent-tools-compact)

(defcustom emagent-tools-compact-diff-budget 32000
  "Max characters kept for git diff output."
  :type 'integer
  :group 'emagent-tools-compact)

(defcustom emagent-tools-compact-read-max-lines 200
  "Default max lines for fs op=read when LIMIT is omitted."
  :type 'integer
  :group 'emagent-tools-compact)

(defcustom emagent-tools-compact-read-hard-max-lines 2000
  "Refuse unbounded fs op=read when the file has more than this many lines."
  :type 'integer
  :group 'emagent-tools-compact)

(defcustom emagent-tools-compact-list-max-lines 500
  "Max lines kept from list_files / find_files results."
  :type 'integer
  :group 'emagent-tools-compact)

(defcustom emagent-tools-compact-grep-line-max 200
  "Max characters kept per grep match line."
  :type 'integer
  :group 'emagent-tools-compact)

(defconst emagent-tools-compact--ansi-re
  (rx (or
       (seq "\e" "[" (* (any "0-9" ";")) (any "A-Za-z"))
       (seq "\e" "(" (any "AB0-2"))
       (seq "\r")))
  "Regexp matching common ANSI/control sequences.")

(defconst emagent-tools-compact--test-cmd-re
  (rx bos (* space)
      (or "./mvnw" "mvn" "./gradlew" "gradle" "make" "cmake"
          "pytest" "python" "python3" "cargo" "go"
          "npm" "pnpm" "yarn" "bun" "npx" "vitest" "jest"
          "ctest" "rake" "tox" "nose2" "rspec"))
  "Heuristic for shell commands that produce verbose build/test output.")

(defconst emagent-tools-compact--fail-line-re
  (rx (or "FAIL" "FAILED" "ERROR" "Error:" "error:" "panic:"
          "Assertion" "assert " "✗" "✘" "✖" "× " "not ok "
          "Traceback" "FAILED " "failures:" "failed "
          "BUILD FAILURE" "[ERROR]" "Tests run:"
          "Caused by:" (seq bos (* space) "at ")))
  "Lines kept when compacting failing build/test output.")

(defconst emagent-tools-compact--success-re
  (rx (or "BUILD SUCCESS"
          (seq "Tests run:" (* not-newline) "Failures: 0")
          (seq (or "passed" "PASSED") (* space) "in")
          (seq bos (* space) (or "OK" "ok") eos)
          (seq (one-or-more digit) (* space) "passed")
          "test result: ok"))
  "Lines/phrases that indicate a successful build or test run.")

(defun emagent-tools-compact--enabled-p ()
  "Return non-nil when compacting is active."
  emagent-tools-compact)

(defun emagent-tools-compact--strip-ansi (text)
  "Remove ANSI escape sequences and CRs from TEXT."
  (let ((out (replace-regexp-in-string
              emagent-tools-compact--ansi-re "" (or text ""))))
    (replace-regexp-in-string "\r" "" out)))

(defun emagent-tools-compact--collapse-blanks (text)
  "Collapse consecutive blank lines in TEXT to one blank line."
  (replace-regexp-in-string "\n\\{3,\\}" "\n\n" (or text "")))

(defun emagent-tools-compact--head-tail (text budget &optional head-ratio)
  "Keep HEAD+TAIL of TEXT within BUDGET characters.

HEAD-RATIO (default 0.7) is the fraction reserved for the head."
  (let* ((text (or text ""))
         (budget (max 200 (or budget emagent-tools-compact-budget)))
         (ratio (or head-ratio 0.7)))
    (if (<= (length text) budget)
        text
      (let* ((marker "\n… (output compacted; middle omitted) …\n")
             (avail (max 100 (- budget (length marker))))
             (head-n (max 50 (floor (* avail ratio))))
             (tail-n (max 50 (- avail head-n)))
             (head (substring text 0 (min head-n (length text))))
             (tail (substring text (max 0 (- (length text) tail-n)))))
        (concat head marker tail)))))

(defun emagent-tools-compact--budgeted (text &optional budget)
  "Strip ANSI, collapse blanks, and head+tail TEXT to BUDGET."
  (emagent-tools-compact--head-tail
   (emagent-tools-compact--collapse-blanks
    (emagent-tools-compact--strip-ansi text))
   (or budget emagent-tools-compact-budget)))

(defun emagent-tools-compact-git-status (text)
  "Compact git status TEXT for the agent."
  (if (not (emagent-tools-compact--enabled-p))
      text
    (let* ((clean (string-trim
                   (emagent-tools-compact--strip-ansi (or text ""))))
           (lines (and (not (string-empty-p clean))
                       (split-string clean "\n" t)))
           (max-lines 120))
      (cond
       ((null lines) "")
       ((<= (length lines) max-lines)
        (emagent-tools-compact--budgeted clean))
       (t
        (let* ((head (cl-subseq lines 0 max-lines))
               (rest (- (length lines) max-lines))
               (body (string-join head "\n")))
          (format "%s\n… (%d more lines)" body rest)))))))

(defun emagent-tools-compact-git-diff (text)
  "Compact git diff TEXT: drop index noise, then budget."
  (if (not (emagent-tools-compact--enabled-p))
      text
    (let* ((clean (emagent-tools-compact--strip-ansi (or text "")))
           (lines (split-string clean "\n"))
           (kept nil))
      (dolist (line lines)
        (unless (or (string-prefix-p "index " line)
                    (string-prefix-p "old mode " line)
                    (string-prefix-p "new mode " line)
                    (string-match-p "\\`similarity index " line))
          (push line kept)))
      (emagent-tools-compact--budgeted
       (string-join (nreverse kept) "\n")
       emagent-tools-compact-diff-budget))))

(defun emagent-tools-compact-git-log (text)
  "Compact git log TEXT to a budgeted oneline-friendly payload."
  (if (not (emagent-tools-compact--enabled-p))
      text
    (emagent-tools-compact--budgeted
     (string-trim (or text ""))
     (min emagent-tools-compact-budget 12000))))

(defun emagent-tools-compact--test-command-p (command)
  "Return non-nil when COMMAND resembles a test/build-check invocation."
  (and (stringp command)
       (string-match-p emagent-tools-compact--test-cmd-re command)))

(defun emagent-tools-compact--failures-focused (text)
  "Keep failure/summary lines from TEXT; note how many were dropped."
  (let* ((lines (split-string (or text "") "\n"))
         (kept nil)
         (dropped 0)
         (summary-re
          (rx (or (seq bos (* space) (or "Ran " "FAILED" "SUCCESS" "ERROR"))
                  "failures="
                  "errors="
                  (seq (or " passed" " failed"))
                  (seq bos (* space) (or "===" "---"))))))
    (dolist (line lines)
      (cond
       ((string-match-p emagent-tools-compact--fail-line-re line)
        (push line kept))
       ((string-match-p summary-re line)
        (push line kept))
       ((string-empty-p (string-trim line))
        nil)
       (t (setq dropped (1+ dropped)))))
    (let ((body (string-join (nreverse kept) "\n")))
      (if (zerop dropped)
          body
        (format "%s\n… (%d non-failure lines omitted)" body dropped)))))

(defun emagent-tools-compact-shell (text &optional command is-error)
  "Compact shell TEXT for COMMAND.

When IS-ERROR is nil and COMMAND looks like a build/test that succeeded,
return a one-line OK summary.  On failure (or IS-ERROR), keep failure lines."
  (if (not (emagent-tools-compact--enabled-p))
      text
    (let* ((clean (emagent-tools-compact--collapse-blanks
                   (emagent-tools-compact--strip-ansi (or text ""))))
           (build-p (emagent-tools-compact--test-command-p command))
           (success-p
            (and build-p
                 (not is-error)
                 (or (string-match-p emagent-tools-compact--success-re clean)
                     (and (not (string-match-p
                               emagent-tools-compact--fail-line-re clean))
                          (not (string-match-p "BUILD FAILURE" clean)))))))
      (cond
       (success-p
        (let ((n (length (split-string clean "\n"))))
          (format "OK (compacted): %s, ~%d lines omitted"
                  (or (and command (car (split-string command))) "build")
                  (max 0 (1- n)))))
       ((or is-error build-p)
        (emagent-tools-compact--budgeted
         (emagent-tools-compact--failures-focused clean)))
       (t (emagent-tools-compact--budgeted clean))))))

(defun emagent-tools-compact-compile (text &optional command is-error)
  "Compact compile TEXT for COMMAND.

IS-ERROR should reflect the process exit status when known."
  (emagent-tools-compact-shell text (or command "compile") is-error))

(defun emagent-tools-compact-file-list (text)
  "Compact list/find TEXT to `emagent-tools-compact-list-max-lines'."
  (if (not (emagent-tools-compact--enabled-p))
      text
    (let* ((clean (string-trim-right (or text "")))
           (lines (and (not (string-empty-p clean))
                       (split-string clean "\n")))
           (max emagent-tools-compact-list-max-lines))
      (cond
       ((null lines) "")
       ((<= (length lines) max) clean)
       (t
        (format "%s\n… (%d more files)"
                (string-join (cl-subseq lines 0 max) "\n")
                (- (length lines) max)))))))

(defcustom emagent-tools-compact-grep-max-per-file 3
  "Max match lines kept per file when folding grep results."
  :type 'integer
  :group 'emagent-tools-compact)

(defun emagent-tools-compact-grep (text)
  "Fold grep TEXT by file and truncate long match lines.

Groups `path:line:…' hits under each path, keeps at most
`emagent-tools-compact-grep-max-per-file' lines per file, and
truncates each line to `emagent-tools-compact-grep-line-max'."
  (if (not (emagent-tools-compact--enabled-p))
      text
    (let* ((max-line emagent-tools-compact-grep-line-max)
           (max-per emagent-tools-compact-grep-max-per-file)
           (by-file (make-hash-table :test 'equal))
           (order nil)
           (out nil))
      (dolist (line (split-string (or text "") "\n" t))
        (let* ((trunc (if (<= (length line) max-line)
                          line
                        (concat (substring line 0 max-line) "…")))
               (path (if (string-match "\\`\\([^:\n]+\\):[0-9]" trunc)
                         (match-string 1 trunc)
                       ""))
               (bucket (or (gethash path by-file)
                           (progn
                             (push path order)
                             (let ((v (list 0 nil)))
                               (puthash path v by-file)
                               v)))))
          (setcar bucket (1+ (car bucket)))
          (when (< (length (cadr bucket)) max-per)
            (setcar (cdr bucket) (append (cadr bucket) (list trunc))))))
      (dolist (path (nreverse order))
        (let* ((bucket (gethash path by-file))
               (total (car bucket))
               (kept (cadr bucket)))
          (setq out (append out kept))
          (when (and (not (string-empty-p path)) (> total max-per))
            (setq out (append out
                              (list (format "… %s: %d more hits"
                                            path (- total max-per))))))))
      (string-join out "\n"))))

(defun emagent-tools-compact-read (text &optional limit-provided)
  "Compact read TEXT when LIMIT-PROVIDED is nil.

Keeps the first `emagent-tools-compact-read-max-lines' lines."
  (cond
   ((or (not (emagent-tools-compact--enabled-p)) limit-provided)
    text)
   (t
    (let* ((text (or text ""))
           (max emagent-tools-compact-read-max-lines)
           (lines (split-string text "\n"))
           (n (length lines)))
      (if (<= n max)
          text
        (format "%s\n… (%d more lines; pass limit= to read more)"
                (string-join (cl-subseq lines 0 max) "\n")
                (- n max)))))))

(provide 'emagent-tools-compact)

;;; emagent-tools-compact.el ends here
