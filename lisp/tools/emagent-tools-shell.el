;;; emagent-tools-shell.el --- Shell and grep tools  -*- lexical-binding: t; -*-

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
;; Shell process helpers and permission/policy matching.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'emagent-chat-ui)
(require 'emagent-log)

(defun emagent-policy-match--words (command)
  "Split shell COMMAND into words, unquoting shell single/double quotes.
Falls back to a whitespace split when COMMAND cannot be shell-parsed."
  (condition-case nil
      (split-string-shell-command (string-trim command))
    (error (split-string (string-trim command) "[[:space:]]+" t))))

(defun emagent-policy-match--strip-quoted (command)
  "Remove single- and double-quoted spans from COMMAND."
  (replace-regexp-in-string "[\"'][^\"']*[\"']" "" command))

(defun emagent-policy-match--split-commands (command)
  "Split COMMAND into segments at top-level `;' `|' `&' and newlines.

Separators inside single- or double-quoted spans are not split points, so a
dangerous argv hidden behind `&&'/`;'/`|' (which the whole-command matchers
miss) surfaces as its own segment.  `&&' and `||' yield an empty middle segment
that is dropped."
  (let ((segments nil) (current nil) (quote nil) (escape nil))
    (dolist (c (append command nil))
      (cond
       ;; A backslash-escaped char is literal — notably `\"' does NOT open a
       ;; quote, so it cannot swallow a following top-level separator.
       (escape (push c current) (setq escape nil))
       ((and (eq c ?\\) (not (eq quote ?\'))) (push c current) (setq escape t))
       (quote (push c current) (when (eq c quote) (setq quote nil)))
       ((memq c '(?\" ?\')) (setq quote c) (push c current))
       ((memq c '(?\; ?| ?& ?\n))
        (push (apply #'string (nreverse current)) segments)
        (setq current nil))
       (t (push c current))))
    (push (apply #'string (nreverse current)) segments)
    (cl-remove-if #'string-empty-p
                  (mapcar #'string-trim (nreverse segments)))))

(defconst emagent-policy-match--shell-wrappers
  '("sh" "bash" "zsh" "dash" "ksh")
  "Shells whose `-c CMD' argument carries an inner command to inspect.")

(defconst emagent-policy-match--prefix-wrappers
  '("sudo" "doas" "env" "nice" "nohup" "setsid" "stdbuf" "command" "builtin"
    "eval" "exec")
  "List wrapper words that run the remaining words as a command.
The wrapper word (and for `env', leading VAR=VALUE assignments) is stripped
and the rest re-inspected.  `eval'/`exec' work because the argument is
unquoted by `--words' and rejoined.")

(defconst emagent-policy-match--xargs-value-flags
  '("-n" "-I" "-i" "-P" "-s" "-L" "-l" "-d" "-E" "-a" "-R" "-S")
  "`xargs' options that consume a following value word.")

(defun emagent-policy-match--inner-c-command (words)
  "Return the argument following `-c' in WORDS, or nil."
  (car (cdr (member "-c" words))))

(defun emagent-policy-match--xargs-inner (words)
  "Return the command WORDS following `xargs' and its options, or nil.
Skips value-less flags (`-0', `-r') and the value of value-taking flags
 (`-n N', `-I {}'); a flag with an embedded value (`-n1') is one token."
  (let ((rest (cdr words)))
    (while (and rest (string-prefix-p "-" (car rest)))
      (if (member (car rest) emagent-policy-match--xargs-value-flags)
          (setq rest (cddr rest))
        (setq rest (cdr rest))))
    rest))

(defun emagent-policy-match--substitution-commands (command)
  "Return inner command strings of $(...) and `...` substitutions in COMMAND.
Substitutions can hide a dangerous argv where the whole-command and leaf-argv
matchers only see it as an operand (`echo $(rm -rf ~)')."
  (let ((results nil) (i 0) (len (length command)))
    (while (< i len)
      (cond
       ((and (< (1+ i) len) (eq (aref command i) ?$) (eq (aref command (1+ i)) ?\())
        (let ((depth 1) (j (+ i 2)) (start (+ i 2)))
          (while (and (< j len) (> depth 0))
            (pcase (aref command j)
              (?\( (setq depth (1+ depth)))
              (?\) (setq depth (1- depth))))
            (setq j (1+ j)))
          (push (substring command start (max start (1- j))) results)
          (setq i j)))
       ((eq (aref command i) ?`)
        (let ((j (1+ i)))
          (while (and (< j len) (not (eq (aref command j) ?`)))
            (setq j (1+ j)))
          (push (substring command (1+ i) (min j len)) results)
          (setq i (1+ j))))
       (t (setq i (1+ i)))))
    (nreverse results)))

(defun emagent-policy-match--strip-leading-assignments (words)
  "Drop leading VAR=VALUE assignments from WORDS (e.g. `FOO=1 rm' → `rm')."
  (while (and words
              (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car words)))
    (setq words (cdr words)))
  words)

(defun emagent-policy-shell-commands (command &optional depth)
  "Return the list of leaf shell commands within COMMAND.

Splits on top-level separators and unwraps `sh -c'/`bash -c', a leading
`sudo'/`env'/`eval'/wrapper, and `xargs' options, recursively, so each dangerous
argv is inspected on its own regardless of how it was composed.  Command
substitutions ($(...) and `...`) are also decomposed.  DEPTH bounds recursion."
  (setq depth (or depth 0))
  (if (> depth 6)
      (list (string-trim command))
    (cl-loop for segment in (emagent-policy-match--split-commands command)
             for words = (emagent-policy-match--strip-leading-assignments
                          (emagent-policy-match--words segment))
             for head = (car words)
             ;; A dangerous argv can hide inside a $(...)/`...` substitution
             ;; where the leaf matchers only see it as an operand.
             for subs = (cl-mapcan
                         (lambda (s) (emagent-policy-shell-commands s (1+ depth)))
                         (emagent-policy-match--substitution-commands segment))
             append
             (append
              (cond
               ((and (member head emagent-policy-match--shell-wrappers)
                     (emagent-policy-match--inner-c-command words))
                (emagent-policy-shell-commands
                 (emagent-policy-match--inner-c-command words) (1+ depth)))
               ((member head emagent-policy-match--prefix-wrappers)
                (emagent-policy-shell-commands
                 (mapconcat #'identity (cdr words) " ") (1+ depth)))
               ((and (equal head "xargs") (emagent-policy-match--xargs-inner words))
                (emagent-policy-shell-commands
                 (mapconcat #'identity (emagent-policy-match--xargs-inner words) " ")
                 (1+ depth)))
               ;; No stripping applied → return the original segment (keeps
               ;; quotes/spacing for the whole-command matchers upstream).
               ((equal words (emagent-policy-match--words segment))
                (list segment))
               (t (list (mapconcat #'identity words " "))))
              subs))))

(defun emagent-policy-match--argv-index-p (index expected words)
  "Return non-nil when the INDEXth word (1-based) equals EXPECTED.

Arguments: INDEX, WORDS."
  (let ((word (nth (1- index) words)))
    (and (stringp word) (string= word expected))))

(defun emagent-policy-match--flag-word-p (flag words)
  "Return non-nil when FLAG appears as a separate word in WORDS."
  (and (stringp flag) (member flag words)))

(defun emagent-policy-match--any-flag-p (flags words)
  "Return non-nil when any of FLAGS appears as a word in WORDS."
  (cl-loop for flag in flags thereis (emagent-policy-match--flag-word-p flag words)))

(defun emagent-policy-match--combined-short-flags-p (flags words)
  "Return non-nil when WORDS include a token matching FLAGS combined (e.g. \"-rf\")."
  (and (stringp flags)
       (cl-loop for word in words
                thereis (and (string-prefix-p "-" word)
                             (string-match-p (format "\\`-%s\\'" (regexp-quote flags))
                                             word)))))

(defun emagent-policy-match--pipe-to-shell-p (command)
  "Return non-nil when COMMAND pipes curl/wget output into a shell."
  (string-match-p "curl[[:space:]]+.*|.*sh\\b" (emagent-policy-match--strip-quoted command)))

(defun emagent-policy-match--spec-p (key value command words stripped)
  "Return non-nil when one :match spec KEY VALUE is satisfied for COMMAND.

Arguments: WORDS, STRIPPED."
  (pcase key
    ('argv-first
     (and (consp words) (string= (car words) value)))
    ('argv-index
     (and (consp value)
          (emagent-policy-match--argv-index-p (car value) (cdr value) words)))
    ('any-flag
     (emagent-policy-match--any-flag-p value words))
    ('all-flags
     (cl-loop for flag in value always (emagent-policy-match--flag-word-p flag words)))
    ('long-flag
     (emagent-policy-match--flag-word-p value words))
    ('combined-short-flags
     (emagent-policy-match--combined-short-flags-p value words))
    ('regexp
     (and (stringp value) (string-match-p value stripped)))
    ('contains
     (and (stringp value) (string-search value stripped)))
    ('pipe-to-shell
     (and value (emagent-policy-match--pipe-to-shell-p command)))
    (_ nil)))

(defun emagent-policy-match--shell-rule-p (rule command)
  "Return non-nil when RULE matches shell COMMAND."
  (when-let* ((match (plist-get rule :match))
              ((listp match)))
    (let* ((words (emagent-policy-match--words command))
           (stripped (emagent-policy-match--strip-quoted command)))
      (cl-loop for (key . value) in match
               always (emagent-policy-match--spec-p key value command words stripped)))))

(defun emagent-policy-match--severity-rank (severity)
  "Return sort rank for SEVERITY (higher wins)."
  (pcase severity
    ('deny 3)
    ('confirm 2)
    ('safe 1)
    (_ 0)))

(defun emagent-policy-match--merge-verdict (current severity reason)
  "Return the higher-precedence verdict between CURRENT and SEVERITY/REASON."
  (let ((new `(,severity . ,reason)))
    (if (or (null current)
            (> (emagent-policy-match--severity-rank severity)
               (emagent-policy-match--severity-rank (car current))))
        new
      current)))

(defun emagent-policy-match--symbols-in-form (form symbols)
  "Return symbols from SYMBOLS found anywhere in FORM."
  (let (found stack)
    (setq stack (list form))
    (while stack
      (let ((sexp (pop stack)))
        (when sexp
          (if (memq sexp symbols)
              (push sexp found)
            (when (consp sexp)
              (push (cdr sexp) stack)
              (push (car sexp) stack))))))
    (delete-dups found)))

(defun emagent-policy-match--elisp-spec-p (key value parsed)
  "Return non-nil when elisp :match spec KEY VALUE is satisfied for PARSED form."
  (pcase key
    ('symbol
     (emagent-policy-match--symbols-in-form parsed (list value)))
    ('any-symbol
     (emagent-policy-match--symbols-in-form parsed value))
    (_ nil)))

(defun emagent-policy-match--elisp-rule-p (rule parsed)
  "Return non-nil when RULE matches parsed elisp form PARSED."
  (when-let* ((match (plist-get rule :match))
              ((listp match)))
    (cl-loop for (key . value) in match
             always (emagent-policy-match--elisp-spec-p key value parsed))))

(defun emagent-policy-match--elisp-matched-symbols (rule parsed)
  "Return symbols from RULE's :match that appear in PARSED."
  (when-let ((match (plist-get rule :match)))
    (pcase (assoc 'any-symbol match)
      (`(any-symbol . ,symbols)
       (emagent-policy-match--symbols-in-form parsed symbols))
      (`(symbol . ,symbol)
       (emagent-policy-match--symbols-in-form parsed (list symbol)))
      (_ nil))))

(defun emagent-policy-match--strip-python (code)
  "Remove comments and string literals from python CODE."
  (let ((s (or code "")))
    (setq s (replace-regexp-in-string "#.*" "" s))
    (setq s (replace-regexp-in-string "\"\"\"\\(?:\\\\.\\|[^\"\\]\\)*\"\"\"" "" s))
    (setq s (replace-regexp-in-string "'''\\(?:\\\\.\\|[^'\\]\\)*'''" "" s))
    (setq s (replace-regexp-in-string "\"\\(?:\\\\.\\|[^\"\\]\\)*\"" "" s))
    (setq s (replace-regexp-in-string "'\\(?:\\\\.\\|[^'\\]\\)*'" "" s))
    s))

(defun emagent-policy-match--python-import-module-p (module stripped)
  "Return non-nil when STRIPPED python imports MODULE."
  (or (string-match-p (format "\\`import[[:space:]]+%s\\>" module) stripped)
      (string-match-p (format "\\`from[[:space:]]+%s\\>" module) stripped)
      (string-match-p (format "[[:space:]]import[[:space:]]+%s\\>" module) stripped)
      (string-match-p (format "[[:space:]]from[[:space:]]+%s\\>" module) stripped)))

(defun emagent-policy-match--python-spec-p (key value stripped)
  "Return non-nil when python :match spec KEY VALUE is satisfied for STRIPPED code."
  (pcase key
    ('regexp
     (and (stringp value) (string-match-p value stripped)))
    ('import-module
     (and (stringp value)
          (emagent-policy-match--python-import-module-p value stripped)))
    (_ nil)))

(defun emagent-policy-match--python-rule-p (rule code)
  "Return non-nil when RULE matches python CODE."
  (when (and (stringp code) (not (string-empty-p (string-trim code))))
    (when-let* ((match (plist-get rule :match))
                ((listp match)))
      (let ((stripped (emagent-policy-match--strip-python code)))
        (cl-loop for (key . value) in match
                 always (emagent-policy-match--python-spec-p key value stripped))))))

(defun emagent-policy-match--python-c-code (command)
  "Return python source from `python -c' style COMMAND, or nil."
  (when (string-match
         "\\`\\(?:python3?\\)[[:space:]]+-c[[:space:]]+\\(.+\\)\\'"
         (string-trim command))
    (let ((code (match-string 1 command)))
      (when (and (stringp code) (not (string-empty-p code)))
        (replace-regexp-in-string "\\`[\"']\\|[\"']\\'" "" code)))))

(defgroup emagent-policy nil
  "Security policy rules for emagent tool execution."
  :group 'emagent
  :prefix "emagent-policy-")

(defcustom emagent-policy-extra-shell-rules nil
  "Extra shell policy rules appended after `emagent-policy-shell-rules'."
  :type '(repeat plist)
  :group 'emagent-policy)

(defconst emagent-policy-shell-rules
  '((:id shell-rm-combined-rf
     :severity confirm
     :reason "rm with combined -rf/-fr flags"
     :match ((regexp . "\\brm[[:space:]]+-[rf]+")))
    (:id shell-rm-recursive
     :severity confirm
     :reason "rm --recursive"
     :match ((argv-first . "rm") (long-flag . "--recursive")))
    (:id shell-rm-force
     :severity confirm
     :reason "rm --force"
     :match ((argv-first . "rm") (long-flag . "--force")))
    (:id shell-dd
     :severity confirm
     :reason "direct disk write (dd)"
     :match ((argv-first . "dd")))
    (:id shell-mkfs
     :severity confirm
     :reason "filesystem format (mkfs)"
     :match ((regexp . "\\bmkfs\\.")))
    (:id shell-mke2fs
     :severity confirm
     :reason "filesystem format (mke2fs)"
     :match ((argv-first . "mke2fs")))
    (:id shell-format
     :severity confirm
     :reason "disk format"
     :match ((argv-first . "format")))
    (:id shell-shutdown
     :severity confirm
     :reason "system shutdown"
     :match ((argv-first . "shutdown")))
    (:id shell-reboot
     :severity confirm
     :reason "system reboot"
     :match ((argv-first . "reboot")))
    (:id shell-init-0
     :severity confirm
     :reason "init 0 (halt)"
     :match ((argv-first . "init") (argv-index . (2 . "0"))))
    (:id shell-sudo-rm
     :severity confirm
     :reason "sudo rm"
     :match ((argv-first . "sudo") (argv-index . (2 . "rm"))))
    (:id shell-curl-pipe-sh
     :severity confirm
     :reason "pipe curl into shell"
     :match ((pipe-to-shell . t)))
    (:id shell-trash
     :severity confirm
     :reason "trash CLI"
     :match ((argv-first . "trash")))
    (:id shell-kill-9
     :severity confirm
     :reason "kill -9"
     :match ((argv-first . "kill") (any-flag . ("-9" "-KILL"))))
    (:id shell-disk-overwrite
     :severity confirm
     :reason "overwrite block device"
     :match ((regexp . ">*/dev/[sh]d[a-z]")))
    (:id shell-chmod-world
     :severity confirm
     :reason "world-writable chmod"
     :match ((argv-first . "chmod") (regexp . "-R[[:space:]]*777"))))
  "Built-in shell policy rules, highest severity wins when several match.")

(defun emagent-policy--all-shell-rules ()
  "Return built-in and user `emagent-policy-extra-shell-rules'."
  (append emagent-policy-shell-rules emagent-policy-extra-shell-rules))

(defcustom emagent-policy-elisp-blocked-symbols
  '(kill-emacs pause-emacs)
  "Symbols hard-blocked in eval; cannot run under any circumstances."
  :type '(repeat symbol)
  :group 'emagent-policy)

(defcustom emagent-policy-elisp-shell-blocked-symbols
  '(shell-command shell-command-to-string
    call-process call-process-shell-command process-file)
  "Synchronous shell functions blocked in eval.
These block Emacs until the subprocess exits; use the run_shell_command
tool instead, which runs the process asynchronously."
  :type '(repeat symbol)
  :group 'emagent-policy)

(defcustom emagent-policy-elisp-dangerous-symbols
  '(delete-file delete-directory
    rename-file rename-directory
    copy-file copy-directory
    write-region write-file
    insert-file-contents
    load load-file load-library
    start-process start-file-process
    kill-buffer kill-buffer-and-save)
  "Symbols in eval that require explicit user confirmation."
  :type '(repeat symbol)
  :group 'emagent-policy)

(defcustom emagent-policy-extra-elisp-rules nil
  "Extra elisp policy rules appended after built-in symbol rules."
  :type '(repeat plist)
  :group 'emagent-policy)

(defun emagent-policy--builtin-elisp-rules ()
  "Return symbol-based elisp rules from `emagent-policy-elisp-*-symbols'."
  (list
   `(:id elisp-blocked
     :severity deny
     :reason-kind blocked
     :match ((any-symbol . ,emagent-policy-elisp-blocked-symbols)))
   `(:id elisp-shell-blocked
     :severity deny
     :reason "Synchronous shell functions block Emacs. Use the run_shell_command tool instead — it runs the process asynchronously."
     :match ((any-symbol . ,emagent-policy-elisp-shell-blocked-symbols)))
   `(:id elisp-dangerous
     :severity confirm
     :reason-kind dangerous
     :match ((any-symbol . ,emagent-policy-elisp-dangerous-symbols)))))

(defun emagent-policy--all-elisp-rules ()
  "Return built-in and user `emagent-policy-extra-elisp-rules'."
  (append (emagent-policy--builtin-elisp-rules)
          emagent-policy-extra-elisp-rules))

(defcustom emagent-policy-extra-python-rules nil
  "Extra python policy rules appended after `emagent-policy-python-rules'."
  :type '(repeat plist)
  :group 'emagent-policy)

(defconst emagent-policy-python-rules
  '((:id python-os-system
     :severity confirm
     :reason "os.system"
     :match ((regexp . "\\<os\\.system[[:space:]]*(")))
    (:id python-os-popen
     :severity confirm
     :reason "os.popen"
     :match ((regexp . "\\<os\\.popen[[:space:]]*(")))
    (:id python-subprocess-import
     :severity confirm
     :reason "subprocess"
     :match ((import-module . "subprocess")))
    (:id python-subprocess-call
     :severity confirm
     :reason "subprocess"
     :match ((regexp . "\\<subprocess\\.")))
    (:id python-shutil-rmtree
     :severity confirm
     :reason "shutil.rmtree"
     :match ((regexp . "\\<shutil\\.rmtree[[:space:]]*(")))
    (:id python-os-remove
     :severity confirm
     :reason "os.remove/unlink"
     :match ((regexp . "\\<os\\.\\(?:remove\\|unlink\\|rmdir\\)[[:space:]]*(")))
    (:id python-exec-eval
     :severity confirm
     :reason "exec/eval/compile"
     :match ((regexp . "\\<\\(?:exec\\|eval\\|compile\\)[[:space:]]*(")))
    (:id python-dunder-import
     :severity confirm
     :reason "__import__"
     :match ((regexp . "\\<__import__[[:space:]]*(")))
    (:id python-ctypes-import
     :severity confirm
     :reason "ctypes"
     :match ((import-module . "ctypes")))
    (:id python-ctypes-call
     :severity confirm
     :reason "ctypes"
     :match ((regexp . "\\<ctypes\\."))))
  "Built-in python policy rules.")

(defun emagent-policy--all-python-rules ()
  "Return built-in and user `emagent-policy-extra-python-rules'."
  (append emagent-policy-python-rules emagent-policy-extra-python-rules))

(defvar emagent-tools--chat-buffer)

(defvar emagent-tools--acp-session-p)

(defun emagent-policy--verdict-from-merge (verdict)
  "Normalize internal VERDICT cons to a permission plist or nil."
  (pcase (car verdict)
    ('deny `(:deny . ,(cdr verdict)))
    ('confirm `(:confirm . ,(cdr verdict)))
    (_ nil)))

(defun emagent-policy--check-rule-list (rules predicate)
  "Check RULES with PREDICATE; return highest-severity internal verdict or nil."
  (let ((verdict nil))
    (dolist (rule rules)
      (when-let ((reason (funcall predicate rule)))
        (setq verdict (emagent-policy-match--merge-verdict
                        verdict
                        (plist-get rule :severity)
                        reason))))
    (emagent-policy--verdict-from-merge verdict)))

(defun emagent-policy--verdict-rank (verdict)
  "Return precedence rank for a permission VERDICT (:deny > :confirm > nil)."
  (pcase (car-safe verdict) (:deny 2) (:confirm 1) (_ 0)))

(defun emagent-policy--merge-plist-verdict (a b)
  "Return the higher-precedence permission verdict between A and B."
  (if (>= (emagent-policy--verdict-rank a) (emagent-policy--verdict-rank b))
      (or a b)
    b))

(defun emagent-policy--check-one-shell (command)
  "Check a single shell COMMAND string against shell and embedded-python rules."
  (or (emagent-policy--check-rule-list
       (emagent-policy--all-shell-rules)
       (lambda (rule)
         (and (emagent-policy-match--shell-rule-p rule command)
              (plist-get rule :reason))))
      (when-let ((code (emagent-policy-match--python-c-code command)))
        (emagent-policy-check-python code))))

(defun emagent-policy-check-shell (command)
  "Check shell COMMAND against shell and embedded-python rules.
Return nil when ok, (:deny . REASON), or (:confirm . REASON).

Checks the whole command (so rules that span a pipeline, e.g. `curl | sh',
still fire) AND each decomposed leaf command (so a dangerous argv hidden behind
`&&'/`;'/`|' or inside `sh -c'/`sudo' is caught), keeping the worst verdict."
  (when (and (stringp command) (not (string-empty-p (string-trim command))))
    (let ((worst (emagent-policy--check-one-shell command)))
      (dolist (leaf (emagent-policy-shell-commands command))
        (unless (string= leaf command)
          (setq worst (emagent-policy--merge-plist-verdict
                       worst (emagent-policy--check-one-shell leaf)))))
      worst)))

(defun emagent-policy-check-python (code)
  "Check python CODE against `emagent-policy-python-rules'.
Return nil when ok, (:deny . REASON), or (:confirm . REASON)."
  (when (and (stringp code) (not (string-empty-p (string-trim code))))
    (emagent-policy--check-rule-list
     (emagent-policy--all-python-rules)
     (lambda (rule)
       (and (emagent-policy-match--python-rule-p rule code)
            (plist-get rule :reason))))))

(defun emagent-policy--elisp-read (form-str)
  "Parse FORM-STR as progn or return a deny plist on read error."
  (condition-case parse-err
      (read (concat "(progn " (string-trim (or form-str "")) ")"))
    (error
     `(:deny . ,(format "Elisp read error: %s"
                        (error-message-string parse-err))))))

(defun emagent-policy--elisp-rule-reason (rule parsed)
  "Return reason string when RULE matches PARSED elisp form."
  (when (emagent-policy-match--elisp-rule-p rule parsed)
    (let ((symbols (emagent-policy-match--elisp-matched-symbols rule parsed)))
      (when symbols
        (pcase (plist-get rule :reason-kind)
          ('blocked
           (format "Eval blocked (%s). Use the dedicated emagent tools instead."
                   (mapconcat #'symbol-name symbols ", ")))
          ('dangerous
           (format "Eval contains: %s"
                   (mapconcat #'symbol-name symbols ", ")))
          (_ (or (plist-get rule :reason)
                 (format "Eval contains: %s"
                         (mapconcat #'symbol-name symbols ", ")))))))))

(defun emagent-policy-check-elisp (form-str)
  "Check elisp FORM-STR against `emagent-policy--all-elisp-rules'.
Return nil when ok, (:deny . REASON), or (:confirm . REASON)."
  (let* ((form-str (string-trim (or form-str "")))
         (parsed (emagent-policy--elisp-read form-str)))
    (if (and (listp parsed) (eq (car parsed) :deny))
        parsed
      (emagent-policy--check-rule-list
       (emagent-policy--all-elisp-rules)
       (lambda (rule)
         (emagent-policy--elisp-rule-reason rule parsed))))))

(defun emagent-policy-check (kind content)
  "Dispatch policy check for KIND (shell, elisp, python) and CONTENT."
  (pcase kind
    ('shell (emagent-policy-check-shell content))
    ('elisp (emagent-policy-check-elisp content))
    ('python (emagent-policy-check-python content))
    (_ nil)))

(defun emagent-policy-shell-needs-confirm-p (command)
  "Return non-nil when COMMAND needs user confirmation."
  (let ((verdict (emagent-policy-check-shell command)))
    (and verdict (memq (car verdict) '(:deny :confirm)))))

(defun emagent-policy-shell-deny-p (command)
  "Return non-nil when COMMAND is hard-blocked by policy."
  (eq (car (emagent-policy-check-shell command)) :deny))

(defun emagent-policy-rule-id-matches-p (kind id content)
  "Return non-nil when rule ID of KIND matches CONTENT."
  (let* ((rules (pcase kind
                  ('shell (emagent-policy--all-shell-rules))
                  ('elisp (emagent-policy--all-elisp-rules))
                  ('python (emagent-policy--all-python-rules))
                  (_ nil)))
         (rule (cl-find id rules :key (lambda (r) (plist-get r :id)) :test #'equal)))
    (and rule
         (pcase kind
           ('shell (emagent-policy-match--shell-rule-p rule content))
           ('elisp
            (let ((parsed (emagent-policy--elisp-read content)))
              (and (not (and (listp parsed) (eq (car parsed) :deny)))
                   (emagent-policy-match--elisp-rule-p rule parsed))))
           ('python (emagent-policy-match--python-rule-p rule content))
           (_ nil)))))

(defun emagent-policy--skip-runtime-confirm-p ()
  "Return non-nil when execution-time confirm dialogue should be skipped."
  (and (boundp 'emagent-tools--acp-session-p)
       emagent-tools--acp-session-p))

(defun emagent-policy--runtime-confirm-p (reason context)
  "Prompt for runtime confirmation; return non-nil when allowed.

Arguments: REASON, CONTEXT.
Uses a synchronous `completing-read' dialog.  The inline button prompt is
asynchronous and cannot drive `emagent-policy-enforce', which must return a
boolean before tool execution continues."
  (or (emagent-policy--skip-runtime-confirm-p)
      (let* ((preview (truncate-string-to-width (or context "") 400 nil nil "…"))
             (preamble (when (not (string-empty-p preview))
                         (format "\n#+begin_src %s\n%s\n#+end_src"
                                 (if (string-match-p "\\`\\(?:python\\|python3\\)" context)
                                     "python"
                                   "shell")
                                 preview)))
             (prompt (format "Policy check: *%s*" reason))
             (choice nil))
        (emagent-tools--buttons-prompt
         prompt
         '(("Allow" . yes) ("Deny" . no))
         nil
         (lambda (v) (setq choice v))
         preamble)
        (eq 'yes choice))))

(defun emagent-policy-enforce (verdict &optional context)
  "Apply VERDICT at execution time; signal `user-error' when blocked.

Arguments: CONTEXT."
  (pcase verdict
    (`(:deny . ,msg) (user-error "%s" msg))
    (`(:confirm . ,msg)
     (unless (emagent-policy--runtime-confirm-p msg context)
       (user-error "Cancelled: %s" msg)))
    (_ nil)))

(defun emagent-policy-enforce-string (verdict &optional context)
  "Like `emagent-policy-enforce' but return an error string instead of signaling.

Arguments: VERDICT, CONTEXT."
  (pcase verdict
    (`(:deny . ,msg) msg)
    (`(:confirm . ,msg)
     (unless (emagent-policy--runtime-confirm-p msg context)
       (format "Cancelled: %s" msg)))
    (_ nil)))

(eval-when-compile
  (require 'cl-lib))

(defgroup emagent-elisp nil
  "Elisp validation helpers for emagent."
  :group 'emagent-tools)

(defcustom emagent-elisp-validate-on-write t
  "When non-nil, reject writes to .el files that fail Elisp validation."
  :type 'boolean
  :group 'emagent-elisp)

(defcustom emagent-elisp-byte-compile-on-check nil
  "When non-nil, run `byte-compile-file' during .el file validation.

WARNING: byte-compiling expands macros, which EXECUTES arbitrary code from the
validated content — a `(defmacro m () (delete-file …)) (m)' payload runs during
the check.  Because this validation runs on agent-supplied file content (via
`check_structural_file' and, with `emagent-elisp-validate-on-write', every .el
write), enabling it lets a misbehaving or prompt-injected agent run code before
any permission gate.  Leave nil unless you fully trust the content being
validated; syntax and paren checks run regardless."
  :type 'boolean
  :group 'emagent-elisp)

;; ── Position helpers ──────────────────────────────────────────────

(defun emagent-elisp--position-line-column (content pos)
  "Return (LINE . COLUMN) one-based for zero-based POS in CONTENT."
  (let ((line 1) (col 1) (i 0))
    (while (< i pos)
      (pcase (aref content i)
        (?\n (setq line (1+ line) col 1))
        (?\r nil)
        (_ (setq col (1+ col))))
      (setq i (1+ i)))
    (cons line col)))

(defun emagent-elisp--error-at (content pos message)
  "Format MESSAGE with line:column for POS in CONTENT."
  (let ((lc (emagent-elisp--position-line-column content (max 0 pos))))
    (format "line %d, column %d: %s" (car lc) (cdr lc) message)))

;; ── Validation ────────────────────────────────────────────────────

(defun emagent-elisp--scan-parens (content)
  "Return nil when CONTENT balances parens, or an error string."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (condition-case err
        (progn
          (while (< (point) (point-max))
            (skip-chars-forward " \t\n")
            (when (< (point) (point-max))
              (goto-char (scan-sexps (point) 1))))
          (skip-chars-forward " \t\n")
          (when (< (point) (point-max))
            (emagent-elisp--error-at content (point)
                                     "extra text after last form")))
      (scan-error
       (emagent-elisp--error-at content (max 0 (nth 2 err)) (nth 1 err))))))

(defun emagent-elisp--read-forms (content)
  "Read all top-level forms from CONTENT.
Return a list of (POS . FORM) or signal with read error string."
  (let ((pos 0) (len (length content)) (forms nil))
    (while (< pos len)
      (while (and (< pos len)
                  (memq (aref content pos) '(?\s ?\t ?\n ?\r)))
        (setq pos (1+ pos)))
      (when (< pos len)
        (condition-case err
            (let ((parsed (read-from-string content pos)))
              (push (cons pos (car parsed)) forms)
              (setq pos (cdr parsed)))
          (end-of-file
           (error "%s" (emagent-elisp--error-at content pos "unexpected end of file")))
          (error
           (error "%s" (emagent-elisp--error-at content pos (error-message-string err)))))))
    (nreverse forms)))

(defun emagent-elisp--byte-compile-content (content)
  "Return nil when CONTENT byte-compiles, or an error string."
  (let ((tmp (make-temp-file "emagent-elisp-" nil ".el")))
    (unwind-protect
        (progn
          (write-region content nil tmp nil 'silent)
          (let ((byte-compile-debug 1) (inhibit-message t))
            (condition-case err
                (progn
                  (byte-compile-file tmp)
                  (ignore-errors (delete-file (concat tmp "c")))
                  nil)
              (error
               (format "byte-compile: %s" (error-message-string err))))))
      (ignore-errors (delete-file tmp)))))

(defun emagent-elisp--validate-content (content &optional _path)
  "Return nil when CONTENT is valid Elisp, or an error description string."
  (or (emagent-elisp--scan-parens content)
      (condition-case err
          (progn (emagent-elisp--read-forms content) nil)
        (error (error-message-string err))
        (user-error (error-message-string err)))))

(defun emagent-elisp--validate-content-strict (content &optional path)
  "Like `emagent-elisp--validate-content' but also byte-compile CONTENT.

Arguments: PATH."
  (or (emagent-elisp--validate-content content path)
      (when emagent-elisp-byte-compile-on-check
        (emagent-elisp--byte-compile-content content))))

(defun emagent-elisp--wrap-form (form-str)
  "Return FORM-STR wrapped for single-expression validation."
  (concat "(progn " form-str ")"))

(defun emagent-elisp-check-form (form-str)
  "Validate FORM-STR.  Return \"OK\" or an error description."
  (let* ((trimmed (string-trim (or form-str "")))
         (wrapped (emagent-elisp--wrap-form trimmed))
         (err (emagent-elisp--validate-content wrapped))
         (doc-warn (unless err (emagent-elisp--check-docstrings trimmed))))
    (cond
     (err
      (format "SYNTAX ERROR -- %s\n\nFix the form and call check_elisp again before eval."
              err))
     (doc-warn
      (format "STYLE WARNING -- %s\n\nShorten docstring lines to ≤%d chars."
              doc-warn emagent-elisp--docstring-max-line))
     (t "OK"))))

(defun emagent-elisp-check-file-content (content &optional path)
  "Validate Elisp file CONTENT.  Return \"OK\" or an error description.

Arguments: PATH."
  (let ((err (emagent-elisp--validate-content-strict content path))
        (doc-warn (emagent-elisp--check-docstrings content)))
    (cond
     (err
      (format "SYNTAX ERROR -- %s\n\nFix the file and call check_structural_file before writing."
              err))
     (doc-warn
      (format "STYLE WARNING -- %s\n\nShorten docstring lines to ≤%d chars."
              doc-warn emagent-elisp--docstring-max-line))
     (t "OK"))))

;; ── Path helpers ──────────────────────────────────────────────────

(defun emagent-elisp-elisp-file-p (path)
  "Return non-nil when PATH resembles an Emacs Lisp file."
  (and (stringp path) (string-match-p "\\.el\\'" path)))

(defun emagent-elisp--defun-name-p (form)
  "Return defined name when FORM is a defun-like top-level form."
  (when (and (listp form) (memq (car form) '(defun cl-defun))
             (symbolp (nth 1 form)))
    (nth 1 form)))

(defconst emagent-elisp--docstring-max-line 80
  "Maximum allowed length for any single line of an Emacs Lisp docstring.")

(defun emagent-elisp--form-docstring (form)
  "Return the docstring of FORM as a string, or nil when absent."
  (when (listp form)
    (pcase (car form)
      ((or 'defun 'cl-defun 'defmacro 'cl-defmacro)
       (when (stringp (nth 3 form)) (nth 3 form)))
      ((or 'defvar 'defconst 'defcustom 'defgroup 'defface)
       (when (stringp (nth 3 form)) (nth 3 form))))))

(defun emagent-elisp--check-docstrings (content)
  "Return a warning string when any docstring line in CONTENT exceeds 80 chars.
Returns nil when all docstrings are within the limit."
  (condition-case nil
      (let ((forms (emagent-elisp--read-forms content)))
        (catch 'found
          (dolist (pos-form forms)
            (let* ((form (cdr pos-form))
                   (name (and (listp form) (symbolp (nth 1 form)) (nth 1 form)))
                   (doc (emagent-elisp--form-docstring form)))
              (when doc
                (dolist (line (split-string doc "\n"))
                  (when (> (length line) emagent-elisp--docstring-max-line)
                    (throw 'found
                           (format "docstring line >%d chars in `%s': \"%s\""
                                   emagent-elisp--docstring-max-line
                                   (or name "?")
                                   (truncate-string-to-width
                                    line 60 nil nil "…"))))))))
          nil))
    (error nil)))

;; Bound by the MCP dispatcher and by `emagent-tools-set-project-directory'
;; (both in `emagent-tools', which requires this file); forward-declared
;; here rather than required back to avoid a cycle.
(defvar emagent-tools--project-directory)

(defvar emagent-tools--root-boundary)

(defconst emagent-tools--icloud-dir
  (expand-file-name "~/Library/Mobile Documents/"))

(defconst emagent-tools--containers-dir
  (expand-file-name "~/Library/Containers/"))

(defconst emagent-tools--group-containers-dir
  (expand-file-name "~/Library/Group Containers/"))

(defun emagent-tools--protected-truename-p (truename)
  "Return non-nil when TRUENAME is in a protected macOS tree.
TRUENAME is an absolute, symlink-resolved path (iCloud or another app
container).  Pure predicate with no session resolution, so
`emagent-tools--root-directory' can call it safely."
  (or (string-prefix-p emagent-tools--icloud-dir truename)
      (string-prefix-p emagent-tools--containers-dir truename)
      (string-prefix-p emagent-tools--group-containers-dir truename)))

(defun emagent-tools--within-boundary-p (resolved)
  "Return non-nil when RESOLVED is inside `emagent-tools--root-boundary'.

Compares symlink-resolved truenames so a symlink inside the root that points
outside it cannot pass the check.  `file-truename' resolves the existing prefix
of a not-yet-created path, so a symlinked parent directory is caught too."
  (or (null emagent-tools--root-boundary)
      (let ((root (file-name-as-directory
                   (file-truename (expand-file-name emagent-tools--root-boundary))))
            (true (file-truename resolved)))
        (or (string-prefix-p root (file-name-as-directory true))
            (string= (directory-file-name true)
                     (directory-file-name root))))))

(defun emagent-tools--root-directory (path)
  "Return PATH resolved against the active emagent session project directory.

A relative PATH is resolved against the session project directory (not the
process `default-directory'), and an omitted PATH yields that directory.
Signal an error when the result escapes `emagent-tools--root-boundary' or lands
in a protected macOS tree (iCloud or another app's container)."
  (let* ((base (or emagent-tools--project-directory default-directory))
         (resolved (expand-file-name (or path base) base)))
    (unless (emagent-tools--within-boundary-p resolved)
      (user-error "Path %s is outside the session root %s"
                  resolved emagent-tools--root-boundary))
    (when (emagent-tools--protected-truename-p (file-truename resolved))
      (user-error "Refusing Emacs access to %s (iCloud or another app's container)"
                  resolved))
    resolved))

(defun emagent-tools--eval-form-read (form-str)
  "Return FORM-STR parsed as `(progn ,@forms)'."
  (read (concat "(progn " (string-trim (or form-str "")) ")")))

(defun emagent-tools--eval-form-guard (form-str)
  "Return nil when FORM-STR passes eval guardrails, else an error string."
  (emagent-policy-enforce-string (emagent-policy-check-elisp form-str) form-str))

(defun emagent-tools--eval-form-safely (form-str)
  "Evaluate FORM-STR with syntax and symbol guardrails; return a result string."
  (let ((check-result (emagent-elisp-check-form form-str)))
    (unless (string= "OK" check-result)
      (user-error "%s" check-result))
    (when-let ((err (emagent-tools--eval-form-guard form-str)))
      (user-error "%s" err))
    (condition-case err
        (let ((result (eval (emagent-tools--eval-form-read form-str))))
          (if (null result) "nil" (prin1-to-string result)))
      (error (format "Eval error: %s" (error-message-string err))))))

(defcustom emagent-tools-subprocess-timeout 60
  "Default seconds before killing an agent subprocess.
Agent tools may override this per call up to
`emagent-tools-subprocess-timeout-max'."
  :type 'integer
  :group 'emagent-tools)

(defcustom emagent-tools-subprocess-timeout-max 300
  "Maximum seconds an agent may request as a per-call subprocess timeout."
  :type 'integer
  :group 'emagent-tools)

(defcustom emagent-tools-display-compile-buffer nil
  "When non-nil, display the `*emagent-compile*' buffer when a build starts.
When nil (the default) the buffer fills in the background without
touching the window layout; switch to it any time for navigable errors
\\(\\[next-error])."
  :type 'boolean
  :group 'emagent-tools)

(defvar emagent-tools--timeout-override nil
  "When non-nil, the per-call subprocess timeout requested by the agent.
Bound dynamically around a tool call and read synchronously when a runner
starts, so it is captured before any process wait.")

(defconst emagent-tools--shell-output-limit 100000
  "Max characters returned from shell/process tool output.")

(defun emagent-tools--clamp-timeout (secs)
  "Clamp SECS to [1, `emagent-tools-subprocess-timeout-max']."
  (max 1 (min secs emagent-tools-subprocess-timeout-max)))

(defun emagent-tools--subprocess-timeout ()
  "Return the effective agent subprocess timeout in seconds.
Honors `emagent-tools--timeout-override' when set, clamped to the max."
  (emagent-tools--clamp-timeout
   (or emagent-tools--timeout-override emagent-tools-subprocess-timeout)))

(defun emagent-tools--timeout-message (secs &optional shell)
  "Return a timeout error string for limit SECS.
When SHELL is non-nil, also suggest background execution."
  (concat
   (format
    "Timed out after %ds. Retry with a larger `timeout` argument (up to %ds)."
    secs emagent-tools-subprocess-timeout-max)
   (when shell
     (concat
      " For genuinely long-running work, use background execution"
      " (append ' > /tmp/out.txt 2>&1 & echo \"PID: $!\"') and read the"
      " output file later with read_file."))))

(defun emagent-tools--run-async-sync (async-fn &rest args)
  "Run ASYNC-FN with ARGS and a result callback; block until it finishes.
For tests and internal callers only — MCP agent tools use the async path."
  (let (result is-error done)
    (apply async-fn
           (lambda (r e)
             (setq result r is-error e done t))
           args)
    (while (not done)
      (accept-process-output nil 0.05))
    (if is-error
        (error "%s" result)
      result)))

(defun emagent-tools--run-process-async (callback program &rest args)
  "Run PROGRAM with ARGS; call CALLBACK with (output is-error) from a sentinel."
  (let* ((buf (generate-new-buffer " *emagent-proc*"))
         (timeout-secs (emagent-tools--subprocess-timeout))
         (done nil)
         (timer nil)
         (proc nil)
         (finish
          (lambda (output is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (when (and proc (process-live-p proc))
                (delete-process proc))
              (when (buffer-live-p buf)
                (kill-buffer buf))
              (funcall callback output is-error)))))
    (condition-case start-err
        (progn
          (setq proc (apply #'start-process "emagent-proc" buf program args))
          (setq timer
                (run-with-timer
                 timeout-secs nil
                 (lambda ()
                   (when (and proc (process-live-p proc))
                     (delete-process proc))
                   (funcall finish
                            (emagent-tools--timeout-message timeout-secs)
                            t))))
          (set-process-sentinel
           proc
           (lambda (p _event)
             (when (memq (process-status p) '(signal exit))
               (let* ((output (with-current-buffer buf (buffer-string)))
                      (status (process-exit-status p))
                      (is-error (or (eq status 'signal)
                                    (and (numberp status)
                                         (not (zerop status))))))
                 (funcall finish output is-error))))))
      (error (funcall finish (error-message-string start-err) t)))))

(defun emagent-tools--run-process-input-async (callback input program &rest args)
  "Pipe INPUT to PROGRAM with ARGS; call CALLBACK with (output is-error)."
  (let* ((buf (generate-new-buffer " *emagent-proc*"))
         (timeout-secs (emagent-tools--subprocess-timeout))
         (done nil)
         (timer nil)
         (proc nil)
         (finish
          (lambda (output is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (when (and proc (process-live-p proc))
                (delete-process proc))
              (when (buffer-live-p buf)
                (kill-buffer buf))
              (funcall callback output is-error)))))
    (condition-case start-err
        (progn
          (setq proc (apply #'make-process
                            `(:name "emagent-proc"
                              :buffer ,buf
                              :command (,program . ,args)
                              :connection-type pipe
                              :noquery t
                              :sentinel
                              ,(lambda (p _event)
                                 (when (memq (process-status p)
                                             '(signal exit))
                                   (let* ((output
                                           (with-current-buffer buf
                                             (buffer-string)))
                                          (status (process-exit-status p))
                                          (is-error
                                           (or (eq status 'signal)
                                               (and (numberp status)
                                                    (not (zerop status))))))
                                     (funcall finish output is-error)))))))
          (process-send-string proc input)
          (process-send-eof proc)
          (setq timer
                (run-with-timer
                 timeout-secs nil
                 (lambda ()
                   (when (and proc (process-live-p proc))
                     (delete-process proc))
                   (funcall finish
                            (emagent-tools--timeout-message timeout-secs)
                            t)))))
      (error (funcall finish (error-message-string start-err) t)))))

(defun emagent-tools--run-shell-async (callback command directory)
  "Run shell COMMAND in DIRECTORY; call CALLBACK with (output is-error)."
  (let* ((default-directory (emagent-tools--root-directory directory))
         (buf (generate-new-buffer " *emagent-shell*"))
         (timeout-secs (emagent-tools--subprocess-timeout))
         (limit emagent-tools--shell-output-limit)
         (done nil)
         (timer nil)
         (proc nil)
         (finish
          (lambda (output is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (when (and proc (process-live-p proc))
                (delete-process proc))
              (when (buffer-live-p buf)
                (kill-buffer buf))
              (funcall callback output is-error)))))
    (condition-case start-err
        (progn
          (setq proc (start-process-shell-command "emagent-shell" buf command))
          (setq timer
                (run-with-timer
                 timeout-secs nil
                 (lambda ()
                   (when (and proc (process-live-p proc))
                     (delete-process proc))
                   (funcall finish
                            (emagent-tools--timeout-message timeout-secs t)
                            t))))
          (set-process-sentinel
           proc
           (lambda (p _event)
             (when (memq (process-status p) '(signal exit))
               (let* ((output (with-current-buffer buf (buffer-string)))
                      (status (process-exit-status p))
                      (is-error (or (eq status 'signal)
                                    (and (numberp status)
                                         (not (zerop status))))))
                 (when (and (not is-error) (> (length output) limit))
                   (setq output (concat (substring output 0 limit)
                                        "\n… (output truncated)")))
                 (funcall finish output is-error))))))
      (error (funcall finish (error-message-string start-err) t)))))

(defun emagent-tools--run-process-to-string (program &rest args)
  "Run PROGRAM with ARGS and return stdout (sync wrapper for the test suite)."
  (emagent-tools--run-async-sync
   (lambda (callback)
     (apply #'emagent-tools--run-process-async callback program args))))

(defconst emagent-tools--grep-max-results 50
  "Maximum matching lines returned by grep tools.")

(defun emagent-tools--grep-emacs (regexp root max)
  "Search REGEXP under ROOT in Emacs, returning at most MAX lines."
  (let ((lines nil)
        (matches 0))
    (dolist (file (directory-files-recursively root "[^.].*" nil t))
      (when (< matches max)
        (unless (string-match-p "/\\.git/" file)
          (with-temp-buffer
            (condition-case nil
                (progn
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (while (and (< matches max)
                              (re-search-forward regexp nil t))
                    (push (format "%s:%s:%s"
                                  (file-relative-name file root)
                                  (line-number-at-pos)
                                  (string-trim
                                   (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
                          lines)
                    (setq matches (1+ matches))))
              (file-missing nil))))))
    (if lines
        (string-join (nreverse lines) "\n")
      "No matches")))

(defun emagent-tool-grep-async (callback pattern &optional path)
  "Search for PATTERN under PATH; call CALLBACK with (output is-error)."
  (let* ((root (emagent-tools--root-directory path))
         (regexp (if (stringp pattern) pattern (format "%s" pattern))))
    (if (and (boundp 'emagent-acp-prefer-emacs) emagent-acp-prefer-emacs)
        (funcall callback
                 (emagent-tools--grep-emacs
                  regexp root emagent-tools--grep-max-results)
                 nil)
      (if (executable-find "rg")
          (let ((default-directory root))
            (emagent-tools--run-process-async
             (lambda (output is-error)
               (funcall callback output is-error))
             "rg" "--no-heading" "--line-number"
             "--max-count" (number-to-string emagent-tools--grep-max-results)
             "--hidden" "--glob" "!/.git/*"
             regexp "."))
        (funcall callback
                 (emagent-tools--grep-emacs
                  regexp root emagent-tools--grep-max-results)
                 nil)))))

(defun emagent-tool-grep (pattern &optional path)
  "Search for PATTERN under PATH and return matching lines as a string.
Uses pure Emacs search when `emagent-acp-prefer-emacs' is non-nil."
  (emagent-tools--run-async-sync #'emagent-tool-grep-async pattern path))

(defconst emagent-tools--list-files-ignored-dirs
  '(".git" ".build" ".venv" ".cache" ".elpaca" "node_modules" "__pycache__"
    "dist" "target" "out")
  "Directory names `emagent-tool-list-files' skips outside git repos.")

(defun emagent-tools--list-files-walk (root)
  "List files under ROOT recursively, skipping well-known artifact dirs."
  (string-join
   (mapcar (lambda (file) (file-relative-name file root))
           (directory-files-recursively
            root "[^.].*" nil
            (lambda (dir)
              (not (member (file-name-nondirectory (directory-file-name dir))
                           emagent-tools--list-files-ignored-dirs)))))
   "\n"))

(defun emagent-tool-list-files (&optional path)
  "List project files under PATH relative to PATH, one per line.

Inside a git repository this is what git considers the project:
tracked plus untracked-but-not-ignored files (`git ls-files'), so
build artifacts and other gitignored trees don't flood the result.
Elsewhere it walks the tree, skipping
`emagent-tools--list-files-ignored-dirs'."
  (let* ((root (emagent-tools--root-directory path))
         (default-directory root))
    (or (when (and (executable-find "git")
                   (locate-dominating-file root ".git"))
          (with-temp-buffer
            (when (zerop (call-process "git" nil t nil "ls-files"
                                       "--cached" "--others"
                                       "--exclude-standard"))
              (string-trim-right (buffer-string)))))
        (emagent-tools--list-files-walk root))))

(defun emagent-tools--glob-to-regexp (glob)
  "Convert a simple shell GLOB to a regexp."
  (let ((parts nil)
        (i 0)
        (len (length glob)))
    (while (< i len)
      (cond
       ((and (< (1+ i) len)
             (eq (aref glob i) ?*)
             (eq (aref glob (1+ i)) ?*))
        (push ".*" parts)
        (setq i (+ i 2)))
       ((eq (aref glob i) ?*)
        (push "[^/]*" parts)
        (setq i (1+ i)))
       ((eq (aref glob i) ??)
        (push "." parts)
        (setq i (1+ i)))
       (t
        (let ((start i))
          (while (and (< i len)
                      (not (memq (aref glob i) '(?* ??))))
            (setq i (1+ i)))
          (push (regexp-quote (substring glob start i)) parts)))))
    (concat (file-name-as-directory "") (string-join (nreverse parts) ""))))

(defun emagent-tool-find-files (glob &optional path)
  "List files under PATH matching shell GLOB, one relative path per line.

A GLOB with no `/' matches against each file's name; a GLOB with `/' matches
against the file's path relative to the search root.  The glob regexp is
`./'-prefixed, so candidates are compared as `./NAME' / `./REL-PATH'."
  (let* ((root (emagent-tools--root-directory path))
         (has-slash (string-match-p "/" glob))
         (regexp (concat "\\`" (emagent-tools--glob-to-regexp glob) "\\'"))
         (files nil))
    (dolist (file (directory-files-recursively root "[^.].*" nil t))
      (unless (string-match-p "/\\.git/" file)
        (let* ((rel (file-relative-name file root))
               (candidate (concat "./" (if has-slash rel
                                         (file-name-nondirectory rel)))))
          (when (string-match-p regexp candidate)
            (push rel files)))))
    (if files
        (string-join (sort files #'string<) "\n")
      "No matches")))

(cl-defun emagent-tools--run-git-async (callback &rest args)
  "Run git ARGS asynchronously; call CALLBACK with (output is-error).
Passes `--no-pager' so pager-using subcommands (log, diff, show) never
launch a pager that blocks on input when stdout is a pipe (which then
hangs the tool)."
  (unless (executable-find "git")
    (funcall callback "git not found on PATH" t)
    (cl-return-from emagent-tools--run-git-async))
  (let ((default-directory (emagent-tools--root-directory nil)))
    (apply #'emagent-tools--run-process-async
           callback "git" "--no-pager" args)))

(defun emagent-tools--run-git (&rest args)
  "Run git ARGS in the session project directory and return stdout."
  (emagent-tools--run-async-sync
   (lambda (callback)
     (apply #'emagent-tools--run-git-async callback args))))

(defun emagent-tool-git-status-async (callback)
  "Return git status asynchronously.

Arguments: CALLBACK."
  (emagent-tools--run-git-async
   (lambda (output is-error)
     (funcall callback (string-trim output) is-error))
   "status" "--short" "--branch"))

(defun emagent-tool-git-status ()
  "Return git status for the session project directory."
  (emagent-tools--run-async-sync #'emagent-tool-git-status-async))

(defun emagent-tool-git-diff-async (callback &optional args)
  "Return git diff output asynchronously.

Arguments: CALLBACK, ARGS."
  (if (and args (not (string-empty-p args)))
      (apply #'emagent-tools--run-git-async
             (lambda (output is-error)
               (funcall callback (string-trim output) is-error))
             "diff" (split-string-shell-command args))
    (emagent-tools--run-git-async
     (lambda (output is-error)
       (funcall callback (string-trim output) is-error))
     "diff")))

(defun emagent-tool-git-diff (&optional args)
  "Return git diff output.  Optional ARGS is extra git diff arguments."
  (emagent-tools--run-async-sync #'emagent-tool-git-diff-async args))

(defun emagent-tool-git-log-async (callback &optional args)
  "Return git log output asynchronously.

Arguments: CALLBACK, ARGS."
  (if (and args (not (string-empty-p args)))
      (apply #'emagent-tools--run-git-async
             (lambda (output is-error)
               (funcall callback (string-trim output) is-error))
             "log" (split-string-shell-command args))
    (emagent-tools--run-git-async
     (lambda (output is-error)
       (funcall callback (string-trim output) is-error))
     "log" "--oneline" "-n" "20")))

(defun emagent-tool-git-log (&optional args)
  "Return git log output.  Optional ARGS is extra git log arguments."
  (emagent-tools--run-async-sync #'emagent-tool-git-log-async args))

(defconst emagent-tools--fetch-url-limit 100000
  "Maximum response body size returned by `emagent-tool-fetch-url'.")

(defconst emagent-tools--fetch-url-timeout 30
  "Seconds to wait for `url-retrieve-synchronously' in fetch-url.")

(defun emagent-tool-undo-file (path &optional steps)
  "Save PATH after undoing edits.
STEPS is the undo depth.  Use to revert `emagent-tool-write-file' changes."
  (let* ((resolved (emagent-tools--root-directory path))
      (steps (max 1 (or steps 1)))
      (buffer (emagent-tools--file-buffer path))
      (done 0))
    (unless buffer
      (user-error "No buffer for %s" resolved))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (catch 'exhausted
          (dotimes (i steps)
            ;; `undo' only continues the previous undo chain when
            ;; `last-command' is `undo'; inside this loop it is not, so bind it
            ;; for every step after the first — otherwise repeated calls
            ;; oscillate (undo then redo) instead of undoing further.
            (condition-case _
              (let ((last-command (if (> i 0) 'undo last-command)))
                (undo)
                (setq done (1+ done)))
              (user-error (throw 'exhausted nil)))))
        (when (buffer-file-name)
          (basic-save-buffer))))
    (format "Undid %d change(s) in %s" done resolved)))

(defun emagent-tool-delete-file (path)
  "Delete PATH after user confirmation."
  (let ((resolved (emagent-tools--root-directory path)))
    (delete-file resolved t)
    (format "Deleted %s" resolved)))

(defun emagent-tool-delete-directory (path &optional recursive)
  "Delete directory PATH after user confirmation.
When RECURSIVE is non-nil, delete contents as well."
  (let ((resolved (emagent-tools--root-directory path)))
    (delete-directory resolved recursive)
    (format "Deleted %s" resolved)))

(defun emagent-tool-fetch-url-async (callback url &optional max-bytes)
  "Fetch URL asynchronously; call CALLBACK with (body is-error).

Arguments: MAX-BYTES."
  (if (not (and (stringp url) (string-match-p "\\`https?://" url)))
    (funcall callback "fetch_url requires an http:// or https:// URL" t)
    (require 'url)
    (let* ((limit (or max-bytes emagent-tools--fetch-url-limit))
        (timeout-secs (if emagent-tools--timeout-override
            (emagent-tools--clamp-timeout
              emagent-tools--timeout-override)
            emagent-tools--fetch-url-timeout))
        (done nil)
        (timer nil)
        (finish
          (lambda (body is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (funcall callback body is-error)))))
      (url-retrieve
        url
        (lambda (_status)
          (let ((buf (current-buffer)))
            (unwind-protect
              (condition-case err
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward "\n\n" nil t)
                    (let ((body (buffer-substring-no-properties (point) (point-max))))
                      (funcall finish
                        (if (> (length body) limit)
                          (concat (substring body 0 limit)
                            "\n… (output truncated)")
                          body)
                        nil))
                    (funcall finish (format "No HTTP body in response from %s" url) t)))
                (error (funcall finish (error-message-string err) t)))
              (when (buffer-live-p buf)
                (kill-buffer buf)))))
        nil t)
      (setq timer
        (run-with-timer
          timeout-secs nil
          (lambda ()
            (funcall finish
              (emagent-tools--timeout-message timeout-secs)
              t)))))))

(defun emagent-tool-fetch-url (url &optional max-bytes)
  "Fetch URL over HTTP/HTTPS and return the response body as a string.
Runs in Emacs (not the agent sandbox), so network access works when the
agent's built-in WebSearch and shell tools are blocked.

Arguments: MAX-BYTES."
  (emagent-tools--run-async-sync #'emagent-tool-fetch-url-async url max-bytes))

(defun emagent-tool-run-shell-command (command &optional directory)
  "Run COMMAND in DIRECTORY through Emacs, not an agent terminal."
  (require 'emagent-tools)
  (when (fboundp 'emagent-shell-run-command)
    (emagent-shell-run-command command directory)))

(defun emagent-tool-run-shell-command-async (command directory callback)
  "Like `emagent-tool-run-shell-command' for COMMAND via CALLBACK.
CALLBACK receives \(OUTPUT IS-ERROR); for long-running commands Emacs
stays responsive because no polling loop is used.

Arguments: DIRECTORY."
  (require 'emagent-tools)
  (when (fboundp 'emagent-shell-run-command-async)
    (emagent-shell-run-command-async command directory callback)))

(defun emagent-tool-org-move-subtree-to-parent ()
  "Move org subtree at point to its parent section after confirmation."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (org-cut-subtree)
  (org-up-element)
  (org-paste-subtree)
  "Moved subtree to parent section")

(defun emagent-tool-compile-async (callback command &optional directory)
  "Run COMMAND via `compilation-mode'; call CALLBACK with output.
Arguments: DIRECTORY."
  (require 'compile)
  (require 'ansi-color)
  (let* ((default-directory (expand-file-name
          (or directory
            emagent-tools--project-directory
            default-directory)))
      (timeout-secs (emagent-tools--subprocess-timeout))
      (limit emagent-tools--shell-output-limit)
      (done nil)
      (timer nil)
      (proc nil)
      (buf nil)
      (finish
        (lambda (text is-error)
          (unless done
            (setq done t)
            (when timer (cancel-timer timer))
            (when (and proc (process-live-p proc))
              (delete-process proc))
            (funcall callback text is-error)))))
    (condition-case err
      (progn
        (setq buf (let ((display-buffer-overriding-action
                (unless emagent-tools-display-compile-buffer
                  (list #'display-buffer-no-window
                    '(allow-no-window . t)))))
            (compilation-start command 'compilation-mode
              (lambda (_) "*emagent-compile*"))))
        (setq proc (get-buffer-process buf))
        (with-current-buffer buf
          (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil t))
        (when proc
          (setq timer
            (run-with-timer
              timeout-secs nil
              (lambda ()
                (when (process-live-p proc)
                  (delete-process proc))
                (funcall finish
                  (emagent-tools--timeout-message timeout-secs t)
                  t))))
          (set-process-sentinel
            proc
            (lambda (_p _event)
              (with-current-buffer buf
                (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                  (if (> (length text) limit)
                    (setq text (concat (substring text 0 limit)
                        "\n… (output truncated)")))
                  (funcall finish text nil))))))
        (unless proc
          (with-current-buffer buf
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (funcall finish text nil)))))
      (error (funcall finish (error-message-string err) t)))))

(defun emagent-tool-compile (command &optional directory)
  "Run COMMAND via `compilation-mode' and return its output as text.

Unlike `run_shell_command', errors appear in a persistent
`*emagent-compile*' buffer navigable with `next-error' / \\[next-error].
The buffer fills in the background; set
`emagent-tools-display-compile-buffer' to show it when a build starts.

Arguments: DIRECTORY."
  (emagent-tools--run-async-sync #'emagent-tool-compile-async command directory))

(defun emagent-tool-buffer-list ()
  "Return paths of open Emacs buffers inside the session project, one per line.
Modified buffers are marked with (modified).  Only files within the session
root (`emagent-tools--project-directory') are included."
  (let ((root (and emagent-tools--project-directory
          (file-name-as-directory
            (expand-file-name emagent-tools--project-directory)))))
    (string-join
      (delq nil
        (mapcar (lambda (buf)
            (when-let ((file (buffer-file-name buf)))
              (let ((expanded (expand-file-name file)))
                (when (or (null root)
                    (string-prefix-p root expanded))
                  (format "%s%s"
                    (if root
                      (file-relative-name expanded root)
                      (abbreviate-file-name expanded))
                    (if (buffer-modified-p buf)
                      " (modified)"
                      ""))))))
          (buffer-list)))
      "\n")))

(defun emagent-tools--imenu-subalist-p (item)
  "Return non-nil when ITEM is an imenu nested alist entry."
  (and (consp (cdr item))
    (listp (cadr item))
    (not (numberp (cadr item)))))

(defun emagent-tool-imenu-index (&optional file)
  "Return a structural outline (functions, classes, sections) for FILE.
When FILE is omitted, uses the current buffer.  Works for any language
that has imenu support configured (Java, Python, Elisp, JS, org, etc.)."
  (require 'imenu)
  (let* ((resolved (when file (emagent-tools--root-directory file)))
      (buf (if resolved
          (or (find-buffer-visiting resolved)
            (find-file-noselect resolved))
          (current-buffer))))
    (with-current-buffer buf
      (let* ((index (condition-case nil
              (when (functionp imenu-create-index-function)
                (save-excursion
                  (funcall imenu-create-index-function)))
              (error nil)))
          (lines nil))
        (cl-labels ((flatten (alist prefix)
              (dolist (entry alist)
                (if (emagent-tools--imenu-subalist-p entry)
                  (flatten (cdr entry)
                    (concat prefix (car entry) "/"))
                  (push (concat prefix (car entry)) lines)))))
          (when index (flatten index "")))
        (if lines
          (string-join (nreverse lines) "\n")
          "No imenu index available for this buffer")))))

(provide 'emagent-tools-shell)
;;; emagent-tools-shell.el ends here
