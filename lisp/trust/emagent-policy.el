;;; emagent-policy.el --- Unified security policy for emagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; SPDX-License-Identifier: MIT

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

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
;; Policy matching and enforcement for tool and shell execution.
;;
;;; Code:

(require 'cl-lib)
(require 'emagent-chat-ui)

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

(provide 'emagent-policy)
;;; emagent-policy.el ends here
