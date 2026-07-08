;;; emagent-policy-match.el --- Matchers for emagent policy rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Evgeniy Tyurkin

;;; Commentary:

;; Tokenizes shell commands and evaluates declarative :match specs from
;; `emagent-policy-rules-shell'.

;;; Code:

(require 'cl-lib)

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
  '("sudo" "doas" "env" "nice" "nohup" "setsid" "stdbuf" "command" "builtin")
  "Wrappers that run the remaining words as a command; the wrapper word (and, for
`env', leading VAR=VALUE assignments) is stripped and the rest re-inspected.")

(defun emagent-policy-match--inner-c-command (words)
  "Return the argument following `-c' in WORDS, or nil."
  (car (cdr (member "-c" words))))

(defun emagent-policy-match--strip-leading-assignments (words)
  "Drop leading VAR=VALUE assignments from WORDS (e.g. `FOO=1 rm' → `rm')."
  (while (and words
              (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car words)))
    (setq words (cdr words)))
  words)

(defun emagent-policy-shell-commands (command &optional depth)
  "Return the list of leaf shell commands within COMMAND.

Splits on top-level separators and unwraps `sh -c'/`bash -c' and a leading
`sudo'/`doas', recursively, so each dangerous argv is inspected on its own
regardless of how it was composed.  DEPTH bounds recursion."
  (setq depth (or depth 0))
  (if (> depth 6)
      (list (string-trim command))
    (cl-loop for segment in (emagent-policy-match--split-commands command)
             for words = (emagent-policy-match--strip-leading-assignments
                          (emagent-policy-match--words segment))
             for head = (car words)
             append
             (cond
              ((and (member head emagent-policy-match--shell-wrappers)
                    (emagent-policy-match--inner-c-command words))
               (emagent-policy-shell-commands
                (emagent-policy-match--inner-c-command words) (1+ depth)))
              ((member head emagent-policy-match--prefix-wrappers)
               (emagent-policy-shell-commands
                (mapconcat #'identity (cdr words) " ") (1+ depth)))
              ;; No stripping applied → return the original segment (keeps
              ;; quotes/spacing for the whole-command matchers upstream).
              ((equal words (emagent-policy-match--words segment))
               (list segment))
              (t (list (mapconcat #'identity words " ")))))))

(defun emagent-policy-match--argv-index-p (index expected words)
  "Return non-nil when the INDEXth word (1-based) equals EXPECTED."
  (let ((word (nth (1- index) words)))
    (and (stringp word) (string= word expected))))

(defun emagent-policy-match--flag-word-p (flag words)
  "Return non-nil when FLAG appears as a separate word in WORDS."
  (and (stringp flag) (member flag words)))

(defun emagent-policy-match--any-flag-p (flags words)
  "Return non-nil when any of FLAGS appears as a word in WORDS."
  (cl-loop for flag in flags thereis (emagent-policy-match--flag-word-p flag words)))

(defun emagent-policy-match--combined-short-flags-p (flags words)
  "Return non-nil when WORDS contains a token matching FLAGS combined (e.g. \"-rf\")."
  (and (stringp flags)
       (cl-loop for word in words
                thereis (and (string-prefix-p "-" word)
                             (string-match-p (format "\\`-%s\\'" (regexp-quote flags))
                                             word)))))

(defun emagent-policy-match--pipe-to-shell-p (command)
  "Return non-nil when COMMAND pipes curl/wget output into a shell."
  (string-match-p "curl[[:space:]]+.*|.*sh\\b" (emagent-policy-match--strip-quoted command)))

(defun emagent-policy-match--spec-p (key value command words stripped)
  "Return non-nil when one :match spec KEY VALUE holds for COMMAND."
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

;;;; Elisp

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
  "Return non-nil when elisp :match spec KEY VALUE holds for PARSED form."
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

;;;; Python

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
  "Return non-nil when python :match spec KEY VALUE holds for STRIPPED code."
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

(provide 'emagent-policy-match)
;;; emagent-policy-match.el ends here
