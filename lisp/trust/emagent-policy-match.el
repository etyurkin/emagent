;;; emagent-policy-match.el --- Matchers for emagent policy rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Evgeniy Tyurkin

;;; Commentary:

;; Tokenizes shell commands and evaluates declarative :match specs from
;; `emagent-policy-rules-shell'.

;;; Code:

(require 'cl-lib)

(declare-function split-string-shell-argument "subr")

(defun emagent-policy-match--words (command)
  "Split shell COMMAND into words, respecting simple quotes."
  (condition-case nil
      (split-string-shell-argument (string-trim command))
    (error (split-string (string-trim command) "[[:space:]]+" t))))

(defun emagent-policy-match--strip-quoted (command)
  "Remove single- and double-quoted spans from COMMAND."
  (replace-regexp-in-string "[\"'][^\"']*[\"']" "" command))

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
