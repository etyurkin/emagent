;;; emagent-elisp.el --- Elisp validation and structural editing for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'subr-x)

(defgroup emagent-elisp nil
  "Elisp validation and structural editing helpers for emagent."
  :group 'emagent-tools)

(defcustom emagent-elisp-validate-on-write t
  "When non-nil, reject writes to .el files that fail Elisp validation."
  :type 'boolean
  :group 'emagent-elisp)

(defcustom emagent-elisp-byte-compile-on-check t
  "When non-nil, run `byte-compile-file' during `check_elisp_file' validation."
  :type 'boolean
  :group 'emagent-elisp)

(defcustom emagent-elisp-use-treesit t
  "When non-nil, use tree-sitter for structural Elisp navigation when available."
  :type 'boolean
  :group 'emagent-elisp)

(defcustom emagent-elisp-require-structural-edits t
  "When non-nil and tree-sitter elisp is available, reject write_file on .el files.
Use elisp_replace_defun and elisp_insert_after_form instead."
  :type 'boolean
  :group 'emagent-elisp)

(defcustom emagent-elisp-eval-after-structural-edit t
  "When non-nil, eval the new/changed form after a successful structural .el write."
  :type 'boolean
  :group 'emagent-elisp)

(defvar emagent-elisp--structural-write-p nil
  "Internal flag: allow write_file from structural Elisp tools.")

(defconst emagent-elisp-anchor-start "__start__"
  "Anchor for `emagent-elisp-insert-after-form': first form in a new/empty file.")

(defconst emagent-elisp-anchor-end "__end__"
  "Anchor for `emagent-elisp-insert-after-form': append after the last form.")

(defconst emagent-elisp-treesit-language 'elisp
  "Tree-sitter language symbol for Emacs Lisp.")

(defconst emagent-elisp-treesit-grammar-source
  '("https://github.com/Wilfred/tree-sitter-elisp")
  "Source URL for `treesit-install-language-grammar'.")

(declare-function treesit-install-language-grammar "treesit")
(declare-function treesit-available-p "treesit")
(declare-function treesit-language-available-p "treesit")
(declare-function treesit-parse-string "treesit")
(declare-function treesit-query-capture "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-text "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-node-child "treesit")
(declare-function treesit-node-children "treesit")
(declare-function treesit-node-child-count "treesit")
(declare-function treesit-node-child-by-field-name "treesit")

(defun emagent-elisp--ensure-treesit-grammar-recipe ()
  "Register the elisp grammar in `treesit-language-source-alist' when missing."
  (when (boundp 'treesit-language-source-alist)
    (unless (assq emagent-elisp-treesit-language treesit-language-source-alist)
      (add-to-list 'treesit-language-source-alist
                   (cons emagent-elisp-treesit-language
                         emagent-elisp-treesit-grammar-source)))))

(defun emagent-elisp--treesit-field-child (node field)
  "Return the child of NODE for grammar FIELD, across Emacs versions."
  (cond
   ((fboundp 'treesit-node-child-by-field-name)
    (treesit-node-child-by-field-name node field))
   ((fboundp 'treesit-node-child-by-field)
    (treesit-node-child-by-field node field))))

(defun emagent-elisp--treesit-node-child0 (node)
  "Return the first child of NODE, or nil."
  (cond
   ((fboundp 'treesit-node-child)
    (treesit-node-child node 0))
   ((fboundp 'treesit-node-first-child)
    (treesit-node-first-child node))))

(defun emagent-elisp-treesit-available-p ()
  "Return non-nil when tree-sitter and the elisp grammar are loadable."
  (and emagent-elisp-use-treesit
       (require 'treesit nil t)
       (fboundp 'treesit-available-p)
       (treesit-available-p)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p emagent-elisp-treesit-language)))

(defun emagent-elisp--treesit-root-node (content)
  "Parse CONTENT with tree-sitter and return the root node, or nil."
  (when (emagent-elisp-treesit-available-p)
    (ignore-errors
      (treesit-parse-string content emagent-elisp-treesit-language))))

(defun emagent-elisp--treesit-pos-label (content pos label)
  "Return LABEL with @LINE:COL suffix for POS in CONTENT."
  (let ((lc (emagent-elisp--position-line-column content pos)))
    (format "%s@%d:%d" label (car lc) (cdr lc))))

(defun emagent-elisp--treesit-name-from-node (node)
  "Return the defined name from a tree-sitter defun-like NODE, or nil."
  (pcase (treesit-node-type node)
    ("function_definition"
     (when-let ((name-node (emagent-elisp--treesit-field-child node "name")))
       (treesit-node-text name-node)))
    ("macro_definition"
     (when-let ((name-node (emagent-elisp--treesit-field-child node "name")))
       (treesit-node-text name-node)))
    ("list"
     (when-let* ((kw-node (emagent-elisp--treesit-node-child0 node))
                 (kw (treesit-node-text kw-node))
                 (name-node (and (fboundp 'treesit-node-child)
                                 (treesit-node-child node 1))))
       (when (member kw '("defun" "defsubst" "defmacro" "cl-defun"))
         (treesit-node-text name-node))))
    ("special_form"
     (let ((text (treesit-node-text node)))
       (when (string-match
              "^(\\(defvar\\|defconst\\|defcustom\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)\\(?:\\(?:\\sw\\|\\s_\\)\\|[-!$%&*+/:<=>?@^_.~#]*\\)*\\)"
              text)
         (match-string 2 text))))
    (_ nil)))

(defun emagent-elisp--treesit-label-from-node (node)
  "Return a short label for tree-sitter form NODE."
  (pcase (treesit-node-type node)
    ("function_definition"
     (when-let ((name (emagent-elisp--treesit-name-from-node node)))
       (format "defun:%s" name)))
    ("macro_definition"
     (when-let ((name (emagent-elisp--treesit-name-from-node node)))
       (format "defmacro:%s" name)))
    ("special_form"
     (let ((text (treesit-node-text node)))
       (when (string-match
              "^(\\(defvar\\|defconst\\|defcustom\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)\\(?:\\(?:\\sw\\|\\s_\\)\\|[-!$%&*+/:<=>?@^_.~#]*\\)*\\)"
              text)
         (format "%s:%s" (match-string 1 text) (match-string 2 text)))))
    ("list"
     (when-let* ((name (emagent-elisp--treesit-name-from-node node))
                 (kw-node (emagent-elisp--treesit-node-child0 node)))
       (format "%s:%s" (treesit-node-text kw-node) name)))
    (_ (treesit-node-type node))))

(defun emagent-elisp--treesit-top-level-forms (content)
  "Return top-level form plists from CONTENT using tree-sitter.
Each plist has :node :name :label :start :end."
  (when-let ((root (emagent-elisp--treesit-root-node content)))
    (unless (string= (treesit-node-type root) "source_file")
      (cl-return-from emagent-elisp--treesit-top-level-forms nil))
    (let (forms)
      (cond
       ((fboundp 'treesit-node-children)
        (dolist (child (treesit-node-children root))
          (when-let ((label (emagent-elisp--treesit-label-from-node child)))
            (push `(:node ,child
                          :name ,(emagent-elisp--treesit-name-from-node child)
                          :label ,label
                          :start ,(treesit-node-start child)
                          :end ,(treesit-node-end child))
                  forms))))
       ((fboundp 'treesit-node-child-count)
        (let ((i 0))
          (while (< i (treesit-node-child-count root))
            (when-let* ((child (treesit-node-child root i))
                        (label (emagent-elisp--treesit-label-from-node child)))
              (push `(:node ,child
                            :name ,(emagent-elisp--treesit-name-from-node child)
                            :label ,label
                            :start ,(treesit-node-start child)
                            :end ,(treesit-node-end child))
                    forms))
            (setq i (1+ i))))))
      (nreverse forms))))

(defun emagent-elisp--position-line-column (content pos)
  "Return (LINE . COLUMN) one-based for zero-based POS in CONTENT."
  (let ((line 1)
        (col 1)
        (i 0))
    (while (< i pos)
      (pcase (aref content i)
        (?\n
         (setq line (1+ line) col 1))
        (?\r
         nil)
        (_ (setq col (1+ col))))
      (setq i (1+ i)))
    (cons line col)))

(defun emagent-elisp--error-at (content pos message)
  "Format MESSAGE with line:column for POS in CONTENT."
  (let ((lc (emagent-elisp--position-line-column content (max 0 pos))))
    (format "line %d, column %d: %s" (car lc) (cdr lc) message)))

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
  (let ((pos 0)
        (len (length content))
        (forms nil))
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
          (let ((byte-compile-debug 1)
                (inhibit-message t))
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
  "Like `emagent-elisp--validate-content' but also byte-compile when enabled."
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
         (err (emagent-elisp--validate-content wrapped)))
    (if err
        (format "SYNTAX ERROR — %s\n\nFix the form and call check_elisp again before eval."
                err)
      "OK")))

(defun emagent-elisp-check-file-content (content &optional path)
  "Validate Elisp file CONTENT.  Return \"OK\" or an error description."
  (if-let ((err (emagent-elisp--validate-content-strict content path)))
      (format "SYNTAX ERROR — %s\n\nFix the file and call check_elisp_file before writing."
              err)
    "OK"))

(defun emagent-elisp-elisp-file-p (path)
  "Return non-nil when PATH looks like an Emacs Lisp file."
  (and (stringp path)
       (string-match-p "\\.el\\'" path)))

(defun emagent-elisp--write-file-blocked-message (path)
  "Return an error string when direct write to PATH must be refused."
  (when (and emagent-elisp-require-structural-edits
             (not emagent-elisp--structural-write-p)
             (emagent-elisp-treesit-available-p)
             (emagent-elisp-elisp-file-p path))
    "Tree-sitter elisp is available: use elisp_replace_defun or elisp_insert_after_form (__start__, __end__, or a symbol), not write_file."))

(defun emagent-elisp--defun-name-p (form)
  "Return defined name when FORM is a defun-like top-level form."
  (when (and (listp form) (memq (car form) '(defun cl-defun))
             (symbolp (nth 1 form)))
    (nth 1 form)))

(defun emagent-elisp--find-defun-bounds-sexp (content symbol)
  "Return (START . END) for SYMBOL's defun in CONTENT via read, or nil."
  (let* ((name (symbol-name (if (symbolp symbol) symbol (intern symbol))))
         (pos 0)
         (len (length content)))
    (catch 'found
      (while (< pos len)
        (while (and (< pos len)
                    (memq (aref content pos) '(?\s ?\t ?\n ?\r)))
          (setq pos (1+ pos)))
        (when (< pos len)
          (let ((start pos))
            (condition-case nil
                (let* ((parsed (read-from-string content pos))
                       (form (car parsed))
                       (end (cdr parsed)))
                  (when (and (listp form)
                             (memq (car form) '(defun cl-defun))
                             (symbolp (nth 1 form))
                             (string= (symbol-name (nth 1 form)) name))
                    (throw 'found (cons start end)))
                  (setq pos end))
              (error (setq pos len))))))
      nil)))

(defun emagent-elisp--find-defun-bounds-treesit (content symbol)
  "Return (START . END) for SYMBOL's defun in CONTENT via tree-sitter, or nil."
  (let ((target (symbol-name (if (symbolp symbol) symbol (intern symbol)))))
    (when-let ((forms (emagent-elisp--treesit-top-level-forms content)))
      (catch 'found
        (dolist (form forms)
          (let ((name (plist-get form :name))
                (label (plist-get form :label)))
            (when (and name
                       (string= name target)
                       (string-match-p
                        "\\`\\(?:cl-defun\\|def\\(?:un\\|macro\\|subst\\)\\):"
                        label))
              (throw 'found (cons (plist-get form :start) (plist-get form :end))))))
        nil))))

(defun emagent-elisp--find-defun-bounds (content symbol)
  "Return (START . END) for SYMBOL's defun in CONTENT, or nil."
  (or (emagent-elisp--find-defun-bounds-sexp content symbol)
      (emagent-elisp--find-defun-bounds-treesit content symbol)))

(defun emagent-elisp-defun-bounds (content symbol)
  "Return \"START:END\" string for SYMBOL in CONTENT, or an error string.
Uses sexp scanning first, then tree-sitter when the buffer is broken."
  (if-let ((bounds (emagent-elisp--find-defun-bounds content symbol)))
      (format "%d:%d" (car bounds) (cdr bounds))
    (format "No defun `%s' found" symbol)))

(defun emagent-elisp--replace-region (content start end replacement)
  "Return CONTENT with [START,END) replaced by REPLACEMENT."
  (concat (substring content 0 start) replacement (substring content end)))

(defun emagent-elisp-replace-defun (content symbol new-body)
  "Replace defun SYMBOL in CONTENT with complete NEW-BODY form.
Return updated content or an error string."
  (let* ((body (string-trim (or new-body "")))
         (bounds (emagent-elisp--find-defun-bounds content symbol)))
    (unless bounds
      (cl-return-from emagent-elisp-replace-defun
        (format "No defun `%s' found" symbol)))
    (unless (string-prefix-p "(" body)
      (cl-return-from emagent-elisp-replace-defun
        "new_body must be a complete (defun ...) form"))
    (let ((updated (emagent-elisp--replace-region content (car bounds) (cdr bounds) body)))
      (if-let ((err (emagent-elisp--validate-content-strict updated)))
          (format "SYNTAX ERROR after replace — %s" err)
        updated))))

(defun emagent-elisp--find-after-form-end-read (content symbol)
  "Return byte position after top-level form SYMBOL in CONTENT via read, or nil."
  (let* ((sym (if (symbolp symbol) symbol (intern symbol)))
         (pos 0)
         (len (length content)))
    (catch 'found
      (while (< pos len)
        (while (and (< pos len)
                    (memq (aref content pos) '(?\s ?\t ?\n ?\r)))
          (setq pos (1+ pos)))
        (when (< pos len)
          (let ((parsed (read-from-string content pos)))
            (let ((form (car parsed))
                  (next (cdr parsed)))
              (when (or (eq (emagent-elisp--defun-name-p form) sym)
                        (and (listp form) (symbolp (cadr form))
                             (string= (symbol-name (cadr form)) (symbol-name sym))))
                (throw 'found next))
              (setq pos next)))))
      nil)))

(defun emagent-elisp--find-after-form-end-treesit (content symbol)
  "Return byte position after top-level form SYMBOL in CONTENT via tree-sitter."
  (let ((target (symbol-name (if (symbolp symbol) symbol (intern symbol)))))
    (when-let ((forms (emagent-elisp--treesit-top-level-forms content)))
      (catch 'found
        (dolist (form forms)
          (when (and (plist-get form :name)
                     (string= (plist-get form :name) target))
            (throw 'found (plist-get form :end))))
        nil))))

(defun emagent-elisp--find-after-form-end (content symbol)
  "Return byte position after the top-level form named SYMBOL in CONTENT."
  (or (emagent-elisp--find-after-form-end-read content symbol)
      (emagent-elisp--find-after-form-end-treesit content symbol)))

(defun emagent-elisp--content-blank-p (content)
  "Return non-nil when CONTENT is nil, empty, or whitespace only."
  (string-empty-p (string-trim (or content ""))))

(defun emagent-elisp--anchor-start-p (symbol)
  "Return non-nil when SYMBOL anchors insertion at file start."
  (let ((s (string-trim (or symbol ""))))
    (or (string-empty-p s) (string= s emagent-elisp-anchor-start))))

(defun emagent-elisp--anchor-end-p (symbol)
  "Return non-nil when SYMBOL anchors insertion after the last form."
  (string= (string-trim (or symbol "")) emagent-elisp-anchor-end))

(defun emagent-elisp--end-of-forms (content)
  "Return byte position after the last top-level form in CONTENT, or 0 when blank."
  (if (emagent-elisp--content-blank-p content)
      0
    (let ((pos 0)
          (len (length content)))
      (condition-case nil
          (progn
            (while (< pos len)
              (while (and (< pos len)
                          (memq (aref content pos) '(?\s ?\t ?\n ?\r)))
                (setq pos (1+ pos)))
              (when (< pos len)
                (setq pos (cdr (read-from-string content pos)))))
            pos)
        (error 0)))))

(defun emagent-elisp--find-insert-position (content after-symbol)
  "Return byte position to insert after in CONTENT for AFTER-SYMBOL anchor."
  (cond
   ((emagent-elisp--anchor-start-p after-symbol)
    (if (emagent-elisp--content-blank-p content)
        0
      (format "%s is only for new/empty files; use %s or a symbol name"
              emagent-elisp-anchor-start emagent-elisp-anchor-end)))
   ((emagent-elisp--anchor-end-p after-symbol)
    (emagent-elisp--end-of-forms content))
   (t (or (emagent-elisp--find-after-form-end content after-symbol)
          (format "No top-level form `%s' found" after-symbol)))))

(defun emagent-elisp-insert-after-form (content after-symbol form)
  "Insert complete top-level FORM in CONTENT after AFTER-SYMBOL anchor.
AFTER-SYMBOL is __start__ (first form in empty file), __end__ (append),
or a symbol name.  Return updated content or an error string."
  (let* ((body (string-trim (or form "")))
         (pos (emagent-elisp--find-insert-position content after-symbol)))
    (when (string-empty-p body)
      (cl-return-from emagent-elisp-insert-after-form "form must not be empty"))
    (unless (string-prefix-p "(" body)
      (cl-return-from emagent-elisp-insert-after-form
        "form must be a complete top-level sexp"))
    (when (stringp pos)
      (cl-return-from emagent-elisp-insert-after-form pos))
    (let* ((blank (emagent-elisp--content-blank-p content))
           (insertion (if (and (zerop pos) blank)
                          body
                        (concat "\n\n" body)))
           (updated (emagent-elisp--replace-region content pos pos insertion)))
      (if-let ((err (emagent-elisp--validate-content-strict updated)))
          (format "SYNTAX ERROR after insert — %s" err)
        updated))))

(defun emagent-elisp--form-label (form)
  "Return a short label string describing top-level FORM."
  (cond
   ((not (listp form)) (prin1-to-string form))
   ((eq (car form) 'defun) (format "defun:%s" (nth 1 form)))
   ((eq (car form) 'cl-defun) (format "cl-defun:%s" (nth 1 form)))
   ((eq (car form) 'defvar) (format "defvar:%s" (nth 1 form)))
   ((eq (car form) 'defcustom) (format "defcustom:%s" (nth 1 form)))
   ((eq (car form) 'defconst) (format "defconst:%s" (nth 1 form)))
   ((symbolp (car form)) (format "%s" (car form)))
   (t "?")))

(defun emagent-elisp--sexp-tree-read (content)
  "Return outline lines for CONTENT via read, or signal."
  (let ((forms (emagent-elisp--read-forms content))
        (lines nil))
    (dolist (entry forms)
      (push (emagent-elisp--form-label (cdr entry)) lines))
    (nreverse lines)))

(defun emagent-elisp--sexp-tree-treesit (content)
  "Return outline lines for CONTENT via tree-sitter, or nil."
  (when-let ((forms (emagent-elisp--treesit-top-level-forms content)))
    (mapcar (lambda (form)
              (emagent-elisp--treesit-pos-label
               content (plist-get form :start) (plist-get form :label)))
            forms)))

(defun emagent-elisp-sexp-tree (content &optional _depth)
  "Return a shallow outline of top-level forms in CONTENT.
Uses tree-sitter when available (lines include @LINE:COL); otherwise read.
DEPTH is ignored (reserved)."
  (let ((read-error nil)
        (lines (when (emagent-elisp-treesit-available-p)
                 (emagent-elisp--sexp-tree-treesit content))))
    (unless lines
      (condition-case err
          (setq lines (emagent-elisp--sexp-tree-read content))
        (error (setq read-error (error-message-string err))))
      (unless lines
        (setq lines (emagent-elisp--sexp-tree-treesit content))))
    (cond
     (lines (string-join lines "\n"))
     (read-error (format "Parse error — %s" read-error))
     (t "No forms"))))

(provide 'emagent-elisp)

(emagent-elisp--ensure-treesit-grammar-recipe)

;;; emagent-elisp.el ends here
