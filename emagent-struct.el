;;; emagent-struct.el --- Structural editing plugin registry for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'subr-x)

(defgroup emagent-struct nil
  "Language plugins for structural file editing in emagent."
  :group 'emagent-tools)

(defcustom emagent-struct-require-edits t
  "When non-nil and a plugin's tree-sitter grammar is available, reject write_file."
  :type 'boolean
  :group 'emagent-struct)

(defconst emagent-struct-anchor-start "__start__"
  "Anchor for structural insert: first node in a new/empty file.")

(defconst emagent-struct-anchor-end "__end__"
  "Anchor for structural insert: append after the last top-level node.")

(defvar emagent-struct--structural-write-p nil
  "Internal flag: allow write_file from structural editing tools.")

(defvar emagent-struct--plugins nil
  "List of registered structural editing plugin plists, newest first.")

(defun emagent-struct-plugin-file-p (plugin path)
  "Return non-nil when PLUGIN applies to PATH."
  (funcall (plist-get plugin :file-p) path))

(defun emagent-struct-plugin-treesit-available-p (plugin)
  "Return non-nil when PLUGIN's tree-sitter grammar is loadable."
  (funcall (plist-get plugin :treesit-available-p)))

(defun emagent-struct-register-plugin (plugin)
  "Register PLUGIN plist for structural editing."
  (unless (plist-get plugin :id)
    (error "Structural plugin missing :id"))
  (setq emagent-struct--plugins
        (cons plugin (seq-remove (lambda (p) (eq (plist-get p :id) (plist-get plugin :id)))
                                 emagent-struct--plugins))))

(defun emagent-struct-plugin-for-path (path)
  "Return the structural plugin plist for PATH, or nil."
  (seq-find (lambda (plugin) (emagent-struct-plugin-file-p plugin path))
            emagent-struct--plugins))

(defun emagent-struct-active-plugins ()
  "Return plugins whose tree-sitter grammar is currently available."
  (seq-filter #'emagent-struct-plugin-treesit-available-p emagent-struct--plugins))

(defun emagent-struct-write-blocked-message (path)
  "Return an error string when direct write to PATH must be refused."
  (when-let ((plugin (emagent-struct-plugin-for-path path)))
    (when (and emagent-struct-require-edits
               (not emagent-struct--structural-write-p)
               (emagent-struct-plugin-treesit-available-p plugin))
      (format "Tree-sitter %s is available: use structural_* tools (__start__, __end__, or a symbol), not write_file."
              (plist-get plugin :id)))))

(defun emagent-struct--call (plugin key &rest args)
  (apply (plist-get plugin key) args))

(defun emagent-struct-check-file (path content)
  "Validate file CONTENT for PATH's plugin; return a result string."
  (if-let ((plugin (emagent-struct-plugin-for-path path)))
      (emagent-struct--call plugin :check-file content path)
    (format "No structural plugin for %s" path)))

(defun emagent-struct-check-node (path node)
  "Validate NODE text for PATH's plugin; return a result string."
  (if-let ((plugin (emagent-struct-plugin-for-path path)))
      (emagent-struct--call plugin :check-node node path)
    (format "No structural plugin for %s" path)))

(defun emagent-struct-outline (path content &optional depth)
  "Return structural outline for CONTENT at PATH."
  (if-let ((plugin (emagent-struct-plugin-for-path path)))
      (emagent-struct--call plugin :outline content depth)
    (format "No structural plugin for %s" path)))

(defun emagent-struct-node-bounds (path content symbol)
  "Return node bounds string for SYMBOL in CONTENT at PATH."
  (if-let ((plugin (emagent-struct-plugin-for-path path)))
      (emagent-struct--call plugin :node-bounds content symbol)
    (format "No structural plugin for %s" path)))

(defun emagent-struct-replace-node (path content symbol new-body)
  "Replace node SYMBOL in CONTENT at PATH with NEW-BODY."
  (if-let ((plugin (emagent-struct-plugin-for-path path)))
      (emagent-struct--call plugin :replace-node content symbol new-body)
    (format "No structural plugin for %s" path)))

(defun emagent-struct-insert-after (path content after-symbol node)
  "Insert NODE after AFTER-SYMBOL in CONTENT at PATH."
  (if-let ((plugin (emagent-struct-plugin-for-path path)))
      (emagent-struct--call plugin :insert-after content after-symbol node)
    (format "No structural plugin for %s" path)))

(defun emagent-struct-validate-write (path content)
  "Return error string when CONTENT fails plugin validation for PATH."
  (when-let ((plugin (emagent-struct-plugin-for-path path)))
    (when (and (funcall (plist-get plugin :validate-on-write-p))
               (emagent-struct-plugin-treesit-available-p plugin))
      (funcall (plist-get plugin :validate-content) content path))))

(defun emagent-struct-after-save (path node)
  "Run optional plugin hook after structural save; return error string or nil."
  (when-let ((plugin (emagent-struct-plugin-for-path path))
             (fn (plist-get plugin :after-save)))
    (funcall fn node path)))

(defun emagent-struct--replace-region (content start end replacement)
  "Return CONTENT with [START,END) replaced by REPLACEMENT."
  (concat (substring content 0 start) replacement (substring content end)))

(defun emagent-struct--position-line-column (content pos)
  "Return (LINE . COLUMN) one-based for zero-based POS in CONTENT."
  (let ((line 1)
        (col 1)
        (i 0))
    (while (< i pos)
      (pcase (aref content i)
        (?\n (setq line (1+ line) col 1))
        (?\r nil)
        (_ (setq col (1+ col))))
      (setq i (1+ i)))
    (cons line col)))

(defun emagent-struct--error-at (content pos message)
  "Format MESSAGE with line:column for POS in CONTENT."
  (let ((lc (emagent-struct--position-line-column content (max 0 pos))))
    (format "line %d, column %d: %s" (car lc) (cdr lc) message)))

(defun emagent-struct--pos-label (content pos label)
  "Return LABEL with @LINE:COL suffix for POS in CONTENT."
  (let ((lc (emagent-struct--position-line-column content pos)))
    (format "%s@%d:%d" label (car lc) (cdr lc))))

(defun emagent-struct--content-blank-p (content)
  "Return non-nil when CONTENT is nil, empty, or whitespace only."
  (string-empty-p (string-trim (or content ""))))

(defun emagent-struct--anchor-start-p (symbol)
  (let ((s (string-trim (or symbol ""))))
    (or (string-empty-p s) (string= s emagent-struct-anchor-start))))

(defun emagent-struct--anchor-end-p (symbol)
  (string= (string-trim (or symbol "")) emagent-struct-anchor-end))

(declare-function treesit-available-p "treesit")
(declare-function treesit-language-available-p "treesit")
(declare-function treesit-parse-string "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-text "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-node-child "treesit")
(declare-function treesit-node-children "treesit")
(declare-function treesit-node-child-count "treesit")
(declare-function treesit-node-child-by-field-name "treesit")

(defun emagent-struct--treesit-field-child (node field)
  (cond
   ((fboundp 'treesit-node-child-by-field-name)
    (treesit-node-child-by-field-name node field))
   ((fboundp 'treesit-node-child-by-field)
    (treesit-node-child-by-field node field))))

(defun emagent-struct--treesit-node-child0 (node)
  (cond
   ((fboundp 'treesit-node-child) (treesit-node-child node 0))
   ((fboundp 'treesit-node-first-child) (treesit-node-first-child node))))

(defun emagent-struct--treesit-available-p (language use-treesit)
  (and use-treesit
       (require 'treesit nil t)
       (fboundp 'treesit-available-p)
       (treesit-available-p)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p language)))

(defun emagent-struct--treesit-root-node (content language available-p)
  (when (funcall available-p)
    (ignore-errors (treesit-parse-string content language))))

(provide 'emagent-struct)

(require 'emagent-struct-elisp)
(require 'emagent-struct-cl)
(require 'emagent-struct-python)

;;; emagent-struct.el ends here
