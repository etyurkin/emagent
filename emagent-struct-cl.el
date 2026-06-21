;;; emagent-struct-cl.el --- Common Lisp structural editing plugin for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'emagent-struct)
(require 'emagent-elisp)

(defgroup emagent-struct-cl nil
  "Common Lisp structural editing for emagent."
  :group 'emagent-struct)

(defcustom emagent-struct-cl-use-treesit t
  "When non-nil, use tree-sitter for Common Lisp structural navigation."
  :type 'boolean
  :group 'emagent-struct-cl)

(defcustom emagent-struct-cl-validate-on-write t
  "When non-nil, reject writes to Common Lisp files that fail read validation."
  :type 'boolean
  :group 'emagent-struct-cl)

(defconst emagent-struct-cl-treesit-language 'commonlisp
  "Tree-sitter language symbol for Common Lisp.")

(defconst emagent-struct-cl-treesit-grammar-source
  '("https://github.com/theHamsta/tree-sitter-commonlisp")
  "Source URL for `treesit-install-language-grammar'.")

(defconst emagent-struct-cl-defun-symbols
  '(defun defmacro defgeneric defmethod defclass)
  "Top-level definers treated as replaceable nodes.")

(defun emagent-struct-cl--ensure-treesit-grammar-recipe ()
  (when (boundp 'treesit-language-source-alist)
    (unless (assq emagent-struct-cl-treesit-language treesit-language-source-alist)
      (add-to-list 'treesit-language-source-alist
                   (cons emagent-struct-cl-treesit-language
                         emagent-struct-cl-treesit-grammar-source)))))

(defun emagent-struct-cl-file-p (path)
  (and (stringp path)
       (string-match-p "\\.\\(?:lisp\\|cl\\)\\'" path)))

(defun emagent-struct-cl-treesit-available-p ()
  (emagent-struct--treesit-available-p
   emagent-struct-cl-treesit-language
   #'(lambda () emagent-struct-cl-use-treesit)))

(defun emagent-struct-cl--root-node (content)
  (emagent-struct--treesit-root-node
   content emagent-struct-cl-treesit-language
   #'emagent-struct-cl-treesit-available-p))

(defun emagent-struct-cl--definer-name-p (form)
  (when (and (listp form) (memq (car form) emagent-struct-cl-defun-symbols)
             (symbolp (nth 1 form)))
    (nth 1 form)))

(defun emagent-struct-cl--find-node-bounds-read (content symbol)
  (let* ((name (symbol-name (if (symbolp symbol) symbol (intern symbol))))
         (pos 0)
         (len (length content)))
    (catch 'found
      (while (< pos len)
        (while (and (< pos len) (memq (aref content pos) '(?\s ?\t ?\n ?\r)))
          (setq pos (1+ pos)))
        (when (< pos len)
          (condition-case nil
              (let* ((parsed (read-from-string content pos))
                     (form (car parsed))
                     (end (cdr parsed)))
                (when (and (listp form)
                           (memq (car form) emagent-struct-cl-defun-symbols)
                           (symbolp (nth 1 form))
                           (string= (symbol-name (nth 1 form)) name))
                  (throw 'found (cons pos end)))
                (setq pos end))
            (error (setq pos len)))))
      nil)))

(defun emagent-struct-cl--label-from-node (node)
  (pcase (treesit-node-type node)
    ((or "defun" "defmacro" "defclass" "defgeneric" "defmethod")
     (when-let ((name-node (emagent-struct--treesit-field-child node "name")))
       (format "%s:%s" (treesit-node-type node) (treesit-node-text name-node))))
    (_ (treesit-node-type node))))

(defun emagent-struct-cl--name-from-node (node)
  (when-let ((name-node (emagent-struct--treesit-field-child node "name")))
    (treesit-node-text name-node)))

(defun emagent-struct-cl--top-level-nodes (content)
  (when-let ((root (emagent-struct-cl--root-node content)))
    (cl-loop for child in (if (fboundp 'treesit-node-children)
                              (treesit-node-children root)
                            nil)
             when (emagent-struct-cl--label-from-node child)
             collect `(:name ,(emagent-struct-cl--name-from-node child)
                             :label ,(emagent-struct-cl--label-from-node child)
                             :start ,(treesit-node-start child)
                             :end ,(treesit-node-end child)))))

(defun emagent-struct-cl--find-node-bounds (content symbol)
  (or (emagent-struct-cl--find-node-bounds-read content symbol)
      (let ((target (symbol-name (if (symbolp symbol) symbol (intern symbol)))))
        (when-let ((nodes (emagent-struct-cl--top-level-nodes content)))
          (catch 'found
            (dolist (node nodes)
              (when (and (plist-get node :name) (string= (plist-get node :name) target))
                (throw 'found (cons (plist-get node :start) (plist-get node :end)))))
            nil)))))

(defun emagent-struct-cl--validate-content (content _path)
  (or (emagent-elisp--scan-parens content)
      (condition-case err
          (progn (emagent-elisp--read-forms content) nil)
        (error (error-message-string err)))))

(defun emagent-struct-cl-check-file-content (content path)
  (if-let ((err (emagent-struct-cl--validate-content content path)))
      (format "SYNTAX ERROR — %s\n\nFix the file and call check_structural_file."
              err)
    "OK"))

(defun emagent-struct-cl-check-node-content (node path)
  (emagent-struct-cl-check-file-content
   (concat node "\n") path))

(defun emagent-struct-cl-outline (content &optional _depth)
  (let ((read-error nil)
        (lines (when (emagent-struct-cl-treesit-available-p)
                 (when-let ((nodes (emagent-struct-cl--top-level-nodes content)))
                   (mapcar (lambda (n)
                             (emagent-struct--pos-label content (plist-get n :start)
                                                        (plist-get n :label)))
                           nodes)))))
    (unless lines
      (condition-case err
          (setq lines
                (mapcar (lambda (entry)
                          (let ((form (cdr entry)))
                            (format "%s:%s" (car form) (nth 1 form))))
                        (emagent-elisp--read-forms content)))
        (error (setq read-error (error-message-string err)))))
    (cond
     (lines (string-join lines "\n"))
     (read-error (format "Parse error — %s" read-error))
     (t "No forms"))))

(defun emagent-struct-cl-node-bounds (content symbol)
  (if-let ((bounds (emagent-struct-cl--find-node-bounds content symbol)))
      (format "%d:%d" (car bounds) (cdr bounds))
    (format "No form `%s' found" symbol)))

(defun emagent-struct-cl-replace-node (content symbol new-body)
  (let* ((body (string-trim (or new-body "")))
         (bounds (emagent-struct-cl--find-node-bounds content symbol)))
    (unless bounds
      (cl-return-from emagent-struct-cl-replace-node
        (format "No form `%s' found" symbol)))
    (unless (string-prefix-p "(" body)
      (cl-return-from emagent-struct-cl-replace-node
        "new_body must be a complete top-level form"))
    (let ((updated (emagent-struct--replace-region content (car bounds) (cdr bounds) body)))
      (if-let ((err (emagent-struct-cl--validate-content updated nil)))
          (format "SYNTAX ERROR after replace — %s" err)
        updated))))

(defun emagent-struct-cl--find-after-read (content symbol)
  (let* ((sym (if (symbolp symbol) symbol (intern symbol)))
         (pos 0)
         (len (length content)))
    (catch 'found
      (while (< pos len)
        (while (and (< pos len) (memq (aref content pos) '(?\s ?\t ?\n ?\r)))
          (setq pos (1+ pos)))
        (when (< pos len)
          (let ((parsed (read-from-string content pos)))
            (let ((form (car parsed))
                  (next (cdr parsed)))
              (when (or (eq (emagent-struct-cl--definer-name-p form) sym)
                        (and (listp form) (symbolp (cadr form))
                             (string= (symbol-name (cadr form)) (symbol-name sym))))
                (throw 'found next))
              (setq pos next)))))
      nil)))

(defun emagent-struct-cl--end-of-forms (content)
  (if (emagent-struct--content-blank-p content)
      0
    (let ((pos 0) (len (length content)))
      (condition-case nil
          (progn
            (while (< pos len)
              (while (and (< pos len) (memq (aref content pos) '(?\s ?\t ?\n ?\r)))
                (setq pos (1+ pos)))
              (when (< pos len)
                (setq pos (cdr (read-from-string content pos)))))
            pos)
        (error 0)))))

(defun emagent-struct-cl--find-insert-position (content after-symbol)
  (cond
   ((emagent-struct--anchor-start-p after-symbol)
    (if (emagent-struct--content-blank-p content)
        0
      (format "%s is only for new/empty files; use %s or a symbol name"
              emagent-struct-anchor-start emagent-struct-anchor-end)))
   ((emagent-struct--anchor-end-p after-symbol)
    (emagent-struct-cl--end-of-forms content))
   (t (or (emagent-struct-cl--find-after-read content after-symbol)
          (format "No form `%s' found" after-symbol)))))

(defun emagent-struct-cl-insert-after (content after-symbol node)
  (let* ((body (string-trim (or node "")))
         (pos (emagent-struct-cl--find-insert-position content after-symbol)))
    (when (string-empty-p body)
      (cl-return-from emagent-struct-cl-insert-after "node must not be empty"))
    (unless (string-prefix-p "(" body)
      (cl-return-from emagent-struct-cl-insert-after
        "node must be a complete top-level form"))
    (when (stringp pos)
      (cl-return-from emagent-struct-cl-insert-after pos))
    (let* ((blank (emagent-struct--content-blank-p content))
           (insertion (if (and (zerop pos) blank) body (concat "\n\n" body)))
           (updated (emagent-struct--replace-region content pos pos insertion)))
      (if-let ((err (emagent-struct-cl--validate-content updated nil)))
          (format "SYNTAX ERROR after insert — %s" err)
        updated))))

(emagent-struct-register-plugin
 `(:id commonlisp
   :node-label "form"
   :file-p ,#'emagent-struct-cl-file-p
   :treesit-available-p ,#'emagent-struct-cl-treesit-available-p
   :validate-on-write-p (lambda () emagent-struct-cl-validate-on-write)
   :validate-content ,#'emagent-struct-cl--validate-content
   :check-file ,#'emagent-struct-cl-check-file-content
   :check-node ,#'emagent-struct-cl-check-node-content
   :outline ,#'emagent-struct-cl-outline
   :node-bounds ,#'emagent-struct-cl-node-bounds
   :replace-node ,#'emagent-struct-cl-replace-node
   :insert-after ,#'emagent-struct-cl-insert-after))

(provide 'emagent-struct-cl)

(emagent-struct-cl--ensure-treesit-grammar-recipe)

;;; emagent-struct-cl.el ends here
