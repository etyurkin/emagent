;;; emagent-struct-python.el --- Python structural editing plugin for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'emagent-struct)

(defgroup emagent-struct-python nil
  "Python structural editing for emagent."
  :group 'emagent-struct)

(defcustom emagent-struct-python-use-treesit t
  "When non-nil, use tree-sitter for Python structural navigation."
  :type 'boolean
  :group 'emagent-struct-python)

(defcustom emagent-struct-python-validate-on-write t
  "When non-nil, reject writes to .py files that fail parse validation."
  :type 'boolean
  :group 'emagent-struct-python)

(defconst emagent-struct-python-treesit-language 'python
  "Tree-sitter language symbol for Python.")

(defconst emagent-struct-python-treesit-grammar-source
  '("https://github.com/tree-sitter/tree-sitter-python")
  "Source URL for `treesit-install-language-grammar'.")

(defun emagent-struct-python--ensure-treesit-grammar-recipe ()
  (when (boundp 'treesit-language-source-alist)
    (unless (assq emagent-struct-python-treesit-language treesit-language-source-alist)
      (add-to-list 'treesit-language-source-alist
                   (cons emagent-struct-python-treesit-language
                         emagent-struct-python-treesit-grammar-source)))))

(defun emagent-struct-python-file-p (path)
  (and (stringp path) (string-match-p "\\.py\\'" path)))

(defun emagent-struct-python-treesit-available-p ()
  (emagent-struct--treesit-available-p
   emagent-struct-python-treesit-language
   #'(lambda () emagent-struct-python-use-treesit)))

(defun emagent-struct-python--root-node (content)
  (emagent-struct--treesit-root-node
   content emagent-struct-python-treesit-language
   #'emagent-struct-python-treesit-available-p))

(defun emagent-struct-python--top-level-nodes (content)
  (when-let ((root (emagent-struct-python--root-node content)))
    (unless (member (treesit-node-type root) '("module" "block"))
      (cl-return-from emagent-struct-python--top-level-nodes nil))
    (cl-loop for child in (if (fboundp 'treesit-node-children)
                              (treesit-node-children root)
                            (let (cs i)
                              (while (< i (treesit-node-child-count root))
                                (push (treesit-node-child root i) cs)
                                (setq i (1+ i)))
                              (nreverse cs)))
             when (member (treesit-node-type child)
                          '("function_definition" "class_definition"
                            "decorated_definition"))
             collect (let* ((target (if (string= (treesit-node-type child)
                                                 "decorated_definition")
                                        (cl-loop for sub in (treesit-node-children child)
                                                 when (member (treesit-node-type sub)
                                                              '("function_definition"
                                                                "class_definition"))
                                                 return sub)
                                      child))
                            (name-node (emagent-struct--treesit-field-child target "name")))
                       (when name-node
                         `(:node ,child
                                 :target ,target
                                 :name ,(treesit-node-text name-node)
                                 :label ,(format "%s:%s"
                                                  (treesit-node-type target)
                                                  (treesit-node-text name-node))
                                 :start ,(treesit-node-start child)
                                 :end ,(treesit-node-end child)))))))

(defun emagent-struct-python--validate-content (content _path)
  (unless (emagent-struct-python-treesit-available-p)
    (cl-return-from emagent-struct-python--validate-content
      "Python tree-sitter grammar is not available"))
  (condition-case err
      (progn (emagent-struct-python--root-node content) nil)
    (error (error-message-string err))))

(defun emagent-struct-python-check-file-content (content path)
  (if-let ((err (emagent-struct-python--validate-content content path)))
      (format "SYNTAX ERROR — %s\n\nFix the file and call check_structural_file."
              err)
    "OK"))

(defun emagent-struct-python-check-node-content (node _path)
  (emagent-struct-python-check-file-content node nil))

(defun emagent-struct-python-outline (content &optional _depth)
  (if-let ((nodes (emagent-struct-python--top-level-nodes content)))
      (string-join
       (mapcar (lambda (n)
                 (emagent-struct--pos-label content (plist-get n :start)
                                            (plist-get n :label)))
               nodes)
       "\n")
    (if (emagent-struct-python-treesit-available-p)
        "No top-level functions or classes"
      "Python tree-sitter grammar is not available")))

(defun emagent-struct-python--find-node (content symbol)
  (let ((target (string-trim (or symbol ""))))
    (cl-find target (emagent-struct-python--top-level-nodes content)
             :key (lambda (n) (plist-get n :name)) :test #'string=)))

(defun emagent-struct-python-node-bounds (content symbol)
  (if-let ((node (emagent-struct-python--find-node content symbol)))
      (format "%d:%d" (plist-get node :start) (plist-get node :end))
    (format "No function or class `%s' found" symbol)))

(defun emagent-struct-python-replace-node (content symbol new-body)
  (let* ((body (string-trim (or new-body "")))
         (node (emagent-struct-python--find-node content symbol)))
    (unless node
      (cl-return-from emagent-struct-python-replace-node
        (format "No function or class `%s' found" symbol)))
    (when (string-empty-p body)
      (cl-return-from emagent-struct-python-replace-node "new_body must not be empty"))
    (let ((updated (emagent-struct--replace-region content (plist-get node :start)
                                                   (plist-get node :end) body)))
      (if-let ((err (emagent-struct-python--validate-content updated nil)))
          (format "SYNTAX ERROR after replace — %s" err)
        updated))))

(defun emagent-struct-python--end-of-nodes (content)
  (if-let ((nodes (emagent-struct-python--top-level-nodes content)))
      (plist-get (car (last nodes)) :end)
    0))

(defun emagent-struct-python--find-insert-position (content after-symbol)
  (cond
   ((emagent-struct--anchor-start-p after-symbol)
    (if (emagent-struct--content-blank-p content)
        0
      (format "%s is only for new/empty files; use %s or a symbol name"
              emagent-struct-anchor-start emagent-struct-anchor-end)))
   ((emagent-struct--anchor-end-p after-symbol)
    (emagent-struct-python--end-of-nodes content))
   (t (if-let ((node (emagent-struct-python--find-node content after-symbol)))
          (plist-get node :end)
        (format "No function or class `%s' found" after-symbol)))))

(defun emagent-struct-python-insert-after (content after-symbol node)
  (let* ((body (string-trim (or node "")))
         (pos (emagent-struct-python--find-insert-position content after-symbol)))
    (when (string-empty-p body)
      (cl-return-from emagent-struct-python-insert-after "node must not be empty"))
    (when (stringp pos)
      (cl-return-from emagent-struct-python-insert-after pos))
    (let* ((blank (emagent-struct--content-blank-p content))
           (insertion (if (and (zerop pos) blank) body (concat "\n\n" body)))
           (updated (emagent-struct--replace-region content pos pos insertion)))
      (if-let ((err (emagent-struct-python--validate-content updated nil)))
          (format "SYNTAX ERROR after insert — %s" err)
        updated))))

(emagent-struct-register-plugin
 `(:id python
   :node-label "function or class"
   :file-p ,#'emagent-struct-python-file-p
   :treesit-available-p ,#'emagent-struct-python-treesit-available-p
   :validate-on-write-p (lambda () emagent-struct-python-validate-on-write)
   :validate-content ,#'emagent-struct-python--validate-content
   :check-file ,#'emagent-struct-python-check-file-content
   :check-node ,#'emagent-struct-python-check-node-content
   :outline ,#'emagent-struct-python-outline
   :node-bounds ,#'emagent-struct-python-node-bounds
   :replace-node ,#'emagent-struct-python-replace-node
   :insert-after ,#'emagent-struct-python-insert-after))

(provide 'emagent-struct-python)

(emagent-struct-python--ensure-treesit-grammar-recipe)

;;; emagent-struct-python.el ends here
