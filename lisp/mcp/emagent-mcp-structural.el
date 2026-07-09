;;; emagent-mcp-structural.el --- lisp-sitter MCP tools for emagent -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; MCP tool entries gated by `emagent-struct-available-p'.  Appended to
;; `emagent-mcp--tools' in emagent-mcp.el.

;;; Code:

(require 'emagent-struct)
(require 'emagent-tools)

(declare-function emagent-mcp--arg "emagent-mcp")
(declare-function emagent-mcp--bool "emagent-mcp")

(defun emagent-mcp--structural-path-prop ()
  "JSON schema property for a structural tool file path."
  '(("path" . ((type . "string")
               (description . "Path to the file, relative to session root.")))))

(defun emagent-mcp--structural-symbol-prop ()
  "JSON schema property for a top-level form name."
  '(("symbol" . ((type . "string")
                 (description . "Top-level form name (defun, defvar, ...).")))))

(defun emagent-mcp--structural-tool (name description properties required handler async-handler)
  "Build a lisp-sitter MCP tool registry entry."
  (list name description properties required handler
        :available #'emagent-struct-available-p
        :async async-handler))

(defconst emagent-mcp--structural-tools
  (list
   (emagent-mcp--structural-tool
    "check_structural_file"
    "[lisp-sitter] Validate a whole file (.el, .lisp, .cl, .scm). Returns \"OK\" or a syntax error."
    (emagent-mcp--structural-path-prop)
    '("path")
    (lambda (args)
      (emagent-tool-check-structural-file (emagent-mcp--arg args "path")))
    (lambda (args cb)
      (emagent-tool-check-structural-file-async cb (emagent-mcp--arg args "path"))))
   (emagent-mcp--structural-tool
    "check_structural_node"
    "[lisp-sitter] Validate a complete top-level node without saving."
    (append (emagent-mcp--structural-path-prop)
            '(("node" . ((type . "string")
                         (description . "Complete top-level node text to validate.")))))
    '("path" "node")
    (lambda (args)
      (emagent-tool-check-structural-node (emagent-mcp--arg args "path")
                                          (emagent-mcp--arg args "node")))
    (lambda (args cb)
      (emagent-tool-check-structural-node-async
       cb (emagent-mcp--arg args "path") (emagent-mcp--arg args "node"))))
   (emagent-mcp--structural-tool
    "structural_find_errors"
    "[lisp-sitter] List MISSING tokens and ERROR nodes in a Lisp file."
    (emagent-mcp--structural-path-prop)
    '("path")
    (lambda (args)
      (emagent-tool-structural-find-errors (emagent-mcp--arg args "path")))
    (lambda (args cb)
      (emagent-tool-structural-find-errors-async cb (emagent-mcp--arg args "path"))))
   (emagent-mcp--structural-tool
    "structural_tree"
    "[lisp-sitter] Outline of top-level forms. Set depth > 1 for sub-form navigation."
    (append (emagent-mcp--structural-path-prop)
            '(("depth" . ((type . "integer")
                          (description . "Outline depth (default 1).")))))
    '()
    (lambda (args)
      (emagent-tool-structural-tree (emagent-mcp--arg args "path")
                                    (emagent-mcp--arg args "depth")))
    (lambda (args cb)
      (emagent-tool-structural-tree-async
       cb (emagent-mcp--arg args "path") (emagent-mcp--arg args "depth"))))
   (emagent-mcp--structural-tool
    "structural_bounds"
    "[lisp-sitter] Return byte positions START:END for a named top-level form."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop))
    '("path" "symbol")
    (lambda (args)
      (emagent-tool-structural-bounds (emagent-mcp--arg args "path")
                                      (emagent-mcp--arg args "symbol")))
    (lambda (args cb)
      (emagent-tool-structural-bounds-async
       cb (emagent-mcp--arg args "path") (emagent-mcp--arg args "symbol"))))
   (emagent-mcp--structural-tool
    "structural_get"
    "[lisp-sitter] Return the full text of a named top-level form."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop))
    '("path" "symbol")
    (lambda (args)
      (emagent-tool-structural-get (emagent-mcp--arg args "path")
                                   (emagent-mcp--arg args "symbol")))
    (lambda (args cb)
      (emagent-tool-structural-get-async
       cb (emagent-mcp--arg args "path") (emagent-mcp--arg args "symbol"))))
   (emagent-mcp--structural-tool
    "structural_context"
    "[lisp-sitter] Return outline, bounds, and full text of each top-level form."
    (emagent-mcp--structural-path-prop)
    '("path")
    (lambda (args)
      (emagent-tool-structural-context (emagent-mcp--arg args "path")))
    (lambda (args cb)
      (emagent-tool-structural-context-async cb (emagent-mcp--arg args "path"))))
   (emagent-mcp--structural-tool
    "structural_replace"
    "[lisp-sitter] Replace one top-level form with complete new text. Validates before save."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop)
            '(("new_body" . ((type . "string")
                             (description . "Complete replacement form text.")))))
    '("path" "symbol" "new_body")
    (lambda (args)
      (emagent-tool-structural-replace (emagent-mcp--arg args "path")
                                       (emagent-mcp--arg args "symbol")
                                       (emagent-mcp--arg args "new_body")))
    (lambda (args cb)
      (emagent-tool-structural-replace-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "symbol")
       (emagent-mcp--arg args "new_body"))))
   (emagent-mcp--structural-tool
    "structural_insert"
    "[lisp-sitter] Insert a complete top-level form after __start__, __end__, or a symbol name."
    (append (emagent-mcp--structural-path-prop)
            '(("after_symbol" . ((type . "string")
                                 (description . "__start__, __end__, or existing form name.")))
              ("node" . ((type . "string")
                         (description . "Complete top-level form text to insert.")))))
    '("path" "after_symbol" "node")
    (lambda (args)
      (emagent-tool-structural-insert (emagent-mcp--arg args "path")
                                      (emagent-mcp--arg args "after_symbol")
                                      (emagent-mcp--arg args "node")))
    (lambda (args cb)
      (emagent-tool-structural-insert-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "after_symbol")
       (emagent-mcp--arg args "node"))))
   (emagent-mcp--structural-tool
    "structural_complete"
    "[lisp-sitter] Complete an unbalanced s-expression by appending missing closing parens."
    '(("lang" . ((type . "string")
                 (description . "elisp, commonlisp, or scheme.")))
      ("body" . ((type . "string")
                 (description . "Incomplete form text."))))
    '("lang" "body")
    (lambda (args)
      (emagent-tool-structural-complete (emagent-mcp--arg args "lang")
                                        (emagent-mcp--arg args "body")))
    (lambda (args cb)
      (emagent-tool-structural-complete-async
       cb (emagent-mcp--arg args "lang") (emagent-mcp--arg args "body"))))
   (emagent-mcp--structural-tool
    "structural_format"
    "[lisp-sitter] Re-indent a file (depth-based). Pass write=true to save."
    (append (emagent-mcp--structural-path-prop)
            '(("write" . ((type . "boolean")
                          (description . "When true, save the formatted file.")))))
    '("path")
    (lambda (args)
      (emagent-tool-structural-format (emagent-mcp--arg args "path")
                                      (emagent-mcp--bool args "write")))
    (lambda (args cb)
      (emagent-tool-structural-format-async
       cb (emagent-mcp--arg args "path") (emagent-mcp--bool args "write"))))
   (emagent-mcp--structural-tool
    "structural_rename"
    "[lisp-sitter] Rename a top-level form and its call sites."
    (append (emagent-mcp--structural-path-prop)
            '(("old" . ((type . "string") (description . "Current form name.")))
              ("new" . ((type . "string") (description . "New form name.")))
              ("refs" . ((type . "boolean")
                         (description . "Also rename quoted references.")))
              ("no_refs" . ((type . "boolean")
                            (description . "Rename definition only.")))))
    '("path" "old" "new")
    (lambda (args)
      (emagent-tool-structural-rename (emagent-mcp--arg args "path")
                                      (emagent-mcp--arg args "old")
                                      (emagent-mcp--arg args "new")
                                      (emagent-mcp--bool args "refs")
                                      (emagent-mcp--bool args "no_refs")))
    (lambda (args cb)
      (emagent-tool-structural-rename-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "old")
       (emagent-mcp--arg args "new")
       (emagent-mcp--bool args "refs")
       (emagent-mcp--bool args "no_refs"))))
   (emagent-mcp--structural-tool
    "structural_wrap"
    "[lisp-sitter] Wrap the body of a named form in a construct (progn, let, if)."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop)
            '(("in" . ((type . "string")
                       (description . "Wrapper: progn, let, let*, if, when, ...")))
              ("bindings" . ((type . "string")
                             (description . "let/let* bindings, e.g. ((x 1)).")))
              ("condition" . ((type . "string")
                              (description . "Condition for if/when wrappers.")))))
    '("path" "symbol" "in")
    (lambda (args)
      (emagent-tool-structural-wrap (emagent-mcp--arg args "path")
                                    (emagent-mcp--arg args "symbol")
                                    (emagent-mcp--arg args "in")
                                    (emagent-mcp--arg args "bindings")
                                    (emagent-mcp--arg args "condition")))
    (lambda (args cb)
      (emagent-tool-structural-wrap-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "symbol")
       (emagent-mcp--arg args "in")
       (emagent-mcp--arg args "bindings")
       (emagent-mcp--arg args "condition"))))
   (emagent-mcp--structural-tool
    "structural_remove"
    "[lisp-sitter] Remove a top-level form (optionally keep call site stubs)."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop)
            '(("keep_calls" . ((type . "boolean")
                               (description . "Leave call sites unchanged.")))))
    '("path" "symbol")
    (lambda (args)
      (emagent-tool-structural-remove (emagent-mcp--arg args "path")
                                      (emagent-mcp--arg args "symbol")
                                      (emagent-mcp--bool args "keep_calls")))
    (lambda (args cb)
      (emagent-tool-structural-remove-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "symbol")
       (emagent-mcp--bool args "keep_calls"))))
   (emagent-mcp--structural-tool
    "structural_move"
    "[lisp-sitter] Move a top-level form after __start__, __end__, or a symbol name."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop)
            '(("after" . ((type . "string")
                          (description . "__start__, __end__, or symbol name.")))))
    '("path" "symbol" "after")
    (lambda (args)
      (emagent-tool-structural-move (emagent-mcp--arg args "path")
                                    (emagent-mcp--arg args "symbol")
                                    (emagent-mcp--arg args "after")))
    (lambda (args cb)
      (emagent-tool-structural-move-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "symbol")
       (emagent-mcp--arg args "after"))))
   (emagent-mcp--structural-tool
    "structural_substitute"
    "[lisp-sitter] Replace a sub-expression inside a named form."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop)
            '(("pattern" . ((type . "string") (description . "S-expression to replace.")))
              ("replacement" . ((type . "string")
                                 (description . "Replacement s-expression.")))))
    '("path" "symbol" "pattern" "replacement")
    (lambda (args)
      (emagent-tool-structural-substitute (emagent-mcp--arg args "path")
                                          (emagent-mcp--arg args "symbol")
                                          (emagent-mcp--arg args "pattern")
                                          (emagent-mcp--arg args "replacement")))
    (lambda (args cb)
      (emagent-tool-structural-substitute-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "symbol")
       (emagent-mcp--arg args "pattern")
       (emagent-mcp--arg args "replacement"))))
   (emagent-mcp--structural-tool
    "structural_extract"
    "[lisp-sitter] Extract a sub-expression into a new function."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop)
            '(("pattern" . ((type . "string") (description . "Sub-expression to extract.")))
              ("name" . ((type . "string") (description . "New function name.")))
              ("params" . ((type . "string")
                            (description . "Comma-separated parameter names.")))))
    '("path" "symbol" "pattern" "name")
    (lambda (args)
      (emagent-tool-structural-extract (emagent-mcp--arg args "path")
                                       (emagent-mcp--arg args "symbol")
                                       (emagent-mcp--arg args "pattern")
                                       (emagent-mcp--arg args "name")
                                       (emagent-mcp--arg args "params")))
    (lambda (args cb)
      (emagent-tool-structural-extract-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "symbol")
       (emagent-mcp--arg args "pattern")
       (emagent-mcp--arg args "name")
       (emagent-mcp--arg args "params"))))
   (emagent-mcp--structural-tool
    "structural_callers"
    "[lisp-sitter] Find all callers of a symbol in a file."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop))
    '("path" "symbol")
    (lambda (args)
      (emagent-tool-structural-callers (emagent-mcp--arg args "path")
                                       (emagent-mcp--arg args "symbol")))
    (lambda (args cb)
      (emagent-tool-structural-callers-async
       cb (emagent-mcp--arg args "path") (emagent-mcp--arg args "symbol"))))
   (emagent-mcp--structural-tool
    "structural_instrument"
    "[lisp-sitter] Instrument a form with tracing (--with, --at, or --wrap)."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop)
            '(("with" . ((type . "string") (description . "Tracing form for the body.")))
              ("at" . ((type . "string") (description . "Sub-expression to instrument.")))
              ("wrap" . ((type . "string") (description . "Wrapper around sub-expression.")))))
    '("path" "symbol")
    (lambda (args)
      (emagent-tool-structural-instrument (emagent-mcp--arg args "path")
                                          (emagent-mcp--arg args "symbol")
                                          (emagent-mcp--arg args "with")
                                          (emagent-mcp--arg args "at")
                                          (emagent-mcp--arg args "wrap")))
    (lambda (args cb)
      (emagent-tool-structural-instrument-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "symbol")
       (emagent-mcp--arg args "with")
       (emagent-mcp--arg args "at")
       (emagent-mcp--arg args "wrap"))))
   (emagent-mcp--structural-tool
    "structural_flatten"
    "[lisp-sitter] Inline all call sites of a function and remove the definition."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop))
    '("path" "symbol")
    (lambda (args)
      (emagent-tool-structural-flatten (emagent-mcp--arg args "path")
                                       (emagent-mcp--arg args "symbol")))
    (lambda (args cb)
      (emagent-tool-structural-flatten-async
       cb (emagent-mcp--arg args "path") (emagent-mcp--arg args "symbol"))))
   (emagent-mcp--structural-tool
    "structural_convert_let"
    "[lisp-sitter] Convert between let and let* bindings in a form."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop)
            '(("to" . ((type . "string")
                       (description . "let or let*.")))))
    '("path" "symbol" "to")
    (lambda (args)
      (emagent-tool-structural-convert-let (emagent-mcp--arg args "path")
                                           (emagent-mcp--arg args "symbol")
                                           (emagent-mcp--arg args "to")))
    (lambda (args cb)
      (emagent-tool-structural-convert-let-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "symbol")
       (emagent-mcp--arg args "to"))))
   (emagent-mcp--structural-tool
    "structural_splice"
    "[lisp-sitter] Paredit splice: remove a wrapper form, elevating its body."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop)
            '(("pattern" . ((type . "string")
                            (description . "Parenthesised list to splice.")))))
    '("path" "symbol" "pattern")
    (lambda (args)
      (emagent-tool-structural-splice (emagent-mcp--arg args "path")
                                      (emagent-mcp--arg args "symbol")
                                      (emagent-mcp--arg args "pattern")))
    (lambda (args cb)
      (emagent-tool-structural-splice-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "symbol")
       (emagent-mcp--arg args "pattern"))))
   (emagent-mcp--structural-tool
    "structural_raise"
    "[lisp-sitter] Paredit raise: promote a sub-expression over its enclosing list."
    (append (emagent-mcp--structural-path-prop)
            (emagent-mcp--structural-symbol-prop)
            '(("pattern" . ((type . "string")
                            (description . "Sub-expression to raise.")))))
    '("path" "symbol" "pattern")
    (lambda (args)
      (emagent-tool-structural-raise (emagent-mcp--arg args "path")
                                     (emagent-mcp--arg args "symbol")
                                     (emagent-mcp--arg args "pattern")))
    (lambda (args cb)
      (emagent-tool-structural-raise-async
       cb (emagent-mcp--arg args "path")
       (emagent-mcp--arg args "symbol")
       (emagent-mcp--arg args "pattern"))))))

(provide 'emagent-mcp-structural)
;;; emagent-mcp-structural.el ends here
