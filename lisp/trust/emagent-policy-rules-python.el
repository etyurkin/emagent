;;; emagent-policy-rules-python.el --- Python policy rule table  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; SPDX-License-Identifier: MIT

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6

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

;; Declarative python rules for `emagent-policy-check-python'.

;;; Code:

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

(provide 'emagent-policy-rules-python)
;;; emagent-policy-rules-python.el ends here
