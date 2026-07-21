;;; emagent-mcp-util.el --- MCP tool argument helpers  -*- lexical-binding: t; -*-

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

;; Leaf module for reading MCP tool call arguments out of the JSON args
;; hash-table.  Split out of `emagent-mcp' so `emagent-mcp-structural' (built
;; on top of `emagent-mcp-registry' via `emagent-mcp--tools') can use these accessors
;; without requiring `emagent-mcp' back.

;;; Code:

(defun emagent-mcp--arg (args key &optional default)
  "Return KEY from ARGS hash-table, or DEFAULT when missing or JSON null."
  (let ((value (and (hash-table-p args) (gethash key args))))
    (if (or (null value) (eq value :null))
        default
      value)))

(defun emagent-mcp--bool (args key)
  "Return non-nil when KEY in ARGS is JSON true."
  (eq (emagent-mcp--arg args key) t))

(provide 'emagent-mcp-util)
;;; emagent-mcp-util.el ends here
