;;; emagent-acp-protocol-json.el --- ACP JSON-RPC helpers for emagent  -*- lexical-binding: t; -*-

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

;; JSON parse/serialize helpers and JSON-RPC error constructors.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'json)
(require 'emagent-acp-custom)

(defconst emagent-acp--jsonrpc-version "2.0")

(defun emagent-acp--parse-json (json)
  "Parse JSON string into an alist."
  (json-parse-string json :object-type 'alist :null-object nil :false-object nil))

(defun emagent-acp--serialize-json (object)
  "Serialize OBJECT to a JSON string with trailing newline."
  (concat (json-serialize object) "\n"))

(cl-defun emagent-acp-make-error (&key code message data)
  "Create a JSON-RPC error object with CODE and MESSAGE.

Arguments: DATA."
  (unless code (error ":code is required"))
  (unless message (error ":message is required"))
  (let ((err `((code . ,code) (message . ,message))))
    (when data (nconc err `((data . ,data))))
    err))

(defun emagent-acp--make-internal-error (message)
  "Create a synthetic internal error (JSON-RPC code -32603) with MESSAGE."
  (emagent-acp-make-error :code -32603 :message message))

(defun emagent-acp--parse-stderr-api-error (raw-output)
  "Parse RAW-OUTPUT from stderr; return a structured error alist or nil."
  (when (string-match
         "Attempt [0-9]+ failed with status [0-9]+\\. Retrying.*ApiError: \\({.*}\\)"
         raw-output)
    (let ((json (match-string 1 raw-output)))
      (condition-case nil
          (let-alist (emagent-acp--parse-json json)
            (condition-case nil
                (map-elt (emagent-acp--parse-json .error.message) 'error)
              (error nil)))
        (error nil)))))

(cl-defun emagent-acp--make-message (&key json object)
  "Wrap JSON string and parsed OBJECT into a message alist."
  (list (cons :object object) (cons :json json)))

(provide 'emagent-acp-protocol-json)

;;; emagent-acp-protocol-json.el ends here
