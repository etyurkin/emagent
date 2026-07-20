;;; emagent-model.el --- ACP model-id helpers  -*- lexical-binding: t; -*-

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

;; Pure helpers for normalizing and labeling ACP model ids (e.g. Cursor's
;; `default[]' / `[thinking=true]' forms).  Kept as a leaf so the ACP model
;; picker and the session model accessor can share them without depending on
;; the chat UI.

;;; Code:

(defun emagent-model-canonical-id (model)
  "Return MODEL id in the form Cursor ACP expects (keep bracket suffixes)."
  (when model
    (if (member model '("auto" "default"))
        "default[]"
      model)))

(defun emagent-model-normalize-id (model)
  "Return a short user-facing label for MODEL.
Strips key=value annotations (e.g. [thinking=true]) and empty brackets ([]).
Maps Cursor default[] to auto."
  (when model
    (let ((stripped (replace-regexp-in-string
                     "\\[\\([^]]*=[^]]*\\)?\\]" "" model)))
      (if (member stripped '("default" "auto")) "auto" stripped))))

(defun emagent-model-choice-label-parts (id &optional name)
  "Return (PRIMARY . SUFFIX) for model ID.
PRIMARY is the base id without bracket annotations; SUFFIX is brackets
plus an optional parenthetical alias when NAME differs from the normalized id."
  (when id
    (let* ((bracket (and (string-match "\\[" id) (match-beginning 0)))
           (base (if bracket (substring id 0 bracket) id))
           (brackets (if bracket (substring id bracket) ""))
           (short-name (and name
                            (not (string= name (emagent-model-normalize-id id)))
                            name)))
      (cons base (concat brackets (if short-name (format " (%s)" short-name) ""))))))

(defun emagent-model-choice-label (id &optional name)
  "Return a `completing-read' label for model ID with the full canonical id.
When NAME differs from the normalized ID (e.g. Auto vs default[]), append it."
  (let ((parts (emagent-model-choice-label-parts id name)))
    (when parts (concat (car parts) (cdr parts)))))

(defun emagent-model-choice-label-display (id &optional name)
  "Like `emagent-model-choice-label', with theme faces for model and details.
The faces `emagent-model-choice-model'/`emagent-model-choice-detail' are
referenced by symbol and resolved at render time, so this stays a leaf.

Arguments: ID, NAME."
  (let ((parts (emagent-model-choice-label-parts id name)))
    (when parts
      (concat (propertize (car parts) 'face 'emagent-model-choice-model)
              (if (string-empty-p (cdr parts))
                  ""
                (propertize (cdr parts) 'face 'emagent-model-choice-detail))))))

(provide 'emagent-model)
;;; emagent-model.el ends here
