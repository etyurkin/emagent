;;; emagent-chat-tools-fontify.el --- Tool-line font-lock and face repair  -*- lexical-binding: t; -*-

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

;; Font-lock keywords and face-repair helpers for Thinking-block tool lines.
;; Depends on formatting constants from `emagent-chat-tools-format'.

;;; Code:

(require 'emagent-chat-tools-format)

(defconst emagent-chat--tool-decision-re
  " \\((Allow: [^)\n]+)\\|(Allow)\\|(Denied)\\)"
  "Regexp matching a permission decision or source annotation on a tool-call line.
No end-anchor: the annotation may appear before a path on the same line.")

(defun emagent-chat--repair-tool-line-faces (start end)
  "Re-apply path and decision faces after org font-lock on tool-call lines.

Arguments: START, END."
  (when (and start end (< start end))
    (with-silent-modifications
      (save-excursion
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward "\\=/[^ \t\n]+" end t))
          (let ((s (match-beginning 0))
                (e (match-end 0)))
            (remove-list-of-text-properties s e '(face))
            (put-text-property s e 'face 'emagent-tool-detail)))
        (goto-char start)
        (when (re-search-forward emagent-chat--tool-decision-re end t)
          (let ((s (match-beginning 1))
                (e (match-end 1)))
            (remove-list-of-text-properties s e '(face))
            (put-text-property s e 'face 'emagent-tool-permission-decision)))))))

(defconst emagent-chat--tool-line-font-lock-keywords
  `((,(concat "^→ .*?" emagent-chat--tool-decision-re)
     1 'emagent-tool-permission-decision prepend))
  "Font-lock keywords that re-apply the permission decision face.
Org font-lock removes manually applied `face' properties on every
fontification pass, so the grey decision suffix on a single-line tool call
must be reapplied as a keyword rather than set once at insertion time.
Block tool calls carry their decision as an in-block comment, which org
fontifies with the comment face natively.")

(defun emagent-chat--fontify-tool-line (start end)
  "Font-lock tool line START..END and repair org emphasis on paths.
Only touch START..END — do not re-fontify the whole response tail."
  (when (and start end (<= start end))
    (ignore-errors
      (font-lock-ensure start end))
    (emagent-chat--repair-tool-line-faces start end)))

(defun emagent-chat--fontify-tool-block (start end)
  "Fontify an Org src-block tool display between START and END natively.
Only touch START..END — do not re-fontify the whole response tail."
  (when (and start end (<= start end))
    (ignore-errors
      (font-lock-ensure start end))))

(provide 'emagent-chat-tools-fontify)
;;; emagent-chat-tools-fontify.el ends here
