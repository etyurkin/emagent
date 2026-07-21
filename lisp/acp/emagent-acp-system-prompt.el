;;; emagent-acp-system-prompt.el --- ACP session system prompt builders  -*- lexical-binding: t; -*-

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

;; Leaf module building the ACP session system prompt.  Split out of
;; `emagent-acp' so `emagent-acp-lifecycle' (required by `emagent-acp') can
;; build session/new and session/load system prompts without requiring
;; `emagent-acp' back.

;;; Code:

(require 'emagent-acp-custom)
(require 'emagent-mcp)
(require 'emagent-prompts)

(defun emagent-acp--system-prompt ()
  "Return the system prompt for new ACP sessions."
  (concat emagent-acp-system-prompt
          (emagent-mcp-gateway-system-prompt)
          (when emagent-acp-prefer-emacs
            (emagent-prompts--prefer-emacs-prompt))
          (when emagent-acp-prefer-emacs
            (emagent-prompts--structural-policy))))

(defun emagent-acp--session-system-prompt (&optional compressed-context)
  "Return the system prompt for session/new, optionally with COMPRESSED-CONTEXT."
  (let ((summary (string-trim (or compressed-context ""))))
    (if (string-empty-p summary)
        (emagent-acp--system-prompt)
      (concat (emagent-acp--system-prompt)
              (format "\n\n[Compressed prior conversation context]\n%s"
                      summary)))))

(provide 'emagent-acp-system-prompt)
;;; emagent-acp-system-prompt.el ends here
