;;; emagent-chat-model-ui.el --- `/model' override link helpers  -*- lexical-binding: t; -*-

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

;; Parsing, building, and rendering the `/model' override marker link
;; (`[[emagent://AGENT/MODEL][short]]').  Kept out of `emagent-chat' so
;; `emagent-chat-reasoning', `emagent-chat-actions', and `emagent-chat-slash'
;; can use the marker without a load cycle through the facade.

;;; Code:

(require 'emagent-model)
(require 'emagent-session)

(defconst emagent-chat--model-link-re
  "\\[\\[emagent://\\([^][]+\\)\\]\\(?:\\[\\([^][]*\\)\\]\\)?\\]"
  "Matches a `/model' override link `[[emagent://AGENT/MODEL][short]]'.
Group 1 is the link target `AGENT/MODEL' (shown on hover); group 2 the
short model label shown as the link text.  Being an org link, the
marker is fontified by org, survives saving the session to disk, and
reveals the full agent/model id on hover.  The `emagent://' scheme
tags this as the model marker so unrelated links a user pastes are not
mistaken for it.")

(defun emagent-chat--model-link-path-id (path)
  "Return the model id from a link PATH `AGENT/MODEL' (or bare MODEL).
PATH may carry a leading `//' authority slash from the raw link.  The
agent is the first segment; the model id is the rest, so model ids are
returned intact even if they contain slashes."
  (let ((path (string-remove-prefix "//" path)))
    (if (string-match "/" path)
        (substring path (match-end 0))
      path)))

(defun emagent-chat--region-turn-model (start end)
  "Return the model id of the first `/model' link between START and END."
  (save-excursion
    (goto-char start)
    (when (re-search-forward emagent-chat--model-link-re end t)
      (emagent-chat--model-link-path-id (match-string-no-properties 1)))))

(defun emagent-chat--strip-model-links (text)
  "Remove `/model' override links from outgoing TEXT.
The marker is client UI — the slash command is documented as never sent
to the agent."
  (string-trim
   (replace-regexp-in-string
    (concat "[ \t]*" emagent-chat--model-link-re) "" text)))

(defun emagent-chat--model-link (model-id)
  "Return the `/model' marker link for MODEL-ID.
The visible text is the short model name; the link target is
`agent/full-model-id', revealed on hover.  The `emagent://' scheme
\(never shown) tags this as the model marker so unrelated links a user
pastes are not mistaken for it."
  (let* ((agent (emagent-session-agent))
         (short (or (emagent-model-normalize-id model-id) model-id))
         (path (if agent (format "%s/%s" agent model-id) model-id)))
    (format "[[emagent://%s][%s]]" path short)))

(defun emagent-chat--follow-model-link (path &optional _prefix)
  "Describe the `/model' override link PATH when activated."
  (message "Model for this turn: %s (delete the link to cancel)"
           (string-remove-prefix "//" path)))

(defun emagent-chat--model-link-help-echo (_window object position)
  "Tooltip for a `/model' link: the `agent/model' target.

Arguments: OBJECT, POSITION."
  (with-current-buffer (if (bufferp object) object (current-buffer))
    (save-excursion
      (goto-char position)
      (when (or (looking-at emagent-chat--model-link-re)
                (and (search-backward "[[" (max (point-min) (- position 200)) t)
                     (looking-at emagent-chat--model-link-re)))
        (format "Model for this turn: %s" (match-string-no-properties 1))))))

(provide 'emagent-chat-model-ui)
;;; emagent-chat-model-ui.el ends here