;;; emagent-acp-claude.el --- Claude ACP provider adapter  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Claude-specific ACP adapter hooks.  Most Claude behavior is handled by
;; shared ACP normalization; add hooks here when claude-agent-acp diverges.

;;; Code:

(require 'emagent-acp-provider)
(require 'emagent-acp-gate)

(defun emagent-acp-claude--detect-p (state)
  "Return non-nil when STATE's agent is claude-agent-acp."
  (when-let ((launch (emagent-acp--agent-launch-string state)))
    (string-match-p "claude-agent-acp" launch)))

(defun emagent-acp-claude--enrich-tool-call (_state update)
  "Normalize a claude-agent-acp tool-call update before display merging.
claude-agent-acp echoes rawInput fields as the title on tool_call_update:
title = rawInput.command for Bash, title = rawInput.description for Agent.
Strip the redundant title so the stored display name (e.g. \"Terminal\",
\"Task\") is kept and the rawInput field becomes the visible detail."
  (let* ((raw (or (map-elt update 'rawInput) (map-elt update 'arguments)))
         (title (map-elt update 'title)))
    (if (and title (listp raw)
             (let ((cmd (alist-get 'command raw))
                   (desc (alist-get 'description raw)))
               (or (and cmd (string= title cmd))
                   (and desc (string= title desc)))))
        (cons (cons 'title nil) update)
      update)))

(defun emagent-acp-claude--external-gate-reason (_state)
  'claude-agent-sdk)

(emagent-acp--register-provider
 'claude
 :detect #'emagent-acp-claude--detect-p
 :enrich-tool-call #'emagent-acp-claude--enrich-tool-call
 :external-gate-reason #'emagent-acp-claude--external-gate-reason)

(provide 'emagent-acp-claude)
;;; emagent-acp-claude.el ends here
