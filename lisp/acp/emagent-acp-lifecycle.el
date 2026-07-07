;;; emagent-acp-lifecycle.el --- ACP lifecycle module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin
(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)
(require 'emagent-acp-state)
(require 'emagent-acp-protocol)
(require 'emagent-acp-model)
(require 'emagent-acp-prompt)
(require 'emagent-acp-gate)
(require 'emagent-acp-usage)
(require 'emagent-acp-notify)

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

;;; Code:
(cl-defun emagent-acp--authenticate (&key state method-id on-ready)
  "Send an authenticate request with METHOD-ID, then connect the session.

Called when `initialize' returns authMethods (e.g. cursor_login).
The authenticate call completes the credential handshake so the agent
grants full plan access (including Auto model) to this ACP session."
  (emagent-acp--progress state (format "authenticating (%s)…" method-id))
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-authenticate-request :method-id method-id)
   :on-success (lambda (_response)
                 (emagent-acp--connect-session :state state :on-ready on-ready))
   :on-failure (lambda (error _raw)
                 (emagent-log "authenticate %s failed: %s — proceeding anyway"
                              method-id
                              (or (map-elt error 'message) (format "%s" error)))
                 (emagent-acp--connect-session :state state :on-ready on-ready))))

(cl-defun emagent-acp--initialize (&key state on-ready)
  (emagent-acp--progress state "initializing ACP…")
  (emagent-acp--send-request
   :state state
   :request (if emagent-acp-file-access
                (emagent-acp-make-initialize-request
                 :protocol-version 1
                 :client-info `((name . "emagent")
                                (title . "Emacs Emagent")
                                (version . "1.0.2"))
                 :read-text-file-capability t
                 :write-text-file-capability t)
              (emagent-acp-make-initialize-request
               :protocol-version 1
               :client-info `((name . "emagent")
                              (title . "Emacs Emagent")
                              (version . "1.0.2"))))
   :on-success (lambda (response)
                 (map-put! state :initialized t)
                 (map-put! state :mcp-http (emagent-acp--mcp-http-capable-p response))
                 (emagent-acp--infer-external-tool-gate-from-agent state)
                 (emagent-acp--infer-external-tool-gate-from-initialize-response state response)
                 (emagent-acp--maybe-log-external-tool-gate-proactive state)
                 (let ((auth-methods (append (map-elt response 'authMethods) nil)))
                   (if-let ((method-id (map-elt (seq-find
                                                 (lambda (m) (map-elt m 'id))
                                                 auth-methods)
                                                'id)))
                       (emagent-acp--authenticate
                        :state state :method-id method-id :on-ready on-ready)
                     (emagent-acp--connect-session :state state :on-ready on-ready))))
   :on-failure (lambda (error _raw)
                 (emagent-acp--fail-connect
                  state
                  (format "emagent: initialize failed: %s"
                          (or (map-elt error 'message) (format "%s" error)))))))

(defun emagent-acp--mcp-http-capable-p (initialize-response)
  "Return non-nil when INITIALIZE-RESPONSE advertises http MCP support."
  (let ((value (map-nested-elt initialize-response
                               '(agentCapabilities mcpCapabilities http))))
    (and value (not (eq value :false)) (not (eq value :json-false)))))

(cl-defun emagent-acp--session-ready (&key state session-id on-ready resumed)
  (map-put! state :session-id session-id)
  (map-put! state :ready t)
  (emagent-acp--persist-session-id state session-id)
  (emagent-acp--hydrate-session-permissions state session-id)
  (emagent-tools-set-project-directory (emagent-acp--session-cwd state))
  (emagent-acp--progress state (if resumed "resumed" "connected"))
  (when-let ((buffer (emagent-acp--chat-buffer state)))
    (with-current-buffer buffer
      (pcase emagent-chat-provider
        ('cursor (emagent-chat-seed-cursor-slash-commands))
        ('claude
         (when (null emagent-chat-slash-commands)
           (emagent-log "loading slash commands from agent…"))))))
  (emagent-acp--start-rss-timer state)
  (emagent-acp--reveal-buffer state)
  (when on-ready (funcall on-ready)))

(cl-defun emagent-acp--new-session (&key state on-ready compressed-context)
  (emagent-acp--progress state "creating session…")
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-new-request
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (map-elt state :mcp-http)
                                                       (emagent-acp--chat-buffer state))
             :meta `((systemPrompt . ((append . ,(emagent-acp--session-system-prompt
                                                  compressed-context))))))
   :on-success (lambda (response)
                 (unless (fboundp 'emagent-acp--configure-model)
                   (require 'emagent-acp-model))
                 (emagent-acp--configure-model
                  :state state
                  :session-id (map-elt response 'sessionId)
                  :response response
                  :on-ready on-ready))
   :on-failure (lambda (error _raw)
                 (emagent-acp--fail-connect
                  state
                  (format "emagent: session/new failed: %s"
                          (or (map-elt error 'message) (format "%s" error)))))))

(cl-defun emagent-acp--load-session (&key state session-id on-ready)
  (emagent-acp--progress state "resuming session…")
  (map-put! state :replaying-history t)
  (emagent-acp--send-request
   :state state
   :request (emagent-acp-make-session-load-request
             :session-id session-id
             :cwd (emagent-acp--session-cwd state)
             :mcp-servers (emagent-mcp-session-servers (map-elt state :mcp-http)
                                                       (emagent-acp--chat-buffer state))
             :meta `((systemPrompt . ((append . ,(emagent-acp--system-prompt))))))
   :on-success (lambda (response)
                 (map-put! state :replaying-history nil)
                 (unless (fboundp 'emagent-acp--configure-model)
                   (require 'emagent-acp-model))
                 (emagent-acp--configure-model
                  :state state
                  :session-id session-id
                  :response response
                  :on-ready on-ready
                  :resumed t))
   :on-failure (lambda (_error _raw)
                 (map-put! state :replaying-history nil)
                 (emagent-acp--progress state "resume failed, creating session…")
                 (when-let ((buf (emagent-acp--chat-buffer state)))
                   (with-current-buffer buf
                     (let ((was-modified (buffer-modified-p)))
                       (unwind-protect
                           (emagent-chat-clear-session-id)
                         (set-buffer-modified-p was-modified)))))
                 (emagent-acp--new-session :state state :on-ready on-ready))))

(cl-defun emagent-acp--connect-session (&key state on-ready)
  (emagent-acp--progress state "connecting session…")
  (let ((saved (emagent-acp--saved-session-id state)))
    (if (and saved (not (string-empty-p saved)))
        (emagent-acp--load-session :state state :session-id saved :on-ready on-ready)
      (emagent-acp--new-session :state state :on-ready on-ready))))

(cl-defun emagent-acp-start (&key client chat-buffer on-ready on-reveal callbacks)
  "Start an emagent ACP session in CHAT-BUFFER.

ON-REVEAL is called once when the chat buffer should be shown.
CALLBACKS is an alist of rendering callbacks keyed by:
  :cb-chunk, :cb-thought, :cb-finish, :cb-fail, :cb-slash-commands."
  (when (and emagent-acp-prefer-emacs (not emagent-acp-file-access))
    (emagent-log "prefer-Emacs mode works best with `emagent-acp-file-access'"))
  (when emagent-acp-trace
    (setq emagent-acp-logging-enabled t))
  (with-current-buffer chat-buffer
    (emagent-chat-clear-slash-commands)
    (setq emagent-acp--session (emagent-acp--make-state :client client
                                                        :chat-buffer chat-buffer
                                                        :on-reveal on-reveal))
    (map-put! emagent-acp--session :provider (or emagent-chat-provider 'cursor))
    (dolist (cb callbacks)
      (map-put! emagent-acp--session (car cb) (cdr cb)))
    (emagent-mcp-register-session :token (emagent-mcp-buffer-token)
                                  :cwd (emagent-chat--session-directory)
                                  :buffer chat-buffer
                                  :prefer-emacs emagent-acp-prefer-emacs
                                  :acp t)
    (emagent-acp--progress emagent-acp--session "starting agent…")
    (emagent-acp--subscribe :state emagent-acp--session)
    (emagent-acp--initialize :state emagent-acp--session :on-ready on-ready)
    emagent-acp--session))

(provide 'emagent-acp-lifecycle)
;;; emagent-acp-lifecycle.el ends here
(declare-function emagent-acp--progress "emagent-acp-prompt")
(declare-function emagent-acp--send-request "emagent-acp-prompt")
(declare-function emagent-acp--infer-external-tool-gate-from-agent "emagent-acp-gate")
(declare-function emagent-acp--infer-external-tool-gate-from-initialize-response "emagent-acp-gate")
(declare-function emagent-acp--maybe-log-external-tool-gate-proactive "emagent-acp-gate")
(declare-function emagent-acp--fail-connect "emagent-acp-prompt")
(declare-function emagent-acp--hydrate-session-permissions "emagent-acp-permit")
(declare-function emagent-acp--persist-session-id "emagent-acp-usage")
(declare-function emagent-acp--session-cwd "emagent-acp-usage")
(declare-function emagent-acp--chat-buffer "emagent-acp-usage")
(declare-function emagent-acp--reveal-buffer "emagent-acp-prompt")
(declare-function emagent-acp--system-prompt "emagent-acp")
(declare-function emagent-acp--session-system-prompt "emagent-acp")
(declare-function emagent-acp--configure-model "emagent-acp-model")
(declare-function emagent-acp--saved-session-id "emagent-acp-usage")
(declare-function emagent-acp--subscribe "emagent-acp-notify")
