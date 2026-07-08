;;; emagent-acp-file.el --- ACP file operation handlers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ACP fs/read_text_file and fs/write_text_file request handlers.
;; Routes file operations through Emacs tools for path safety.

;;; Code:

(require 'cl-lib)
(require 'emagent-tools)
(require 'emagent-guard)
(require 'emagent-acp-custom)
(require 'emagent-acp-protocol)
(require 'emagent-session)

(declare-function emagent-acp--prepare-interactive-context "emagent-acp")
(declare-function emagent-acp--notify-user "emagent-acp")
(declare-function emagent-acp--chat-buffer "emagent-acp-usage")

(defvar emagent-tools--root-boundary)
(defvar emagent-tools--project-directory)

(defun emagent-acp--fs-session-root (state)
  "Return the project root ACP fs/* operations must stay within, or nil.

Mirrors the boundary the MCP dispatcher binds for its tools; without it the
fs/* handlers would resolve agent-supplied paths with no project confinement."
  (when-let ((buf (emagent-acp--chat-buffer state)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (ignore-errors (emagent-session-project-directory))))))

(defun emagent-acp--fs-unavailable-response (method)
  (emagent-acp-make-error
   :code -32601
   :message (format "%s disabled; use the external agent's project file tools"
                    method)))

(defun emagent-acp--fs-send (client make request-id &rest response-args)
  "Send the fs response built by MAKE for REQUEST-ID over CLIENT.
MAKE is a `*-text-file-response' constructor; RESPONSE-ARGS are its remaining
keyword arguments (`:content' or `:error')."
  (emagent-acp-send-response
   :client client
   :response (apply make :request-id request-id response-args)))

(defun emagent-acp--fs-send-error (client make request-id code message)
  "Send an fs error response with CODE and MESSAGE (see `emagent-acp--fs-send')."
  (emagent-acp--fs-send client make request-id
                        :error (emagent-acp-make-error :code code :message message)))

(cl-defun emagent-acp--on-fs-read (&key state emagent-acp-request)
  (let ((client (emagent-acp-state-client state))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path)))
        (make #'emagent-acp-make-fs-read-text-file-response))
    (if (not emagent-acp-file-access)
        (emagent-acp--fs-send client make request-id
                              :error (emagent-acp--fs-unavailable-response
                                      "fs/read_text_file"))
      (let* ((emagent-tools--root-boundary (emagent-acp--fs-session-root state))
             (emagent-tools--project-directory
              (or emagent-tools--root-boundary emagent-tools--project-directory))
             (verdict (emagent-guard-check 'read path)))
        (if (not (emagent-guard-allow-p verdict))
            (emagent-acp--fs-send-error client make request-id -32603
                                        (emagent-guard-reason verdict))
          (condition-case err
              (let* ((canonical (emagent-guard-resolved verdict))
                     (line (or (map-nested-elt emagent-acp-request '(params line)) 1))
                     (limit (map-nested-elt emagent-acp-request '(params limit)))
                     (content (emagent-tools--read-file-content canonical line limit)))
                (emagent-acp--fs-send client make request-id :content content))
            (file-missing
             (emagent-acp--fs-send-error client make request-id -32002
                                         "Resource not found"))
            (error
             (emagent-acp--fs-send-error client make request-id -32603
                                         (error-message-string err)))))))))

(cl-defun emagent-acp--on-fs-write (&key state emagent-acp-request)
  (let ((client (emagent-acp-state-client state))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path)))
        (content (or (map-nested-elt emagent-acp-request '(params content)) ""))
        (make #'emagent-acp-make-fs-write-text-file-response))
    (if (not emagent-acp-file-access)
        (emagent-acp--fs-send client make request-id
                              :error (emagent-acp--fs-unavailable-response
                                      "fs/write_text_file"))
      (let* ((emagent-tools--root-boundary (emagent-acp--fs-session-root state))
             (emagent-tools--project-directory
              (or emagent-tools--root-boundary emagent-tools--project-directory))
             (verdict (emagent-guard-check 'write path)))
        (if (not (emagent-guard-allow-p verdict))
            (emagent-acp--fs-send-error client make request-id -32603
                                        (emagent-guard-reason verdict))
          (let ((resolved (emagent-guard-resolved verdict)))
            (when emagent-acp-confirm-fs-writes
              (emagent-acp--prepare-interactive-context state))
            (condition-case err
                (if (and emagent-acp-confirm-fs-writes
                         (not (emagent-tools--confirm-write
                               'emagent-tool-write-file resolved content
                               (emagent-acp--chat-buffer state))))
                    (emagent-acp--fs-send-error client make request-id -32603
                                                "Write denied by user")
                  (let ((written (emagent-tools--write-file-content resolved content)))
                    (emagent-acp--notify-user
                     state (format "emagent: wrote %s (C-/ to undo in that buffer)"
                                   written))
                    (emagent-acp--fs-send client make request-id)))
              (error
               (emagent-acp--fs-send-error client make request-id -32603
                                           (error-message-string err))))))))))

(provide 'emagent-acp-file)
;;; emagent-acp-file.el ends here
