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

(cl-defun emagent-acp--on-fs-read (&key state emagent-acp-request)
  (let ((client (emagent-acp-state-client state))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path))))
    (if (not emagent-acp-file-access)
        (emagent-acp-send-response
         :client client
         :response (emagent-acp-make-fs-read-text-file-response
                    :request-id request-id
                    :error (emagent-acp--fs-unavailable-response "fs/read_text_file")))
      (let* ((emagent-tools--root-boundary (emagent-acp--fs-session-root state))
             (emagent-tools--project-directory
              (or emagent-tools--root-boundary emagent-tools--project-directory))
             (verdict (emagent-guard-check 'read path)))
        (if (not (emagent-guard-allow-p verdict))
            (emagent-acp-send-response
             :client client
             :response (emagent-acp-make-fs-read-text-file-response
                        :request-id request-id
                        :error (emagent-acp-make-error
                                :code -32603 :message (emagent-guard-reason verdict))))
          (condition-case err
              (let* ((canonical (emagent-guard-resolved verdict))
                     (line (or (map-nested-elt emagent-acp-request '(params line)) 1))
                     (limit (map-nested-elt emagent-acp-request '(params limit)))
                     (content (emagent-tools--read-file-content canonical line limit)))
                (emagent-acp-send-response
                 :client client
                 :response (emagent-acp-make-fs-read-text-file-response
                            :request-id request-id
                            :content content)))
            (file-missing
             (emagent-acp-send-response
              :client client
              :response (emagent-acp-make-fs-read-text-file-response
                         :request-id request-id
                         :error (emagent-acp-make-error :code -32002
                                                :message "Resource not found"))))
            (error
             (emagent-acp-send-response
              :client client
              :response (emagent-acp-make-fs-read-text-file-response
                         :request-id request-id
                         :error (emagent-acp-make-error :code -32603
                                                :message (error-message-string err)))))))))))

(cl-defun emagent-acp--on-fs-write (&key state emagent-acp-request)
  (let ((client (emagent-acp-state-client state))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path)))
        (content (or (map-nested-elt emagent-acp-request '(params content)) "")))
    (if (not emagent-acp-file-access)
        (emagent-acp-send-response
         :client client
         :response (emagent-acp-make-fs-write-text-file-response
                    :request-id request-id
                    :error (emagent-acp--fs-unavailable-response "fs/write_text_file")))
      (let* ((emagent-tools--root-boundary (emagent-acp--fs-session-root state))
             (emagent-tools--project-directory
              (or emagent-tools--root-boundary emagent-tools--project-directory))
             (verdict (emagent-guard-check 'write path)))
        (if (not (emagent-guard-allow-p verdict))
            (emagent-acp-send-response
             :client client
             :response (emagent-acp-make-fs-write-text-file-response
                        :request-id request-id
                        :error (emagent-acp-make-error
                                :code -32603 :message (emagent-guard-reason verdict))))
          (let ((resolved (emagent-guard-resolved verdict)))
            (when emagent-acp-confirm-fs-writes
              (emagent-acp--prepare-interactive-context state))
            (condition-case err
                (if (and emagent-acp-confirm-fs-writes
                         (not (emagent-tools--confirm-write
                               'emagent-tool-write-file resolved content
                               (emagent-acp--chat-buffer state))))
                    (emagent-acp-send-response
                     :client client
                     :response (emagent-acp-make-fs-write-text-file-response
                                :request-id request-id
                                :error (emagent-acp-make-error :code -32603
                                       :message "Write denied by user")))
                  (let ((written (emagent-tools--write-file-content resolved content)))
                    (emagent-acp--notify-user
                     state (format "emagent: wrote %s (C-/ to undo in that buffer)"
                                   written))
                    (emagent-acp-send-response
                     :client client
                     :response (emagent-acp-make-fs-write-text-file-response
                                :request-id request-id))))
              (error
               (emagent-acp-send-response
                :client client
                :response (emagent-acp-make-fs-write-text-file-response
                           :request-id request-id
                           :error (emagent-acp-make-error :code -32603
                                  :message (error-message-string err))))))))))))

(provide 'emagent-acp-file)
;;; emagent-acp-file.el ends here
