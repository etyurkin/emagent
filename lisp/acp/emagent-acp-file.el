;;; emagent-acp-file.el --- ACP file operation handlers  -*- lexical-binding: t; -*-

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

;; ACP fs/read_text_file and fs/write_text_file request handlers.
;; Routes file operations through Emacs tools for path safety.

;;; Code:

(require 'cl-lib)
(require 'emagent-tools)
(require 'emagent-acp-custom)
(require 'emagent-acp-protocol)

(declare-function emagent-acp--prepare-interactive-context "emagent-acp")
(declare-function emagent-acp--notify-user "emagent-acp")
(declare-function emagent-acp--chat-buffer "emagent-acp-usage")

(defun emagent-acp--protected-fs-error (path)
  (emagent-acp-make-error
   :code -32603
   :message (format "Refusing Emacs access to %s (iCloud or another app's container)"
                    (emagent-tools--root-directory path))))

(defun emagent-acp--fs-unavailable-response (method)
  (emagent-acp-make-error
   :code -32601
   :message (format "%s disabled; use the external agent's project file tools"
                    method)))

(cl-defun emagent-acp--on-fs-read (&key state emagent-acp-request)
  (let ((client (map-elt state :client))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path))))
    (if (not emagent-acp-file-access)
        (emagent-acp-send-response
         :client client
         :response (emagent-acp-make-fs-read-text-file-response
                    :request-id request-id
                    :error (emagent-acp--fs-unavailable-response "fs/read_text_file")))
      (if (emagent-tools--protected-fs-path-p path)
          (emagent-acp-send-response
           :client client
           :response (emagent-acp-make-fs-read-text-file-response
                      :request-id request-id
                      :error (emagent-acp--protected-fs-error path)))
        (condition-case err
            (let* ((line (or (map-nested-elt emagent-acp-request '(params line)) 1))
                   (limit (map-nested-elt emagent-acp-request '(params limit)))
                   (content (emagent-tools--read-file-content path line limit)))
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
                                              :message (error-message-string err))))))))))

(cl-defun emagent-acp--on-fs-write (&key state emagent-acp-request)
  (let ((client (map-elt state :client))
        (request-id (map-elt emagent-acp-request 'id))
        (path (map-nested-elt emagent-acp-request '(params path)))
        (resolved (emagent-tools--root-directory
                   (map-nested-elt emagent-acp-request '(params path)))))
    (if (not emagent-acp-file-access)
        (emagent-acp-send-response
         :client client
         :response (emagent-acp-make-fs-write-text-file-response
                    :request-id request-id
                    :error (emagent-acp--fs-unavailable-response "fs/write_text_file")))
      (if (emagent-tools--protected-fs-path-p path)
          (emagent-acp-send-response
           :client client
           :response (emagent-acp-make-fs-write-text-file-response
                      :request-id request-id
                      :error (emagent-acp--protected-fs-error path)))
        (progn
          (when emagent-acp-confirm-fs-writes
            (emagent-acp--prepare-interactive-context state))
          (condition-case err
              (if (and emagent-acp-confirm-fs-writes
                       (not (emagent-tools--confirm-write
                             'emagent-tool-write-file resolved
                             (or (map-nested-elt emagent-acp-request '(params content)) "")
                             (emagent-acp--chat-buffer state))))
                  (emagent-acp-send-response
                   :client client
                   :response (emagent-acp-make-fs-write-text-file-response
                              :request-id request-id
                              :error (emagent-acp-make-error :code -32603
                                     :message "Write denied by user")))
                (let ((written (emagent-tools--write-file-content
                                path (map-nested-elt emagent-acp-request '(params content)))))
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
                                :message (error-message-string err)))))))))))

(provide 'emagent-acp-file)
;;; emagent-acp-file.el ends here
