;;; emagent-acp-protocol-log.el --- ACP protocol logging for emagent  -*- lexical-binding: t; -*-

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

;; Logging helpers and the `emagent-acp' customization group.

;;; Code:

(require 'map)
(require 'emagent-acp-custom)

(defgroup emagent-acp nil
  "ACP (Agent Client Protocol) implementation."
  :group 'tools
  :prefix "emagent-acp-")

(defcustom emagent-acp-logging-enabled nil
  "When non-nil, log ACP wire traffic to the client log buffer."
  :type 'boolean
  :group 'emagent-acp)

(cl-defun emagent-acp-logs-buffer (&key client)
  "Return (creating if needed) the log buffer for CLIENT."
  (let ((name (format "*acp-(%s)-%s log*"
                      (map-elt client :command)
                      (map-elt client :instance-count))))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (buffer-disable-undo)
          (current-buffer)))))

(defun emagent-acp--log (client label format-string &rest args)
  "Log to CLIENT's log buffer when `emagent-acp-logging-enabled' is set.

Arguments: LABEL, FORMAT-STRING, ARGS."
  (when emagent-acp-logging-enabled
    (with-current-buffer (emagent-acp-logs-buffer :client client)
      (goto-char (point-max))
      (if label
          (insert (format "%s >\n\n%s\n\n" label (apply #'format format-string args)))
        (insert (format "%s\n\n" (apply #'format format-string args)))))))

(provide 'emagent-acp-protocol-log)

;;; emagent-acp-protocol-log.el ends here
