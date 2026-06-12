;;; emagent-acp-compat.el --- Work around upstream acp message-queue stalls -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (acp "0.12.2"))

;;; Commentary:
;;
;; acp.el 0.12.2 queues incoming JSON and drains it from a `run-at-time 0'
;; timer.  Messages that arrive while a drain is active, or after a drain
;; finishes but before the busy flag clears, can sit in the queue forever.
;; Emagent replaces `acp--start-client' with an equivalent implementation that
;; drains synchronously and reschedules when the queue is still non-empty.
;; The acp package on disk is not modified.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cl-lib)
(require 'acp)

(defconst emagent-acp-compat--acp-version "0.12.2")

(defvar emagent-acp-compat--installed nil
  "Non-nil when the acp queue workaround advice is active.")

(defun emagent-acp-compat--mutable-map (map)
  "Return MAP as a mutable eq hash table when it is a plain alist."
  (if (hash-table-p map)
      map
    (let ((ht (make-hash-table :test 'eq)))
      (map-do (lambda (key value) (puthash key value ht)) map)
      ht)))

(defun emagent-acp-compat--mutable-client (client)
  "Return CLIENT as a hash table so acp can use `map-put!' on Emacs 30."
  (emagent-acp-compat--mutable-map client))

(cl-defun emagent-acp-compat--start-client (&key client)
  "Start CLIENT with a synchronous, rescheduling message-queue drain.

Mirrors `acp--start-client' from acp.el 0.12.2 except that incoming
messages are routed immediately instead of via `run-at-time'."
  (unless client
    (error ":client is required"))
  (unless (map-elt client :command)
    (error ":command is required"))
  (unless (executable-find (map-elt client :command)
                           (file-remote-p default-directory))
    (error "\"%s\" command line utility not found.  Please install it"
           (map-elt client :command)))
  (when (acp--client-started-p client)
    (error "Client already started"))
  (let* ((coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (pending-input "")
         (message-queue nil)
         (message-queue-busy nil)
         (process-environment (append (map-elt client :environment-variables)
                                      process-environment))
         (stderr-buffer (get-buffer-create (format "acp-client-stderr(%s)-%s"
                                                   (map-elt client :command)
                                                   (map-elt client :instance-count)))))
    (with-current-buffer stderr-buffer
      (add-hook 'after-change-functions
                (lambda (beg end _len)
                  (let ((raw-output (buffer-substring-no-properties beg end)))
                    (acp--log client "STDERR" "%s" (string-trim raw-output))
                    (when-let ((std-error (cond
                                           ((acp--parse-stderr-api-error raw-output)
                                            (acp--parse-stderr-api-error raw-output))
                                           ((not (string-empty-p (string-trim raw-output)))
                                            (acp--make-internal-error raw-output)))))
                      (acp--log client "API-ERROR" "%s" (string-trim raw-output))
                      (dolist (handler (map-elt client :error-handlers))
                        (funcall handler std-error)))))
                nil t))
    (cl-labels
        ((route-message (incoming)
           (let ((print-circle t)
                 (print-level 25)
                 (print-length 200))
             (acp--route-incoming-message
              :message incoming
              :client client
              :on-notification
              (lambda (notification)
                (dolist (handler (map-elt client :notification-handlers))
                  (condition-case-unless-debug err
                      (funcall handler notification)
                    (error
                     (acp--log client "NOTIFICATION HANDLER ERROR"
                               "Failed with error: %S" err)))))
              :on-request
              (lambda (request)
                (dolist (handler (map-elt client :request-handlers))
                  (condition-case-unless-debug err
                      (funcall handler request)
                    (error
                     (acp--log client "REQUEST HANDLER ERROR"
                               "Failed with error: %S" err))))))))
         (drain-message-queue ()
           (unless message-queue-busy
             (setq message-queue-busy t)
             (unwind-protect
                 (while message-queue
                   (let ((incoming (car message-queue)))
                     (setq message-queue (cdr message-queue))
                     (route-message incoming)))
               (setq message-queue-busy nil))
             (when message-queue
               (drain-message-queue))))
         (enqueue-message (incoming)
           (setq message-queue (append message-queue (list incoming)))
           (drain-message-queue)))
      (let ((process (make-process
                      :name (format "acp-client(%s)-%s"
                                    (map-elt client :command)
                                    (map-elt client :instance-count))
                      :command (cons (map-elt client :command)
                                     (map-elt client :command-params))
                      :stderr stderr-buffer
                      :connection-type 'pipe
                      :noquery t
                      :file-handler (file-remote-p default-directory)
                      :filter (lambda (_proc input)
                                (acp--log client "INCOMING TEXT" "%s" input)
                                (setq pending-input (concat pending-input input))
                                (let ((start 0) pos)
                                  (while (setq pos (string-search "\n" pending-input start))
                                    (let ((json (substring pending-input start pos)))
                                      (acp--log client "INCOMING LINE" "%s" json)
                                      (when-let* ((object (condition-case nil
                                                              (acp--parse-json json)
                                                            (error
                                                             (acp--log client "JSON PARSE ERROR"
                                                                        "Invalid JSON: %s" json)
                                                             nil))))
                                        (enqueue-message (acp--make-message :json json
                                                                            :object object))))
                                    (setq start (1+ pos)))
                                  (setq pending-input (substring pending-input start))))
                      :sentinel (lambda (process event)
                                  (when (buffer-live-p stderr-buffer)
                                    (kill-buffer stderr-buffer))
                                  (when (memq (process-status process) '(exit signal))
                                    (acp--fail-pending-requests :client client :event event))))))
        (map-put! client :process process)))))

(defun emagent-acp-compat--install ()
  "Install emagent workarounds for acp.el without modifying the acp package."
  (unless emagent-acp-compat--installed
    (when (and (boundp 'acp-package-version)
               (not (string= acp-package-version emagent-acp-compat--acp-version)))
      (message "emagent-acp-compat: acp %s differs from tested %s"
               acp-package-version emagent-acp-compat--acp-version))
    (advice-add 'acp--start-client :override #'emagent-acp-compat--start-client)
    (advice-add 'acp-make-client :filter-return #'emagent-acp-compat--mutable-client)
    (setq emagent-acp-compat--installed t)))

(emagent-acp-compat--install)

(provide 'emagent-acp-compat)

;;; emagent-acp-compat.el ends here
