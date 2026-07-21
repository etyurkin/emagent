;;; emagent-tools-process.el --- Subprocess helpers for tools  -*- lexical-binding: t; -*-

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

;; Timeouts and async subprocess helpers shared by tool modules.

;;; Code:

(require 'emagent-tools-core)

(defcustom emagent-tools-subprocess-timeout 60
  "Default seconds before killing an agent subprocess.
Agent tools may override this per call up to
`emagent-tools-subprocess-timeout-max'."
  :type 'integer
  :group 'emagent-tools)

(defcustom emagent-tools-subprocess-timeout-max 300
  "Maximum seconds an agent may request as a per-call subprocess timeout."
  :type 'integer
  :group 'emagent-tools)

(defcustom emagent-tools-display-compile-buffer nil
  "When non-nil, display the `*emagent-compile*' buffer when a build starts.
When nil (the default) the buffer fills in the background without
touching the window layout; switch to it any time for navigable errors
\\(\\[next-error])."
  :type 'boolean
  :group 'emagent-tools)

(defvar emagent-tools--timeout-override nil
  "When non-nil, the per-call subprocess timeout requested by the agent.
Bound dynamically around a tool call and read synchronously when a runner
starts, so it is captured before any process wait.")

(defconst emagent-tools--shell-output-limit 100000
  "Max characters returned from shell/process tool output.")

(defun emagent-tools--clamp-timeout (secs)
  "Clamp SECS to [1, `emagent-tools-subprocess-timeout-max']."
  (max 1 (min secs emagent-tools-subprocess-timeout-max)))

(defun emagent-tools--subprocess-timeout ()
  "Return the effective agent subprocess timeout in seconds.
Honors `emagent-tools--timeout-override' when set, clamped to the max."
  (emagent-tools--clamp-timeout
   (or emagent-tools--timeout-override emagent-tools-subprocess-timeout)))

(defun emagent-tools--timeout-message (secs &optional shell)
  "Return a timeout error string for limit SECS.
When SHELL is non-nil, also suggest background execution."
  (concat
   (format
    "Timed out after %ds. Retry with a larger `timeout` argument (up to %ds)."
    secs emagent-tools-subprocess-timeout-max)
   (when shell
     (concat
      " For genuinely long-running work, use background execution"
      " (append ' > /tmp/out.txt 2>&1 & echo \"PID: $!\"') and read the"
      " output file later with read_file."))))

(defun emagent-tools--run-async-sync (async-fn &rest args)
  "Run ASYNC-FN with ARGS and a result callback; block until it finishes.
For tests and internal callers only — MCP agent tools use the async path."
  (let (result is-error done)
    (apply async-fn
           (lambda (r e)
             (setq result r is-error e done t))
           args)
    (while (not done)
      (accept-process-output nil 0.05))
    (if is-error
        (error "%s" result)
      result)))

(defun emagent-tools--run-process-async (callback program &rest args)
  "Run PROGRAM with ARGS; call CALLBACK with (output is-error) from a sentinel."
  (let* ((buf (generate-new-buffer " *emagent-proc*"))
         (timeout-secs (emagent-tools--subprocess-timeout))
         (done nil)
         (timer nil)
         (proc nil)
         (finish
          (lambda (output is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (when (and proc (process-live-p proc))
                (delete-process proc))
              (when (buffer-live-p buf)
                (kill-buffer buf))
              (funcall callback output is-error)))))
    (condition-case start-err
        (progn
          (setq proc (apply #'start-process "emagent-proc" buf program args))
          (setq timer
                (run-with-timer
                 timeout-secs nil
                 (lambda ()
                   (when (and proc (process-live-p proc))
                     (delete-process proc))
                   (funcall finish
                            (emagent-tools--timeout-message timeout-secs)
                            t))))
          (set-process-sentinel
           proc
           (lambda (p _event)
             (when (memq (process-status p) '(signal exit))
               (let* ((output (with-current-buffer buf (buffer-string)))
                      (status (process-exit-status p))
                      (is-error (or (eq status 'signal)
                                    (and (numberp status)
                                         (not (zerop status))))))
                 (funcall finish output is-error))))))
      (error (funcall finish (error-message-string start-err) t)))))

(defun emagent-tools--run-process-input-async (callback input program &rest args)
  "Pipe INPUT to PROGRAM with ARGS; call CALLBACK with (output is-error)."
  (let* ((buf (generate-new-buffer " *emagent-proc*"))
         (timeout-secs (emagent-tools--subprocess-timeout))
         (done nil)
         (timer nil)
         (proc nil)
         (finish
          (lambda (output is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (when (and proc (process-live-p proc))
                (delete-process proc))
              (when (buffer-live-p buf)
                (kill-buffer buf))
              (funcall callback output is-error)))))
    (condition-case start-err
        (progn
          (setq proc (apply #'make-process
                            `(:name "emagent-proc"
                              :buffer ,buf
                              :command (,program . ,args)
                              :connection-type pipe
                              :noquery t
                              :sentinel
                              ,(lambda (p _event)
                                 (when (memq (process-status p)
                                             '(signal exit))
                                   (let* ((output
                                           (with-current-buffer buf
                                             (buffer-string)))
                                          (status (process-exit-status p))
                                          (is-error
                                           (or (eq status 'signal)
                                               (and (numberp status)
                                                    (not (zerop status))))))
                                     (funcall finish output is-error)))))))
          (process-send-string proc input)
          (process-send-eof proc)
          (setq timer
                (run-with-timer
                 timeout-secs nil
                 (lambda ()
                   (when (and proc (process-live-p proc))
                     (delete-process proc))
                   (funcall finish
                            (emagent-tools--timeout-message timeout-secs)
                            t)))))
      (error (funcall finish (error-message-string start-err) t)))))

(defun emagent-tools--run-shell-async (callback command directory)
  "Run shell COMMAND in DIRECTORY; call CALLBACK with (output is-error)."
  (let* ((default-directory (emagent-tools--root-directory directory))
         (buf (generate-new-buffer " *emagent-shell*"))
         (timeout-secs (emagent-tools--subprocess-timeout))
         (limit emagent-tools--shell-output-limit)
         (done nil)
         (timer nil)
         (proc nil)
         (finish
          (lambda (output is-error)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (when (and proc (process-live-p proc))
                (delete-process proc))
              (when (buffer-live-p buf)
                (kill-buffer buf))
              (funcall callback output is-error)))))
    (condition-case start-err
        (progn
          (setq proc (start-process-shell-command "emagent-shell" buf command))
          (setq timer
                (run-with-timer
                 timeout-secs nil
                 (lambda ()
                   (when (and proc (process-live-p proc))
                     (delete-process proc))
                   (funcall finish
                            (emagent-tools--timeout-message timeout-secs t)
                            t))))
          (set-process-sentinel
           proc
           (lambda (p _event)
             (when (memq (process-status p) '(signal exit))
               (let* ((output (with-current-buffer buf (buffer-string)))
                      (status (process-exit-status p))
                      (is-error (or (eq status 'signal)
                                    (and (numberp status)
                                         (not (zerop status))))))
                 (when (and (not is-error) (> (length output) limit))
                   (setq output (concat (substring output 0 limit)
                                        "\n… (output truncated)")))
                 (funcall finish output is-error))))))
      (error (funcall finish (error-message-string start-err) t)))))

(defun emagent-tools--run-process-to-string (program &rest args)
  "Run PROGRAM with ARGS and return stdout (sync wrapper for the test suite)."
  (emagent-tools--run-async-sync
   (lambda (callback)
     (apply #'emagent-tools--run-process-async callback program args))))

(provide 'emagent-tools-process)
;;; emagent-tools-process.el ends here
