;;; emagent-shell.el --- Shell command routing for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.8
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
;; Intercepts `emagent-tool-run-shell-command' to block unsafe git usage,
;; suggest Emacs-native alternatives, and auto-redirect simple commands.

;;; Code:

(require 'emagent-policy)
(require 'emagent-shell-redirect)

(defun emagent-shell-run-command-async (command directory callback)
  "Like `emagent-shell-run-command' for COMMAND via CALLBACK.
CALLBACK is called as \(CALLBACK OUTPUT IS-ERROR).  Synchronous guards
\(policy, --no-verify) run immediately; push guard, redirects, compile,
and shell fallback are non-blocking.

Arguments: DIRECTORY."
  (let* ((cmd (string-trim command))
         (words (emagent-shell--words cmd))
         (timeout (emagent-shell--captured-timeout))
         (guard-error
          (condition-case err
              (progn
                (emagent-policy-enforce (emagent-policy-check-shell cmd) cmd)
                (when (and emagent-shell-block-no-verify
                           (emagent-shell--git-no-verify-p cmd))
                  (user-error
                   "--no-verify bypasses pre-commit hooks; fix the pre-commit issue instead"))
                nil)
            (error (error-message-string err)))))
    (if guard-error
        (funcall callback guard-error t)
      (if (emagent-shell--git-push-p cmd)
          (emagent-shell--guard-git-push-async
           directory
           (lambda (push-err)
             (if push-err
                 (funcall callback push-err t)
               (emagent-shell--run-command-body-async
                cmd words directory callback timeout)))
           timeout)
        (emagent-shell--run-command-body-async
         cmd words directory callback timeout)))))

(defun emagent-shell-run-command (command &optional directory)
  "Run COMMAND with Emacs-native routing, guards, and redirects.

Arguments: DIRECTORY."
  (let* ((cmd (string-trim command))
         (words (emagent-shell--words cmd)))
    (emagent-policy-enforce (emagent-policy-check-shell cmd) cmd)
    (when (and emagent-shell-block-no-verify
               (emagent-shell--git-no-verify-p cmd))
      (user-error
       "--no-verify bypasses pre-commit hooks; fix the pre-commit issue instead"))
    (when (emagent-shell--git-push-p cmd)
      (emagent-shell--guard-git-push directory))
    ;; Build/test commands always go through compilation-mode for navigable errors,
    ;; regardless of the prefer-emacs setting.
    (if (emagent-shell--build-command-p words)
        (emagent-tool-compile cmd directory)
      (or (emagent-shell--try-redirect cmd directory)
          (let ((suggestion (emagent-shell--suggest-alternative cmd)))
            (when suggestion
              (user-error "%s" suggestion))
            (let* ((default-directory (emagent-tools--root-directory directory))
                   (output (emagent-shell--command-to-string cmd)))
              (if (> (length output) emagent-tools--shell-output-limit)
                  (concat (substring output 0 emagent-tools--shell-output-limit)
                          "\n… (output truncated)")
                output)))))))

(provide 'emagent-shell)
;;; emagent-shell.el ends here
