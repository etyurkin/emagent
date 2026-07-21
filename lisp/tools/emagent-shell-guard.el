;;; emagent-shell-guard.el --- Git push guards for emagent shell -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.7
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
;; Git push guards that refuse pushing a branch whose PR is already merged.

;;; Code:

(require 'emagent-log)
(require 'emagent-shell-parse)
(require 'emagent-tools-git)
(require 'emagent-tools-process)

(defun emagent-shell--current-branch ()
  "Return the current git branch name, or nil."
  (string-trim (apply #'emagent-tools--run-git "branch" "--show-current")))

(defun emagent-shell--branch-pr-merged-p (branch)
  "Return non-nil when gh reports BRANCH has a merged PR."
  (when (and branch (not (string-empty-p branch)) (executable-find "gh"))
    (let ((state (string-trim
                  (emagent-shell--command-to-string
                   (format "gh pr view --head %s --json state -q .state 2>/dev/null"
                           (shell-quote-argument branch))))))
      (string= state "MERGED"))))

(defun emagent-shell--guard-git-push (directory)
  "Signal an error when pushing a branch whose PR is already merged.

Arguments: DIRECTORY."
  (when emagent-shell-guard-push
    (emagent-shell--run-in-directory
     directory
     (lambda ()
       (let ((branch (emagent-shell--current-branch)))
         (when (and branch (not (string-empty-p branch)))
           (if (executable-find "gh")
               (when (emagent-shell--branch-pr-merged-p branch)
                 (user-error
                  "Branch '%s' has an already-merged PR; checkout main, pull, and create a new branch instead"
                  branch))
             (require 'emagent-log)
             (emagent-log "emagent: gh CLI not found; skipping merged-PR check for push"))))))))

(defun emagent-shell--guard-git-push-async (directory callback &optional timeout)
  "Call CALLBACK with nil on success or an error string when push is blocked.

Arguments: DIRECTORY, TIMEOUT."
  (if (not emagent-shell-guard-push)
      (funcall callback nil)
    (emagent-shell--call-with-timeout timeout
     (lambda ()
       (emagent-tools--run-git-async
        (lambda (branch-out is-error)
          (if is-error
              (funcall callback branch-out)
            (let ((branch (string-trim branch-out)))
              (cond
               ((or (null branch) (string-empty-p branch))
                (funcall callback nil))
               ((not (executable-find "gh"))
                (require 'emagent-log)
                (emagent-log "emagent: gh CLI not found; skipping merged-PR check for push")
                (funcall callback nil))
               (t
                (emagent-shell--call-with-timeout timeout
                 (lambda ()
                   (emagent-tools--run-shell-async
                    (lambda (state-out is-error-gh)
                      (if is-error-gh
                          (funcall callback nil)
                        (if (string= (string-trim state-out) "MERGED")
                            (funcall callback
                                     (format "Branch '%s' has an already-merged PR. Checkout main, pull, and create a new branch instead."
                                             branch))
                          (funcall callback nil))))
                    (format "gh pr view --head %s --json state -q .state 2>/dev/null"
                            (shell-quote-argument branch))
                    directory))))))))
        "branch" "--show-current")))))

(provide 'emagent-shell-guard)
;;; emagent-shell-guard.el ends here
