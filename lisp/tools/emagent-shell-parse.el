;;; emagent-shell-parse.el --- Shell parse helpers for emagent -*- lexical-binding: t; -*-

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
;; Parse helpers, classifiers, and customs for shell command routing.

;;; Code:

(require 'emagent-tools-process)

(defvar emagent-acp-prefer-emacs)

(defgroup emagent-shell nil
  "Shell command routing for emagent."
  :group 'emagent)

(defcustom emagent-shell-block-no-verify t
  "When non-nil, refuse git commands that use --no-verify."
  :type 'boolean
  :group 'emagent-shell)

(defcustom emagent-shell-guard-push t
  "When non-nil, refuse git push when gh reports the branch PR is merged."
  :type 'boolean
  :group 'emagent-shell)

(defcustom emagent-shell-redirect t
  "When non-nil with `emagent-acp-prefer-emacs', redirect simple shell commands."
  :type 'boolean
  :group 'emagent-shell)

(defcustom emagent-shell-suggest t
  "When non-nil with `emagent-acp-prefer-emacs', refuse substitutable shell."
  :type 'boolean
  :group 'emagent-shell)

(defun emagent-shell--prefer-emacs-p ()
  "Return non-nil when Emacs-native routing is active."
  (and (boundp 'emagent-acp-prefer-emacs)
       emagent-acp-prefer-emacs
       emagent-shell-redirect))

(defun emagent-shell--suggest-p ()
  "Return non-nil when shell suggestions are active."
  (and (boundp 'emagent-acp-prefer-emacs)
       emagent-acp-prefer-emacs
       emagent-shell-suggest))

(defun emagent-shell--strip-quoted (command)
  "Remove single- and double-quoted spans from COMMAND."
  (replace-regexp-in-string "[\"'][^\"']*[\"']" "" command))

(defun emagent-shell--git-no-verify-p (command)
  "Return non-nil when COMMAND is git with --no-verify."
  (and (string-match-p "\\<git\\>" command)
       (string-match-p "--no-verify" (emagent-shell--strip-quoted command))))

(defun emagent-shell--git-push-p (command)
  "Return non-nil when COMMAND is a git push."
  (string-match-p "\\`git[[:space:]]+push\\>" (string-trim command)))

(defconst emagent-shell--build-executables
  '("mvn" "./mvnw" "gradle" "./gradlew" "make" "cmake" "ninja"
    "cargo" "go" "pytest" "python" "python3"
    "npm" "yarn" "pnpm" "bun")
  "Executable names that produce compiler-style output.
These are always redirected to `emagent-tool-compile' for navigable errors.")

(defun emagent-shell--build-command-p (words)
  "Return non-nil when WORDS names a build/test/compile executable."
  (member (car words) emagent-shell--build-executables))

(defvar emagent-tools--timeout-override)
(defvar emagent-tools--shell-output-limit)

(defun emagent-shell--call-with-timeout (timeout thunk)
  "Call THUNK with TIMEOUT bound as `emagent-tools--timeout-override'."
  (if timeout
      (let ((emagent-tools--timeout-override timeout))
        (funcall thunk))
    (funcall thunk)))

(defun emagent-shell--captured-timeout ()
  "Return the per-call timeout from the current dynamic binding, or nil."
  (and emagent-tools--timeout-override
       (emagent-tools--clamp-timeout emagent-tools--timeout-override)))

(defun emagent-shell--run-in-directory (directory fn)
  "Run FN with `default-directory' set to DIRECTORY."
  (let ((default-directory (emagent-tools--root-directory directory)))
    (funcall fn)))

(defun emagent-shell--unquote (text)
  "Strip one layer of shell quotes from TEXT."
  (if (and (stringp text) (>= (length text) 2))
      (pcase (aref text 0)
        (?\" (if (= (aref text (1- (length text))) ?\")
                 (substring text 1 -1)
               text))
        (?\' (if (= (aref text (1- (length text))) ?\')
                 (substring text 1 -1)
               text))
        (_ text))
    text))

(defun emagent-shell--words (command)
  "Split COMMAND into words, respecting simple quotes."
  (if (fboundp 'split-string-shell-argument)
      (split-string-shell-argument command)
    (split-string command "[[:space:]]+" t)))

(defun emagent-shell--command-to-string (command)
  "Like `shell-command-to-string' for COMMAND, yielding to the event loop."
  (let ((buf (generate-new-buffer " *emagent-shell*"))
        done)
    (unwind-protect
        (progn
          (let ((proc (start-process-shell-command "emagent-shell" buf command)))
            (set-process-sentinel proc (lambda (_p _e) (setq done t))))
          (while (not done)
            (accept-process-output nil 0.05))
          (with-current-buffer buf
            (buffer-string)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun emagent-shell--read-only-network-p (command)
  "Return non-nil when COMMAND is a read-only HTTP GET via curl or wget."
  (let ((cmd (emagent-shell--strip-quoted (string-trim command))))
    (cond
     ((string-match-p "\\`curl\\>" cmd)
      (and (string-match-p "https?://" cmd)
           (not (string-match-p
                 "\\(?:-X[[:space:]]*\\(?:POST\\|PUT\\|DELETE\\|PATCH\\)\\|-d\\|--data\\|-F\\|--form\\|-o[[:space:]]\\|-O\\|>[[:space:]]\\)"
                 cmd))))
     ((string-match-p "\\`wget\\>" cmd)
      (and (string-match-p "https?://" cmd)
           (not (string-match-p "\\(?:--post\\|-O\\|--output-document\\|>[[:space:]]\\)" cmd))))
     (t nil))))

(provide 'emagent-shell-parse)
;;; emagent-shell-parse.el ends here
