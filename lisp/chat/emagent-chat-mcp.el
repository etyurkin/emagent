;;; emagent-chat-mcp.el --- Client /mcp slash for Claude and Cursor  -*- lexical-binding: t; -*-

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

;; Client-side `/mcp' for both ACP providers.  Mirrors the interactive CLI:
;;
;;   Cursor: `cursor-agent mcp list' / `mcp login' / `mcp enable'
;;   Claude: `claude mcp list' / `mcp login'
;;
;; Never forwarded to the ACP agent (same idea as `/model').

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-session)
(require 'emagent-chat-slash)
(require 'emagent-mcp-server)
(require 'emagent-cursor-command)

(defvar emagent-chat-provider)

(defcustom emagent-claude-mcp-command "claude"
  "Claude Code CLI used for `mcp list' / `mcp login'.

Distinct from `emagent-claude-acp-command' (the ACP bridge binary)."
  :type 'string
  :group 'emagent-chat)

(defun emagent-chat--mcp-command-p (text)
  "Return non-nil when TEXT is a bare `/mcp' (optional server id)."
  (let ((trimmed (string-trim text)))
    (when (string-prefix-p "/" trimmed)
      (let* ((body (substring trimmed 1))
             (space (cl-position-if (lambda (c) (memq c '(?\s ?\t))) body))
             (cmd (if space (substring body 0 space) body)))
        (string= cmd "mcp")))))

(defun emagent-chat--mcp-arg (text)
  "Return the optional server id after `/mcp' in TEXT, or nil."
  (let ((trimmed (string-trim text)))
    (when (string-prefix-p "/mcp" trimmed)
      (let ((rest (string-trim (substring trimmed (length "/mcp")))))
        (and (not (string-empty-p rest)) rest)))))

(defun emagent-chat--mcp-parse-list-line (line)
  "Parse one `mcp list' LINE into \(NAME . STATUS\) or nil.

Supports Cursor \(`name: status'\) and Claude
\(`name: detail - Connected'\) formats."
  (let ((trimmed (string-trim line)))
    (cond
     ((string-empty-p trimmed) nil)
     ((string-prefix-p "⚠" trimmed) nil)
     ((string-prefix-p "Checking MCP" trimmed) nil)
     ((not (string-match "\\`\\([^:]+\\):[[:space:]]*\\(.*\\)\\'" trimmed)) nil)
     (t
      (let* ((name (string-trim (match-string 1 trimmed)))
             (rest (string-trim (match-string 2 trimmed)))
             (status
              (cond
               ((string-match-p "requires_authentication" rest)
                "requires_authentication")
               ((string-match-p "needs[_-]?auth" rest)
                "needs_authentication")
               ((or (string-match-p "✔[[:space:]]*Connected" rest)
                    (string-match-p "\\bConnected\\b" rest))
                "ready")
               ((string-match-p "Pending approval" rest)
                "pending_approval")
               ((string-match-p "\\bready\\b" rest)
                "ready")
               ((string-match-p "\\bdisabled\\b" rest)
                "disabled")
               ((string-match-p "\\berror\\b\\|failed\\|✗\\|✘" rest)
                "error")
               (t rest))))
        (and (not (string-empty-p name))
             (cons name status)))))))

(defun emagent-chat--mcp-parse-list (output)
  "Parse full `mcp list' OUTPUT into an alist of \(NAME . STATUS\)."
  (let (servers)
    (dolist (line (split-string output "\n" t))
      (when-let ((entry (emagent-chat--mcp-parse-list-line line)))
        (push entry servers)))
    (nreverse servers)))

(defun emagent-chat--mcp-needs-auth-p (status)
  "Return non-nil when STATUS indicates OAuth/login is required."
  (and (stringp status)
       (string-match-p
        "requires_authentication\\|needs[_-]?auth\\|authentication\\|unauthorized\\|login"
        (downcase status))))

(defun emagent-chat--mcp-cli ()
  "Return (PROGRAM . DEFAULT-DIRECTORY) for the current provider's MCP CLI."
  (pcase emagent-chat-provider
    ('cursor
     (emagent-cursor-check-command)
     (cons (emagent-cursor-command) (emagent-session-project-directory)))
    ('claude
     (unless (executable-find emagent-claude-mcp-command)
       (user-error "Claude CLI not found on PATH (%s)"
                   emagent-claude-mcp-command))
     (cons emagent-claude-mcp-command (emagent-session-project-directory)))
    (_
     (user-error "/mcp is only supported for Claude and Cursor"))))

(defun emagent-chat--mcp-call (program directory &rest args)
  "Run PROGRAM with ARGS in DIRECTORY; return (EXIT-CODE . OUTPUT).

Prefer `emagent-chat--mcp-start' for interactive UI: a synchronous list can
deadlock when the CLI health-checks the in-Emacs emagent MCP server."
  (with-temp-buffer
    (let* ((default-directory (or directory default-directory))
           (status (apply #'call-process program nil t nil args))
           (out (buffer-string)))
      (cons status out))))

(defun emagent-chat--mcp-list-servers ()
  "Return alist of (NAME . STATUS) from the provider MCP CLI (synchronous).

Only for tests/scripts.  Interactive `/mcp' uses the async path."
  (pcase-let* ((`(,program . ,directory) (emagent-chat--mcp-cli))
               (`(,status . ,out) (emagent-chat--mcp-call program directory
                                                          "mcp" "list")))
    (when (and (numberp status) (/= status 0))
      (emagent-log "mcp list exit %s: %s" status (emagent-log-truncate-line out 200)))
    (let ((servers (emagent-chat--mcp-parse-list out)))
      (unless servers
        (user-error "No MCP servers reported by %s mcp list"
                    program))
      servers)))

(defun emagent-chat--mcp-start (program directory args on-done &optional pty)
  "Start PROGRAM with ARGS in DIRECTORY; call ON-DONE with exit status.

When PTY is non-nil, use a pty (needed for some OAuth CLIs).  A process
filter echoes notable login progress lines to the echo area so feedback
is not lost after `completing-read' returns."
  (let* ((default-directory (or directory default-directory))
         (buf (generate-new-buffer " *emagent-mcp*"))
         (label (mapconcat #'identity args " "))
         (proc
          (make-process
           :name "emagent-mcp"
           :buffer buf
           :command (cons program args)
           :connection-type (if pty 'pty 'pipe)
           :filter
           (lambda (_p chunk)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (goto-char (point-max))
                 (insert chunk)))
             (when (string-match-p
                    "Opening your browser\\|Listening on http\\|Preparing\\|requires authentication\\|login successful\\|authorization"
                    chunk)
               (let ((line (car (last (split-string (string-trim chunk) "\n" t)))))
                 (when (and line (not (string-empty-p line)))
                   (message "emagent: %s" (emagent-log-truncate-line line 120))))))
           :sentinel
           (lambda (p _msg)
             (when (memq (process-status p) '(exit signal))
               (let ((code (process-exit-status p))
                     (out (with-current-buffer (process-buffer p)
                            (buffer-string))))
                 (emagent-log "mcp %s exit %s: %s"
                              label
                              code
                              (emagent-log-truncate-line out 240))
                 (when (buffer-live-p buf)
                   (kill-buffer buf))
                 (when on-done
                   (funcall on-done code out))))))))
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun emagent-chat--mcp-login (name &optional provider)
  "Authenticate MCP server NAME via the provider CLI (async).

PROVIDER defaults to `emagent-chat-provider'.  Captured explicitly so the
exit callback does not depend on buffer-local state after the minibuffer."
  (let* ((prov (or provider emagent-chat-provider))
         (emagent-chat-provider prov))
    (pcase-let ((`(,program . ,directory) (emagent-chat--mcp-cli)))
      ;; Defer past minibuffer teardown — a bare `message' right after
      ;; `completing-read' is often cleared and looks like a no-op.
      (run-at-time
       0.05 nil
       (lambda ()
         (message "emagent: authenticating MCP server %s (browser may open)…"
                  name)))
      (emagent-chat--mcp-start
       program directory (list "mcp" "login" name)
       (lambda (code _out)
         (if (zerop code)
             (progn
               (when (eq prov 'cursor)
                 (let ((emagent-chat-provider prov))
                   (ignore-errors
                     (emagent-cursor-write-mcp-approvals directory))
                   (emagent-chat--mcp-enable name)))
               (message
                "emagent: MCP server %s authenticated — reconnect (toggle emagent-mode) to load tools"
                name))
           (message "emagent: MCP login for %s failed (exit %s); see *Emagent Log*"
                    name code)))
       t))))

(defun emagent-chat--mcp-enable (name)
  "Approve Cursor MCP server NAME for the project cwd (async)."
  (when (eq emagent-chat-provider 'cursor)
    (pcase-let ((`(,program . ,directory) (emagent-chat--mcp-cli)))
      (emagent-chat--mcp-start
       program directory (list "mcp" "enable" name)
       (lambda (code _out)
         (if (zerop code)
             (emagent-log "mcp enable %s: ok" name)
           (emagent-log "mcp enable %s failed (exit %s)" name code)))))))

(defun emagent-chat--mcp-pick-server (servers &optional preferred)
  "Prompt for a server from SERVERS alist and login/enable as needed.

PREFERRED, when non-nil, selects that server id without prompting."
  (let* ((provider emagent-chat-provider)
         (names (mapcar #'car servers))
         (name
          (or (and preferred (assoc-string preferred servers) preferred)
              (let ((completion-extra-properties
                     (list :annotation-function
                           (lambda (cand)
                             (concat "  "
                                     (or (cdr (assoc-string cand servers))
                                         ""))))))
                (completing-read "MCP server: " names nil t nil nil
                                 preferred)))))
    (when (and name (not (string-empty-p name)))
      (let ((status (cdr (assoc-string name servers))))
        (cond
         ((emagent-chat--mcp-needs-auth-p status)
          (emagent-chat--mcp-login name provider))
         ((eq provider 'cursor)
          (emagent-chat--mcp-enable name)
          (run-at-time
           0.05 nil
           (lambda ()
             (message "emagent: MCP server %s (%s)"
                      name (or status "ready")))))
         (t
          (run-at-time
           0.05 nil
           (lambda ()
             (message "emagent: MCP server %s (%s)"
                      name (or status "ready"))))))))))

(defun emagent-chat--mcp-select-and-act (&optional preferred)
  "List MCP servers asynchronously, then select one (or PREFERRED).

Must be async: a synchronous `mcp list' deadlocks Emacs when Cursor
health-checks the in-process emagent MCP server."
  (let ((buf (current-buffer))
        (provider emagent-chat-provider))
    (pcase-let ((`(,program . ,directory) (emagent-chat--mcp-cli)))
      (message "emagent: listing MCP servers…")
      (emagent-chat--mcp-start
       program directory '("mcp" "list")
       (lambda (code out)
         (let ((servers (emagent-chat--mcp-parse-list out)))
           (cond
            ((not (buffer-live-p buf))
             nil)
            ((and (numberp code) (/= code 0) (null servers))
             (message "emagent: mcp list failed (exit %s); see *Emagent Log*"
                      code))
            ((null servers)
             (message "emagent: no MCP servers reported"))
            (t
             ;; Idle timer: more reliable than run-at-time 0 for
             ;; minibuffer prompts after an async process sentinel.
             (run-with-idle-timer
              0 nil
              (lambda ()
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (condition-case err
                        (let ((emagent-chat-provider provider))
                          (emagent-chat--mcp-pick-server
                           servers preferred))
                      (quit
                       (message "emagent: MCP selection cancelled"))
                      (error
                       (emagent-log "mcp UI error: %s"
                                    (error-message-string err))
                       (message "emagent: %s"
                                (error-message-string err))))))))))))))))

(defun emagent-chat--slash-mcp-apply (&optional text)
  "Run client `/mcp' UI.  TEXT may be `/mcp' or `/mcp NAME'."
  (unless (derived-mode-p 'emagent-mode)
    (user-error "Turn on emagent-mode in this buffer first"))
  (unless (memq emagent-chat-provider '(cursor claude))
    (user-error "/mcp requires a Claude or Cursor session"))
  (when-let ((bounds (emagent-chat--slash-token-bounds)))
    (delete-region (car bounds) (cdr bounds)))
  (emagent-chat--mcp-select-and-act (and text (emagent-chat--mcp-arg text))))

;; `emagent-chat-slash' (required above) cannot require this file back (this
;; file requires it, for the slash-token helpers), so the callback slot it
;; declares is wired here once the real implementation is defined.
(setq emagent-chat--on-slash-mcp #'emagent-chat--slash-mcp-apply)

(defun emagent-cursor-approve-configured-mcp-servers ()
  "Approve non-emagent servers from ~/.cursor/mcp.json for the session cwd.

Writes Cursor's per-project mcp-approvals.json (required for ACP to load
http MCP servers) and also runs `cursor-agent mcp enable' as a belt-and-
braces.  Best-effort; failures are logged."
  (require 'emagent-mcp)
  (when (and (eq emagent-chat-provider 'cursor)
             (executable-find (emagent-cursor-command)))
    (let* ((directory (emagent-session-project-directory))
           (program (emagent-cursor-command))
           (file (bound-and-true-p emagent-mcp-cursor-config-file))
           (data (and file (file-readable-p file)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'alist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :false))))
           (servers (map-elt data 'mcpServers)))
      (ignore-errors (emagent-cursor-write-mcp-approvals directory))
      (dolist (pair servers)
        (let ((name (if (symbolp (car pair))
                        (symbol-name (car pair))
                      (format "%s" (car pair)))))
          (unless (equal name "emagent")
            (emagent-chat--mcp-start
             program directory (list "mcp" "enable" name)
             (lambda (code _out)
               (emagent-log "mcp enable %s: %s"
                            name
                            (if (zerop code) "ok" "failed"))))))))))

(provide 'emagent-chat-mcp)
;;; emagent-chat-mcp.el ends here
