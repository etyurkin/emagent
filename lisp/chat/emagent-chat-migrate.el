;;; emagent-chat-migrate.el --- legacy session migration  -*- lexical-binding: t; -*-

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
;; One-shot migration from the legacy # --- emagent --- delimiter format to the
;; current ** Thinking / ** Response headline structure.  This file is easy to
;; delete once all sessions have been upgraded.

;;; Code:
(require 'cl-lib)
(require 'emagent-chat-markup)

(defconst emagent-chat--legacy-response-begin-re
  "^# --- emagent ---[ \t]*$"
  "Regexp matching legacy emagent response start delimiters.")

(defconst emagent-chat--legacy-response-end-re
  "^# --- /emagent ---[ \t]*$"
  "Regexp matching legacy emagent response end delimiters.")

(defconst emagent-chat--legacy-reasoning-begin-re
  "^#\\+begin_quote\\(?:[ \t]+\\(?:Thinking\\|Reasoning\\)\\)?[ \t]*$"
  "Regexp matching legacy quote-based reasoning block starts.")

(defconst emagent-chat--legacy-reasoning-end-re
  "^#\\+end_quote[ \t]*$"
  "Regexp matching legacy quote-based reasoning block ends.")

(defun emagent-chat--legacy-trim-outer-newlines (text)
  "Return TEXT with only outer blank lines removed."
  (replace-regexp-in-string
   "\n\\'" ""
   (replace-regexp-in-string "\\`\n+" "" (or text ""))))

(defun emagent-chat--legacy-unescape-reasoning-text (text)
  "Convert legacy quote escapes in TEXT back to their literal forms.

Legacy quote blocks escaped Org-leading syntax with a comma, e.g.
\",#+end_quote\" or \",* heading\".  The new Thinking subsection uses space
escaping, so we first remove the legacy comma escape and then re-escape."
  (replace-regexp-in-string "^\\([ \t]*\\),\\([*#]\\)" "\\1\\2" (or text "")))

(defun emagent-chat--legacy-response-subsections (body)
  "Return new Thinking/Response subsection text for legacy response BODY."
  (let ((case-fold-search t)
        (thinking nil)
        (result body))
    (with-temp-buffer
      (insert (or body ""))
      (goto-char (point-min))
      (when (re-search-forward emagent-chat--legacy-reasoning-begin-re nil t)
        (let* ((quote-begin (match-beginning 0))
               (thinking-begin (min (point-max) (1+ (line-end-position)))))
          (when (re-search-forward emagent-chat--legacy-reasoning-end-re nil t)
            (let* ((thinking-end (match-beginning 0))
                   (after-quote (min (point-max) (1+ (line-end-position))))
                   (before (buffer-substring-no-properties (point-min) quote-begin))
                   (after (buffer-substring-no-properties after-quote (point-max))))
              (setq thinking (buffer-substring-no-properties thinking-begin thinking-end)
                    result (concat before after)))))))
    (let* ((thinking-text (emagent-chat--legacy-trim-outer-newlines
                           (emagent-chat--legacy-unescape-reasoning-text thinking)))
           (result-text (emagent-chat--legacy-trim-outer-newlines (or result "")))
           (result-text (if (string-empty-p result-text)
                            ""
                          (emagent-chat--demote-response-headings result-text)))
           (thinking-part (if (string-empty-p thinking-text)
                              ""
                            (format "** Thinking\n%s\n\n"
                                    (emagent-chat--escape-reasoning-text thinking-text)))))
      (concat
       thinking-part
       "** Response"
       (if (string-empty-p result-text) "\n" (format "\n%s\n" result-text))))))

(defun emagent-chat--migrate-legacy-response-delimiters ()
  "Rewrite legacy delimiter-based responses in current buffer.

Transforms:
  # --- emagent ---
  #+begin_quote Thinking|Reasoning
  ...
  #+end_quote
  ...
  # --- /emagent ---

into native Org subsections:
  ** Thinking
  ...
  ** Response
  ...

Returns the number of migrated responses."
  (save-excursion
    (save-restriction
      (widen)
      (let ((case-fold-search t)
            (inhibit-read-only t)
            (migrated 0))
        (goto-char (point-min))
        (while (re-search-forward emagent-chat--legacy-response-begin-re nil t)
          (let* ((begin-line (match-beginning 0))
                 (body-begin (min (point-max) (1+ (line-end-position))))
                 (end-line
                  (save-excursion
                    (when (re-search-forward emagent-chat--legacy-response-end-re nil t)
                      (match-beginning 0)))))
            (when end-line
              (let* ((body (buffer-substring-no-properties body-begin end-line))
                     (replace-end
                      (save-excursion
                        (goto-char end-line)
                        (min (point-max) (1+ (line-end-position)))))
                     (converted (emagent-chat--legacy-response-subsections body)))
                (delete-region begin-line replace-end)
                (goto-char begin-line)
                (insert converted)
                (unless (or (eobp) (looking-at-p "\n"))
                  (insert "\n"))
                (setq migrated (1+ migrated))))))
        migrated))))

(defun emagent-chat--maybe-migrate-legacy-format ()
  "Migrate old emagent response format in current buffer when detected.

Logs a message to *Messages* when migration runs."
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward emagent-chat--legacy-response-begin-re nil t))
    (message "emagent: migrating schema for %s"
             (or (buffer-file-name) (buffer-name)))
    (let ((was-modified (buffer-modified-p))
          (migrated (emagent-chat--migrate-legacy-response-delimiters)))
      (unless (> migrated 0)
        (set-buffer-modified-p was-modified)))))

(provide 'emagent-chat-migrate)
;;; emagent-chat-migrate.el ends here
