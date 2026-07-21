;;; emagent-chat-thought-fence.el --- fence/open-markup helpers  -*- lexical-binding: t; -*-

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

;; Streaming fence-state and open-markup split helpers for thought chunks.

;;; Code:

(require 'cl-lib)
(require 'emagent-chat-markup)

(defvar-local emagent-chat--fence-state nil
  "Streaming code-fence buffer for the open Thinking block.
Nil when not inside a fenced code block.
Non-nil: (lang . accumulated-body-so-far) while waiting for the closing ```.")

(defun emagent-chat--split-fences (text)
  "Convert complete markdown fences in TEXT to org src blocks.
Returns (safe-text . fence-state) where fence-state is nil when all fences
are closed, or (lang . body-so-far) for the last unclosed fence."
  (let ((pos 0)
        (parts nil)
        (incomplete nil))
    (while (and (not incomplete) (string-match "```" text pos))
      (let* ((fence-pos (match-beginning 0))
             (after-fence (match-end 0)))
        (push (substring text pos fence-pos) parts)
        (cond
         ;; No newline after ``` — partial lang tag at end of chunk
         ((not (string-match "\n" text after-fence))
          (setq incomplete (cons (substring text after-fence) ""))
          (setq pos (length text)))
         ;; Newline found — complete lang tag, look for closing ```
         (t
          (let* ((tag-end (match-beginning 0))
                 (body-start (match-end 0))
                 (lang (string-trim (substring text after-fence tag-end))))
            (if (not (string-match "```" text body-start))
                ;; No closing fence — buffer the rest
                (progn
                  (setq incomplete (cons lang (substring text body-start)))
                  (setq pos (length text)))
              ;; Complete fence — emit as org src block when lang is given,
              ;; or as plain text (fence stripped) when lang is empty so
              ;; reasoning notes wrapped in plain ``` don't become src blocks.
              (let* ((body-end (match-beginning 0))
                     (close-end (match-end 0))
                     (body (string-trim-right
                            (substring text body-start body-end))))
                (push (if (string-empty-p lang)
                          body
                        (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                                (emagent-chat--lang-from-src-tag lang)
                                (emagent-chat--escape-src-body body)))
                      parts)
                (setq pos close-end))))))))
    (unless incomplete
      (push (substring text pos) parts))
    (cons (apply #'concat (nreverse parts)) incomplete)))

(defun emagent-chat--feed-fences (text state)
  "Continue fence conversion for TEXT from prior STATE.

STATE is nil or (LANG . BODY) as returned by `emagent-chat--split-fences'.
When BODY is empty, fall back to reconstructing an open fence (handles an
incomplete language tag cheaply).  When BODY is non-empty, append TEXT and
search for a closing fence from a 2-character overlap so open code blocks
stay O(chunk) rather than re-scanning the full buffered body each time."
  (let ((fence (make-string 3 ?`)))
    (cond
     ((null state)
      (emagent-chat--split-fences text))
     ((zerop (length (cdr state)))
      (emagent-chat--split-fences
       (concat fence (car state) "\n" (cdr state) text)))
     (t
      (let* ((lang (car state))
             (body (cdr state))
             (overlap (min 2 (length body)))
             (combined (concat body text))
             (close (cl-search fence combined :start2
                               (max 0 (- (length body) overlap)))))
        (if (not close)
            (cons "" (cons lang combined))
          (let* ((complete (string-trim-right (substring combined 0 close)))
                 (rest (substring combined (+ close 3)))
                 (block (if (string-empty-p lang)
                            complete
                          (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
                                  (emagent-chat--lang-from-src-tag lang)
                                  (emagent-chat--escape-src-body complete))))
                 (after (emagent-chat--split-fences rest)))
            (cons (concat block (car after)) (cdr after)))))))))

(defun emagent-chat--count-substring (needle s end)
  "Return the count of non-overlapping NEEDLE occurrences in S before END."
  (let ((n 0) (pos 0) (len (length needle)) hit)
    (while (setq hit (cl-search needle s :start2 pos :end2 end))
      (setq n (1+ n)
            pos (+ hit len)))
    n))

(defun emagent-chat--open-markup-start (text)
  "Return start index of a trailing incomplete markdown span in TEXT.
Return nil when TEXT ends on a complete boundary.  Covers inline code,
bold, and links whose closing delimiter may still arrive later.  Each
pattern is anchored to the end of TEXT and forbids an interior newline."
  (let (starts)
    ;; Inline code: a trailing unmatched opening backtick.
    (when (and (string-match "`[^`\n]*\\'" text)
               (cl-evenp (cl-count ?` text :end (match-beginning 0))))
      (push (match-beginning 0) starts))
    ;; Bold: a trailing `**' opener with no closing `**' yet.
    (when (and (string-match "\\*\\*[^*\n]*\\'" text)
               (cl-evenp (emagent-chat--count-substring "**" text
                                                        (match-beginning 0))))
      (push (match-beginning 0) starts))
    ;; Link, in any partial state: `[text', `[text]', or `[text](url'.
    (dolist (re '("\\[[^][\n]*\\'"
                  "\\[[^][\n]+\\][ \t]*\\'"
                  "\\[[^][\n]+\\][ \t]*([^)\n]*\\'"))
      (when (string-match re text)
        (push (match-beginning 0) starts)))
    (when starts (apply #'min starts))))

(defun emagent-chat--split-open-markup (text)
  "Split TEXT before a trailing, still-incomplete markdown span.
Return (EMIT . HOLD): HOLD begins at an inline code, bold, or link span whose
closing delimiter may arrive in a later chunk, or \"\" when TEXT ends on a
complete boundary.  Holding the partial span keeps markup that a streaming
boundary split from rendering as raw `*', backtick, or bracket characters."
  (if-let ((start (emagent-chat--open-markup-start text)))
      (cons (substring text 0 start) (substring text start))
    (cons text "")))

(provide 'emagent-chat-thought-fence)
;;; emagent-chat-thought-fence.el ends here
