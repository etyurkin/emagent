;;; emagent-chat-test.el --- ERT tests for emagent chat UI -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-chat)

;;;; Slugs and labels

(ert-deftest emagent-chat-test-sanitize-slug ()
  (should (string= "my-project" (emagent-chat--sanitize-slug "My Project")))
  (should (string= emagent-chat-default-slug (emagent-chat--sanitize-slug "   "))))

(ert-deftest emagent-chat-test-short-cwd-label ()
  (should (string= "dev-emagent" (emagent-chat--short-cwd-label "~/dev/emagent"))))

(ert-deftest emagent-chat-test-buffer-name-for-label ()
  (should (string= "*Emagent foo*" (emagent-chat--buffer-name-for-label "foo"))))

;;;; Sendable text

(ert-deftest emagent-chat-test-sendable-line-p ()
  (should (emagent-chat--sendable-line-p "hello"))
  (should-not (emagent-chat--sendable-line-p "# comment"))
  (should-not (emagent-chat--sendable-line-p "#+EMAGENT_PROJECT: x"))
  (should-not (emagent-chat--sendable-line-p "# --- emagent ---")))

(ert-deftest emagent-chat-test-sendable-text-p ()
  (should (emagent-chat--sendable-text-p "line one\nline two"))
  (should-not (emagent-chat--sendable-text-p "# metadata\nprompt")))

;;;; Slash commands

(ert-deftest emagent-chat-test-normalize-slash-commands ()
  (let ((cmds `[((name . "/workflow:dev") (description . "Dev flow")
                 (input . ((hint . "hint"))))]))
    (let ((norm (emagent-chat--normalize-slash-commands cmds)))
      (should (= (length norm) 1))
      (should (string= (map-elt (car norm) 'name) "workflow:dev"))
      (should (string= (map-elt (car norm) 'hint) "hint")))))

(ert-deftest emagent-chat-test-merge-slash-commands ()
  (let ((base (list (emagent-chat--slash-command-plist "plan" "Plan mode")))
        (extra (list (emagent-chat--slash-command-plist "compress" "Compress")
                     (emagent-chat--slash-command-plist "plan" "Override"))))
    (let ((merged (emagent-chat--merge-slash-commands base extra)))
      (should (= (length merged) 2))
      (should (string= (map-elt (nth 0 merged) 'name) "compress"))
      (should (string= (map-elt (nth 1 merged) 'name) "plan"))
      (should (string= (map-elt (nth 1 merged) 'description) "Override")))))

(ert-deftest emagent-chat-test-compress-command-p ()
  (should (emagent-chat--compress-command-p "/compress"))
  (should (emagent-chat--compress-command-p "/compact"))
  (should (emagent-chat--compress-command-p "/summarize"))
  (should-not (emagent-chat--compress-command-p "/plan")))

(ert-deftest emagent-chat-test-compress-prompt-text ()
  (should (string-match-p "<conversation>" (emagent-chat--compress-prompt-text "hello")))
  (should (string-match-p "hello" (emagent-chat--compress-prompt-text "hello"))))

(ert-deftest emagent-chat-test-apply-compression ()
  (with-temp-buffer
    (insert emagent-chat-initial-comment)
    (insert (format "%sfirst\n\n# --- emagent ---\nreply\n# --- /emagent ---\n"
                    (emagent-chat--user-heading-prefix)))
    (emagent-chat--sync-user-zone-marker)
    (emagent-chat-apply-compression "summary text")
    (should (string-match-p "\\* emagent> \\[compressed\\]" (buffer-string)))
    (should (string-match-p "summary text" (buffer-string)))
    (should-not (string-match-p "first" (buffer-string)))))

(ert-deftest emagent-chat-test-bare-slash-command-p ()
  (should (emagent-chat--bare-slash-command-p "/compress"))
  (should (emagent-chat--bare-slash-command-p "/plan refactor auth"))
  (should (emagent-chat--bare-slash-command-p "/workflow:dev"))
  (should-not (emagent-chat--bare-slash-command-p "hello"))
  (should-not (emagent-chat--bare-slash-command-p "/compress\nmore")))

(ert-deftest emagent-chat-test-command-matching ()
  (should (emagent-chat--command-matches-needle-p "/workflow:dev" "workflow"))
  (should (emagent-chat--command-matches-needle-p "/skill:relax" "relax"))
  (should (string= "workflow" (emagent-chat--command-needle-base "workflow:")))
  (should (string= "relax" (emagent-chat--command-skill-part "/skill:relax"))))

;;;; Response markup

(ert-deftest emagent-chat-test-lang-from-filename ()
  (should (string= "elisp" (emagent-chat--lang-from-filename "foo.el")))
  (should (string= "python" (emagent-chat--lang-from-filename "bar.py")))
  (should (eq nil (emagent-chat--lang-from-filename "foo.xyz"))))

(ert-deftest emagent-chat-test-convert-code-fences ()
  (let ((out (emagent-chat--convert-code-fences "```elisp\n(+ 1 2)\n```")))
    (should (string-match-p "#\\+BEGIN_SRC elisp" out))
    (should (string-match-p "(\\+ 1 2)" out))
    (should (string-match-p "#\\+END_SRC" out))))

(ert-deftest emagent-chat-test-table-detection ()
  (should (emagent-chat--table-row-p "| a | b |"))
  (should (emagent-chat--table-hline-p "|---+---|"))
  (should-not (emagent-chat--table-hline-p "| a | b |"))
  (let ((fixed (emagent-chat--fix-table-block '("| a | b |" "|---|---|" "| 1 | 2 |"))))
    (should (= (length fixed) 3))
    (should (emagent-chat--table-hline-p (nth 1 fixed)))))

;;;; Spinner

(ert-deftest emagent-chat-test-spinner-frame-count ()
  (let ((emagent-chat-spinner-style 'dots))
    (should (= 4 (emagent-chat--spinner-frame-count))))
  (let ((emagent-chat-spinner-style 'braille))
    (should (= 10 (emagent-chat--spinner-frame-count)))))

(ert-deftest emagent-chat-test-spinner-dot-chase ()
  (let ((emagent-chat-spinner-style 'dots)
        (emagent-chat-spinner-dot-on "●")
        (emagent-chat-spinner-dot-off "○"))
    (setq emagent-chat--spinner-frame 0)
    (should (string= "●○○" (substring-no-properties (emagent-chat--spinner-dot-grid))))
    (setq emagent-chat--spinner-frame 1)
    (should (string= "○●○" (substring-no-properties (emagent-chat--spinner-dot-grid))))
    (setq emagent-chat--spinner-frame 2)
    (should (string= "○○●" (substring-no-properties (emagent-chat--spinner-dot-grid))))
    (setq emagent-chat--spinner-frame 3)
    (should (string= "○●○" (substring-no-properties (emagent-chat--spinner-dot-grid))))
    (setq emagent-chat--spinner-frame
          (% (1+ emagent-chat--spinner-frame)
             (emagent-chat--spinner-frame-count)))
    (should (string= "●○○" (substring-no-properties (emagent-chat--spinner-dot-grid))))))

(ert-deftest emagent-chat-test-mode-line-thinking ()
  (emagent-test--with-busy-session
   (lambda ()
     (let* ((parts (emagent-chat--mode-line-strings))
            (head (substring-no-properties (car parts))))
       (should (string-match-p "^Thinking " head))
       (should (string-match-p "●\\|○" head))))))

(ert-deftest emagent-chat-test-restore-window-views-keeps-point ()
  (with-temp-buffer
    (insert "line1\nline2\n")
    (let ((saved-point 6)
          (win (selected-window)))
      (goto-char saved-point)
      (emagent-chat--restore-window-views
       `((:window ,win :start 1 :at-bottom t)))
      (should (= saved-point (point))))))

(ert-deftest emagent-chat-test-show-tool-call-no-open-response ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (emagent-chat--user-zone-start))
       (let ((pos (point)))
         (emagent-chat-show-tool-call "id1" "Read")
         (should (= pos (point))))))))

;;;; Tool calls during thinking

(ert-deftest emagent-chat-test-tool-call-during-reasoning-no-executing ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "thinking...")
          (emagent-chat-show-tool-call "id1" "read_file")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "Thinking" text))
            (should (string-match-p "→ read_file" text))
            (should-not (string-match-p "Executing" text)))))))))

(ert-deftest emagent-chat-test-tool-call-before-thought-on-new-line ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-show-tool-call "id1" "Read: /some/file.txt")
          (emagent-chat-append-thought "Test 123")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "→ Read: /some/file.txt\nTest 123" text))
            (should-not (string-match-p "/some/file.txtTest" text)))))))))

(ert-deftest emagent-chat-test-tool-call-after-close-thought-no-executing ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "thinking...")
          (emagent-chat-append-assistant "Hi")
          (emagent-chat-show-tool-call "id2" "grep")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "Thinking" text))
            (should (string-match-p "→ grep" text))
            (should-not (string-match-p "Executing" text)))))))))

(ert-deftest emagent-chat-test-tool-call-line-recorded-for-update ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-show-tool-call "id1" "Edit File")
          (should (gethash "id1" emagent-chat--tool-call-lines))
          (emagent-chat-show-tool-call "id1" "Edit File: foo.el")
          (let ((text (substring-no-properties (buffer-string))))
            (should (= 1 (length (remove nil (mapcar (lambda (line)
                                                       (when (string-match-p "→ Edit File" line)
                                                         line))
                                                     (split-string text "\n"))))))
            (should (string-match-p "→ Edit File: foo.el" text))
            (should-not (string-match-p "→ Edit File\n→ Edit File: foo.el" text)))))))))

(ert-deftest emagent-chat-test-tool-call-update-then-thought-on-new-line ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-show-tool-call "id1" "Read")
          (emagent-chat-show-tool-call "id1" "Read: /some/file.txt")
          (emagent-chat-append-thought "more thinking")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "→ Read: /some/file.txt\nmore thinking" text))
            (should-not (string-match-p "/some/file.txtmore" text)))))))))

(ert-deftest emagent-chat-test-tool-call-before-thought-opens-reasoning ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-show-tool-call "id3" "list_files")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "Thinking" text))
            (should (string-match-p "→ list_files" text))
            (should-not (string-match-p "Executing" text)))))))))

(ert-deftest emagent-chat-test-thought-after-fake-end-quote-appends-at-tail ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "before\n#+end_quote\nmiddle")
          (setq emagent-chat--thought-marker nil)
          (emagent-chat-append-thought " after")
          (emagent-chat-show-tool-call "id1" "grep: pattern")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p
                     "before\n#\\+end_quote\nmiddle after\n→ grep: pattern"
                     text))
            (should-not (string-match-p "middle\n→ grep" text)))))))))

(ert-deftest emagent-chat-test-stream-with-point-away-from-tail ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "line one")
          (goto-char (point-min))
          (emagent-chat-append-thought "\nline two")
          (emagent-chat-show-tool-call "id1" "Read: foo.el")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "line one\nline two\n→ Read: foo.el" text)))))))))

(ert-deftest emagent-chat-test-finish-keeps-tools-in-reasoning ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "planning...")
          (emagent-chat-show-tool-call "id1" "grep: pattern")
          (emagent-chat-show-tool-call "id1" "grep: foo")
          (emagent-chat-finish-assistant "Done." "planning...")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "Thinking" text))
            (should (string-match-p "planning..." text))
            (should (string-match-p "→ grep: foo" text))
            (should (string-match-p "Done\\." text))
            (should-not (string-match-p "Executing" text)))))))))

(provide 'emagent-chat-test)

;;; emagent-chat-test.el ends here
