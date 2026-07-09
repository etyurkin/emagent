;;; emagent-chat-test.el --- ERT tests for emagent chat UI -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-chat)
(require 'emagent-acp-tool-call)

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

(ert-deftest emagent-chat-test-lone-pipe-not-table ()
  "A lone \"|\" must not be treated as a table row (substring 1 -1 would
signal), so finalizing a response containing one does not crash."
  (should-not (emagent-chat--table-row-p "|"))
  (should-not (emagent-chat--table-row-p " | "))
  ;; Full conversion over content with a lone pipe must not signal.
  (should (stringp (emagent-chat--convert-agent-markup "before\n|\nafter"))))

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

(ert-deftest emagent-chat-test-normalize-model-id ()
  (should (string= "auto" (emagent-chat--normalize-model-id "default[]")))
  (should (string= "auto" (emagent-chat--normalize-model-id "default")))
  (should (string= "grok-4.3"
                   (emagent-chat--normalize-model-id "grok-4.3[context=200k]")))
  (should (string= "claude-sonnet-4-6"
                   (emagent-chat--normalize-model-id
                    "claude-sonnet-4-6[thinking=true]")))
  (should (string= "gpt-4" (emagent-chat--normalize-model-id "gpt-4"))))

(ert-deftest emagent-chat-test-canonical-model-id ()
  (should (string= "default[]" (emagent-chat--canonical-model-id "auto")))
  (should (string= "default[]" (emagent-chat--canonical-model-id "default")))
  (should (string= "grok-4.3[context=200k]"
                   (emagent-chat--canonical-model-id "grok-4.3[context=200k]"))))

(ert-deftest emagent-chat-test-model-choice-label ()
  (should (string= "grok-4.3[context=200k]"
                   (emagent-chat--model-choice-label
                    "grok-4.3[context=200k]" "grok-4.3")))
  (should (string= "default[] (Auto)"
                   (emagent-chat--model-choice-label "default[]" "Auto")))
  (should (string= "gpt-4 (GPT 4)"
                   (emagent-chat--model-choice-label "gpt-4" "GPT 4"))))

(ert-deftest emagent-chat-test-model-choice-label-display ()
  (let ((label (emagent-chat--model-choice-label-display
                "composer-2.5[fast=true]" "composer-2.5")))
    (should (string= "composer-2.5[fast=true]" (substring-no-properties label)))
    (should (eq 'emagent-model-choice-model (get-text-property 0 'face label)))
    (should (eq 'emagent-model-choice-detail (get-text-property 12 'face label))))
  (let ((label (emagent-chat--model-choice-label-display "default[]" "Auto")))
    (should (string= "default[] (Auto)" (substring-no-properties label)))
    (should (eq 'emagent-model-choice-model (get-text-property 0 'face label)))
    (should (eq 'emagent-model-choice-detail (get-text-property 7 'face label))))
  (let ((label (emagent-chat--model-choice-label-display "haiku" "Haiku")))
    (should (string= "haiku (Haiku)" (substring-no-properties label)))
    (should (eq 'emagent-model-choice-model (get-text-property 0 'face label)))
    (should (eq 'emagent-model-choice-detail (get-text-property 5 'face label)))))

(ert-deftest emagent-chat-test-set-model-stores-canonical-id ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (emagent-chat-set-model "default[]")
       (should (string= "default[]" (emagent-chat-model)))
       (should (string= "auto" (emagent-chat-model-display)))
       (should (string-match-p "^#\\+EMAGENT_MODEL: default\\[\\]"
                               (buffer-string)))
       (emagent-chat-set-model "grok-4.3[context=200k]")
       (should (string= "grok-4.3[context=200k]" (emagent-chat-model)))
       (should (string= "grok-4.3" (emagent-chat-model-display)))))))

(ert-deftest emagent-chat-test-mode-line-thinking ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (setq emagent-acp--session (emagent-acp--make-state))
     (setf (emagent-acp-state-busy emagent-acp--session) t)
     (pop-to-buffer buffer)
     (with-current-buffer buffer
       (emagent-test--sync-status)
       (let* ((parts (emagent-chat--mode-line-strings))
              (head (substring-no-properties (car parts))))
         (should (string-match-p "^Thinking " head))
         (should (string-match-p "●\\|○" head)))))))

(ert-deftest emagent-chat-test-mode-line-renders-from-pushed-status ()
  "The mode line renders purely from the pushed status snapshot, with no ACP
session present (the UI no longer pulls from the ACP layer)."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (pop-to-buffer buffer)
     (with-current-buffer buffer
       (setq emagent-acp--session nil)
       (emagent-chat-set-status '(:busy t))
       (should (string-match-p "^Thinking " (car (emagent-chat--mode-line-strings))))
       (emagent-chat-set-status '(:waiting-permission t :busy t))
       (should (string-match-p "^emagent:Allow\\?"
                               (car (emagent-chat--mode-line-strings))))
       (should-not (emagent-chat--spinner-active-p))
       (emagent-chat-set-status '(:ready t))
       (should-not (string-match-p "Thinking"
                                   (car (emagent-chat--mode-line-strings))))))))

(ert-deftest emagent-chat-test-mode-line-allow-no-spinner ()
  (emagent-test--with-busy-session
   (lambda ()
     (setf (emagent-acp-state-permission-busy emagent-acp--session) t)
     (emagent-test--sync-status)
     (let* ((parts (emagent-chat--mode-line-strings))
            (head (substring-no-properties (car parts))))
       (should (string-match-p "^emagent:Allow\\?" head))
       (should (not (string-match-p "●\\|○" head)))
       (should (not (emagent-chat--spinner-active-p)))))))

(ert-deftest emagent-chat-test-mode-line-idle-when-busy-clears ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (setq emagent-acp--session (emagent-acp--make-state))
     (setf (emagent-acp-state-busy emagent-acp--session) t)
     (setf (emagent-acp-state-ready emagent-acp--session) t)
     (with-current-buffer buffer
       (pop-to-buffer buffer)
       (emagent-test--sync-status)
       (emagent-chat--mode-line-recompute)
       (should (string-match-p "^Thinking " emagent-chat--mode-line-head))
       (setf (emagent-acp-state-busy emagent-acp--session) nil)
       (emagent-test--sync-status)
       (emagent-chat--refresh-mode-line)
       (should (string-match-p "Idle" emagent-chat--mode-line-head))))))

(ert-deftest emagent-chat-test-mode-line-refresh-deferred-when-inactive ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (setq emagent-acp--session (emagent-acp--make-state))
     (setf (emagent-acp-state-busy emagent-acp--session) t)
     (with-temp-buffer
       (pop-to-buffer (current-buffer))
       (with-current-buffer buffer
         (setq emagent-chat--mode-line-stale-p nil
               emagent-chat--mode-line-head "old")
         (emagent-chat--refresh-mode-line-soon)
         (should emagent-chat--mode-line-stale-p)
         (should (string= "old" emagent-chat--mode-line-head)))
       (pop-to-buffer buffer)
       (with-current-buffer buffer
         (emagent-test--sync-status)
         (emagent-chat--refresh-mode-line-on-focus)
         (should-not emagent-chat--mode-line-stale-p)
         (should (string-match-p "^Thinking" emagent-chat--mode-line-head)))))))

(ert-deftest emagent-chat-test-font-lock-deferred-when-inactive ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-temp-buffer
       (pop-to-buffer (current-buffer))
       (with-current-buffer buffer
         (setq emagent-chat--font-lock-deferred-p nil)
         (emagent-chat--maybe-font-lock-flush)
         (should emagent-chat--font-lock-deferred-p))
       (pop-to-buffer buffer)
       (with-current-buffer buffer
         (emagent-chat--flush-deferred-font-lock)
         (should-not emagent-chat--font-lock-deferred-p))))))

(ert-deftest emagent-chat-test-inactive-bell-rings-when-background-update ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (let ((rings 0)
             (emagent-chat-inactive-bell t)
             (emagent-chat-inactive-osx-notification nil)
             (emagent-chat-inactive-bell-cooldown 0)
             (emagent-chat--emacs-focused-p nil)
             (emagent-chat--last-inactive-bell-time 0.0))
         (cl-letf (((symbol-function 'get-buffer-window)
                    (lambda (&rest _args) nil))
                   ((symbol-function 'ding)
                    (lambda (&rest _args)
                      (setq rings (1+ rings)))))
           (emagent-chat--notify-inactive-update)
           (should (= rings 1))))))))

(ert-deftest emagent-chat-test-inactive-bell-throttled-and-suppressed-when-active ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (let ((rings 0)
             (emagent-chat-inactive-bell t)
             (emagent-chat-inactive-osx-notification nil)
             (emagent-chat-inactive-bell-cooldown 60)
             (emagent-chat--emacs-focused-p nil)
             (emagent-chat--last-inactive-bell-time 0.0)
             (now 1000.0))
         (cl-letf (((symbol-function 'float-time)
                    (lambda () now))
                   ((symbol-function 'get-buffer-window)
                    (lambda (&rest _args) nil))
                   ((symbol-function 'ding)
                    (lambda (&rest _args)
                      (setq rings (1+ rings)))))
           (emagent-chat--notify-inactive-update)
           (should (= rings 1))
           (emagent-chat--notify-inactive-update)
           (should (= rings 1)))
         (cl-letf (((symbol-function 'get-buffer-window)
                    (lambda (&rest _args) t))
                   ((symbol-function 'ding)
                    (lambda (&rest _args)
                      (setq rings (1+ rings)))))
           (emagent-chat--notify-inactive-update)
           (should (= rings 1)))
         (let ((emagent-chat--emacs-focused-p t))
           (cl-letf (((symbol-function 'get-buffer-window)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'ding)
                      (lambda (&rest _args)
                        (setq rings (1+ rings)))))
             (emagent-chat--notify-inactive-update)
             (should (= rings 1)))))))))

(ert-deftest emagent-chat-test-spinner-sync-frame ()
  (let ((emagent-chat-spinner-interval 0.1)
        (emagent-chat-spinner-style 'dots))
    (setq emagent-chat--spinner-start-time (- (float-time) 0.25))
    (emagent-chat--spinner-sync-frame)
    (should (= (% (floor 0.25 0.1) 4) emagent-chat--spinner-frame))))

(ert-deftest emagent-chat-test-spinner-ensure-running ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (setq emagent-acp--session (emagent-acp--make-state))
     (setf (emagent-acp-state-busy emagent-acp--session) t)
     (setq emagent-chat--spinner-timer nil
           emagent-chat--spinner-start-time nil)
     (with-current-buffer buffer
       (pop-to-buffer buffer)
       (emagent-test--sync-status)
       (emagent-chat--spinner-ensure-running)
       (should emagent-chat--spinner-timer)
       (setf (emagent-acp-state-permission-busy emagent-acp--session) t)
       (emagent-test--sync-status)
       (emagent-chat--spinner-refresh-idle)
       (should-not emagent-chat--spinner-timer)
       (setf (emagent-acp-state-permission-busy emagent-acp--session) nil)
       (emagent-test--sync-status)
       (emagent-chat--spinner-ensure-running)
       (should emagent-chat--spinner-timer)
       (emagent-chat--spinner-stop)))))

(ert-deftest emagent-chat-test-spinner-only-when-active ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (setq emagent-acp--session (emagent-acp--make-state))
     (setf (emagent-acp-state-busy emagent-acp--session) t)
     (setq emagent-chat--spinner-timer nil)
     (with-temp-buffer
       (pop-to-buffer (current-buffer))
       (with-current-buffer buffer
         (emagent-test--sync-status)
         (emagent-chat--mode-line-recompute)
         (should (string-match-p "^Thinking$" (substring-no-properties emagent-chat--mode-line-head)))
         (should-not (string-match-p "●\\|○" emagent-chat--mode-line-head))
         (should-not (emagent-chat--spinner-animate-p buffer)))
       (pop-to-buffer buffer)
       (with-current-buffer buffer
         (emagent-chat--mode-line-recompute)
         (should (string-match-p "●\\|○" emagent-chat--mode-line-head))
         (should (emagent-chat--spinner-animate-p buffer))
         (emagent-chat--spinner-ensure-running)
         (should emagent-chat--spinner-timer)
         (emagent-chat--spinner-stop))))))

(ert-deftest emagent-chat-test-tool-line-decision-face ()
  "The permission decision suffix on a tool line gets the grey decision face."
  (dolist (case '(("→ ls ~/.cargo/registry (Allow: Session)" . "(Allow: Session)")
                  ("→ rm -rf build (Denied)" . "(Denied)")))
    (with-temp-buffer
      (insert (car case))
      (emagent-chat--repair-tool-line-faces (point-min) (point-max))
      (goto-char (point-min))
      (should (re-search-forward (regexp-quote (cdr case)) nil t))
      (should (eq 'emagent-tool-permission-decision
                  (get-text-property (match-beginning 0) 'face))))))

(ert-deftest emagent-chat-test-spinner-animates-in-unselected-window ()
  "Spinner keeps animating while the buffer is shown in a non-selected window."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (setq emagent-acp--session (emagent-acp--make-state))
     (setf (emagent-acp-state-busy emagent-acp--session) t)
     (setq emagent-chat--spinner-timer nil)
     (with-current-buffer buffer (emagent-test--sync-status))
     (let ((other (get-buffer-create "*emagent-other-window*")))
       (unwind-protect
           (progn
             (delete-other-windows)
             (switch-to-buffer other)
             (set-window-buffer (split-window) buffer)
             ;; `buffer' is displayed but not in the selected window.
             (should-not (emagent-chat--buffer-active-p buffer))
             (should (emagent-chat--buffer-displayed-p buffer))
             (should (emagent-chat--spinner-animate-p buffer)))
         (delete-other-windows)
         (when (buffer-live-p other) (kill-buffer other)))))))

(ert-deftest emagent-chat-test-spinner-refresh-without-cache ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (setq emagent-acp--session (emagent-acp--make-state))
     (setf (emagent-acp-state-busy emagent-acp--session) t)
     (with-current-buffer buffer
       (emagent-test--sync-status)
       (setq emagent-chat--mode-line-head nil
             emagent-chat--mode-line-tail nil
             emagent-chat--mode-line-cache nil
             emagent-chat--spinner-frame 0)
       (pop-to-buffer buffer)
       (should (emagent-chat--spinner-refresh-buffer buffer))
       (should (string-match-p "Thinking ●" emagent-chat--mode-line-head))
       (setq emagent-chat--spinner-frame 1)
       (should (emagent-chat--spinner-refresh-buffer buffer))
       (should (string-match-p "Thinking ○●○" emagent-chat--mode-line-head))))))

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
            (should (string-match-p "→ Read: =/some/file.txt=\n\nTest 123" text))
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

(ert-deftest emagent-chat-test-tool-call-multiline-renders-src-block ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-show-tool-call
           "id-blk" "Shell: cd /tmp && run"
           "sh" "cd /tmp && python3 - <<'PY'\nprint(1)\nPY")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "#\\+begin_src sh" text))
            (should (string-match-p "python3 - <<'PY'" text))
            (should (string-match-p "#\\+end_src" text))
            ;; arrow names the tool above the block; no =verbatim= path mangling.
            (should (string-match-p "→ Shell" text))
            (should-not (string-match-p "=/tmp=" text)))))))))

(ert-deftest emagent-chat-test-tool-call-multiline-updates-in-place ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-show-tool-call "id-blk" "Shell: run"
                                       "sh" "echo one\necho two")
          (emagent-chat-show-tool-call "id-blk" "Shell: run"
                                       "sh" "echo one\necho two\necho three")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "echo three" text))
            ;; only one src block remains after the in-place update.
            (should (= 1 (cl-count-if
                          (lambda (line) (string-match-p "#\\+begin_src" line))
                          (split-string text "\n")))))))))))

(ert-deftest emagent-chat-test-tool-call-non-code-stays-arrow ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          ;; No CODE argument: a plain read/grep-style line stays compact.
          (emagent-chat-show-tool-call "id-line" "Read: /some/file.txt")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "→ Read:" text))
            (should-not (string-match-p "#\\+begin_src" text)))))))))

(ert-deftest emagent-chat-test-tool-call-singleline-command-renders-block ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          ;; A single long command (CODE provided) renders as a src block
          ;; beneath an arrow line that names the tool.
          (emagent-chat-show-tool-call "id-c" "Shell: cargo" "sh"
                                       "cargo add foo --dry-run")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "#\\+begin_src sh" text))
            (should (string-match-p "cargo add foo --dry-run" text))
            (should (string-match-p "→ Shell: cargo" text)))))))))

(ert-deftest emagent-chat-test-tool-call-multiline-decision-annotation ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-show-tool-call
           "id-blk" "Shell: run (Allow: Always)"
           "sh" "echo one\necho two")
          (let ((text (substring-no-properties (buffer-string))))
            ;; annotation rides on the arrow line above the block, not on the
            ;; command lines or dangling beneath the block.
            (should (string-match-p "→ Shell (Allow: Always)" text))
            (should-not (string-match-p "echo two (Allow: Always)" text))
            (should-not (string-match-p "#\\+end_src\n(Allow: Always)" text)))))))))

(ert-deftest emagent-chat-test-tool-call-flushes-pending-reasoning ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat--insert-thought-now "first half")
          ;; Buffered reasoning not yet flushed when the tool call arrives.
          (setq emagent-chat--thought-pending " second half")
          (emagent-chat-show-tool-call "id1" "grep: pattern")
          (let ((text (substring-no-properties (buffer-string))))
            ;; the sentence stays intact, with the tool line after it.
            (should (string-match-p "first half second half\n\n→ grep: pattern" text))
            (should-not (string-match-p "first half\n\n→ grep" text)))))))))

(ert-deftest emagent-chat-test-tool-line-separated-from-prose ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "planning the change")
          (emagent-chat-show-tool-call "id-a" "Read: /a.el")
          (emagent-chat-show-tool-call "id-b" "Read: /b.el")
          (let ((text (substring-no-properties (buffer-string))))
            ;; one blank line between prose and the first tool line,
            (should (string-match-p "planning the change\n\n→ Read: /a.el" text))
            ;; but consecutive distinct tool lines stay adjacent.
            (should (string-match-p "→ Read: /a.el\n→ Read: /b.el" text)))))))))

(ert-deftest emagent-chat-test-reasoning-headline-marker-owned ()
  "Multi-chunk reasoning uses the owned `** Thinking' headline marker, and the
marker points at the actual Thinking headline."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "first reasoning ")
          (should (markerp emagent-chat--thinking-headline-marker))
          (should (= (emagent-chat--open-reasoning-begin)
                     (marker-position emagent-chat--thinking-headline-marker)))
          (emagent-chat-append-thought "second reasoning")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "\\*\\* Thinking" text))
            (should (string-match-p "first reasoning" text))
            (should (string-match-p "second reasoning" text))
            ;; The marker sits exactly at the Thinking headline.
            (goto-char (marker-position emagent-chat--thinking-headline-marker))
            (should (looking-at-p "\\*\\* Thinking")))))))))

(ert-deftest emagent-chat-test-tool-call-block-then-thought-separated ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-show-tool-call "id-blk" "Shell: run (Allow: Always)"
                                       "sh" "echo one\necho two")
          (emagent-chat-append-thought "resuming reasoning")
          (let ((text (substring-no-properties (buffer-string))))
            ;; prose never glues onto the block close or its decision comment.
            (should (string-match-p "#\\+end_src\n\nresuming reasoning" text))
            (should-not (string-match-p "#\\+end_srcresuming" text))
            (should-not (string-match-p "Always)resuming" text)))))))))

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
            ;; prose resumes one blank line below the tool line.
            (should (string-match-p "→ Read: =/some/file.txt=\n\nmore thinking" text))
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
                     "before\n #\\+end_quote\nmiddle after\n\n→ grep: pattern"
                     text))
            (should-not (string-match-p "middle\n→ grep" text)))))))))

(ert-deftest emagent-chat-test-escape-reasoning-text ()
  (should (string= " #+end_quote"
                   (emagent-chat--escape-reasoning-text "#+end_quote")))
  (should (string= " * headline"
                   (emagent-chat--escape-reasoning-text "* headline")))
  ;; #+begin_src/#+end_src markers are preserved (not space-escaped) so they
  ;; stay valid org src-block delimiters.
  (should (string= "plain\n#+BEGIN_SRC elisp"
                   (emagent-chat--escape-reasoning-text "plain\n#+BEGIN_SRC elisp"))))

(ert-deftest emagent-chat-test-markup-preserves-code-interiors ()
  "Prose transforms must not rewrite the inside of a code block: fenced
backticks, markdown headings, and org-star lines survive verbatim while the
same markup outside the block is converted."
  (let ((out (emagent-chat--convert-agent-markup
              (concat "Run `date` now.\n\n"
                      "```sh\n"
                      "echo `pwd`\n"
                      "## not a heading\n"
                      "* not a bullet\n"
                      "```\n\n"
                      "## Real Heading"))))
    ;; Inline code and heading OUTSIDE the block are converted.
    (should (string-match-p "Run =date= now" out))
    (should (string-match-p "^\\*\\* Real Heading" out))
    ;; Code INTERIOR is untouched.
    (should (string-match-p "echo `pwd`" out))
    (should (string-match-p "^## not a heading" out))
    (should (string-match-p "^\\* not a bullet" out))))

(ert-deftest emagent-chat-test-markup-preserves-code-links-and-sentences ()
  "Markdown-link and sentence-spacing transforms also skip src-block interiors."
  (let ((out (emagent-chat--convert-agent-markup
              (concat "See [docs](http://x) and note path.Join.\n\n"
                      "```go\n"
                      "y = arr[i](fn)\n"
                      "call p.Do done.Next\n"
                      "```"))))
    ;; Prose: link converted, sentence spaced.
    (should (string-match-p "\\[\\[http://x\\]\\[docs\\]\\]" out))
    (should-not (string-match-p "note path.Join" out)) ; spaced to "path. Join"
    ;; Code interior: array-index-then-call and glued sentence untouched.
    (should (string-match-p "arr\\[i\\](fn)" out))
    (should (string-match-p "done.Next" out))))

(ert-deftest emagent-chat-test-markup-escapes-interior-src-delimiters ()
  "A fenced block documenting Org (literal #+END_SRC in its body) stays one
block: the interior delimiters are comma-escaped, so neither our matcher nor
Org closes early and the trailing prose is still converted."
  (let ((out (emagent-chat--convert-agent-markup
              (concat "How a block looks:\n\n"
                      "```org\n"
                      "#+BEGIN_SRC emacs-lisp\n"
                      "(+ 1 2)\n"
                      "#+END_SRC\n"
                      "```\n\n"
                      "See [docs](http://x) after."))))
    ;; Interior delimiters escaped, so exactly one block is matched.
    (should (string-match-p "^,#\\+END_SRC" out))
    (let ((n 0) (pos 0))
      (while (string-match emagent-chat--src-block-re out pos)
        (setq n (1+ n) pos (match-end 0)))
      (should (= n 1)))
    ;; Trailing prose after the real terminator is still transformed.
    (should (string-match-p "\\[\\[http://x\\]\\[docs\\]\\]" out))))

(ert-deftest emagent-chat-test-turn-model-region-scan ()
  "A `/model'-stamped model in the prompt is read back by the region scanner."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (insert "commit, use ")
    (insert (propertize "haiku" emagent-chat--turn-model-property "haiku"))
    (should (equal "haiku"
                   (emagent-chat--region-turn-model (point-min) (point-max))))
    ;; Only the stamped text carries it; plain text does not.
    (should-not (emagent-chat--region-turn-model (point-min) (+ (point-min) 6)))))

(ert-deftest emagent-chat-test-turn-model-thinking-indicator ()
  "The Thinking headline shows the per-turn model; the regex still matches both."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (goto-char (point-max))
    (setq emagent-chat--turn-model "haiku"
          emagent-chat--response-body-start (copy-marker (point) nil))
    (emagent-chat--insert-reasoning-scaffold)
    (goto-char (marker-position emagent-chat--thinking-headline-marker))
    (should (looking-at-p "\\*\\* Thinking (haiku)"))
    (should (string-match-p emagent-chat--thinking-headline-re "** Thinking (haiku)"))
    (should (string-match-p emagent-chat--thinking-headline-re "** Thinking"))))

(ert-deftest emagent-chat-test-turn-model-restore-clears ()
  "A successful turn restores the base model and clears the override state."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (setq emagent-chat--turn-model "haiku"
          emagent-chat--turn-model-base "sonnet")
    (let (restored)
      (cl-letf (((symbol-function 'emagent-acp-set-model-transient)
                 (lambda (m _cb) (setq restored m))))
        (emagent--restore-turn-model))
      (should (equal "sonnet" restored))
      (should-not emagent-chat--turn-model)
      (should-not emagent-chat--turn-model-base))))

(ert-deftest emagent-chat-test-markup-normalizes-crlf ()
  "CRLF output is normalized to LF so src blocks still segment and no ^M leaks."
  (let ((out (emagent-chat--convert-agent-markup
              (concat "Use `x` here.\r\n\r\n"
                      "```python\r\n"
                      "y = `z`\r\n"
                      "```\r\n\r\n"
                      "Done."))))
    (should-not (string-match-p "\r" out))
    (should (string-match-p "Use =x=" out))       ; prose converted
    (should (string-match-p "#\\+BEGIN_SRC" out))  ; block recognized despite CRLF
    (should (string-match-p "y = `z`" out))))      ; code interior preserved

(ert-deftest emagent-chat-test-demote-preserves-code-stars ()
  "Heading demotion skips org-star lines inside src blocks."
  (let ((out (emagent-chat--demote-response-headings
              "* Heading\n#+BEGIN_SRC sh\n* starred code line\n#+END_SRC\n")))
    (should (string-match-p "^\\*\\*\\* Heading" out))
    (should (string-match-p "^\\* starred code line" out))))

(ert-deftest emagent-chat-test-close-unclosed-code-fence ()
  (let ((out (emagent-chat--convert-agent-markup "text\n```elisp\n(+ 1 2)\n")))
    (should (string-match-p "#\\+BEGIN_SRC elisp" out))
    (should (string-match-p "(\\+ 1 2)" out))
    (should (string-match-p "#\\+END_SRC" out))
    (should-not (string-match-p "```" out))))

(ert-deftest emagent-chat-test-close-unclosed-org-src ()
  (let ((out (emagent-chat--convert-agent-markup
              "see:\n#+BEGIN_SRC shell\n#!/bin/sh\necho hi\n")))
    (should (string-match-p "#\\+END_SRC" out))
    (should-not (string-match-p "#\\+BEGIN_SRC shell\\n#!/bin/sh\\necho hi\\s-*\\'" out))))

(ert-deftest emagent-chat-test-sentence-space-preserves-filenames ()
  "An ALL-CAPS filename like VDUNGEON.DAT must not get a space inserted.
Regression: the sentence-glue-space fix used to fire on any [.?!] followed
by a capital letter, so `VDUNGEON.DAT' rendered as `VDUNGEON. DAT'."
  (should (string= "VDUNGEON.DAT"
                   (emagent-chat--convert-agent-markup "VDUNGEON.DAT")))
  (should (string= "see TileRenderer.add() for details"
                   (emagent-chat--convert-agent-markup
                    "see TileRenderer.add() for details")))
  ;; A genuinely glued sentence (lowercase word run into a capitalized one)
  ;; still gets its space back.
  (should (string= "Done. Next step"
                   (emagent-chat--convert-agent-markup "Done.Next step"))))

(ert-deftest emagent-chat-test-begin-response-no-eager-thinking ()
  "`begin-response' opens a response without inserting an empty Thinking block."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (emagent-chat--begin-response (point))
       (should (emagent-chat--open-response-p))
       (let ((text (substring-no-properties (buffer-string))))
         (should-not (string-match-p (regexp-quote emagent-chat-thinking-headline)
                                     text)))))))

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
            (should (string-match-p "line one\nline two\n\n→ Read: foo.el" text)))))))))

(ert-deftest emagent-chat-test-thought-blank-chunks-do-not-pile-up ()
  "Repeated blank-only reasoning deltas collapse to one blank line.
Some agents stream bare paragraph-break chunks (no other content) while
still composing; each used to insert its newlines verbatim, so a long
pause produced a growing run of blank lines at the end of the buffer."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "Actual reasoning text.")
          (dotimes (_ 20)
            (emagent-chat-append-thought "\n\n"))
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p
                     "Actual reasoning text\\.\n\n\\'" text))
            (should-not (string-match-p
                         "Actual reasoning text\\.\n\n\n" text)))))))))

(ert-deftest emagent-chat-test-thought-tool-cycles-do-not-pile-up ()
  "Interleaved reasoning and tool cycles never grow a blank tail.
After an in-place tool-call update the streaming marker sits before the tool
line's trailing newline; blank-only reasoning deltas used to strand those
newlines, growing the Thinking tail one blank line per tool cycle."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (dotimes (i 5)
            (emagent-chat-append-thought (format "Reasoning paragraph %d." i))
            (emagent-chat-append-thought "\n\n")
            (let ((id (format "tool%d" i)))
              (emagent-chat-show-tool-call id (format "Read: /file%d.el" i))
              (emagent-chat-show-tool-call
               id (format "Read: /file%d.el (Allow: Agent)" i)))
            (emagent-chat-append-thought "\n")
            (emagent-chat-append-thought "\n\n"))
          ;; Inspect only the Thinking content (the scaffold above it keeps a
          ;; blank line by design); it must never hold two blank lines in a row.
          (let* ((text (substring-no-properties (buffer-string)))
                 (start (string-match "^\\*\\* Thinking" text))
                 (thinking (substring text start)))
            (should-not (string-match-p "\n\n\n" thinking)))))))))

(ert-deftest emagent-chat-test-thought-close-reopen-cycles-do-not-pile-up ()
  "Thought close/reopen cycles neither glue thoughts nor grow a blank tail.
On reopen the stream marker re-syncs by skipping back over the previous
thought's trailing newlines; the resumed text used to be inserted before
that stranded run, gluing onto the previous thought while the tail grew
two newlines per cycle."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (dotimes (i 4)
            (emagent-chat-append-thought (format "Thought %d.\n\n" i))
            (emagent-chat-close-thought)
            (emagent-chat-begin-thought))
          (let* ((text (substring-no-properties (buffer-string)))
                 (start (string-match "^\\*\\* Thinking" text))
                 (thinking (substring text start)))
            (should (string-match-p "Thought 0\\.\n\nThought 1\\." thinking))
            (should-not (string-match-p "Thought 0\\.Thought 1\\." thinking))
            (should-not (string-match-p "\n\n\n" thinking)))))))))

(ert-deftest emagent-chat-test-interleaved-thought-and-response ()
  "Alternating thought and assistant chunks keep both sections clean.
Each assistant chunk closes the thought, so an interleaving agent reopens
the Thinking block many times per turn; resumed thoughts used to glue
together while a blank run grew before the `** Response' headline, and the
run stranded at the reasoning tail leaked blank lines into the Response
body when the headline was created."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "Thought one.\n\n")
          (emagent-chat-append-assistant "Answer part one. ")
          (emagent-chat-append-thought "Thought two.\n\n")
          (emagent-chat-append-assistant "Answer part two.")
          (let* ((text (substring-no-properties (buffer-string)))
                 (start (string-match "^\\*\\* Thinking" text))
                 (body (substring text start)))
            (should (string-match-p "Thought one\\.\n\nThought two\\." body))
            (should (string-match-p
                     "Thought two\\.\n\n\\*\\* Response\nAnswer part one\\. Answer part two\\."
                     body))
            (should-not (string-match-p "\n\n\n" body)))))))))

(ert-deftest emagent-chat-test-thought-inline-code-split-across-chunks ()
  "A `code' span split across streaming chunks converts to =verbatim=.
The opening backtick arrives in one delta and the closing backtick in the
next; a per-chunk conversion left both raw, so the fix holds the open span
until it closes."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "I need no `anim_")
          (emagent-chat-append-thought "right` layer here.")
          (emagent-chat-close-thought)
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "=anim_right=" text))
            (should-not (string-match-p "`anim_right`" text)))))))))

(ert-deftest emagent-chat-test-thought-bold-split-across-chunks ()
  "A `**bold**' span split across streaming chunks converts to org `*bold*'.
The opener arrives in one delta and the closer in the next; holding the open
span until it closes avoids raw `**' markers and keeps a single space before
the resumed text rather than the leading space `escape-reasoning-line' would
add to a `*'-initial line."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "use **anim")
          (emagent-chat-append-thought "_right** here")
          (emagent-chat-close-thought)
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "use \\*anim_right\\* here" text))
            (should-not (string-match-p "\\*\\*anim" text)))))))))

(ert-deftest emagent-chat-test-thought-link-split-across-chunks ()
  "A markdown link split across streaming chunks converts to an org link.
The boundary can fall inside the text, before the `(', or inside the URL; each
partial state is held until the closing `)' arrives.  A bare bracket that is
not a link (e.g. a `[1]' citation) must not stall the stream."
  (dolist (case '(("see [docs](http://ex" "ample.com) now")
                  ("see [do" "cs](http://example.com) now")
                  ("see [docs]" "(http://example.com) now")))
    (emagent-test--with-emagent-buffer
     (lambda (buffer _dir)
       (emagent-test--with-busy-session
        (lambda ()
          (with-current-buffer buffer
            (goto-char (point-max))
            (emagent-chat--begin-response (point))
            (emagent-chat-begin-thought)
            (dolist (chunk case) (emagent-chat-append-thought chunk))
            (emagent-chat-close-thought)
            (let ((text (substring-no-properties (buffer-string))))
              (should (string-match-p
                       "\\[\\[http://example.com\\]\\[docs\\]\\] now" text))
              (should-not (string-match-p "](http" text))))))))))

(ert-deftest emagent-chat-test-thought-bracket-citation-not-held ()
  "A `[1]' citation followed by prose is not mistaken for an open link.
Holding it would stall the stream until an unrelated `)' or newline, so the
bracket must flush once the following non-`(' text confirms it is not a link."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "ref [1]")
          (emagent-chat-append-thought " and more")
          (emagent-chat-close-thought)
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "ref \\[1\\] and more" text)))))))))

(ert-deftest emagent-chat-test-beginning-of-line-on-user-prompt ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (emagent-chat--sync-user-zone-marker)
       (emagent-chat--insert-user-heading-stub)
       (insert "hello")
       (let ((input (emagent-chat--user-prompt-input-pos))
             (bol (line-beginning-position)))
         (end-of-line)
         (emagent-chat-beginning-of-line)
         (should (= (point) input))
         (emagent-chat-beginning-of-line)
         (should (= (point) bol))
         (emagent-chat-beginning-of-line)
         (should (= (point) bol)))))))

(ert-deftest emagent-chat-test-history-prev-next-on-user-input ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (let ((prefix (emagent-chat--user-heading-prefix)))
         (insert "\n" prefix "alpha\n\n" prefix "beta\n\n" prefix "current"))
       (goto-char (point-max))
       (should (string= "current" (emagent-chat--current-user-input)))
       (call-interactively #'emagent-chat-history-previous-or-previous-line)
       (should (string= "beta" (emagent-chat--current-user-input)))
       (call-interactively #'emagent-chat-history-previous-or-previous-line)
       (should (string= "alpha" (emagent-chat--current-user-input)))
       (call-interactively #'emagent-chat-history-next-or-next-line)
       (should (string= "beta" (emagent-chat--current-user-input)))
       (call-interactively #'emagent-chat-history-next-or-next-line)
       (should (string= "current" (emagent-chat--current-user-input)))))))

(ert-deftest emagent-chat-test-history-only-after-user-prefix ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (let ((prefix (emagent-chat--user-heading-prefix)))
         (insert "\n" prefix "alpha\n\n" prefix "current"))
       (goto-char (point-max))
       (let ((target (line-beginning-position)))
         (beginning-of-line)
         (call-interactively #'emagent-chat-history-previous-or-previous-line)
         (should (< (point) target)))
       (goto-char (point-max))
       (should (string= "current" (emagent-chat--current-user-input)))))))

(ert-deftest emagent-chat-test-org-verbatim-paths ()
  (should (string= "Read: =/Users/etyurkin/foo.el="
                   (emagent-chat--org-verbatim-paths "Read: /Users/etyurkin/foo.el")))
  (should (string= "→ Read: =/tmp/x="
                   (emagent-chat--format-tool-line "Read: /tmp/x"))))

(ert-deftest emagent-chat-test-finish-moves-point-to-user-prompt ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "planning...")
          (goto-char (point-min))
          (emagent-chat-finish-assistant "Done.")
          (should (>= (point) (emagent-chat--user-zone-start)))
          (save-excursion
            (beginning-of-line)
            (should (looking-at (emagent-chat--user-heading-re))))))))))

(ert-deftest emagent-chat-test-finish-no-thinking-inserts-result ()
  "finish-assistant with no reasoning still renders the answer under ** Response."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-finish-assistant "Hello world.")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "^\\*\\* Response$" text))
            (should (string-match-p "Hello world\\." text))
            (should-not (string-match-p "^\\*\\* Thinking" text))
            (should-not (string-match-p (regexp-quote (emagent-chat--user-heading-prefix))
                                        (match-string 0 text))))))))))

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

(ert-deftest emagent-chat-test-thinking-created-lazily ()
  "The `** Thinking' subsection appears only once reasoning is streamed."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (should-not emagent-chat--thought-open-p)
          (should-not (string-match-p "^\\*\\* Thinking"
                                      (substring-no-properties (buffer-string))))
          (emagent-chat-append-thought "reasoning...")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match "^\\*\\* Thinking" text))
            (should-not (string-match "#\\+begin_quote" text))
            (should emagent-chat--thought-open-p)
            (should (markerp emagent-chat--thought-marker))
            (should (string-match-p "reasoning\\.\\.\\." text)))))))))

(ert-deftest emagent-chat-test-format-permission-line ()
  (should (string= "? make test" (emagent-chat--format-permission-line "make test"))))

(ert-deftest emagent-chat-test-reasoning-scaffold-repairs-missing-markers ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (setq emagent-chat--thought-marker nil
                emagent-chat--thought-open-p nil)
          (emagent-chat-show-tool-call "id1" "Read: foo.el")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "^\\*\\* Thinking" text))
            (should (string-match-p "→ Read: foo.el" text)))))))))

(ert-deftest emagent-chat-test-ensure-scaffold-after-close-reopens ()
  "`ensure-reasoning-scaffold' reopens thought after close when end_quote exists."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-close-thought)
          (should-not emagent-chat--thought-open-p)
          ;; Without explicit begin-thought, ensure-scaffold should re-open it
          (emagent-chat--ensure-reasoning-scaffold)
          (should emagent-chat--thought-open-p)
          (should (markerp emagent-chat--thought-marker))))))))

(ert-deftest emagent-chat-test-permission-buttons-below-end-quote ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-show-tool-call "id1" "compile")
          (let (layout args tool-call)
            (setq args (make-hash-table :test 'equal))
            (puthash "command" "make test" args)
            (setq tool-call `((toolCallId . "tool_compile")
                               (title . "compile")
                               (arguments . ,args)))
            (emagent-chat-permission-prompt
             "make test"
             '(("Allow once" . :allow-once))
             (lambda (_choice)
               (setq layout (substring-no-properties (buffer-string))))
             tool-call)
            (setq layout (substring-no-properties (buffer-string)))
            (should (string-match-p "^\\*\\* Allow execute" layout))
            (should (string-match-p "#\\+BEGIN_SRC sh\nmake test\n#\\+END_SRC" layout))
            (should (string-match-p "#\\+END_SRC\n\\[Allow once\\]" layout))
            (should-not (string-match-p "\\? make test" layout))
            (emagent-test--push-first-button buffer)
            (let ((text (substring-no-properties (buffer-string))))
              (should (string-match-p "^\\*\\* Thinking" text))
              (should-not (string-match-p "\\? make test" text))
              (should-not (string-match-p "\\[Allow once\\]" text))))))))))

(ert-deftest emagent-chat-test-permission-eval-content-block ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-show-tool-call "id1" "emagent-eval: eval")
          (let ((args (make-hash-table :test 'equal))
                layout)
            (puthash "form" "(require 'emagent)" args)
            (emagent-chat-permission-prompt
             "emagent-eval: eval"
             '(("Allow once" . :allow-once))
             (lambda (_choice) nil)
             `((toolCallId . "tool_eval")
               (title . "emagent-eval: eval")
               (arguments . ,args)))
            (setq layout (substring-no-properties (buffer-string)))
            (should (string-match-p "^\\*\\* Allow eval" layout))
            (should (string-match-p "#\\+BEGIN_SRC elisp\n(require 'emagent)\n#\\+END_SRC"
                                    layout))
            (should-not (string-match-p "\\? emagent-eval: eval" layout)))))))))

(ert-deftest emagent-chat-test-permission-eval-content-block-dedup ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          ;; Same toolCallId as the permission request below: the pending
          ;; tool-call line already shows the eval form as a src block.
          (emagent-chat-show-tool-call "tool_eval" "emagent-eval: eval"
                                       "elisp" "(require 'emagent)")
          (let ((args (make-hash-table :test 'equal))
                layout count start)
            (puthash "form" "(require 'emagent)" args)
            (emagent-chat-permission-prompt
             "emagent-eval: eval"
             '(("Allow once" . :allow-once))
             (lambda (_choice) nil)
             `((toolCallId . "tool_eval")
               (title . "emagent-eval: eval")
               (arguments . ,args)))
            (setq layout (substring-no-properties (buffer-string)))
            (setq count 0 start 0)
            (while (string-match (regexp-quote "(require 'emagent)") layout start)
              (setq count (1+ count) start (match-end 0)))
            (should (= count 1))
            (should-not (string-match-p "\\*\\* Allow eval" layout))
            (should (string-match-p "\\[Allow once\\]" layout)))))))))

(ert-deftest emagent-chat-test-permission-question-dedup ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          ;; Same toolCallId: the pending tool-call arrow line already
          ;; shows the path, so the "? path" line would just repeat it.
          (emagent-chat-show-tool-call "tool_read" "Read /tmp/l2r4.png")
          (let (layout)
            (emagent-chat-permission-prompt
             "/tmp/l2r4.png"
             '(("Allow once" . :allow-once))
             (lambda (_choice) nil)
             `((toolCallId . "tool_read")
               (title . "Read /tmp/l2r4.png")))
            (setq layout (substring-no-properties (buffer-string)))
            (should-not (string-match-p "\\? " layout))
            (should (string-match-p "\\[Allow once\\]" layout)))))))))

(ert-deftest emagent-chat-test-permission-heredoc-dedup ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (let* ((cmd "python3 - <<'EOF'\nimport json\nprint(1)\nEOF")
                 (args (make-hash-table :test 'equal))
                 (tool-call `((toolCallId . "tool_py")
                              (title . "python3")
                              (arguments . ,args)))
                 spec layout count start)
            (puthash "command" cmd args)
            ;; The pending tool-call preview also unwraps the heredoc, so it
            ;; renders the same python body the permission block would show.
            (setq spec (emagent-acp--tool-call-block-spec tool-call))
            (should (equal (car spec) "python"))
            (emagent-chat-show-tool-call "tool_py" "python3" (car spec) (cdr spec))
            (emagent-chat-permission-prompt
             "python3"
             '(("Allow once" . :allow-once))
             (lambda (_choice) nil)
             tool-call)
            (setq layout (substring-no-properties (buffer-string)))
            (setq count 0 start 0)
            (while (string-match (regexp-quote "import json") layout start)
              (setq count (1+ count) start (match-end 0)))
            (should (= count 1))
            (should-not (string-match-p "\\*\\* Allow execute" layout))
            (should-not (string-match-p "python3 - <<" layout))
            (should (string-match-p "\\[Allow once\\]" layout)))))))))

(ert-deftest emagent-chat-test-permission-edit-content-block ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (let ((path (expand-file-name "target.py" dir)))
            (write-region "before\n" nil path)
            (goto-char (point-max))
            (emagent-chat--begin-response (point))
            (emagent-chat-show-tool-call "id1" "Edit File")
            (let ((tool-call `((toolCallId . "tool_edit")
                               (title . "Edit File")
                               (rawInput . ((path . ,path)
                                            (content . "after\n")))))
                  layout)
              (emagent-chat-permission-prompt
               path
               '(("Allow once" . :allow-once))
               (lambda (_choice) nil)
               tool-call)
              (setq layout (substring-no-properties (buffer-string)))
              (should (string-match-p "^\\*\\* Allow edit: target.py"
                                      layout))
              (should (string-match-p "#\\+BEGIN_SRC diff" layout))
              (should (string-match-p "after" layout))
              (should-not (string-match-p "\\? " layout))))))))))

(ert-deftest emagent-chat-test-buffer-display ()
  (let ((disable-visual
         (lambda ()
           (when (derived-mode-p 'emagent-mode)
             (visual-line-mode -1)
             (setq-local truncate-lines t)))))
    (unwind-protect
        (progn
          (add-hook 'org-mode-hook disable-visual 50)
          (emagent-test--with-emagent-buffer
           (lambda (buffer _dir)
             (with-current-buffer buffer
               (should (bound-and-true-p visual-line-mode))
               (should-not truncate-lines)
               (when (fboundp 'org-phscroll-mode)
                 (should (bound-and-true-p org-phscroll-mode)))))))
      (remove-hook 'org-mode-hook disable-visual 50))))

(ert-deftest emagent-chat-test-insert-user-heading-replaces-stub ()
  (with-temp-buffer
    (insert (format "%sfirst\n\n# --- emagent ---\n# --- /emagent ---\n%s\n"
                    (emagent-chat--user-heading-prefix)
                    (emagent-chat--user-heading-prefix)))
    (emagent-chat--sync-user-zone-marker)
    (emagent-chat--insert-user-heading-with-text "btw, hello")
    (should (string-match-p (concat (regexp-quote (emagent-chat--user-heading-prefix))
                                    "btw, hello")
                            (buffer-string)))
    (should (= 2 (how-many (emagent-chat--user-heading-re)
                           (point-min) (point-max))))))

(ert-deftest emagent-chat-test-btw-finalizes-when-busy ()
  (with-temp-buffer
    (setq emagent-acp--session (emagent-test--make-acp-state nil (current-buffer)))
    (setf (emagent-acp-state-busy emagent-acp--session) t)
    (setf (emagent-acp-state-ready emagent-acp--session) t)
    (let ((sent nil)
          (finalized nil))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-acp--finalize-in-flight-prompt)
            (lambda (&optional _notice) (setq finalized t) t))
           ((symbol-function 'emagent-chat--begin-response) (lambda (&optional _at) nil)))
        (setq emagent-chat--on-send (lambda (text) (setq sent text)))
        (let ((inhibit-message t))
          (emagent-btw "check tests"))
        (should finalized)
        (should (string= sent "btw, check tests"))))))

(ert-deftest emagent-chat-test-btw-sends-immediately-when-idle ()
  (with-temp-buffer
    (setq emagent-acp--session (emagent-test--make-acp-state nil (current-buffer)))
    (setf (emagent-acp-state-busy emagent-acp--session) nil)
    (let ((sent nil)
          (finalized nil))
      (emagent-test--with-mocks
          (((symbol-function 'emagent-acp--finalize-in-flight-prompt)
            (lambda (&optional _notice) (setq finalized t) t))
           ((symbol-function 'emagent-chat--begin-response) (lambda (&optional _at) nil)))
        (setq emagent-chat--on-send (lambda (text) (setq sent text)))
        (let ((inhibit-message t))
          (emagent-btw "note"))
        (should-not finalized)
        (should (string= sent "btw, note"))))))

(ert-deftest emagent-chat-test-interrupt-keeps-streamed-thinking ()
  "Interrupting mid-thought keeps the streamed reasoning text and stop notice."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (setq emagent-acp--session (emagent-test--make-acp-state nil buffer))
       (setf (emagent-acp-state-busy emagent-acp--session) t)
       (setf (emagent-acp-state-ready emagent-acp--session) t)
       (setf (emagent-acp-state-prompt-generation emagent-acp--session) 0)
       (setf (emagent-acp-state-cb-finish emagent-acp--session) #'emagent-chat-finish-assistant)
       (goto-char (point-max))
       (emagent-chat--begin-response (point))
       (emagent-chat-begin-thought)
       (emagent-chat-append-thought "weighing options")
       (setf (emagent-acp-state-assistant-text emagent-acp--session) "partial answer")
       (let ((inhibit-message t))
         (emagent-acp-interrupt))
       (let ((text (substring-no-properties (buffer-string))))
         (should (string-match-p "weighing options" text))
         (should (string-match-p "partial answer" text))
         (should (string-match-p "Stopped" text)))))))

(ert-deftest emagent-chat-test-interrupt-keeps-unstreamed-thinking ()
  "Interrupting keeps reasoning that lived only in state thought-text."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (setq emagent-acp--session (emagent-test--make-acp-state nil buffer))
       (setf (emagent-acp-state-busy emagent-acp--session) t)
       (setf (emagent-acp-state-ready emagent-acp--session) t)
       (setf (emagent-acp-state-prompt-generation emagent-acp--session) 0)
       (setf (emagent-acp-state-cb-finish emagent-acp--session) #'emagent-chat-finish-assistant)
       (goto-char (point-max))
       (emagent-chat--begin-response (point))
       (setf (emagent-acp-state-thought-text emagent-acp--session) "internal reasoning")
       (setf (emagent-acp-state-assistant-text emagent-acp--session) "partial answer")
       (let ((inhibit-message t))
         (emagent-acp-interrupt))
       (let ((text (substring-no-properties (buffer-string))))
         (should (string-match-p "internal reasoning" text))
         (should (string-match-p "partial answer" text))
         (should (string-match-p "Stopped" text)))))))

(provide 'emagent-chat-test)

;;; emagent-chat-test.el ends here
