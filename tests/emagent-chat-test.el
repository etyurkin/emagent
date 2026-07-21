;;; emagent-chat-test.el --- ERT tests for emagent chat UI -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-chat)
(require 'emagent-acp-tool-call)
(require 'emagent-acp-connect)

;;;; Slugs and labels

(ert-deftest emagent-chat-test-sanitize-slug ()
  (should (string= "my-project" (emagent-chat--sanitize-slug "My Project")))
  (should (string= emagent-chat-default-slug (emagent-chat--sanitize-slug "   "))))

(ert-deftest emagent-chat-test-short-cwd-label ()
  (should (string= "dev-emagent" (emagent-chat--short-cwd-label "~/dev/emagent"))))

(ert-deftest emagent-chat-test-buffer-name-for-label ()
  (should (string= "*Emagent foo*" (emagent-chat--buffer-name-for-label "foo"))))

;;;; Send bounds

(ert-deftest emagent-chat-test-send-bounds ()
  "C-c C-c sends only on or inside a `* user>' prompt."
  (with-temp-buffer
    (insert (format "* %s> first prompt\nbody line\n\n** Thinking\nstuff\n** Response\n| a | b |\n\n* %s> next"
                    (user-login-name) (user-login-name)))
    ;; On the prompt heading.
    (goto-char (point-min))
    (should (emagent-chat--send-bounds))
    ;; In the prompt's direct body.
    (search-forward "body")
    (let ((bounds (emagent-chat--send-bounds)))
      (should bounds)
      (should (= (car bounds) (point-min))))
    ;; On a response subsection heading: nothing to send.
    (search-forward "** Thinking")
    (should-not (emagent-chat--send-bounds))
    ;; Inside response content (a table): org's C-c C-c territory.
    (search-forward "| a |")
    (should-not (emagent-chat--send-bounds))
    ;; On a later prompt: re-evaluable.
    (search-forward "next")
    (should (emagent-chat--send-bounds))))


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

(ert-deftest emagent-chat-test-dispatch-compress-empty-fails ()
  "A /compress with no prior conversation fails the response, not dispatch."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (let (dispatched)
         (setq emagent-chat--on-send (lambda (&rest _args) (setq dispatched t)))
         (goto-char (point-max))
         (insert (emagent-chat--user-heading-prefix) "/compress")
         (emagent-chat-send)
         (should-not dispatched)
         (should (string-match-p "No conversation to compress" (buffer-string))))))))

(ert-deftest emagent-chat-test-dispatch-compress-sends-summary ()
  "A /compress with prior conversation sends a summary prompt with COMPRESS set."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert (emagent-chat--user-heading-prefix) "hello\n\n** Response\nhi there\n\n")
       (let (sent-text sent-compress)
         (setq emagent-chat--on-send
               (lambda (text &optional compress)
                 (setq sent-text text sent-compress compress)))
         (goto-char (point-max))
         (insert (emagent-chat--user-heading-prefix) "/compress")
         (emagent-chat-send)
         (should sent-compress)
         (should (string-match-p "<conversation>" sent-text))
         (should (string-match-p "hello" sent-text)))))))

(ert-deftest emagent-chat-test-bare-slash-command-p ()
  (should (emagent-chat--bare-slash-command-p "/compress"))
  (should (emagent-chat--bare-slash-command-p "/plan refactor auth"))
  (should (emagent-chat--bare-slash-command-p "/workflow:dev"))
  (should-not (emagent-chat--bare-slash-command-p "hello"))
  (should-not (emagent-chat--bare-slash-command-p "/compress\nmore")))

(ert-deftest emagent-chat-test-mode-enable-seeds-cursor-slash-commands ()
  "Cursor built-ins are available after mode enable without connecting."
  (with-temp-buffer
    (let ((emagent-default-provider 'cursor)
          (emagent-chat-slash-commands nil))
      (delay-mode-hooks (emagent-mode))
      (emagent-chat--on-mode-enable)
      (should (eq emagent-chat-provider 'cursor))
      (should (cl-find "compress" emagent-chat-slash-commands
                       :key (lambda (c) (map-elt c 'name))
                       :test #'string=)))))

(ert-deftest emagent-chat-test-slash-token-bounds-midline ()
  "A `/name' token is detected at point anywhere on the prompt line, and the
`/model' completion offers the client command; a path like `src/a' is not one."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (goto-char (point-max))
    (insert "* etyurkin> commit, use /model")
    (let ((b (emagent-chat--slash-token-bounds)))
      (should b)
      (should (equal "/model"
                     (buffer-substring-no-properties (car b) (cdr b)))))
    (let ((capf (emagent-chat-slash-command-completion-at-point)))
      (should (member "model" (nth 2 capf)))
      (should (plist-member (nthcdr 3 capf) :exit-function)))
    ;; start-of-line still works
    (goto-char (point-max))
    (insert "\n* etyurkin> /mod")
    (should (equal "/mod"
                   (let ((b (emagent-chat--slash-token-bounds)))
                     (buffer-substring-no-properties (car b) (cdr b)))))
    ;; a path (no leading whitespace before `/') is not a slash command
    (goto-char (point-max))
    (insert "\n* etyurkin> see src/a")
    (should-not (emagent-chat--slash-token-bounds))))

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
  (should (string= "auto" (emagent-model-normalize-id "default[]")))
  (should (string= "auto" (emagent-model-normalize-id "default")))
  (should (string= "grok-4.3"
                   (emagent-model-normalize-id "grok-4.3[context=200k]")))
  (should (string= "claude-sonnet-4-6"
                   (emagent-model-normalize-id
                    "claude-sonnet-4-6[thinking=true]")))
  (should (string= "gpt-4" (emagent-model-normalize-id "gpt-4"))))

(ert-deftest emagent-chat-test-canonical-model-id ()
  (should (string= "default[]" (emagent-model-canonical-id "auto")))
  (should (string= "default[]" (emagent-model-canonical-id "default")))
  (should (string= "grok-4.3[context=200k]"
                   (emagent-model-canonical-id "grok-4.3[context=200k]"))))

(ert-deftest emagent-chat-test-model-choice-label ()
  (should (string= "grok-4.3[context=200k]"
                   (emagent-model-choice-label
                    "grok-4.3[context=200k]" "grok-4.3")))
  (should (string= "default[] (Auto)"
                   (emagent-model-choice-label "default[]" "Auto")))
  (should (string= "gpt-4 (GPT 4)"
                   (emagent-model-choice-label "gpt-4" "GPT 4"))))

(ert-deftest emagent-chat-test-model-choice-label-display ()
  (let ((label (emagent-model-choice-label-display
                "composer-2.5[fast=true]" "composer-2.5")))
    (should (string= "composer-2.5[fast=true]" (substring-no-properties label)))
    (should (eq 'emagent-model-choice-model (get-text-property 0 'face label)))
    (should (eq 'emagent-model-choice-detail (get-text-property 12 'face label))))
  (let ((label (emagent-model-choice-label-display "default[]" "Auto")))
    (should (string= "default[] (Auto)" (substring-no-properties label)))
    (should (eq 'emagent-model-choice-model (get-text-property 0 'face label)))
    (should (eq 'emagent-model-choice-detail (get-text-property 7 'face label))))
  (let ((label (emagent-model-choice-label-display "haiku" "Haiku")))
    (should (string= "haiku (Haiku)" (substring-no-properties label)))
    (should (eq 'emagent-model-choice-model (get-text-property 0 'face label)))
    (should (eq 'emagent-model-choice-detail (get-text-property 5 'face label)))))

(ert-deftest emagent-chat-test-set-model-stores-canonical-id ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (emagent-chat-set-model "default[]")
       (should (string= "default[]" (emagent-session-model)))
       (should (string= "auto" (emagent-chat-model-display)))
       (should (string-match-p "^#\\+EMAGENT_MODEL: default\\[\\]"
                               (buffer-string)))
       (emagent-chat-set-model "grok-4.3[context=200k]")
       (should (string= "grok-4.3[context=200k]" (emagent-session-model)))
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

(ert-deftest emagent-chat-test-font-lock-deferred-while-turn-in-flight ()
  "Visible buffers still defer org font-lock while the ACP turn is busy."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let* ((state (emagent-test--make-acp-state nil buffer))
            (flushed 0))
       (setq emagent-acp--session state)
       (setf (emagent-acp-state-busy state) t)
       (with-current-buffer buffer
         (pop-to-buffer buffer)
         (setq emagent-chat--font-lock-deferred-p nil)
         (emagent-test--with-mocks
             (((symbol-function 'emagent-chat--font-lock-response-tail)
               (lambda () (cl-incf flushed))))
           (emagent-chat--maybe-font-lock-flush)
           (should emagent-chat--font-lock-deferred-p)
           (should (= flushed 0))
           (setf (emagent-acp-state-busy state) nil)
           (emagent-chat--flush-deferred-font-lock)
           (should-not emagent-chat--font-lock-deferred-p)
           (should (= flushed 1))))))))

(ert-deftest emagent-chat-test-schedule-align-org-tables-region-only ()
  "Schedule one idle align of the response region; never full-buffer from hooks.

Regression: spinner `redisplay' + window-configuration-change-hook scanned
the whole buffer with `org-at-table-p'/`org-element-at-point' and pegged CPU."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let* ((aligned-bounds nil)
            (idle-fns nil))
       (with-current-buffer buffer
         (insert "| a | b |\n|---+---|\n| 1 | 2 |\n")
         (let ((start (point-min))
               (end (point-max)))
           (emagent-test--with-mocks
               (((symbol-function 'emagent-chat--align-org-tables-in-region)
                 (lambda (s e &rest _)
                   (push (cons s e) aligned-bounds)))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (_secs _repeat fn &rest _args)
                   (push fn idle-fns)
                   'fake-timer)))
             (emagent-chat--schedule-align-org-tables start end)
             (should (markerp emagent-chat--table-align-start))
             (should (markerp emagent-chat--table-align-end))
             (should (= (marker-position emagent-chat--table-align-start) start))
             (should (= (marker-position emagent-chat--table-align-end) end))
             (should (= (length idle-fns) 1))
             (should-not aligned-bounds)
             (funcall (car idle-fns))
             (should-not emagent-chat--table-align-start)
             (should-not emagent-chat--table-align-end)
             (should (= (length aligned-bounds) 1))
             (should (equal (car aligned-bounds) (cons start end))))))))))

(ert-deftest emagent-chat-test-spinner-refresh-does-not-redisplay ()
  "Spinner ticks must not call `redisplay' (re-enters window-config hooks)."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (setq emagent-acp--session (emagent-acp--make-state))
     (setf (emagent-acp-state-busy emagent-acp--session) t)
     (let ((redisplays 0)
           (mode-line-updates 0))
       (with-current-buffer buffer
         (pop-to-buffer buffer)
         (emagent-test--sync-status)
         (emagent-test--with-mocks
             (((symbol-function 'redisplay)
               (lambda (&rest _) (cl-incf redisplays)))
              ((symbol-function 'force-mode-line-update)
               (lambda (&optional _all) (cl-incf mode-line-updates))))
           (emagent-chat--spinner-refresh-idle)
           (should (= redisplays 0))
           (should (>= mode-line-updates 1))))))))

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
       ;; Nil start-time so `emagent-chat--spinner-sync-frame' does not
       ;; overwrite the frame index we set explicitly below.
       (setq emagent-chat--mode-line-head nil
             emagent-chat--mode-line-tail nil
             emagent-chat--mode-line-cache nil
             emagent-chat--spinner-start-time nil
             emagent-chat--spinner-frame 0)
       (pop-to-buffer buffer)
       (should (emagent-chat--spinner-refresh-buffer buffer))
       (should (string-match-p "Thinking ●" emagent-chat--mode-line-head))
       (setq emagent-chat--spinner-frame 1)
       (should (emagent-chat--spinner-refresh-buffer buffer))
       (should (string-match-p "Thinking ○●○" emagent-chat--mode-line-head))))))

(ert-deftest emagent-chat-test-restore-window-views-follows-bottom ()
  (with-temp-buffer
    (insert "line1\nline2\n")
    (let ((win (selected-window))
          (buf (current-buffer)))
      (set-window-buffer win buf)
      (goto-char 6)
      (emagent-chat--restore-window-views
       `((:window ,win :start 1 :at-bottom t)))
      ;; Follow moves window-point (and selected-window point) to EOB —
      ;; matching `emagent-log' — so redisplay cannot undo the scroll.
      (should (eq buf (window-buffer win)))
      (should (= (point-max) (window-point win)))
      (should (= (point-max) (point))))))

(ert-deftest emagent-chat-test-restore-window-views-keeps-scroll-when-pinned ()
  (with-temp-buffer
    (insert (mapconcat #'identity (make-list 80 "line") "\n"))
    (let ((win (selected-window))
          (saved-point 6))
      (set-window-buffer win (current-buffer))
      (goto-char saved-point)
      (set-window-start win 1)
      (emagent-chat--restore-window-views
       `((:window ,win :start 1 :at-bottom nil)))
      (should (= saved-point (point)))
      (should (= 1 (window-start win))))))

(ert-deftest emagent-chat-test-window-at-bottom-requires-point-at-eob ()
  "Follow only when window-point is at EOB, not merely when EOB is visible.

Short chats fit in the window, so visibility alone used to keep yanking the
cursor back to the end on every thought chunk."
  (with-temp-buffer
    (insert "line1\nline2\n")
    (let ((win (selected-window)))
      (set-window-buffer win (current-buffer))
      (goto-char (point-max))
      (should (emagent-chat--window-at-bottom-p win))
      (goto-char (point-min))
      (should-not (emagent-chat--window-at-bottom-p win)))))

(ert-deftest emagent-chat-test-streaming-view-keeps-point-when-not-at-eob ()
  "Thought streaming must not yank point when the user left EOB."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "first")
          (goto-char (point-min))
          (let ((pinned (point)))
            (emagent-chat--with-streaming-view
             (lambda ()
               (save-excursion
                 (goto-char (point-max))
                 (insert "\nmore thinking"))))
            (should (= pinned (point)))
            (should (= pinned (window-point (selected-window)))))))))))

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

(ert-deftest emagent-chat-test-tool-call-heredoc-in-command-subst-keeps-sh ()
  "Shell `$()' wrapping a heredoc still uses lang `sh' (keep highlighting).

`sh-mode' font-lock can signal `end-of-buffer' on that pattern; emagent
swallows it via buffer-local safe src fontify instead of demoting lang."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-show-tool-call
           "id-gh" "Shell: gh pr create" "sh"
           (concat "gh pr create --body \"$(cat <<'EOF'\n"
                   "## Problem\nuse `null` here\n"
                   "EOF\n)\""))
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "#\\+begin_src sh" text))
            (should (string-match-p "gh pr create" text)))))))))

(ert-deftest emagent-chat-test-format-tool-block-escapes-src-delimiters ()
  (let ((out (emagent-chat--format-tool-block
              "#+END_SRC\necho hi" "sh" nil)))
    (should (string-match-p ",#\\+END_SRC" out))
    (should (string-match-p "#\\+begin_src sh" out))))

(ert-deftest emagent-chat-test-font-lock-region-start-caps-open-response ()
  "Open responses only re-fontify a trailing window, not the whole turn."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert (make-string 40000 ?x))
       (setq emagent-chat--response-body-start
             (copy-marker (- (point-max) 40000) nil))
       (let ((start (emagent-chat--font-lock-region-start)))
         (should (>= start (- (point-max) 12000)))
         (should (>= start (marker-position emagent-chat--response-body-start))))))))

(ert-deftest emagent-chat-test-fragile-shell-src-p ()
  (with-temp-buffer
    (insert "gh pr create --body \"$(cat <<'EOF'\nhi\nEOF\n)\"\n")
    (should (emagent-chat--fragile-shell-src-p "sh" (point-min) (point-max)))
    (erase-buffer)
    (insert "python3 - <<'PY'\nprint(1)\nPY\n")
    (should-not (emagent-chat--fragile-shell-src-p "sh" (point-min) (point-max)))
    (erase-buffer)
    (insert "echo $(date)\n")
    (should-not (emagent-chat--fragile-shell-src-p "sh" (point-min) (point-max)))))

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
  "The `/model' link in the prompt is read back by the region scanner."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (insert "commit ")
    (insert "[[emagent://claude/claude-haiku-4-5][haiku]]")
    ;; The model id is the path after the agent segment.
    (should (equal "claude-haiku-4-5"
                   (emagent-chat--region-turn-model (point-min) (point-max))))
    ;; Only the link carries it; plain text does not.
    (should-not (emagent-chat--region-turn-model (point-min) (+ (point-min) 6)))))

(ert-deftest emagent-chat-test-turn-model-strip ()
  "The `/model' link never reaches the agent; prose and other links do."
  (should (equal "what's the status? make it brief"
                 (emagent-chat--strip-model-links
                  "what's the status? [[emagent://claude/claude-haiku-4-5][haiku]] make it brief")))
  (should (equal "no marker here"
                 (emagent-chat--strip-model-links "no marker here")))
  ;; A user's own org link is not mistaken for the model marker.
  (should (equal "see [[https://x][docs]]"
                 (emagent-chat--strip-model-links "see [[https://x][docs]]"))))

(ert-deftest emagent-chat-test-turn-model-thinking-indicator ()
  "The Thinking headline shows the per-turn model link; the regex matches both."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (goto-char (point-max))
    (setq emagent-chat--turn-model "haiku"
          emagent-chat--response-body-start (copy-marker (point) nil))
    (emagent-chat--insert-reasoning-scaffold)
    (goto-char (marker-position emagent-chat--thinking-headline-marker))
    ;; No session agent in a temp buffer, so the link path is the bare
    ;; model id and the text is the short name.
    (should (looking-at-p
             (regexp-quote "** Thinking [[emagent://haiku][haiku]]")))
    (should (string-match-p emagent-chat--thinking-headline-re
                            (buffer-substring-no-properties
                             (point) (line-end-position))))
    (should (string-match-p emagent-chat--thinking-headline-re "** Thinking"))))

(ert-deftest emagent-chat-test-turn-model-switching-scaffold ()
  "A `/model' send opens `** Switching model' and promotes to Thinking."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (goto-char (point-max))
    (insert "\n")
    (setq emagent-chat--turn-model "haiku"
          emagent-chat--response-body-start (copy-marker (point) nil))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (emagent-chat--insert-switching-scaffold))
    (should emagent-chat--switching-model-p)
    (should-not emagent-chat--thought-open-p)
    (goto-char (marker-position emagent-chat--thinking-headline-marker))
    (should (looking-at emagent-chat--switching-headline-re))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (emagent-chat--ensure-reasoning-scaffold))
    (should-not emagent-chat--switching-model-p)
    (should emagent-chat--thought-open-p)
    (goto-char (marker-position emagent-chat--thinking-headline-marker))
    (should (looking-at-p
             (regexp-quote "** Thinking [[emagent://haiku][haiku]]")))))

(ert-deftest emagent-chat-test-preparing-scaffold-promotes-to-thinking ()
  "A regular send opens `** Preparing…' and promotes to Thinking on stream."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (goto-char (point-max))
    (insert "\n")
    (setq emagent-chat--response-body-start (copy-marker (point) nil))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (emagent-chat--insert-preparing-scaffold))
    (should emagent-chat--preparing-p)
    (should-not emagent-chat--thought-open-p)
    (goto-char (marker-position emagent-chat--thinking-headline-marker))
    (should (looking-at emagent-chat--preparing-headline-re))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (emagent-chat--ensure-reasoning-scaffold))
    (should-not emagent-chat--preparing-p)
    (should emagent-chat--thought-open-p)
    (goto-char (marker-position emagent-chat--thinking-headline-marker))
    (should (looking-at-p (regexp-quote "** Thinking")))))

(ert-deftest emagent-chat-test-turn-model-face ()
  "The /model marker renders as a plain org link (default `org-link' face,
no custom fontification), even on org heading lines (prompt and Thinking)."
  (with-temp-buffer
    (emagent-mode)
    (goto-char (point-max))
    (insert "* etyurkin> run it [[emagent://haiku][haiku]]\n")
    (setq emagent-chat--turn-model "haiku"
          emagent-chat--response-body-start (copy-marker (point) nil))
    (emagent-chat--insert-reasoning-scaffold)
    (font-lock-ensure)
    (dolist (needle '("run it [[" "Thinking [["))
      (goto-char (point-min))
      (search-forward needle)
      (search-forward "][")  ; into the description
      (let ((face (get-text-property (point) 'face)))
        (should (memq 'org-link (if (listp face) face (list face))))))))

(ert-deftest emagent-chat-test-slash-model-connects-first ()
  "/model calls `emagent-acp-ensure-connected' when no session is active yet."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (goto-char (point-max))
    (insert "* etyurkin> use /model")
    (search-backward "/model")
    (setq emagent-acp--session nil)
    (let ((called nil))
      (cl-letf (((symbol-function 'emagent-acp--connected-p) (lambda () nil))
                ((symbol-function 'emagent-acp-ensure-connected)
                 (lambda (&rest _) (setq called t))))
        (emagent-chat--slash-model-apply)
        (should called)))))

(ert-deftest emagent-chat-test-turn-model-restore-clears ()
  "A successful turn restores the base model and clears the override state."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (setq emagent-chat--turn-model "haiku"
          emagent-chat--turn-model-base "sonnet")
    (let (restored)
      (cl-letf (((symbol-function 'emagent-acp-set-model-transient)
                 (lambda (m _cb) (setq restored m))))
        (emagent-acp--restore-turn-model))
      (should (equal "sonnet" restored))
      (should-not emagent-chat--turn-model)
      (should-not emagent-chat--turn-model-base))))

(ert-deftest emagent-chat-test-turn-model-fatal-failure-restores ()
  "Permanent prompt failures restore the buffer model without a keep dialog."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (setq emagent-chat--turn-model "haiku"
          emagent-chat--turn-model-base "sonnet")
    (let ((prompted nil))
      (cl-letf (((symbol-function 'emagent-tools--buttons-prompt)
                 (lambda (&rest _) (setq prompted t)))
                ((symbol-function 'emagent-acp-set-model-transient)
                 (lambda (_ _cb) nil)))
        (emagent-acp--turn-model-on-failure
         "prompt failed: Internal error: Prompt is too long"))
      (should-not prompted)
      (should-not emagent-chat--turn-model)
      (should-not emagent-chat--turn-model-base))))

(ert-deftest emagent-chat-test-fail-assistant-clears-switching ()
  "A failed send removes a `** Switching model' scaffold from the buffer."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (goto-char (point-max))
    (insert "\n")
    (setq emagent-chat--turn-model "haiku"
          emagent-chat--response-body-start (copy-marker (point) nil))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (emagent-chat--insert-switching-scaffold))
    (emagent-chat-fail-assistant "Prompt is too long")
    (should-not (string-match-p emagent-chat--switching-headline-re
                                (buffer-string)))
    (should (string-match-p "\\*Error:\\* Prompt is too long" (buffer-string)))))

(ert-deftest emagent-chat-test-fail-assistant-clears-preparing ()
  "A failed send removes a `** Preparing…' scaffold from the buffer."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (goto-char (point-max))
    (insert "\n")
    (setq emagent-chat--response-body-start (copy-marker (point) nil))
    (let ((inhibit-read-only t))
      (emagent-chat--writable)
      (emagent-chat--insert-preparing-scaffold))
    (emagent-chat-fail-assistant "Connection refused")
    (should-not (string-match-p emagent-chat--preparing-headline-re
                                (buffer-string)))
    (should (string-match-p "\\*Error:\\* Connection refused" (buffer-string)))))

(ert-deftest emagent-chat-test-send-pending-spinner ()
  "Pre-dispatch work (model switch, connect) animates the mode-line spinner."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (setq emagent-chat--status '(:busy nil :waiting-permission nil))
    (should-not (emagent-chat--spinner-active-p))
    (emagent-chat--send-pending-begin)
    (should emagent-chat--send-pending)
    (should (emagent-chat--spinner-active-p))
    (setq emagent-chat--turn-model "haiku")
    (let ((head (car (emagent-chat--mode-line-strings))))
      (should (string-match-p "Switching" head)))
    ;; `:busy' alone must not hide the pre-dispatch label.
    (setq emagent-chat--status '(:busy t :waiting-permission nil))
    (let ((head (car (emagent-chat--mode-line-strings))))
      (should (string-match-p "Switching" head))
      (should-not (string-match-p "Thinking" head)))
    (setq emagent-chat--status '(:busy nil :waiting-permission nil))
    (emagent-chat--send-pending-end)
    (should-not emagent-chat--send-pending)
    (should-not (emagent-chat--spinner-active-p))))

(ert-deftest emagent-chat-test-turn-begin-clears-send-pending ()
  "Dispatch clears the pre-dispatch marker so the mode line shows Thinking."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (setq emagent-chat--status '(:busy nil :waiting-permission nil))
    (emagent-chat--send-pending-begin)
    (setq emagent-chat--turn-model nil)
    (emagent-chat--begin-response (point-max))
    (emagent-chat--insert-preparing-scaffold)
    (let ((state (emagent-test--make-acp-state nil (current-buffer))))
      (emagent-acp--turn-begin state)
      (should-not emagent-chat--send-pending)
      (should-not emagent-chat--preparing-p)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "\\*\\* Thinking" text))
        (should-not (string-match-p "Preparing" text)))
      (setq emagent-chat--status '(:busy t :waiting-permission nil))
      (let ((head (car (emagent-chat--mode-line-strings))))
        (should (string-match-p "Thinking" head))
        (should-not (string-match-p "Preparing" head))))))

(ert-deftest emagent-chat-test-tool-after-block-stays-adjacent ()
  "A second tool line after a src block does not grow a blank gap."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-show-tool-call "id1" "Shell" "sh" "cat foo")
          (emagent-chat-show-tool-call "id2" "git_status")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "#\\+end_src\n→ git_status" text))
            (should-not (string-match-p "#\\+end_src\n\n→" text)))))))))

(ert-deftest emagent-chat-test-tool-update-does-not-grow-newlines ()
  "In-place tool-call label updates do not append blank lines."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-show-tool-call "id1" "git")
          (dotimes (_ 6)
            (emagent-chat-show-tool-call "id1" "git (Allow: Emacs)"))
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "→ git (Allow: Emacs)" text))
            (should-not (string-match-p "→ git (Allow: Emacs)\n\n" text)))))))))

(ert-deftest emagent-chat-test-interrupt-clears-send-pending-when-busy ()
  "ESC ESC during thinking clears a stale pre-dispatch marker and spinner."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (setq emagent-acp--session (emagent-test--make-acp-state nil buffer))
       (setf (emagent-acp-state-busy emagent-acp--session) t)
       (setf (emagent-acp-state-ready emagent-acp--session) t)
       (setf (emagent-acp-state-prompt-generation emagent-acp--session) 0)
       (setf (emagent-acp-state-cb-finish emagent-acp--session) #'emagent-chat-finish-assistant
             (emagent-acp-state-cb-status emagent-acp--session) #'emagent-chat-set-status)
       (emagent-test--sync-status)
       (emagent-chat--send-pending-begin)
       (goto-char (point-max))
       (emagent-chat--begin-response (point))
       (emagent-chat-begin-thought)
       (let ((inhibit-message t))
         (emagent-chat-interrupt))
       (should-not emagent-chat--send-pending)
       (should-not (emagent-chat--spinner-active-p))
       (let ((head (car (emagent-chat--mode-line-strings))))
         (should-not (string-match-p "Preparing" head)))))))

(ert-deftest emagent-chat-test-interrupt-cancels-send-pending ()
  "ESC ESC stops pre-dispatch work and clears the Preparing spinner."
  (with-temp-buffer
    (delay-mode-hooks (emagent-mode))
    (insert "* etyurkin> hello\n")
    (setq emagent-chat--status '(:busy nil :waiting-permission nil))
    (emagent-chat--send-pending-begin)
    (emagent-chat--begin-response (point-max))
    (let ((queued t))
      (cl-letf (((symbol-function 'emagent-acp--clear-when-connected-queue)
                 (lambda () (setq queued nil))))
        (let ((inhibit-message t))
          (emagent-chat-interrupt))
        (should (not queued))
        (should-not emagent-chat--send-pending)
        (should-not (emagent-chat--spinner-active-p))
        (should-not (emagent-chat--open-response-p))))))

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

(ert-deftest emagent-chat-test-thought-leading-blank-chunks-do-not-pile-up ()
  "Blank-only reasoning deltas cannot grow a run after `** Thinking'."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat--insert-preparing-scaffold)
          (dotimes (_ 12) (emagent-chat-append-thought "\n"))
          (emagent-chat-append-thought "First paragraph.")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "\\*\\* Thinking\n+First paragraph\\." text))
            (should-not (string-match-p "\\*\\* Thinking\n\n\n" text)))))))))

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

(ert-deftest emagent-chat-test-display-path ()
  (let* ((project (expand-file-name "demo-app" (emagent-test--temp-directory)))
         (in-project (expand-file-name "src/main/Foo.java" project))
         (home-outside (expand-file-name "Desktop/image.png" (expand-file-name "~")))
         (outside-home "/tmp/outside-home/x"))
    (make-directory project t)
    (make-directory (file-name-directory in-project) t)
    (let ((default-directory project)
          (emagent-chat-project-directory project))
      (should (string= "./demo-app/src/main/Foo.java"
                       (emagent-chat--display-path in-project)))
      (should (string= (abbreviate-file-name home-outside)
                       (emagent-chat--display-path home-outside)))
      (should (string= (file-truename outside-home)
                       (emagent-chat--display-path outside-home))))
    (should (string= (file-name-as-directory (abbreviate-file-name project))
                     (emagent-session-store-display-project-directory project)))))

(ert-deftest emagent-chat-test-display-path-relative-ignores-default-directory ()
  "Relative paths resolve against the project, not `default-directory'.
Saving the session file moves `default-directory' to the session file's
directory; agent tool paths must still display under the project root."
  (let* ((project (expand-file-name "clef-v2" (emagent-test--temp-directory)))
         (sessions (expand-file-name "emagent-sessions" (emagent-test--temp-directory))))
    (make-directory (expand-file-name "src/vm" project) t)
    (make-directory sessions t)
    (let ((default-directory sessions)
          (emagent-chat-project-directory project))
      (should (string= "./clef-v2/src/vm/Main.swift"
                       (emagent-chat--display-path "src/vm/Main.swift"))))))

(ert-deftest emagent-chat-test-org-verbatim-paths ()
  (let ((home-file (expand-file-name "foo.el" (expand-file-name "~"))))
    (should (string= (format "Read: =%s="
                             (abbreviate-file-name home-file))
                     (emagent-chat--org-verbatim-paths
                      (format "Read: %s" home-file))))
    (should (string= (format "→ Read: =%s=" (file-truename "/tmp/x"))
                     (emagent-chat--format-tool-line "Read: /tmp/x")))))

(ert-deftest emagent-chat-test-backtick-urls-become-org-links ()
  "Backtick-wrapped URLs must stay clickable as org links, not =verbatim=.
Agents often write `https://…` as inline code; converting that to =url=
makes the link unclickable in org-mode."
  (should (string-match-p
           "\\[\\[https://example.com/x\\]\\]"
           (emagent-chat--convert-agent-markup
            "see `https://example.com/x` now")))
  (should (string-match-p
           "=code="
           (emagent-chat--convert-agent-markup "use `code` here")))
  (should-not (string-match-p
               "=https://"
               (emagent-chat--convert-agent-markup
                "see `https://example.com/x` now")))
  (should (string= "see https://example.com/a/b"
                   (emagent-chat--org-verbatim-paths
                    "see https://example.com/a/b"))))

(ert-deftest emagent-chat-test-inline-code-preserves-backslashes ()
  "Inline-code conversion must not reinterpret backslashes in the span.

Regression for finish failing with Invalid use of backslash in
replacement text when agent output contains paths like C:\\Users."
  (should (string-match-p
           "=foo\\\\1bar="
           (emagent-chat--convert-agent-markup "use `foo\\1bar` here")))
  (should (string-match-p
           "=C:\\\\Users\\\\x="
           (emagent-chat--convert-agent-markup "path `C:\\Users\\x`")))
  (should (string-match-p
           "\\[\\[https://example.com/a\\\\b\\]\\]"
           (emagent-chat--convert-agent-markup
            "see `https://example.com/a\\b`"))))

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
          (should (emagent-chat--user-prompt-input-pos))
          (should (= (point) (emagent-chat--user-prompt-input-pos)))
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
            (should (string-match-p (emagent-chat--user-heading-re) text))
            (should (= (point) (emagent-chat--user-prompt-input-pos))))))))))

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

(ert-deftest emagent-chat-test-permission-prompt-focuses-buttons ()
  "Permission dialogs move point onto the first button for key shortcuts."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-min))
          (emagent-chat--begin-response (point-max))
          (emagent-chat-permission-prompt
           "make test"
           '(("Allow once" . :allow-once))
           (lambda (_) nil))
          (should (button-at (point)))))))))

(ert-deftest emagent-chat-test-permission-line-keymap-at-bol ()
  "Button shortcuts are bound on the whole button line, including line start."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-min))
          (emagent-chat--begin-response (point-max))
          (emagent-chat-permission-prompt
           "make test"
           '(("Allow once" . :allow-once))
           (lambda (_) nil))
          (goto-char (line-beginning-position))
          (should (keymap-parent (get-text-property (point) 'keymap)))))))))

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
