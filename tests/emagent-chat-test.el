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
      (should (string= (map-elt (car norm) 'name) "/workflow:dev"))
      (should (string= (map-elt (car norm) 'hint) "hint")))))

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

(provide 'emagent-chat-test)

;;; emagent-chat-test.el ends here
