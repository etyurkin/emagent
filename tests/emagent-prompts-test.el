;;; emagent-prompts-test.el --- ERT tests for emagent prompt constants -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-prompts)

(ert-deftest emagent-prompts-test-system-prompt ()
  (should (stringp emagent-acp-system-prompt))
  (should (> (length emagent-acp-system-prompt) 200))
  (should (string-match-p "org markup" emagent-acp-system-prompt))
  (should (string-match-p "read_file" emagent-acp-system-prompt)))

(ert-deftest emagent-prompts-test-prefer-emacs-prompt ()
  (should (string-match-p "Tool preference" emagent-acp-system-prompt-prefer-emacs))
  (should (string-match-p "check_elisp" emagent-acp-system-prompt-prefer-emacs))
  (should (string-match-p "check_elisp_file" emagent-acp-system-prompt-prefer-emacs))
  (should (string-match-p "elisp_replace_defun" emagent-acp-system-prompt-prefer-emacs)))

(ert-deftest emagent-prompts-test-elisp-guide ()
  (should (string-match-p "Paren rules" emagent-acp-elisp-guide))
  (should (string-match-p "Structural editing" emagent-acp-elisp-guide))
  (should (string-match-p "elisp_replace_defun" emagent-acp-elisp-guide))
  (should (string-match-p "check_elisp_file" emagent-acp-elisp-guide))
  (should (string-match-p "tree-sitter" emagent-acp-elisp-guide))
  (should (string-match-p "Multi-form refactors" emagent-acp-elisp-guide))
  (should (string-match-p "json-parse-string" emagent-acp-elisp-guide)))

(ert-deftest emagent-prompts-test-structural-policy ()
  (should (string-match-p "Elisp editing" (emagent-prompts--elisp-structural-policy))))

(ert-deftest emagent-prompts-test-gateway-prompt ()
  (should (string-match-p "OAuth" emagent-acp-system-prompt-gateway)))

(provide 'emagent-prompts-test)

;;; emagent-prompts-test.el ends here
