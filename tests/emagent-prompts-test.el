;;; emagent-prompts-test.el --- ERT tests for emagent prompt constants -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emagent-tools)

(ert-deftest emagent-prompts-test-system-prompt ()
  (should (stringp emagent-acp-system-prompt))
  (should (> (length emagent-acp-system-prompt) 200))
  (should (string-match-p "org markup" emagent-acp-system-prompt))
  (should (string-match-p "fs" emagent-acp-system-prompt)))

(ert-deftest emagent-prompts-test-prefer-emacs-prompt ()
  (require 'emagent-tools)
  (let ((prompt (emagent-prompts--prefer-emacs-prompt)))
    (should (string-match-p "Tool preference" prompt))
    (should (string-match-p "elisp op=check" prompt))
    (if (emagent-struct-available-p)
        (progn
          (should (string-match-p "structural op=check_file" prompt))
          (should (string-match-p "structural op=replace" prompt))
          (should (string-match-p "fs write refused" prompt)))
      (progn
        (should (string-match-p (regexp-quote "fs op=write + elisp op=check") prompt))
        (should-not (string-match-p "fs write refused" prompt))
        (should-not (string-match-p "structural op=replace" prompt))))))

(ert-deftest emagent-prompts-test-prefer-emacs-prompt-simulated-no-lisp-sitter ()
  (cl-letf (((symbol-function 'emagent-struct-available-p) (lambda () nil)))
    (let ((prompt (emagent-prompts--prefer-emacs-prompt)))
      (should (string-match-p "lisp-sitter is not installed" prompt))
      (should (string-match-p (regexp-quote "fs op=write + elisp op=check") prompt))
      (should-not (string-match-p "structural op=replace" prompt))
      (should-not (string-match-p "fs write refused" prompt)))))

(ert-deftest emagent-prompts-test-elisp-guide ()
  (should (string-match-p "Paren rules" emagent-acp-elisp-guide))
  (should (string-match-p "Structural editing" emagent-acp-elisp-guide))
  (should (string-match-p "structural op=replace" emagent-acp-elisp-guide))
  (should (string-match-p "structural op=check_file" emagent-acp-elisp-guide))
  (should (string-match-p "lisp-sitter" emagent-acp-elisp-guide))
  (should (string-match-p "Multi-node refactors" emagent-acp-elisp-guide))
  (should (string-match-p "json-parse-string" emagent-acp-elisp-guide)))

(ert-deftest emagent-prompts-test-structural-policy ()
  (require 'emagent-tools)
  (let ((policy (emagent-prompts--structural-policy)))
    (should (string-match-p "Structural editing" policy))
    (when (emagent-struct-available-p)
      (should (string-match-p "fs op=write is refused" policy))
      (should (string-match-p "structural op=replace" policy)))))

(ert-deftest emagent-prompts-test-gateway-prompt ()
  (should (string-match-p "OAuth" emagent-acp-system-prompt-gateway))
  (should (string-match-p "External MCP servers" emagent-acp-system-prompt-gateway))
  (should (string-match-p "meta-prox" emagent-acp-system-prompt-gateway)))

(provide 'emagent-prompts-test)

;;; emagent-prompts-test.el ends here
