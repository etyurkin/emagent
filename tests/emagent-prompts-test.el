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
    (should (string-match-p "Context discipline" prompt))
    (should (string-match-p "imenu_index" prompt))
    (should (string-match-p "shell op=compile" prompt))
    (should (string-match-p "mvn" prompt))
    (should (string-match-p "/compact" prompt))
    (should (string-match-p "elisp op=guide" prompt))
    (if (emagent-struct-available-p)
        (progn
          (should (string-match-p "op=check_file"
                                  (emagent-prompts--structural-policy)))
          (should (string-match-p "op=get/replace/insert"
                                  (emagent-prompts--structural-policy)))
          (should (string-match-p "fs op=write refused"
                                  (emagent-prompts--structural-policy))))
      (progn
        (should (string-match-p (regexp-quote "fs op=write + elisp op=check")
                                (emagent-prompts--structural-policy)))
        (should-not (string-match-p "fs op=write refused"
                                    (emagent-prompts--structural-policy)))))))

(ert-deftest emagent-prompts-test-prefer-emacs-prompt-simulated-no-lisp-sitter ()
  (cl-letf (((symbol-function 'emagent-struct-available-p) (lambda () nil)))
    (let ((prompt (emagent-prompts--prefer-emacs-prompt))
          (policy (emagent-prompts--structural-policy)))
      (should (string-match-p "Context discipline" prompt))
      (should (string-match-p "Install lisp-sitter" policy))
      (should (string-match-p (regexp-quote "fs op=write + elisp op=check") policy))
      (should-not (string-match-p "fs op=write refused" policy)))))

(ert-deftest emagent-prompts-test-elisp-guide ()
  (should (string-match-p "Paren rules" emagent-acp-elisp-guide))
  (should (string-match-p "Structural editing" emagent-acp-elisp-guide))
  (should (string-match-p "structural op=replace" emagent-acp-elisp-guide))
  (should (string-match-p "structural op=check_file" emagent-acp-elisp-guide))
  (should (string-match-p "lisp-sitter" emagent-acp-elisp-guide))
  (should (string-match-p "Multi-node refactors" emagent-acp-elisp-guide))
  (should (string-match-p "json-parse-string" emagent-acp-elisp-guide))
  (should (string-match-p "Think in code" emagent-acp-elisp-guide)))

(ert-deftest emagent-prompts-test-structural-policy ()
  (require 'emagent-tools)
  (let ((policy (emagent-prompts--structural-policy)))
    (should (string-match-p "Lisp editing" policy))
    (when (emagent-struct-available-p)
      (should (string-match-p "fs op=write refused" policy))
      (should (string-match-p "op=get/replace/insert" policy)))))

(ert-deftest emagent-prompts-test-gateway-prompt ()
  (should (string-match-p "OAuth" emagent-acp-system-prompt-gateway))
  (should (string-match-p "External MCP servers" emagent-acp-system-prompt-gateway))
  (should (string-match-p "meta-prox" emagent-acp-system-prompt-gateway)))

(provide 'emagent-prompts-test)

;;; emagent-prompts-test.el ends here
