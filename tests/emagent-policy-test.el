;;; emagent-policy-test.el --- ERT tests for emagent policy -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-acp)
(require 'emagent-policy)

(defun emagent-policy-test--confirm-p (command)
  "Return non-nil when COMMAND yields a :confirm verdict."
  (let ((verdict (emagent-policy-check-shell command)))
    (and verdict (eq (car verdict) :confirm))))

(ert-deftest emagent-policy-test-safe-commands ()
  (dolist (cmd '("make test" "git status" "ls -la" "mvn compile"))
    (should-not (emagent-policy-shell-needs-confirm-p cmd))))

(ert-deftest emagent-policy-test-shell-rm-combined-rf ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-rm-combined-rf "rm -rf /tmp"))
  (should (emagent-policy-test--confirm-p "rm -fr /tmp")))

(ert-deftest emagent-policy-test-shell-rm-recursive ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-rm-recursive "rm --recursive foo")))

(ert-deftest emagent-policy-test-shell-rm-force ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-rm-force "rm --force foo")))

(ert-deftest emagent-policy-test-shell-dd ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-dd "dd if=/dev/zero of=/dev/null")))

(ert-deftest emagent-policy-test-shell-mkfs ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-mkfs "mkfs.ext4 /dev/sda1")))

(ert-deftest emagent-policy-test-shell-mke2fs ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-mke2fs "mke2fs /dev/sda1")))

(ert-deftest emagent-policy-test-shell-format ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-format "format c:")))

(ert-deftest emagent-policy-test-shell-shutdown ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-shutdown "shutdown -h now")))

(ert-deftest emagent-policy-test-shell-reboot ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-reboot "reboot")))

(ert-deftest emagent-policy-test-shell-init-0 ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-init-0 "init 0")))

(ert-deftest emagent-policy-test-shell-sudo-rm ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-sudo-rm "sudo rm foo")))

(ert-deftest emagent-policy-test-shell-curl-pipe-sh ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-curl-pipe-sh "curl http://x | sh")))

(ert-deftest emagent-policy-test-shell-trash ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-trash "trash foo")))

(ert-deftest emagent-policy-test-shell-kill-9 ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-kill-9 "kill -9 1234"))
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-kill-9 "kill -KILL 1234")))

(ert-deftest emagent-policy-test-shell-disk-overwrite ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-disk-overwrite "echo x > /dev/sda")))

(ert-deftest emagent-policy-test-shell-chmod-world ()
  (should (emagent-policy-rule-id-matches-p 'shell 'shell-chmod-world "chmod -R 777 foo")))

(ert-deftest emagent-policy-test-permission-validate-shell ()
  (let ((args (make-hash-table :test 'equal)))
    (puthash "command" "rm -rf /tmp" args)
    (let ((result (emagent-acp--permission-validate
                   `((kind . "execute") (arguments . ,args)))))
      (should (eq (car result) :confirm))
      (should (string-match-p "rm" (cdr result))))))

(ert-deftest emagent-policy-test-safe-mode-skips-dangerous-shell ()
  (emagent-test--with-mocks
      (((symbol-function 'emagent-permissions-global-fingerprints) (lambda () nil))
       ((symbol-function 'emagent-permissions-session-fingerprints) (lambda (_) nil)))
    (let* ((state (emagent-test--make-acp-state))
           (args (make-hash-table :test 'equal))
           (tool-call `((kind . "execute") (arguments . ,args))))
      (puthash "command" "rm -rf /tmp" args)
      (let ((emagent-acp-auto-approve-permissions 'safe))
        (should-not (emagent-acp--permission-gate-auto-approve-p
                     state tool-call
                     (emagent-acp--permission-validate tool-call)
                     "execute:rm" nil))))))

(ert-deftest emagent-policy-test-stored-grant-does-not-silence-confirm ()
  "A stored fingerprint grant must not auto-approve a policy :confirm command:
an `execute:rm' grant (e.g. from `rm foo.log') cannot auto-run `rm -rf /'."
  (emagent-test--with-mocks
      (((symbol-function 'emagent-permissions-global-fingerprints)
        (lambda () '("execute:rm")))
       ((symbol-function 'emagent-permissions-session-fingerprints) (lambda (_) nil))
       ((symbol-function 'emagent-permissions-project-fingerprints) (lambda (_) nil)))
    (let* ((state (emagent-test--make-acp-state))
           (args (make-hash-table :test 'equal))
           (tool-call `((kind . "execute") (arguments . ,args))))
      (puthash "command" "rm -rf /" args)
      (let ((emagent-acp-auto-approve-permissions nil))
        ;; The grant matches the fingerprint but policy says :confirm → no auto.
        (should (member "execute:rm" (emagent-permissions-global-fingerprints)))
        (should-not (emagent-acp--permission-gate-auto-approve-p
                     state tool-call
                     (emagent-acp--permission-validate tool-call)
                     "execute:rm" nil))
        ;; Allow-all (session) is the explicit opt-out and still auto-approves.
        (map-put! state :session-auto-approve t)
        (should (emagent-acp--permission-gate-auto-approve-p
                 state tool-call
                 (emagent-acp--permission-validate tool-call)
                 "execute:rm" nil))))))

(ert-deftest emagent-policy-test-elisp-blocked ()
  (let ((result (emagent-policy-check-elisp "(kill-emacs)")))
    (should (eq (car result) :deny))
    (should (string-match-p "kill-emacs" (cdr result)))))

(ert-deftest emagent-policy-test-elisp-dangerous ()
  (let ((result (emagent-policy-check-elisp "(delete-file \"foo\")")))
    (should (eq (car result) :confirm))
    (should (string-match-p "delete-file" (cdr result)))))

(ert-deftest emagent-policy-test-elisp-safe ()
  (should-not (emagent-policy-check-elisp "(+ 1 2)")))

(ert-deftest emagent-policy-test-python-os-system ()
  (should (emagent-policy-rule-id-matches-p 'python 'python-os-system "os.system('x')")))

(ert-deftest emagent-policy-test-python-subprocess ()
  (should (emagent-policy-rule-id-matches-p 'python 'python-subprocess-import "import subprocess"))
  (should (emagent-policy-rule-id-matches-p 'python 'python-subprocess-call "subprocess.run(['ls'])")))

(ert-deftest emagent-policy-test-python-safe ()
  (should-not (emagent-policy-check-python "print('hello')")))

(ert-deftest emagent-policy-test-shell-python-c ()
  (let ((result (emagent-policy-check-shell "python3 -c \"import os; os.system('x')\"")))
    (should (eq (car result) :confirm))
    (should (string-match-p "os.system" (cdr result)))))

(ert-deftest emagent-policy-test-shell-enforce-deny ()
  (should-error (emagent-policy-enforce '(:deny . "blocked") "rm -rf /")))

(ert-deftest emagent-policy-test-shell-run-command-enforce ()
  (require 'cl-lib)
  (require 'emagent-shell)
  (cl-letf (((symbol-function 'emagent-policy--runtime-confirm-p)
             (lambda (&rest _) nil)))
    (should-error (emagent-shell-run-command "rm -rf /tmp/foo"))))

(provide 'emagent-policy-test)

;;; emagent-policy-test.el ends here
