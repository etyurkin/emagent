;;; emagent-chat-integration-test.el --- ERT integration tests for chat rendering -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emagent-test-utils)
(require 'emagent-acp)
(require 'emagent-chat)

(ert-deftest emagent-chat-integration-test-response-cycle ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (let ((at (emagent-chat--insert-user-heading-with-text "hello")))
         (emagent-chat--begin-response at)
         (emagent-chat-append-assistant "partial ")
         (emagent-chat-finish-assistant "Hello *world*"))
       (let ((text (substring-no-properties (buffer-string))))
         (should (string-match-p "hello" text))
         (should (string-match-p "Hello" text))
         (should (string-match-p "^\\*\\* Response" text))
         (should-not (string-match-p emagent-chat--progress-line text)))))))

(ert-deftest emagent-chat-integration-test-finalize-preserves-later-exchanges ()
  "Finalizing a mid-buffer response must not delete the exchanges below it
(re-evaluating an earlier prompt opens a response with content still below)."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       ;; First exchange.
       (let ((at (emagent-chat--insert-user-heading-with-text "first")))
         (emagent-chat--begin-response at)
         (emagent-chat-finish-assistant "first answer"))
       ;; A later exchange below it (as if the user continued the conversation).
       (goto-char (point-max))
       (emagent-chat--insert-user-heading-with-text "LATER-MARKER")
       ;; Re-evaluate the first response: open a response mid-buffer, above the
       ;; later exchange, and finalize it.
       (goto-char (point-min))
       (let ((at (progn (re-search-forward (emagent-chat--user-heading-re))
                        (line-end-position))))
         (emagent-chat--begin-response at)
         (emagent-chat-finish-assistant "revised answer"))
       (let ((text (substring-no-properties (buffer-string))))
         (should (string-match-p "revised answer" text))
         ;; The later exchange must survive finalization.
         (should (string-match-p "LATER-MARKER" text)))))))

(ert-deftest emagent-chat-integration-test-response-content-marker-owned ()
  "After the Response headline exists, the body content-start is read from an
owned marker (not re-searched), and multi-chunk streaming renders in order."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (let ((at (emagent-chat--insert-user-heading-with-text "q")))
         (emagent-chat--begin-response at)
         (emagent-chat-append-assistant "one ")
         ;; The content marker is now owned and live.
         (should (markerp emagent-chat--response-content-marker))
         (should (marker-position emagent-chat--response-content-marker))
         (let ((content-start (car (emagent-chat--response-body-bounds))))
           (should (= content-start
                      (marker-position emagent-chat--response-content-marker))))
         (emagent-chat-append-assistant "two ")
         (emagent-chat-append-assistant "three"))
       (let ((text (substring-no-properties (buffer-string))))
         (should (string-match-p "one two three" text)))
       ;; Last exchange in the buffer: the end marker is the point-max sentinel.
       (should (eq 'point-max emagent-chat--response-end-marker))))))

(ert-deftest emagent-chat-integration-test-end-marker-bounds-mid-buffer ()
  "Re-evaluating an earlier prompt owns a live end marker at the following
exchange's heading, so the region is bounded there rather than at point-max."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (let ((at (emagent-chat--insert-user-heading-with-text "first")))
         (emagent-chat--begin-response at)
         (emagent-chat-finish-assistant "first answer"))
       (goto-char (point-max))
       (emagent-chat--insert-user-heading-with-text "LATER")
       (goto-char (point-min))
       (let ((at (progn (re-search-forward (emagent-chat--user-heading-re))
                        (line-end-position))))
         (emagent-chat--begin-response at)
         ;; End marker is a live marker sitting at/above the LATER heading.
         (should (markerp emagent-chat--response-end-marker))
         (should (< (marker-position emagent-chat--response-end-marker)
                    (point-max)))
         (emagent-chat-finish-assistant "revised"))
       (let ((text (substring-no-properties (buffer-string))))
         (should (string-match-p "revised" text))
         (should (string-match-p "LATER" text)))))))

(ert-deftest emagent-chat-integration-test-reasoning-then-response-separated ()
  "Reasoning streams into `** Thinking' and the answer into `** Response'; once
the Response headline exists the Thinking tail is read from the owned marker.
Reasoning text must not leak into the Response body and vice versa."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (emagent-test--with-busy-session
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (emagent-chat--begin-response (point))
          (emagent-chat-begin-thought)
          (emagent-chat-append-thought "planning the approach")
          (emagent-chat-append-assistant "the answer")
          ;; Response headline now exists → owned content marker is set.
          (should (markerp emagent-chat--response-content-marker))
          (let* ((text (substring-no-properties (buffer-string)))
                 (thinking-at (string-match "\\*\\* Thinking" text))
                 (response-at (string-match "\\*\\* Response" text))
                 (reasoning-at (string-match "planning the approach" text))
                 (answer-at (string-match "the answer" text)))
            (should (and thinking-at response-at reasoning-at answer-at))
            ;; Reasoning sits under Thinking (before Response); answer under Response.
            (should (< thinking-at reasoning-at response-at))
            (should (< response-at answer-at)))))))))

(ert-deftest emagent-chat-integration-test-streamed-code-block-preserved ()
  "Streaming a completed code block must not rewrite its interior backticks or
double-stars, while inline markup in surrounding prose is still converted."
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (let ((at (emagent-chat--insert-user-heading-with-text "q")))
         (emagent-chat--begin-response at)
         (emagent-chat-append-assistant
          (concat "Use `x` and a**b.\n\n"
                  "```python\n"
                  "y = `z`\n"
                  "w = a**b\n"
                  "```\n\n"
                  "Done `now`.")))
       (let ((text (substring-no-properties (buffer-string))))
         ;; Prose inline code converted.
         (should (string-match-p "Use =x=" text))
         (should (string-match-p "Done =now=" text))
         ;; Code interior preserved verbatim.
         (should (string-match-p "y = `z`" text))
         (should (string-match-p "w = a\\*\\*b" text)))))))

(ert-deftest emagent-chat-integration-test-fail-assistant ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (with-current-buffer buffer
       (goto-char (point-max))
       (let ((at (emagent-chat--insert-user-heading-with-text "boom")))
         (emagent-chat--begin-response at)
         (emagent-chat-fail-assistant "agent died"))
       (let ((text (substring-no-properties (buffer-string))))
         (should (string-match-p "\\*Error:\\* agent died" text))
         (should (string-match-p "^\\*\\* Response" text)))))))

(ert-deftest emagent-chat-integration-test-acp-chunk-to-buffer ()
  (emagent-test--with-emagent-buffer
   (lambda (buffer _dir)
     (let* ((client (emagent-test--make-test-client))
            (state (emagent-test--make-acp-state client buffer)))
       (setq emagent-acp-stream-to-buffer t)
       (setf (emagent-acp-state-cb-chunk state) #'emagent-chat-append-assistant)
       (setf (emagent-acp-state-busy state) t)
       (with-current-buffer buffer
         (goto-char (point-max))
         (emagent-chat--begin-response (point))
         (emagent-acp--on-notification
          :state state
          :emagent-acp-notification (emagent-test--notification-chunk "streamed "))
         (emagent-acp--on-notification
          :state state
          :emagent-acp-notification (emagent-test--notification-chunk "text"))
         (should (string-match-p "streamed text"
                                 (substring-no-properties (buffer-string)))))))))

(provide 'emagent-chat-integration-test)

;;; emagent-chat-integration-test.el ends here
