;;; emagent-context.el --- Context injection for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'org)
(require 'org-element)
(require 'map)
(require 'subr-x)

(defun emagent-context--point-info ()
  "Return point line and column as an alist."
  (save-excursion
    (list (cons :line (line-number-at-pos))
          (cons :column (current-column)))))

(defun emagent-context--region-info ()
  "Return region bounds and text when active."
  (when (region-active-p)
    (list (cons :begin (region-beginning))
          (cons :end (region-end))
          (cons :text (buffer-substring-no-properties (region-beginning) (region-end))))))

(defun emagent-context--org-info ()
  "Return org headline info at point when in org-mode."
  (when (derived-mode-p 'org-mode)
    (let ((element (org-element-at-point)))
      (when (eq (org-element-type element) 'headline)
        (list (cons :title (org-element-property :raw-value element))
              (cons :level (org-element-property :level element))
              (cons :tags (org-element-property :tags element)))))))

(defun emagent-context-auto ()
  "Build automatic Emacs context for the current buffer."
  (list (cons :buffer (buffer-name))
        (cons :file (or (buffer-file-name) nil))
        (cons :major-mode (symbol-name major-mode))
        (cons :default-directory default-directory)
        (cons :point (emagent-context--point-info))
        (cons :region (emagent-context--region-info))
        (cons :org (emagent-context--org-info))))

(defun emagent-context-format (context)
  "Format CONTEXT alist as a readable string block."
  (let ((lines
         (list "[Emacs context]"
               (format "buffer: %s" (map-elt context :buffer))
               (format "file: %s" (or (map-elt context :file) "<none>"))
               (format "major-mode: %s" (map-elt context :major-mode))
               (format "default-directory: %s" (map-elt context :default-directory)))))
    (when-let* ((point (map-elt context :point)))
      (setq lines (append lines
                          (list (format "point: line %s, column %s"
                                        (map-elt point :line)
                                        (map-elt point :column))))))
    (when-let* ((region (map-elt context :region)))
      (setq lines (append lines
                          (list (format "region: %s-%s"
                                        (map-elt region :begin)
                                        (map-elt region :end))
                                (format "region-text:\n%s" (map-elt region :text))))))
    (when-let* ((org (map-elt context :org)))
      (setq lines (append lines
                          (list (format "org-headline: level %s, title %s"
                                        (map-elt org :level)
                                        (map-elt org :title))))))
    (string-join lines "\n")))

(defun emagent-context-buffer-summary ()
  "Return a short summary of the current buffer."
  (let ((lines (count-lines (point-min) (point-max)))
        (chars (- (point-max) (point-min))))
    (format "[Buffer summary]\nname: %s\nlines: %s\nchars: %s\nmode: %s"
            (buffer-name) lines chars (symbol-name major-mode))))

(defun emagent-context-region ()
  "Return the active region text or signal an error."
  (unless (region-active-p)
    (user-error "No active region"))
  (format "[Region]\n%s"
          (buffer-substring-no-properties (region-beginning) (region-end))))

(defun emagent-context-build-prompt (user-text &optional extra-blocks)
  "Combine USER-TEXT with auto context and EXTRA-BLOCKS."
  (let* ((auto (emagent-context-format (emagent-context-auto)))
         (blocks (cons auto (or extra-blocks nil))))
    (string-join (cons user-text blocks) "\n\n")))

(provide 'emagent-context)

;;; emagent-context.el ends here
