;;; emagent-tools-intro.el --- Emacs introspection tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Evgeniy Tyurkin, Mike Ivanov

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; Assisted-by: Cursor:claude-sonnet-4.6

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Emacs introspection tool handlers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'emagent-tools-file)

(declare-function emagent-tools--root-directory "emagent-tools")
(declare-function emagent-tools--eval-form-guard "emagent-tools")
(declare-function emagent-tools--eval-form-safely "emagent-tools")

(provide 'emagent-tools-intro)
;;; emagent-tools-intro.el ends here
