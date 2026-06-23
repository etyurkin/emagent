;;; emagent-tools-intro.el --- Emacs introspection tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'emagent-struct)
(require 'emagent-elisp)
(require 'emagent-tools-file)

(declare-function emagent-tools--root-directory "emagent-tools")
(declare-function emagent-tools--eval-form-guard "emagent-tools")
(declare-function emagent-tools--eval-form-safely "emagent-tools")

(provide 'emagent-tools-intro)
;;; emagent-tools-intro.el ends here
