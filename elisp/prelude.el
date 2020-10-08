;;; prelude.el --- Helper functions and global dependencies used everywhere  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Caio Oliveira

;; Author: Caio Oliveira
;; Keywords:

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

;;; Commentary:

;;; Code:

(defun sorted (xs compare)
  "Non destructive sort.  Return XS sorted by comparing elements with COMPARE."
  (sort (copy-sequence xs) compare))

(defun symbol-name< (a b)
  "Compare symbols A and B by their names."
  (string< (symbol-name a) (symbol-name b)))

(defvar default-external-term "urxvt")

;; For pt-Br dead keys to work
(set-input-mode nil nil 1)
(require 'iso-transl)

;; Set encoding
(setenv "LC_CTYPE" "UTF-8")
(prefer-coding-system 'utf-8-unix)

;; yes or no becomes y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package eglot :ensure t)

(use-package diminish :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LANG" "LC_ALL")))

(use-package major-mode-hydra :ensure t
  :demand t
  :bind ("M-SPC" . major-mode-hydra))

(setq gc-cons-threshold 100000000)

;; direnv setup
(use-package direnv :ensure t
  :config
  (direnv-mode))

;; Some other modes
(use-package dockerfile-mode :ensure t)

(provide 'prelude)
;;; prelude.el ends here
