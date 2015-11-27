;;; cljcfg.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2015

;; Author:  <caio@caio-ntb>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Most of it was copied from the article "clojure and emacs without cider":
;; http://martintrojer.github.io/clojure/2014/10/02/clojure-and-emacs-without-cider/

;;; Code:

(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))
(setq inferior-lisp-program "lein repl")


(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-o")))

(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-region)))

(require 'clojure-snippets)

(provide 'cljcfg)
;;; cljcfg.el ends here
