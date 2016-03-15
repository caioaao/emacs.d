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

;;; Code:

(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))

(require 'midje-mode)
;; Remap midje-mode (`C-c p` is the prefix cmd to projectile).
(define-key midje-mode-map (kbd "C-c n") nil)
(define-key midje-mode-map (kbd "C-c p") nil)
(define-key midje-mode-map (kbd "C-c m n") 'midje-next-fact)
(define-key midje-mode-map (kbd "C-c m p") 'midje-previous-fact)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (cider-mode 1)
                               (clj-refactor-mode 1)
                               (midje-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-o")))

(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

(require 'clojure-snippets)

(require 'cider)

(add-hook 'cider-repl-mode-hook 'paredit-mode)

(provide 'cljcfg)
;;; cljcfg.el ends here
