;;; guicfg.el --- GUI config                         -*- lexical-binding: t; -*-

;; Copyright (C) 2015

;; Author:  Caio Oliveira
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

;;

;;; Code:


;; theme
(load-theme 'doom-nord t)



;; Remove toolbar, menubar and scroll-bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)



;; Auto linum-mode
(global-linum-mode 1)



;; Highlight current line
(global-hl-line-mode 1)



;; Better scrolling
(setq scroll-step 1)



;; Column number
(define-globalized-minor-mode
  global-colnum-mode column-number-mode (lambda ()
                                          (column-number-mode 1)))

(global-colnum-mode t)




;; No initial screen
(setq-default inhibit-startup-screen t)



;; Clear scratch buffer
(setq-default initial-scratch-message nil)



;; 80 character rule
(setq-default fill-column 80)



;; window resizing shortcuts
(smartrep-define-key
    global-map "C-c w r" '(("<left>" . 'enlarge-window-horizontally)
                           ("<right>" . 'shrink-window-horizontally)
                           ("<up>" . 'shrink-window)
                           ("<down>" . 'enlarge-window)))



;; window navigation
(require 'windmove)
(smartrep-define-key
    global-map "C-c o" '(("h" . 'windmove-left)
                         ("l" . 'windmove-right)
                         ("k" . 'windmove-up)
                         ("j" . 'windmove-down)))
(setq windmove-wrap-around t)



(add-hook
 'prog-mode-hook
 (lambda()
   (rainbow-delimiters-mode 1)
   (show-paren-mode 1)))



;; fic-mode
(require 'fic-mode)
(add-hook 'prog-mode-hook (lambda () (fic-mode 1)))



;; info+
;; (require 'info+)



;; modeline
(require 'spaceline-config)
(setq powerline-default-separator 'arrow)
(setq ns-use-srgb-colorspace nil)
(spaceline-emacs-theme)
(spaceline-helm-mode)



;; hiding minor modes
(require 'diminish)
(eval-after-load "anzu" '(diminish 'anzu-mode))
(eval-after-load "auto-revert-mode" '(diminish 'auto-revert-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "yas-minor-mode" '(diminish 'yas-minor-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "clj-refactor" '(diminish 'clj-refactor-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))


;; parenthesis
(provide 'guicfg)
;;; guicfg.el ends here
