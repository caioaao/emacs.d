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
(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-nord t))

;; customizing UI
(setq default-frame-alist '((undecorated . t)
                            (drag-internal-border . 1)
                            (internal-border-width . 5)))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(setq-default inhibit-startup-screen t)
(setq-default initial-scratch-message nil)
(setq-default fill-column 80)
(setq-default ns-auto-hide-menu-bar t)
(setq max-mini-window-height 0.1)

(define-globalized-minor-mode
  global-colnum-mode column-number-mode (lambda ()
                                          (column-number-mode 1)))
(global-colnum-mode t)

(use-package smartrep
  :ensure t
  :config
  (smartrep-define-key
      global-map "C-c w r" '(("<left>" . 'enlarge-window-horizontally)
                             ("<right>" . 'shrink-window-horizontally)
                             ("<up>" . 'shrink-window)
                             ("<down>" . 'enlarge-window))))

;; window navigation
(use-package windmove
  :ensure t
  :config
  (progn
    (smartrep-define-key
        global-map "C-c o" '(("h" . 'windmove-left)
                             ("l" . 'windmove-right)
                             ("k" . 'windmove-up)
                             ("j" . 'windmove-down)))
    (setq windmove-wrap-around t)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook (lambda () (show-paren-mode 1)))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(set-face-attribute 'show-paren-mismatch nil :weight 'extra-bold)

;; fic-mode
(use-package fic-mode
  :ensure t
  :hook (prog-mode . fic-mode))

(setq ns-use-srgb-colorspace nil)

(use-package powerline
  :ensure t
  :config (setq powerline-default-separator 'arrow))

(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (spaceline-helm-mode))

(use-package diminish :ensure t
  :hook
  (auto-revert-mode . (lambda () (diminish 'auto-revert-mode))))

(use-package nlinum :ensure t
  :config
  (global-nlinum-mode 1))

(provide 'guicfg)
;;; guicfg.el ends here
