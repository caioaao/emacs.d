;;; guicfg.el --- GUI config                         -*- lexical-binding: t; -*-

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

;;

;;; Code:


;; theme
(load-theme 'colorsarenice-light t)



;; Remove toolbar and menubar
(tool-bar-mode -1)
(menu-bar-mode -1)



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



;; window navigation
(global-set-key (kbd "C-c o h") 'windmove-left)
(global-set-key (kbd "C-c o l") 'windmove-right)
(global-set-key (kbd "C-c o k") 'windmove-up)
(global-set-key (kbd "C-c o j") 'windmove-down)
(setq windmove-wrap-around t)



;; golden ratio auto resizing
(require 'golden-ratio)
(golden-ratio-mode 1)


;; smart-line
(require 'smart-mode-line)
(require 'smart-mode-line-powerline-theme)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'light-powerline)
(sml/setup)



(provide 'guicfg)
;;; guicfg.el ends here
