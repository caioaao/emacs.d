;;; editcfg.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Caio Augusto Araujo Oliveira

;; Author: Caio Augusto Araujo Oliveira <caiooliveira@Caios-MacBook-Pro-2.local>
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
(require 'undo-tree)
(global-undo-tree-mode 1)
(global-set-key (kbd "C-;") 'undo-tree-redo)



;; from Emacs Prelude (https://github.com/bbatsov/prelude)
;; Unfortunatelly, Prelude core library is not available as package :(
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'prelude-move-beginning-of-line)



;; imenu
(require 'helm)
(global-set-key (kbd "C-c i") 'helm-imenu)

(add-to-list 'auto-mode-alist '("\\.json.base\\'" . json-mode))

(require 'tramp)
;; run this to make tramp+ssh work with colored shells
(setq tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*")

(require 'anzu)
(global-anzu-mode 1)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(require 'avy)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

(require 'ido)
(setq ido-enable-flex-matching t)
(ido-mode t)

(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'turn-on-flyspell)

(require 'flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)



;; Useful for reading logs (by disabling line-wrap)
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)



;; better *help* buffer
(require 'helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

;; helpful keybinds
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-:") 'eval-expression)

(provide 'editcfg)
;;; editcfg.el ends here
