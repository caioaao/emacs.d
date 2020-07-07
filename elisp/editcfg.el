;;; editcfg.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Caio Augusto Araujo Oliveira

;; Author: Caio Augusto Araujo Oliveira
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

(use-package diminish :ensure t)

;; Indent using spaces only
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)

;; better scrolling
(setq scroll-step 1)

;; yasnippet
; (should be loaded before auto complete so that they can work
; together)
(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    ;; Fix yasnippet 0.8/ac bug
    (defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
    (defalias 'yas/table-hash 'yas--table-hash)
    (setq yas-snippet-dirs (append yas-snippet-dirs
                                   '("~/.emacs.d/snippets")))
    (diminish 'yas-minor-mode)))


(use-package undo-tree
  :ensure t
  :config
  (progn (global-undo-tree-mode 1)
         (diminish 'undo-tree-mode)))

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

(use-package helm
  :ensure t
  :bind (("C-c i" . helm-imenu)
         ("M-y" . helm-show-kill-ring)))

(use-package json-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json.base\\'" . json-mode)))

(use-package jsonnet-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.libjsonnet\\'" . jsonnet-mode)))

(use-package tramp
  ;; run this to make tramp+ssh work with colored shells
  :config (setq tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*"))

(use-package anzu
  :ensure t
  :config (progn
            (global-anzu-mode 1)
            (diminish 'anzu-mode)))

(use-package avy
  :ensure t
  :bind (("M-g e" . avy-goto-word-0)))

(use-package ido
  :ensure t
  :config (progn
            (setq ido-enable-flex-matching t)
            (ido-mode 1)))

(use-package flycheck
  :ensure t
  :config (diminish 'flycheck-mode))

(use-package flyspell
  :ensure t
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :config (progn
            (diminish 'flyspell-mode)
            (diminish 'flyspell-prog-mode)))

;; better *help* buffer
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h f" . helpful-function)
         ("C-h c" . helpful-command)))

(use-package company
  :ensure t
  :bind ("C-<tab>" . company-complete)
  :config
  (global-company-mode 1)
  (diminish 'company-mode))

(use-package paredit
  :ensure t
  :config
  (diminish 'paredit-mode))

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "M-x") nil)

;; Useful for reading logs (by disabling line-wrap)
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (progn
    (projectile-mode 1)
    (diminish 'projectile-mode)
    (setq projectile-enable-caching t)
    (setq projectile-git-submodule-command "")))

(use-package helm-projectile
  :ensure t
  :config
  (progn
    (helm-projectile-on)
    (setq projectile-completion-system 'helm)))

(use-package helm-ag :ensure t)

(use-package flymake :ensure t)
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)

;; deleting trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; reenabling useful functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package eldoc :ensure t)

(use-package rainbow-mode :ensure t)

(provide 'editcfg)
;;; editcfg.el ends here
