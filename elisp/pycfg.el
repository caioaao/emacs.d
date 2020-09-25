;;; pycfg.el --- Python configs                      -*- lexical-binding: t; -*-

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

(use-package company :ensure t)

(use-package py-isort :ensure t
  :commands (py-isort-buffer py-isort-region))

(use-package blacken :ensure t)

(use-package python-pytest :ensure t
  :bind (("C-c C-x t" . python-pytest-dispatch)))

(use-package python
  :after (eglot)
  :config
  (setq eldoc-message-function 'eldoc-minibuffer-message)
  :mode-hydra
  ("Nav"
   (("n" python-nav-forward-defun "next-defun" :exit nil)
    ("p" python-nav-backward-defun "prev-defun" :exit nil))
   "Errors"
   (("<" flycheck-previous-error "prev" :exit nil)
    (">" flycheck-next-error "next" :exit nil)
    ("l" flycheck-list-errors "list"))
   "Env"
   (("a" pipenv-activate "pipenv-activate" :exit nil)
    ("d" pipenv-deactivate "pipenv-deactivate" :exit nil)
    ("w" pyvenv-workon "workon...")
    ("s" run-python "pyshell"))
   "Tools"
   (("f" blacken-buffer "reformat")
    ("i" py-isort-buffer "sort imports"))
   "Test"
   (("t" python-pytest-popup "pytest..."))))

(use-package toml-mode :ensure t
  :mode ("Pipfile" . toml-mode))

(use-package pipenv :ensure t
  :defer t
  :after (eglot)
  :hook
  (python-mode . pipenv-mode)
  (pipenv-mode . eglot-ensure)
  :diminish pipenv-mode
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-default)
  (setq pipenv-keymap-prefix (kbd "C-c C-o")))

(use-package jupyter :ensure t)

(provide 'pycfg)
;;; pycfg.el ends here
