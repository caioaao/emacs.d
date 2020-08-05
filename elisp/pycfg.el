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

(use-package lsp-mode :ensure t
  :defines lsp-clients-python-library-directories
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     (lambda () '("pipenv" "run" "pyls")))
                    :major-modes '(python-mode cython-mode)
                    :priority -2
                    :server-id 'pipenv-pyls
                    :library-folders-fn (lambda (_workspace) lsp-clients-python-library-directories))))

(use-package python-mode :ensure t
  :after (lsp)
  :hook
  (python-mode . lsp))

(use-package toml-mode :ensure t
  :mode ("Pipfile" . toml-mode))

(use-package pipenv :ensure t
  :hook (python-mode . pipenv-mode))

(provide 'pycfg)
;;; pycfg.el ends here
