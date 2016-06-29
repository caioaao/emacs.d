;;; pycfg.el --- Python configs                      -*- lexical-binding: t; -*-

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

;; Shared packages config (probably shared with other languages
(require 'company)
(require 'flycheck)

(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
(add-to-list 'company-backends 'company-anaconda)


;; flycheck config
(declare-function python-shell-calculate-exec-path "python")
                                        ; We can safely declare this function,
                                        ; since we'll only call it in Python
                                        ; Mode, that is, when python.el was
                                        ; already loaded.

(defun flycheck-virtualenv-set-python-executables ()
  "Set Python executables for the current buffer."
  (let ((exec-path (python-shell-calculate-exec-path)))
    (setq-local flycheck-python-pylint-executable
                (executable-find "pylint"))
    (setq-local flycheck-python-flake8-executable
                (executable-find "flake8"))
    (setenv "PYTHONPATH" )))

(defun flycheck-virtualenv-setup ()
  "Setup Flycheck for the current virtualenv."
  (interactive)
  (when (derived-mode-p 'python-mode)
    (add-hook 'hack-local-variables-hook
              #'flycheck-virtualenv-set-python-executables 'local)))


(eval-after-load "python-mode"
  (lambda () (setq python-fill-docstring-style 'django)))


;; anaconda-mode setup
(add-hook 'python-mode-hook 'anaconda-mode)

(defun flycheck-setup ()
  "Setup flycheck-mode to check correct executables."
  (when (derived-mode-p 'python-mode)
    (add-hook 'hack-local-variables-hook #'flycheck-set-execs)))

(provide 'pycfg)
;;; pycfg.el ends here
