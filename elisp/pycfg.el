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


;; virtualenvwrapper
(require 'virtualenvwrapper)
(defun virtualenvwrapper-init-full-shell-support()
  "Just initializes some support functions."
  (interactive)
  (venv-initialize-interactive-shells) ;; interactive shell support
  (venv-initialize-eshell)) ;; eshell support
(setq venv-location "~/my/dev/venvs/")
(setenv "WORKON_HOME" venv-location)

(require 'flycheck)

;; We can safely declare this function, since we'll only call it in Python Mode,
;; that is, when python.el was already loaded.
(declare-function python-shell-calculate-exec-path "python")

(defun flycheck-virtualenv-set-python-executables ()
  "Set Python executables for the current buffer."
  (let ((exec-path (python-shell-calculate-exec-path)))
    (setq-local flycheck-python-pylint-executable
                (executable-find "pylint"))
    (setq-local flycheck-python-flake8-executable
                (executable-find "flake8"))))

(defun flycheck-virtualenv-setup ()
  "Setup Flycheck for the current virtualenv."
  (interactive)
  (when (derived-mode-p 'python-mode)
    (add-hook 'hack-local-variables-hook
              #'flycheck-virtualenv-set-python-executables 'local)))

(require 'company)
(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)

;; (require 'ac-python)
;; (add-hook 'python-mode-hook '(lambda() (ac-python)))

;; Auto insert template
(define-auto-insert "\.py$" "python-template.py")

;; jedi.el - python autocomplete
;; (require 'jedi)
;;
;; (setq jedi:complete-on-dot t)
;; (add-hook 'python-mode-hook 'jedi:setup)

;; ipython
;; (require 'python-mode)
;;
;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;    "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;    "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(provide 'pycfg)
;;; pycfg.el ends here
