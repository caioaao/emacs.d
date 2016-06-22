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

(require 'pyvenv)
(require 'flycheck)

(require 'company)
(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)

(defun flycheck-set-execs ()
  "Local vars to for flycheck."
  (let ((exec-path (python-shell-calculate-exec-path)))
    (setq-local flycheck-python-pylint-executable (executable-find "pylint"))
    (setq-local flycheck-python-flake8-executable (executable-find "flake8"))
    (setq python-shell-interpreter (executable-find "python"))))

(defun flycheck-setup ()
  "Setup flycheck-mode to check correct executables."
  (when (derived-mode-p 'python-mode)
    (add-hook 'hack-local-variables-hook #'flycheck-set-execs)))

(add-hook 'pyvenv-post-activate-hooks #'flycheck-set-execs)

(add-hook 'flycheck-mode-hook #'flycheck-setup)

(provide 'pycfg)
;;; pycfg.el ends here
