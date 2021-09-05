;;; gocfj.el --- golang config                       -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  Caio Oliveira

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package go-mode
  :ensure t
  :after (eglot)
  :hook
  (go-mode . eglot-ensure)
  (before-save . gofmt-before-save)
  :mode-hydra
  ("Tools"
   (("e" my:flycheck-hydra/body)
    ("r" my:eglot-hydra/body))))



(defun my:project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(use-package project
  :config
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  (add-hook 'project-find-functions #'my:project-find-go-module))

(provide 'gocfg)
;;; gocfg.el ends here
