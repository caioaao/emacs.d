;;; lspcfg.el --- LSP mode config                    -*- lexical-binding: t; -*-

;; Copyright (C) 2020

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-enable-xref t)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-prefer-flymake t)
  :hook
  (hack-local-variables
   . (lambda ()
       (when (derived-mode-p
              'tuareg-mode
              'reason-mode)
         (lsp)))))

(use-package company-lsp
  :ensure t
  :config (push 'company-lsp company-backends))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil))

(provide 'lspcfg)
;;; lspcfg.el ends here
