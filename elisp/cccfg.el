;;; cccfg.el --- C config                            -*- lexical-binding: t; -*-

;; Copyright (C) 2015

;; Author:  <coliveira@POS6419D>
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

;; cc-mode
(use-package cc-mode :ensure t
  :hook
  (c-c++-mode . (lambda () (setq require-final-newline t)))
  :init
  (setq c-default-style "linux"
        c-basic-offset 4))

(use-package lsp-mode
  :hook (c-c++-mode . lsp))

;; (use-package flycheck
;;   :ensure t
;;   :hook
;;   (c++-mode . (lambda () (setq flycheck-gcc-language-standard "c++11"))))
;; ;; auto complete
;; ;; (require 'auto-complete-clang)

;; ;; ggtags
;; (use-package ggtags
;;   :ensure t
;;   :hook
;;   (dired-mode . (lambda () (ggtags-mode 1)))
;;   (c-mode . (lambda () (ggtags-mode 1)))
;;   (c++-mode . (lambda () (ggtags-mode 1)))
;;   :bind
;;   (:map ggtags-mode-map
;;         ("C-c g s" . ggtags-find-other-symbol)
;;         ("C-c g h" . ggtags-view-tag-history)
;;         ("C-c g r" . ggtags-find-reference)
;;         ("C-c g f" . ggtags-find-file)
;;         ("C-c g c" . ggtags-create-tags)
;;         ("C-c g u" . ggtags-update-tags)
;;         ("M-," . pop-tag-mark))
;;   :init
;;   (setq ggtags-oversize-limit (* 1 1024 1024)))

;; (use-package semantic
;;   :ensure t
;;   :init
;;   (setq semantic-c-obey-conditional-section-parsing-flag nil)
;;   :config
;;   (global-semanticdb-minor-mode 1)
;;   (global-semantic-idle-scheduler-mode 1)
;;   (global-semantic-stickyfunc-mode 1)
;;   (semanticdb-enable-gnu-global-databases 'c-mode)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode))

;; (use-package cedet
;;   :ensure t)

;; (defun cccfg:cedet-hook ()
;;   (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
;;   (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

;; (add-hook 'c-mode-common-hook 'cccfg:cedet-hook)
;; (add-hook 'c-mode-hook 'cccfg:cedet-hook)
;; (add-hook 'c++-mode-hook 'cccfg:cedet-hook)

;; (use-package ansi-color :ensure t)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region (point-min) (point-max))
;;   (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; (defun setup-flycheck-proj-path ()
;;   (let ((prj-root (ignore-errors (projectile-project-root))))
;;     (when prj-root
;;       (add-to-list (make-variable-buffer-local 'flycheck-gcc-include-path)
;;                    prj-root)
;;       (let ((prj-src-dir (expand-file-name "src" prj-root)))
;;         (when (file-exists-p prj-src-dir)
;;           (add-to-list (make-variable-buffer-local 'flycheck-gcc-include-path)
;;                        prj-src-dir))))))

;; (add-hook 'c-mode-common-hook 'setup-flycheck-proj-path)
;; (add-hook 'c-mode-hook 'setup-flycheck-proj-path)
;; (add-hook 'c++-mode-hook 'setup-flycheck-proj-path)

(provide 'cccfg)
;;; cccfg.el ends here
