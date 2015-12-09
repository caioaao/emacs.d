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
(require 'cc-mode)

;; Adding newline after files
(add-hook 'c-c++-mode-hook
          (lambda () (setq require-final-newline t)))

;; sets extended mode curly braces as default
(setq c-default-style "linux"
      c-basic-offset 4)

;; C++11 as standard
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-gcc-language-standard "c++11")))
;; auto complete
;; (require 'auto-complete-clang)
(define-auto-insert "sol\.cpp$" "competitive-template.cpp")

;; ggtags
(require 'ggtags)

(setq ggtags-oversize-limit (* 1 1024 1024))


(add-hook 'dired-mode-hook (lambda () (ggtags-mode 1)))
(add-hook 'c-mode-hook (lambda () (ggtags-mode 1)))
(add-hook 'c++-mode-hook (lambda () (ggtags-mode 1)))
(add-hook 'java-mode-hook (lambda () (ggtags-mode 1)))


(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

(require 'cc-mode)
(require 'semantic)
(require 'cedet)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)

(semantic-mode 1)

(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(setq semantic-c-obey-conditional-section-parsing-flag nil)

(defun cccfg:cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'cccfg:cedet-hook)
(add-hook 'c-mode-hook 'cccfg:cedet-hook)
(add-hook 'c++-mode-hook 'cccfg:cedet-hook)


(require 'company)
(global-company-mode 1)


(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun setup-flycheck-proj-path ()
  (let ((prj-root (ignore-errors (projectile-project-root))))
    (when prj-root
      (add-to-list (make-variable-buffer-local 'flycheck-gcc-include-path)
                   prj-root)
      (let ((prj-src-dir (expand-file-name "src" prj-root)))
        (when (file-exists-p prj-src-dir)
          (add-to-list (make-variable-buffer-local 'flycheck-gcc-include-path)
                       prj-src-dir))))))

(add-hook 'c-mode-common-hook 'setup-flycheck-proj-path)
(add-hook 'c-mode-hook 'setup-flycheck-proj-path)
(add-hook 'c++-mode-hook 'setup-flycheck-proj-path)

(provide 'cccfg)
;;; cccfg.el ends here
