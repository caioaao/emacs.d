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

;;
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

(provide 'cccfg)
;;; cccfg.el ends here
