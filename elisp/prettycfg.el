;;; prettycfg.el --- Config to show pretty symbols   -*- lexical-binding: t; -*-

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

(add-hook
 'clojure-mode-hook
 (lambda ()
   (push '("fn" . ?λ) prettify-symbols-alist)
   (prettify-symbols-mode 1)))

(add-hook
 'lisp-mode-hook
 (lambda ()
   (push '("lambda" . ?λ) prettify-symbols-alist)
   (prettify-symbols-mode 1)))

(provide 'prettycfg)
;;; prettycfg.el ends here
