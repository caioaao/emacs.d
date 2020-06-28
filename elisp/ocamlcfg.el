;;; ocamlcfg.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  Caio
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

(use-package tuareg :ensure t)

(use-package dune :ensure t
  :mode ("dune-project\\'" . dune-mode))

(use-package paredit
  :hook (dune-mode . paredit-mode))

(defun my:ocaml-exec-competitive ()
  (interactive)
  (compile "ocamlopt nums.cmxa str.cmxa -pp camlp4o -unsafe -o sol.exe-ocaml in.txt | tee out.txt"))

(provide 'ocamlcfg)
;;; ocamlcfg.el ends here
