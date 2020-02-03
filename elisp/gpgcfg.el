;;; gpgcfg.el --- Generic GPG config                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Caio Oliveira

;; Author: Caio Oliveira <caio@pop-os>
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

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
;; copying GPG stuff (needed in Ubuntu for some reason)


(provide 'gpgcfg)
;;; gpgcfg.el ends here
