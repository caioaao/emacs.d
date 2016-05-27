;;; clcfg.el --- Common Lisp config                  -*- lexical-binding: t; -*-

;; Copyright (C) 2016

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

(defvar quicklisp-path (expand-file-name "~/.local/opt/quicklisp/"))
(defvar slime-helper-path (concat (file-name-as-directory quicklisp-path) "slime-helper.el"))

(setq inferior-lisp-program "sbcl")

(if (executable-find inferior-lisp-program)
    (progn
      (when (and (not (file-exists-p quicklisp-path))
                 (y-or-n-p (format "Quicklisp is not installed in %s.  Do you wish to download and install? " quicklisp-path)))
        (let ((quicklisp-installscript-path "~/tmp/quicklisp.lisp")
              (quicklisp-install-instructions-path "~/tmp/qlinstall.lisp"))
          (url-copy-file "https://beta.quicklisp.org/quicklisp.lisp" quicklisp-installscript-path)
          (with-temp-file quicklisp-install-instructions-path
            (insert (format "(load \"%s\") (quicklisp-quickstart:install :path \"%s\") (ql:quickload \"quicklisp-slime-helper\") (quit)"
                            quicklisp-installscript-path quicklisp-path)))
          (shell-command (concat inferior-lisp-program " < " quicklisp-install-instructions-path)))))
  (message (format "Inferior lisp program % not found. Couldn't load SLIME" inferior-lisp-program)))

(when (and (executable-find inferior-lisp-program)
           (file-exists-p slime-helper-path))
  (load slime-helper-path))

(require 'paredit)
(add-hook 'inferior-lisp-mode-hook 'paredit-mode)

(provide 'clcfg)
;;; clcfg.el ends here
