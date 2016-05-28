;;; orgcfg.el --- Org mode config, agenda, etc       -*- lexical-binding: t; -*-

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

;; trailing slash required when dir is a symlink
(defvar my-org-files-dirs '("~/.emacs.d/orgfiles"))

(require 'org)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq  org-return-follows-link t)


;; Snippet to collect all .org from my Org directory and subdirs
(defun load-org-agenda-files-recursively (dir)
  "Find all directories in DIR."
  (let ((file-regexp "\\`[^.].*\\.org\\'"))
    (unless (file-directory-p dir) (error "Not a directory `%s'" dir))
    (unless (equal (directory-files dir nil file-regexp t) nil)
      (add-to-list 'org-agenda-files dir))
    (dolist (file-name (directory-files dir nil nil t))
      (unless (member file-name '("." ".."))
        (let ((file-path (expand-file-name file-name dir)))
          (when (file-directory-p file-path)
            (load-org-agenda-files-recursively file-path)))))))

(dolist (p my-org-files-dirs)
  (unless (file-exists-p p)
    (make-directory p t))
  (load-org-agenda-files-recursively p))

(setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (sh . t)
   (python . t)
   (lisp . t)))

(add-hook 'org-mode-hook 'turn-on-flyspell)

(provide 'orgcfg)
;;; orgcfg.el ends here
