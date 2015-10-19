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

(require 'org)

(defvar my-org-files-dir)
(defvar my-org-agenda-dir)
(defvar my-org-local-agenda-dir)



(setq my-org-files-dir "~/.emacs.d/orgfiles/")

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq  org-return-follows-link t)

;; initial org folders setup
(when (not (file-exists-p my-org-files-dir))
  (make-directory my-org-files-dir t))

;; Snippet to collect all .org from my Org directory and subdirs
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\'") ; default value
(defun load-org-agenda-files-recursively (dir) "Find all directories in DIR."
    (unless (file-directory-p dir) (error "Not a directory `%s'" dir))
    (unless (equal (directory-files dir nil org-agenda-file-regexp t) nil)
      (add-to-list 'org-agenda-files dir)
    )
    (dolist (file (directory-files dir nil nil t))
        (unless (member file '("." ".."))
            (let ((file (concat dir file "/")))
                (when (file-directory-p file)
                    (load-org-agenda-files-recursively file))))))

(load-org-agenda-files-recursively my-org-files-dir)
                                        ; trailing slash required

(provide 'orgcfg)
;;; orgcfg.el ends here
