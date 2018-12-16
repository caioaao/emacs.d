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

;;======================
;; agenda
;;======================

;; trailing slash required when dir is a symlink
(defvar my-org-files-dir "~/reps/orgfiles/")

(require 'org)
(require 'org-agenda)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(setq  org-return-follows-link t)


(defun load-org-agenda-files-recursively (dir)
  "Collect all org agenda files in DIR."
  (unless (file-directory-p dir) (error "Not a directory `%s'" dir))
  (add-to-list 'org-agenda-files dir)
  (dolist (file-name (directory-files dir nil nil t))
    (unless (member file-name '("." ".."))
      (let ((file-path (expand-file-name file-name dir)))
        (when (file-directory-p file-path)
          (load-org-agenda-files-recursively file-path))))))

(defun load-my-agenda-files ()
  "Load all agenda files recursively."
  (interactive)
  (unless (file-exists-p my-org-files-dir)
    (make-directory my-org-files-dir t))
  (load-org-agenda-files-recursively my-org-files-dir))

(load-my-agenda-files)

;;======================
;; clocking
;;======================
(require 'org-mru-clock)

(setq org-mru-clock-how-many 20)
(setq org-mru-clock-completing-read #'ivy-completing-read)

;; global keys with history for clocking in and out
(global-set-key (kbd "C-c I") #'org-mru-clock-in)
(global-set-key (kbd "C-c J") #'org-mru-clock-select-recent-task)
(global-set-key (kbd "C-c O") #'org-clock-out)

;;======================
;; babel
;;======================

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (shell . t)
   (python . t)
   (ipython . t)
   (lisp . t)
   (clojure . t)
   (gnuplot . t)
   (R . t)
   (plantuml . t)))

(add-to-list 'org-src-lang-modes '("edn" . "clojure"))

(setq org-confirm-babel-evaluate nil)

(setq org-export-use-babel nil)

;;======================
;; UI
;;======================

(setq org-src-fontify-natively t)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; increase inline latex images size
(plist-put org-format-latex-options :scale 1.5)

;;======================
;; Other
;;======================

(add-hook 'org-mode-hook 'turn-on-flyspell)

(custom-set-variables
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-log-into-drawer t))


(require 'org-tree-slide)
(require 'ox-gfm)

(define-key org-tree-slide-mode-map (kbd "<right>") 'org-tree-slide-move-next-tree)
(define-key org-tree-slide-mode-map (kbd "<left>") 'org-tree-slide-move-previous-tree)

(require 'toc-org nil t)
(add-hook 'org-mode-hook 'toc-org-enable)

;;======================
;; GTD
;;======================

(defvar gtd-inbox-p (concat my-org-files-dir "gtd/inbox.org"))
(defvar gtd-main-p (concat my-org-files-dir "gtd/main.org"))
(defvar gtd-someday-p (concat my-org-files-dir "gtd/someday.org"))
(defvar gtd-tickler-p (concat my-org-files-dir "gtd/tickler.org"))

(setq org-refile-targets '((gtd-main-p :maxlevel . 3)
                           (gtd-someday-p :level . 1)
                           (gtd-tickler-p :maxlevel . 2)))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline gtd-inbox-p "Tasks")
                               "* TODO %i%?\n  %U\n"
                               :prepend t :empty-lines 1)
                              ("T" "Tickler" entry
                               (file+headline gtd-tickler-p "Tickler")
                               "* %i%? \n %U")))

(global-set-key (kbd "C-M-r") 'org-capture)

(provide 'orgcfg)
;;; orgcfg.el ends here
