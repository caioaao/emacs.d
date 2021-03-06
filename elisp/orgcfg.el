;;; orgcfg.el --- Org mode config, agenda, etc       -*- lexical-binding: t; -*-

;; Copyright (C) 2015

;; Author:  Caio Oliveira
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
(defvar my-org-files-dir "~/reps/orgfiles/")

;; gtd stuff
(defvar gtd-inbox-p (concat my-org-files-dir "gtd/inbox.org"))
(defvar gtd-main-p (concat my-org-files-dir "gtd/main.org"))
(defvar gtd-someday-p (concat my-org-files-dir "gtd/someday.org"))
(defvar gtd-tickler-p (concat my-org-files-dir "gtd/tickler.org"))
(defvar my:org-projects-pattern "CATEGORY=\"PROJECTS\"+LEVEL=2")

(use-package org
  :bind
  (:map global-map
        ("C-c l" . org-store-link)
        ("C-M-r" . org-capture))

  :init
  (setq org-log-done t)
  (setq org-return-follows-link t)
  (setq org-src-fontify-natively t)
  (setq org-log-into-drawer t)
  (setq org-refile-targets '((gtd-main-p :maxlevel . 3)
                             (gtd-someday-p :level . 1)
                             (gtd-tickler-p :maxlevel . 2)))
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-preserve-indentation nil
        org-edit-src-content-indentation 0)

  :hook
  (org-babel-after-execute . org-redisplay-inline-images)

  :config
  (plist-put org-format-latex-options :scale 1.5)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (shell . t)
     (python . t)
     ;; (ipython . t) this breaks everything if jupyter is not installed
     (lisp . t)
     (clojure . t)
     (gnuplot . t)
     (R . t)
     (plantuml . t)
     (lua . t))))

(use-package org-capture
  :init
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline gtd-inbox-p "Tasks")
                                 "* TODO %i%?\n  %U\n"
                                 :prepend t :empty-lines 1)
                                ("T" "Tickler" entry
                                 (file+headline gtd-tickler-p "Tickler")
                                 "* %i%? \n %U"))))

(use-package org-agenda
  :bind
  (:map global-map
        ("C-c a" . org-agenda))
  :init
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-stuck-projects `(,my:org-projects-pattern ("DOING") nil ""))
  (setq org-agenda-custom-commands
        `(("W" "Weekly Review"
           ((agenda "" ((org-agenda-span 7)))
            (tags "CATEGORY=\"TASKS\"|CATEGORY=\"PROJECTS\"/DONE")
            (tags-todo "CATEGORY=\"INBOX\"")
            (stuck "")
            (todo "DOING")
            (tags "CATEGORY=\"PROJECTS\"+LEVEL=2")
            (tags-todo "CATEGORY=\"SOMEDAY\"")
            (todo "WAITING")))
          ("E" "Export TODOS"
           ((tags-todo "CATEGORY=\"TASKS\""))
           nil
           ("/tmp/org-exported/todos.org"))
          ("g" . "GTD contexts")
          ("gw" "Work" tags-todo "@work")
          ("gh" "Home" tags-todo "@home")
          ("gp" "Pc" tags-todo "@pc")
          ("gi" "Internet" tags-todo "@internet")
          ("ge" "Errands" tags-todo "@errands")
          ("gf" "Freetime" tags-todo "@freetime")))
  :config
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
  (load-my-agenda-files))

(use-package org-tree-slide
  :ensure t
  :bind
  (:map org-tree-slide-mode-map
        ("<right>" . org-tree-slide-move-next-tree)
        ("<left>" . org-tree-slide-move-previous-tree)))

(use-package ox-gfm
  :ensure t)

(use-package toc-org
  :ensure t
  :hook
  (org-mode . toc-org-enable))

;; FIXME: workaround
;; https://github.com/syl20bnr/spacemacs/issues/11798
(when (version<= "9.2" (org-version))
  (require 'org-tempo))

(provide 'orgcfg)
;;; orgcfg.el ends here
