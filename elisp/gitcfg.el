;;; gitcfg.el --- Config for GIT packages            -*- lexical-binding: t; -*-

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

(setq tramp-ssh-controlmaster-options "") ;; see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20015
(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-git-executable "git")
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m b") 'magit-blame)

;; See https://github.com/magit/magit/issues/2541
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))

(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(add-hook 'git-commit-mode-hook 'markdown-mode)

(provide 'gitcfg)
;;; gitcfg.el ends here
