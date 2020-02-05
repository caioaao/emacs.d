;;; cljcfg.el ---                                    -*- lexical-binding: t; -*-

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

;;; Code:

(use-package paredit
  :ensure t)

(use-package clojure-mode
  :ensure t
  :mode ("\\.cljs\\'" . clojure-mode)
  :hook (clojure-mode . paredit-mode)
  :config
  (setq clojure-align-forms-automatically t))


(require 'ob-clojure)

(use-package cider
  :ensure t
  :hook ((cider-repl-mode . paredit-mode)
         (clojure-mode . cider-mode)
         (cider-mode . (lambda () (unbind-key "C-c C-o" cider-mode-map))))
  :bind (:map cider-repl-mode-map
              ("C-c C-l" . cider-repl-clear-buffer))
  :config
  (add-to-list 'cider-test-defining-forms "defflow")
  (setq org-babel-clojure-backend 'cider))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-o")
  (setq cljr-favor-prefix-notation nil)
  (setq cljr-auto-clean-ns t)
  (diminish 'clj-refactor-mode))

(use-package clojure-snippets :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ((clojure-mode . lsp)
   (clojurec-mode . lsp)
   (clojurescript-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :bind
  (:map lsp-command-map
        ("c n" . lsp-clojure-clean-ns))
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (diminish 'lsp-mode)
  :init
  (setq lsp-enable-indentation nil))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; (defun cljcfg:cider-launch-external-repl ()
;;   "Launch a REPL in a separate terminal."
;;   (interactive)
;;   (cl-flet ((nrepl-start-server-process (directory cmd &rest _)
;;                                         (async-shell-command (format "cd %s && urxvt -e zsh -c '%s'" directory cmd))))
;;     (call-interactively 'cider-jack-in)))
;; (define-key clojure-mode-map (kbd "C-c s-j") 'cljcfg:cider-launch-external-repl)

(provide 'cljcfg)
;;; cljcfg.el ends here
