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

(use-package ob-clojure)

;; (use-package lsp-mode
;;   :ensure t
;;   :hook
;;   (clojure-mode . lsp)
;;   (clojurec-mode . lsp)
;;   (clojurescript-mode . lsp)
;;   :config
;;   (require 'lsp-clojure)
;;   (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure"))
;;   (add-to-list 'lsp-language-id-configuration '(clojurec-mode . "clojure"))
;;   (add-to-list 'lsp-language-id-configuration '(clojurescript-mode . "clojurescript")))

(use-package cider :ensure t
  :after (lsp)
  :hook
  (cider-repl-mode . paredit-mode)
  (clojure-mode . cider-mode)
  :bind
  (:map cider-repl-mode-map
        ("C-c C-l" . cider-repl-clear-buffer)
        :map cider-mode-map
        ("C-c C-o" . nil))
  :config
  (add-to-list 'cider-test-defining-forms "defflow")
  (setq org-babel-clojure-backend 'cider)
  (defun my:jump-to-definition ()
    (interactive)
    (if (cider-connected-p)
        (call-interactively #'cider-find-var)
      (call-interactively #'xref-find-definitions)))

  (defun my:cider:unalias (&optional _arg)
    (interactive "P")
    (let* ((sexp-bounds (cider-last-sexp 'bounds))
           (form (format "(ns-unalias *ns* '%s)" (apply #'buffer-substring-no-properties sexp-bounds))))
      (cider-interactive-eval form nil sexp-bounds (cider--nrepl-pr-request-map))))

  (bind-keys :map cider-mode-map
             ("M-." . my:jump-to-definition)
             ("M-," . xref-pop-marker-stack)
             ("C-c C-r u" . my:cider:unalias)))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-o")
  (setq cljr-favor-prefix-notation nil)
  (setq cljr-auto-clean-ns t)
  (diminish 'clj-refactor-mode))

(use-package clojure-snippets :ensure t)

(use-package org
  :init
  (add-to-list 'org-src-lang-modes '("edn" . "clojure")))

;; (defun cljcfg:cider-launch-external-repl ()
;;   "Launch a REPL in a separate terminal."
;;   (interactive)
;;   (cl-flet ((nrepl-start-server-process (directory cmd &rest _)
;;                                         (async-shell-command (format "cd %s && urxvt -e zsh -c '%s'" directory cmd))))
;;     (call-interactively 'cider-jack-in)))
;; (define-key clojure-mode-map (kbd "C-c s-j") 'cljcfg:cider-launch-external-repl)

(provide 'cljcfg)
;;; cljcfg.el ends here
