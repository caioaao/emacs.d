;;; cljcfg.el ---                                    -*- lexical-binding: t; -*-

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

;;; Code:

(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (cider-mode 1)
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-o")
                               (setq clojure-align-forms-automatically t)))
(setq cljr-favor-prefix-notation nil)
(setq cljr-auto-clean-ns t)

(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

(require 'clojure-snippets)

(require 'cider)
(setq cider-repl-pop-to-buffer-on-connect nil)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(define-key cider-repl-mode-map (kbd "C-c C-l") 'cider-repl-clear-buffer)

;; Clojure imenu support (from https://gist.github.com/luxbock/0f9d6c05c9a8f0002715)
(defconst clojure-imenu-generic-expression
  '((nil "^\\s-*(\\(?:[st]/\\)?defn-?\\s-+\\(?:\\^[^[:space:]\n]+\\s-+\\)?\\([^[:space:]\n]+\\)" 1)
    ("Variable""^\\s-*(\\(?:s\\|t/\\)?def[[:space:]\n]+\\(?:\\(?:\\^{[^}]+}[[:space:]\n]+\\)\\|\\(?:\\^:[^[:space:]\n]+\\s-+\\)\\)?\\([^[:space:]\n\)]+\\)" 1)
    ("Macro" "^\\s-*(defmacro\\s-+\\([^[:space:]\n]+\\)" 1)
    ("Record" "^\\s-*(\\(?:s/\\)?defrecord\\s-+\\([^[:space:]\n]+\\)" 1)
    ("Type" "^\\s-*(deftype\\+?\\s-+\\([^[:space:]\n]+\\)" 1)
    ("Protocol" "^\\s-*(\\(?:def\\(?:-abstract-type\\|interface\\+?\\|protocol\\)\\)\\s-+\\([^[:space:]\n]+\\)" 1)
    ("Multimethod" "^\\s-*(defmulti\\s-+\\([^[:space:]\n]+\\)" 1)
    ("Multimethod" "^\\s-*(defmethod\\s-+\\([^[:space:]\n]+\\)" 1)))

(defun cljcfg:cider-launch-external-repl ()
  "Launch a REPL in a separate terminal."
  (interactive)
  (cl-flet ((nrepl-start-server-process (directory cmd &rest _)
                                        (async-shell-command (format "cd %s && urxvt -e zsh -c '%s'" directory cmd))))
    (call-interactively 'cider-jack-in)))

(define-key clojure-mode-map (kbd "C-c s-j") 'cljcfg:cider-launch-external-repl)

(defun after-clj-mode ()
  "Add clojure support to imenu."
  (add-hook
     'clojure-mode-hook
     (lambda ()
       (progn
         (setq imenu-generic-expression clojure-imenu-generic-expression
               imenu-create-index-function 'imenu-default-create-index-function)))))

(eval-after-load "clojure-mode"
  #'after-clj-mode)


(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)


(provide 'cljcfg)
;;; cljcfg.el ends here
