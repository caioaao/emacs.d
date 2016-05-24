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

(require 'midje-mode)
;; Remap midje-mode (`C-c p` is the prefix cmd to projectile).
(define-key midje-mode-map (kbd "C-c n") nil)
(define-key midje-mode-map (kbd "C-c p") nil)
(define-key midje-mode-map (kbd "C-c m n") 'midje-next-fact)
(define-key midje-mode-map (kbd "C-c m p") 'midje-previous-fact)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (cider-mode 1)
                               (clj-refactor-mode 1)
                               (midje-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-o")))

(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

(require 'clojure-snippets)

(require 'cider)

(add-hook 'cider-repl-mode-hook 'paredit-mode)



;; Clojure imenu support (from https://gist.github.com/luxbock/0f9d6c05c9a8f0002715)
(setq clojure-imenu-generic-expression
      '((nil "^\\s-*(\\(?:s\\|t/\\)?defn-?\\s-+\\(?:\\^[^[:space:]\n]+\\s-+\\)?\\([^[:space:]\n]+\\)" 1)
        ("Variable""^\\s-*(\\(?:s\\|t/\\)?def[[:space:]\n]+\\(?:\\(?:\\^{[^}]+}[[:space:]\n]+\\)\\|\\(?:\\^:[^[:space:]\n]+\\s-+\\)\\)?\\([^[:space:]\n\)]+\\)" 1)
        ("Macro" "^\\s-*(defmacro\\s-+\\([^[:space:]\n]+\\)" 1)
        ("Record" "^\\s-*(\\(?:s/\\)?defrecord\\s-+\\([^[:space:]\n]+\\)" 1)
        ("Type" "^\\s-*(deftype\\+?\\s-+\\([^[:space:]\n]+\\)" 1)
        ("Protocol" "^\\s-*(\\(?:def\\(?:-abstract-type\\|interface\\+?\\|protocol\\)\\)\\s-+\\([^[:space:]\n]+\\)" 1)
        ("Multimethod" "^\\s-*(defmulti\\s-+\\([^[:space:]\n]+\\)" 1)
        ("Multimethod" "^\\s-*(defmethod\\s-+\\([^[:space:]\n]+\\)" 1)))


(eval-after-load "clojure-mode"
  '(progn
     (add-hook
      'clojure-mode-hook
      (lambda ()
        (progn
          (setq imenu-generic-expression clojure-imenu-generic-expression
                imenu-create-index-function 'imenu-default-create-index-function))))))

(provide 'cljcfg)
;;; cljcfg.el ends here