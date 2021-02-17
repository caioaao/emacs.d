;;; webcfg.el --- Web config                         -*- lexical-binding: t; -*-

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

(use-package eglot :ensure t
  :config
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio"))))

(use-package web-mode
  :after (eglot)
  :ensure t
  :mode (("\\.jsx?\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :bind (:map web-mode-map
              ("C-c C-r" . eglot-code-actions))

  :mode-hydra
  ("Tools"
   (("e" my:flycheck-hydra/body)
    ("r" my:eglot-hydra/body)))

  :hook
  (web-mode .
            (lambda ()
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx")
                (message "now set to: %s" web-mode-content-type))))
  (web-mode . eglot-ensure)
  :config
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-literal-interpolation t))


(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

;; (use-package flow-minor-mode
;;   :ensure t
;;   :config
;;   (diminish 'flow-minor-mode)

;;   :hook
;;   (web-mode .
;;             (lambda ()
;;               (if (or (equal web-mode-content-type "javascript")
;;                       (equal web-mode-content-type "jsx"))
;;                   (flow-minor-enable-automatically))))
;;   (js-jsx-mode . flow-minor-enable-automatically))

;; (use-package company-flow :ensure t)

(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-flow))

(use-package js2-mode
  :ensure t
  :config
  (setq js-indent-level 2))

;; (use-package flycheck-flow :ensure t)

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
;;   (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
;;   (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(use-package lsp-clients
  :after (lsp)
  :defines lsp-clients-typescript-javascript-server-args
  :config
  (add-to-list 'lsp-clients-typescript-javascript-server-args "--jsx"))

(use-package ts-comint
  :ensure t
  :hook
  (web-mode . (lambda ()
                (when (string-match "tsx?\\'" (file-name-extension buffer-file-name))
                  (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
                  (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
                  (local-set-key (kbd "C-c C-k") 'ts-send-buffer)
                  (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
                  (local-set-key (kbd "C-c l") 'ts-load-file-and-go)))))

(provide 'webcfg)
;;; webcfg.el ends here
