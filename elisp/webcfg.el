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

(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :hook
  (web-mode .
            (lambda ()
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx")
                (message "now set to: %s" web-mode-content-type))))
  :config
  (progn
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)))


(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package diminish :ensure t)

(use-package flow-minor-mode
  :ensure t
  :config
  (diminish 'flow-minor-mode)

  :hook
  (web-mode .
            (lambda ()
              (if (or (equal web-mode-content-type "javascript")
                      (equal web-mode-content-type "jsx"))
                  (flow-minor-enable-automatically)))))

(provide 'webcfg)
;;; webcfg.el ends here
