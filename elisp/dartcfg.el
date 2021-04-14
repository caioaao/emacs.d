;;; dartcfg.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Caio Oliveira

;; Author: Caio Oliveira
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package dart-mode :ensure t
  :init
  (add-hook 'before-save-hook 'lsp-format-buffer nil :LOCAL))

(use-package flutter
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload)))

(use-package lsp-mode :ensure t)
(use-package lsp-dart
  :hook (dart-mode . lsp)
  :defines lsp-dart-sdk-dir
  :config
  (setq lsp-dart-sdk-dir "/opt/flutter/bin/cache/dart-sdk/"))

(provide 'dartcfg)
;;; dartcfg.el ends here
