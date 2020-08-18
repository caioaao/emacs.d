;;; package --- Summary
;; This is my personal config file :D

;;; Commentary:

;;; Code:

;; REALLY important macro
(defmacro comment (&rest body)
  "Ignore BODY."
  nil)

;; package managers
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Init packages before
(setq package-enable-at-startup nil)
(package-initialize)

(when (not (require 'use-package nil 'noerror))
  (message "Package 'use-package' not found. Updating package list and installing it.")
  (package-refresh-contents)
  (package-install 'use-package))

;; Some vars
(defvar bin-folder "~/.emacs.d/bin")
(defvar elisp-folder "~/.emacs.d/elisp")

(defvar local-root-folder "~/.emacs.d/local")
(defvar local-bin-folder (concat local-root-folder "/bin"))
(defvar local-elisp-folder (concat local-root-folder "/elisp"))

;; Creating folders
(when (not (file-exists-p bin-folder))
  (make-directory bin-folder t))

(when (not (file-exists-p local-root-folder))
  (make-directory local-root-folder t))
(when (not (file-exists-p local-elisp-folder))
  (make-directory local-elisp-folder t))
(when (not (file-exists-p local-bin-folder))
  (make-directory local-bin-folder t))

;;; Initial config

;; Adding package paths
(add-to-list 'load-path elisp-folder)
(add-to-list 'load-path local-elisp-folder)
(add-to-list 'load-path (concat elisp-folder "/vendor"))
(dolist (filepath (directory-files (expand-file-name "~/.emacs.d/vendor")
                                   t "^[a-zA-Z0-9]" nil))
        (add-to-list 'load-path filepath))

(require 'prelude)

;; local config file
(when (file-exists-p (expand-file-name "local-init.el" local-elisp-folder))
  (require 'local-init))

;; Fixing autosave/backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Binary paths
(add-to-list 'exec-path bin-folder)
(add-to-list 'exec-path local-bin-folder)

(require 'win32-utils)
(require 'editcfg)

;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode))

(require 'gpgcfg)
(require 'lspcfg)
(require 'guicfg)
(require 'orgcfg)
(require 'texcfg)
(require 'webcfg)
(require 'cljcfg)
(require 'cpcfg)
(require 'clcfg)
(require 'ocamlcfg)
(require 'rustcfg)
(require 'ispellcfg)
(require 'utils)
(require 'pycfg)
(require 'cccfg)
(require 'gitcfg)
(require 'lispcfg)
(require 'javacfg)
(require 'gocfg)
(require 'sqlcfg)
(require 'dartcfg)
(require 'inocfg)
(require 'cmakecfg)
(require 'dotcfg)

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto$" . protobuf-mode))

(use-package terraform-mode :ensure t)

;; ledger-mode (accounting)
(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger$" . ledger-mode))

(use-package ledger-helper)

;; vendor packages
(use-package flatbuffers-mode
  :mode ("\\.flatc$" . flatbuffers-mode))

(use-package ebnf-mode
  :mode ("\\.ebnf$" . ebnf-mode))


;; plantuml
(use-package plantuml-mode
  :ensure t
  :mode ("\\.puml\\'" . plantuml-mode)
  :config
  (setq plantuml-jar-path "~/utils/jars/plantuml.jar"))

(use-package ob-plantuml
  :config
  (setq org-plantuml-jar-path "~/utils/jars/plantuml.jar"))

(use-package yaml-mode :ensure t)

;; set scratch mode to clojure
(setq initial-major-mode 'clojure-mode)

;; set default browser
(setq browse-url-browser-function 'browse-url-chromium)

(provide 'init)
