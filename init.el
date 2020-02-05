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
        ("melpa" . "http://melpa.milkbox.net/packages/")))

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
(require 'guicfg)
(require 'orgcfg)
(require 'texcfg)
(require 'webcfg)
(require 'cljcfg)
(require 'cpcfg)
(require 'clcfg)
(require 'rustcfg)
(require 'ispellcfg)
(require 'utils)
(require 'pycfg)
(require 'cccfg)
(require 'gitcfg)
(require 'lispcfg)
(require 'javacfg)
(require 'gocfg)


;; ledger-mode (accounting)
(require 'ledger-mode)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(require 'ledger-helper)

;; vendor packages
(use-package flatbuffers-mode
  :mode ("\\.flatc$" . flatbuffers-mode))

(use-package ebnf-mode
  :mode ("\\.ebnf$" . ebnf-mode))


;; plantuml
(require 'plantuml-mode)
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(setq plantuml-jar-path "~/utils/jars/plantuml.jar")
(setq org-plantuml-jar-path plantuml-jar-path)



;; deleting trailing whitespaces
(add-hook 'before-save-hook (lambda ()
                              (set (make-local-variable 'delete-trailing-lines) nil)
                              (delete-trailing-whitespace)))



;; reenabling useful functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)



;; set scratch mode to clojure
(setq initial-major-mode 'clojure-mode)



;; set default browser
(setq browse-url-browser-function 'browse-url-chromium)



(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eclim-eclipse-dirs (quote ("/opt/eclipse")))
 '(eclimd-executable
   "~/.eclipse/org.eclipse.platform_4.14.0_1473617060_linux_gtk_x86_64/eclimd")
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-log-into-drawer t)
 '(package-selected-packages
   (quote
    (ob-clojure use-package lsp-ui company-lsp lsp-mode yaml-mode web-mode typescript-mode tup-mode toml-mode toc-org spacemacs-theme spaceline smartrep slime shut-up restclient rainbow-delimiters racer python-mode pretty-lambdada plantuml-mode ox-gfm org-tree-slide org-present org-mru-clock ob-ipython molokai-theme material-theme magit-popup magit lua-mode ledger-mode jsonnet-mode json-mode helpful helm-projectile helm-ag haskell-mode groovy-mode graphviz-dot-mode gradle-mode golden-ratio gnuplot-mode gnuplot gnu-elpa-keyring-update glsl-mode ggtags flycheck-rust fill-column-indicator fic-mode exec-path-from-shell evil-paredit ess ensime elpy ein eclim doom-themes dockerfile-mode docker-tramp dired-narrow diminish cython-mode company-jedi company-go clojure-snippets clj-refactor avy anzu align-cljlet ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
