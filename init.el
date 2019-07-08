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



;; Some vars
(defvar bin-folder "~/.emacs.d/bin")
(defvar elisp-folder "~/.emacs.d/elisp")

(defvar local-root-folder "~/.emacs.d/local")
(defvar local-bin-folder (concat local-root-folder "/bin"))
(defvar local-elisp-folder (concat local-root-folder "/elisp"))



;; Locale
;; Emacs server not getting this from env?
;; (when (not (getenv "LC_ALL"))
;;   (setenv "LC_ALL" "en_US.UTF-8"))

;; Creating folders
(when (not (file-exists-p bin-folder))
  (make-directory bin-folder t))

(when (not (file-exists-p local-root-folder))
  (make-directory local-root-folder t))
(when (not (file-exists-p local-elisp-folder))
  (make-directory local-elisp-folder t))
(when (not (file-exists-p local-bin-folder))
  (make-directory local-bin-folder t))



;; Initial config
(defvar flag-file-path "~/.emacs.d/.installed")

(defvar main-packages
  '(fill-column-indicator
    golden-ratio
    spaceline
    yasnippet
    flycheck
    company
    smartrep
    ledger-mode
    magit
    paredit
    ggtags
    material-theme
    helm
    pkg-info
    pretty-lambdada
    web-mode
    molokai-theme
    ag
    helm-ag
    projectile
    helm-projectile
    tup-mode
    glsl-mode
    clojure-mode
    clj-refactor
    clojure-snippets
    align-cljlet
    rainbow-delimiters
    exec-path-from-shell
    undo-tree
    dockerfile-mode
    yaml-mode
    haskell-mode
    fic-mode
    python-mode
    company-jedi
    pyvenv
    ob-ipython
    elpy
    ein
    json-mode
    restclient
    anzu
    diminish
    gnuplot
    gnuplot-mode
    ess
    slime
    graphviz-dot-mode
    avy
    plantuml-mode
    org-present
    cython-mode
    org-tree-slide
    rust-mode
    racer
    flycheck-rust
    toml-mode
    ensime
    evil-paredit
    docker-tramp
    ox-gfm
    spacemacs-theme
    toc-org
    typescript-mode
    dired-narrow
    org-mru-clock
    helpful
    jsonnet-mode
    terraform-mode
    lua-mode))


(when (not (file-exists-p flag-file-path))
  (message
   "This emacs config folder is not installed. Installing needed packages.")
  (package-refresh-contents)
  (dolist (package main-packages)
    (package-install package))
  (write-region "" nil flag-file-path)
  (save-buffers-kill-terminal))



;; Adding package paths
(add-to-list 'load-path elisp-folder)
(add-to-list 'load-path local-elisp-folder)



;; some (potentially) global dependencies
(require 'smartrep)



;; important env vars
(defvar default-external-term "urxvt")



;; local config file
(when (file-exists-p (expand-file-name "local-init.el" local-elisp-folder))
  (require 'local-init))



;; windows utility functions (required for setting PATH)
(require 'win32-utils)

;; setting path for command line processes in win32
(when (or (eq 'windows-nt system-type) (eq 'ms-dos system-type))
  (setenv "PATH" (concat
                  (w32utils-convert-to-std-path local-bin-folder) ";"
                  (w32utils-convert-to-std-path bin-folder) ";"
                  (getenv "PATH"))))



;; Fixing autosave/backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))



;; Adding all bin folders
(exec-path-from-shell-initialize)

(add-to-list 'exec-path bin-folder)
(add-to-list 'exec-path local-bin-folder)



;; copying GPG stuff (needed in Ubuntu for some reason)
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")



;; Indent using spaces only
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)



;; markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))



;; For pt-Br dead keys to work
(set-input-mode nil nil 1)
(require 'iso-transl)



;; Set encoding
(setenv "LC_CTYPE" "UTF-8")
(prefer-coding-system 'utf-8-unix)



(require 'guicfg)


;; yes or no becomes y or n
(defalias 'yes-or-no-p 'y-or-n-p)



;; yasnippet
; (should be loaded before auto complete so that they can work
; together)
(require 'yasnippet)
(yas-global-mode 1)

;; Fix yasnippet 0.8/ac bug
(defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
(defalias 'yas/table-hash 'yas--table-hash)

(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets")))



;; flycheck
(require 'flymake)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)



;; org mode
(require 'orgcfg)



;; latex
; force latex to use pdflatex
(set-variable (quote latex-run-command) "pdflatex")
(set-variable (quote tex-dvi-view-command) "evince")

; add latex mode to auto-complete
;; (add-to-list 'ac-modes 'latex-mode)



;; Auto-insert mode
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)



;; Python config
(require 'pycfg)



;; web-mode (html, css, javascript)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))



;; C/C++ config
(require 'cccfg)



;; ledger-mode (accounting)
(require 'ledger-mode)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(require 'ledger-helper)



;; git stuff
(require 'gitcfg)



;; paredit
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)



;; edit config
(require 'editcfg)



;; pretty symbols
(require 'prettycfg)



;; project related
(require 'prjcfg)



;; web config
(require 'webcfg)



;; react cfg
(require 'reactcfg)



;; clojure cfg
(require 'cljcfg)



;; Competitive programming cfg
(require 'cpcfg)



;; Common Lisp cfg
(require 'clcfg)



;; Rust cfg
(require 'rustcfg)



;; ispell
(require 'ispellcfg)



;; utils
(require 'utils)



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
