;;; package --- Summary
;; This is my personal config file :D

;;; Commentary:

;;; Code:

;; Some vars

(defvar bin-folder "~/.emacs.d/bin")
(defvar elisp-folder "~/.emacs.d/elisp")

(defvar local-root-folder "~/.emacs.d/local")
(defvar local-bin-folder (concat local-root-folder "/bin"))
(defvar local-elisp-folder (concat local-root-folder "/elisp"))



;; Creating folders
(when (not (file-exists-p bin-folder))
  (make-directory bin-folder))

(when (not (file-exists-p local-root-folder))
  (make-directory local-root-folder))
(when (not (file-exists-p local-elisp-folder))
  (make-directory local-elisp-folder))
(when (not (file-exists-p local-bin-folder))
  (make-directory local-bin-folder))



;; Adding package paths
(add-to-list 'load-path elisp-folder)
(add-to-list 'load-path local-root-folder)
(add-to-list 'load-path local-elisp-folder)



;; Getting local config file
(when (file-exists-p (concat local-root-folder "/local-init.el"))
  (require 'local-init))



;; Init packages before
(setq package-enable-at-startup nil)
(package-initialize)



;; windows utility functions (required for setting PATH)
(require 'win32-utils)



;; Fixing autosave/backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))



;; Adding a bin folders
(setq exec-path
      (append '(local-bin-folder
                bin-folder) exec-path))

(when (boundp 'using-win32)
  (setenv "PATH" (concat
                  (w32utils-convert-to-std-path bin-folder) ";"
                  (w32utils-convert-to-std-path local-bin-folder) ";"
                  (getenv "PATH"))))
                                                              



;; Indent using spaces only
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq-default c-basic-offset 4)



;; markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))



;; C/C++ mode
(require 'cc-mode)



;; For pt-Br dead keys to work
(set-input-mode nil nil 1)
(require 'iso-transl)



;; Set encoding
(setenv "LC_CTYPE" "UTF-8")
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)



;; package managers
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))



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



;; auto complete mode
;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; set the trigger key so that it can work together with yasnippet on
;; tab key, if the word exists in yasnippet, pressing tab will cause
;; yasnippet to activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")



;; flycheck
(require 'flymake)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)



;; org mode
(require 'orgcfg)



;; window resizing shortcuts
(require 'smartrep)
(smartrep-define-key
    global-map "C-c w r" '(("<left>" . 'enlarge-window-horizontally)
			   ("<right>" . 'shrink-window-horizontally)
			   ("<up>" . 'shrink-window)
			   ("<down>" . 'enlarge-window)))



;; latex
; force latex to use pdflatex
(set-variable (quote latex-run-command) "pdflatex")
(set-variable (quote tex-dvi-view-command) "evince")

; add latex mode to auto-complete
(add-to-list 'ac-modes 'latex-mode)



;; Auto-insert mode
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)



;; Python config
(require 'pycfg)



;; web-mode (html, css, javascript)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(require 'ac-html)
(add-to-list 'ac-modes 'web-mode)



;; C/C++ config
;; cc-mode
(require 'cc-mode)

;; sets extended mode curly braces as default
(setq c-default-style "linux"
      c-basic-offset 4)
;; C++11 as standard
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-gcc-language-standard "c++11")))
;; auto complete
(require 'auto-complete-clang)
(define-auto-insert "sol\.cpp$" "competitive-template.cpp")



;; ledger-mode (accounting)
(require 'ledger-mode)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(require 'ledger-helper)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
