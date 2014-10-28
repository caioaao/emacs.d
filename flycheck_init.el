;;; package --- Summary
;; This is my personal config file :D

;;; Commentary:

;;; Code:
;; Init packages before
(setq package-enable-at-startup nil)
(package-initialize)


;; :D
(require 'cc-mode)

;; Adding package paths
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; For pt-Br dead keys to work
(require 'iso-transl)
;; Set encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; package managers
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; theme
(load-theme 'wombat t)

;; Remove toolbar
(tool-bar-mode -1)

;; Auto linum-mode
(global-linum-mode 1)

;; No initial screen
(setq-default inhibit-startup-screen t)

;; Clear scratch buffer
(setq-default initial-scratch-message nil)

;; yes or no becomes y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; yasnippet
;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)

;; Fix yasnippet 0.8/ac bug
(defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
(defalias 'yas/table-hash 'yas--table-hash)

;; auto complete mode
;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; set the trigger key so that it can work together with yasnippet on tab key,
;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; auto-complete-clang
(require 'auto-complete-clang)
;; (define-key c-mode-base-map [(control tab)] 'ac-complete-clang) -> DEFEITO

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; flycheck-pyflakes
(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)

;; Custom quick c++ compile
(defun cpp-compile-and-run ()
  "Quick compile & run command for single cpp files."
  (interactive)
  
  (let ((f (file-name-base
	    (buffer-file-name
	     (window-buffer (minibuffer-selected-window))
	     ))))
    
    (compile (format "touch %s.in && make %s && ./%s < %s.in" f f f f)))
)
(define-key c-mode-base-map [f9] 'cpp-compile-and-run)

;; arduino mode (needs debug)
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/.emacs.d/orgfiles/"
			"~/.emacs.d/orgfiles/local/"))


;; window resizing shortcuts
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(provide 'init)
;;; init.el ends here
