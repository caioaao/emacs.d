;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file replaces itself with the actual configuration at first run. Taken from:
;; https://tammymakesthings.com/2020/04/2020.04.28_literate-emacs-configuration-in-org-mode.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We can't tangle without org!
(require 'org)

(defvar user/init-org-file (concat user-emacs-directory "init.org"))
(defvar user/init-el-file  (concat user-emacs-directory "init.el"))

(find-file user/init-org-file)
(org-babel-tangle)
(load-file user/init-el-file)
(byte-compile-file user/init-el-file)
