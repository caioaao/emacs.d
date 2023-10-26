;;; Bootstrapping
;; setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
	(expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
	"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; setup use-package
(straight-use-package 'use-package)

;; make sure environment variables are here
(straight-use-package 'exec-path-from-shell)
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

;; setup org early to avoid version mismatch errors (https://www.reddit.com/r/emacs/comments/qcj33a/problem_and_workaround_with_orgmode_function/)
(straight-use-package 'org)

;;; Tangle and auto load literate config
(defvar user/config-org-file (concat user-emacs-directory "config.org"))
(defvar user/config-el-file  (concat user-emacs-directory "config.el"))

(defun user/reload-config ()
  (interactive)
  (when (file-exists-p user/config-el-file)
      (load-file user/config-el-file))
  (when (file-newer-than-file-p user/config-org-file user/config-el-file)
    (require 'org)
    (org-babel-tangle-file user/config-org-file user/config-el-file)
    (byte-compile-file user/config-el-file)
    (load-file user/config-el-file)))

(user/reload-config)
