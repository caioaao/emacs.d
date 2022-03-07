(defvar user/config-org-file (concat user-emacs-directory "config.org"))
(defvar user/config-el-file  (concat user-emacs-directory "config.el"))

(when (file-newer-than-file-p user/config-org-file user/config-el-file)
  (require 'org)
  (org-babel-tangle-file user/config-org-file)
  ;; TODO byte-compile is broken right now
  ;; (byte-compile-file user/config-el-file)
  )

(load-file user/config-el-file)
