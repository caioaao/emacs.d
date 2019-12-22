;;; win32-utils.el --- Some util functions for win32 environment  -*- lexical-binding: t; -*-

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

;;; Code:

(defun w32utils-convert-to-std-path (oldpath)
  "Convert OLDPATH to win32 standard path, when in win32 environment."
  (convert-standard-filename (expand-file-name oldpath)))

(defun w32utils-find-original-path (drive)
  "Find original path for drive, if it belongs to subst.  DRIVE format: 'X:'."
  (let ((path nil))
    (with-temp-buffer
      (call-process "subst" nil t)
      (goto-char (point-min))
      (when (re-search-forward (format "^%s\\\\: => \\(.*\\)$" drive) nil t)
        (setq path (match-string 1))))
    (when (not path)
      (setq path drive))
    path))

(defun w32utils-expand-subst (path)
  "Expand PATH to original, if contained in SUBST."
  (when path
    (if (string-match "^.:" path)
        (let ((original-drive-path
               (w32utils-find-original-path (match-string 0 path))))
          (string-match "^.:" path)
          (replace-match original-drive-path nil t path))
      path)))


;; using old defadvice to be compatible with projectile
(defadvice find-file (around w32-find-file)
    "Expand filename and dir before calling 'find-file'."
    (ad-set-arg 0 (w32utils-expand-subst (ad-get-arg 0))) ;; expand filename
    ad-do-it)

(defadvice compilation-find-file (around w32-compilation-find-file)
    "Expand filename and dir before calling 'compilation-find-file'."
    (ad-set-arg 1 (w32utils-expand-subst (ad-get-arg 1))) ;; expand filename
    (ad-set-arg 2 (w32utils-expand-subst (ad-get-arg 2))) ;; expand directory
    ad-do-it)


(defvar w32utils-mode-keymap (make-sparse-keymap) "Keymap for pw-mode.")

(define-minor-mode global-w32utils-mode "Minor mode for Windows"
  :lighter " w32utils" :group 'w32utils
  :keymap w32utils-mode-keymap
  :global t)

(when (or
       (eq system-type 'ms-dos)
       (eq system-type 'windows-nt)
       (eq system-type 'cygwin))
  (ad-activate 'compilation-find-file)
  (ad-activate 'find-file)
  (global-w32utils-mode t))

(provide 'win32-utils)
;;; win32-utils.el ends here
