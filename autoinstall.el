;;; autoinstall.el --- Automatic installation of emacs packages  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'prelude)

(defcustom autoinstall:required-packages '()
  "Packages required to be installed.")

(defun autoinstall:load-installed (flag-file-path)
  "Return installed packages, as listed in FLAG-FILE-PATH."
  (when (file-exists-p flag-file-path)
    (with-temp-buffer
      (insert-file-contents flag-file-path)
      (read (current-buffer)))))

(defun autoinstall:missing-packages--sorted (required-packages installed-packages)
  "Same as missing-packages, but assumes REQUIRED-PACKAGES and INSTALLED-PACKAGES are sorted."
  (cond
   ((not required-packages)
    '())
   ((not installed-packages)
    required-packages)
   ((eq (car required-packages) (car installed-packages))
    (autoinstall:missing-packages--sorted (cdr required-packages)
                                          (cdr installed-packages)))
   ((symbol-name< (car required-packages) (car installed-packages))
    (cons (car required-packages)
          (autoinstall:missing-packages--sorted (cdr required-packages)
                                                installed-packages)))
   (t (autoinstall:missing-packages--sorted required-packages
                                            (cdr installed-packages)))))

(defun autoinstall:missing-packages (required-packages installed-packages)
  "Receive a list of REQUIRED-PACKAGES and INSTALLED-PACKAGES and return the missing installed packages."
  (autoinstall:missing-packages--sorted (sorted required-packages 'symbol-name<)
                                        (sorted installed-packages 'symbol-name<)))

(defun autoinstall:install-missing (flag-file-path)
  "Install missing packages.  State of the current install is retrieved from FLAG-FILE-PATH."
  (let ((missing-packages (autoinstall:missing-packages
                           autoinstall:required-packages
                           (autoinstall:load-installed flag-file-path))))
    (when missing-packages
      (message "Missing packages found. Installing needed packages.")
      (package-refresh-contents)
      (dolist (package missing-packages)
        (package-install package))
      (write-region (prin1-to-string autoinstall:required-packages) nil flag-file-path nil))))

(provide 'autoinstall)
;;; autoinstall.el ends here
