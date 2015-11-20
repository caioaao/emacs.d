;;; reactcfg.el --- react development helpers        -*- lexical-binding: t; -*-

;; Copyright (C) 2015

;; Author:  <caio@caio-ntb>
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

;;

;;; Code:


(defun react-open-android-dev-menu ()
  "Remotelly open menu using adb."
  (interactive)
  (start-process
   "open react dev menu" nil
   "adb" "shell" "input" "keyevent" "82"))

(defun react-android-reload-js ()
  "Remotelly reload JS using adb."
  (interactive)
  (start-process
   "open react dev menu" nil
   "adb" "shell" "input" "keyevent" "82" "66" "66"))

(provide 'reactcfg)
;;; reactcfg.el ends here
