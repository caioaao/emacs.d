;;; utils.el --- utils...                            -*- lexical-binding: t; -*-

;; Copyright (C) 2017

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

(defun avg-calc-ask (n)
  (/ (reduce #'+
             (loop for i in (number-sequence 1 n)
                   collect (read-number (format "Measurement #%d: " i))))
     (float n)))

(defun avg-org-table-insert (n)
  (interactive "nNumber of measurements: ")
  (insert (format "%.4f" (avg-calc-ask n)))
  (org-table-next-field))

(provide 'utils)
;;; utils.el ends here
