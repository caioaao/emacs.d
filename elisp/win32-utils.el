;;; win32-utils.el --- Some util functions for win32 environment  -*- lexical-binding: t; -*-

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

(defun w32utils-convert-to-std-path (oldpath)
  "Convert OLDPATH to win32 standard path, when in win32 environment."
  (convert-standard-filename (expand-file-name oldpath)))

(provide 'win32-utils)
;;; win32-utils.el ends here
