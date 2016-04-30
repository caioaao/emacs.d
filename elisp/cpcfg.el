;;; cpcfg.el --- Functions to aid in programming competitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Caio Augusto Araujo Oliveira

;; Author: Caio Augusto Araujo Oliveira <caiooliveira@Caios-MacBook-Pro-2.local>
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

(defvar last-gcj-input-file "in.txt")

(defun do-gcj (input-file)
  "Compile GCJ solution and generate output file from INPUT-FILE.
The output file will have the same name as the input file, but with a `.out' extension."
  (interactive "fInput file: ")
  (setq last-gcj-input-file input-file)
  (let ((output-file (concat input-file ".sol")))
    (compile (format "make sol && ./sol < %s > %s && head -n100 %s" input-file output-file output-file))))

(defun redo-gcj ()
  "Redo `do-gcj' with last input file provided."
  (interactive)
  (do-gcj last-gcj-input-file))

(defun do-or-redo-gcj (arg)
  "To accept prefix argument (stored as ARG)."
  (interactive "P")
  (print (listp arg))
  (if (and arg (listp arg))
      (call-interactively 'do-gcj)
    (redo-gcj)))


(defun set-competitive-cc-keys ()
  "Set competitive cc keys."
  (define-key c++-mode-map (kbd "C-c C-c") 'do-or-redo-gcj))

(eval-after-load 'cc-mode
  '(set-competitive-cc-keys))
(provide 'cpcfg)
;;; cpcfg.el ends here
