;;; cpcfg.el --- Functions to aid in programming competitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Caio Augusto Araujo Oliveira

;; Author: Caio Augusto Araujo Oliveira
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

(use-package cc-mode
  :ensure t)

(use-package lisp-mode)

(defvar last-gcj-input-file "in.txt")

(defun do-gcj (input-file)
  "Compile GCJ solution and generate output file from INPUT-FILE.
The output file will have the same name as the input file, but with a `.out' extension."
  (interactive "fInput file: ")
  (setq last-gcj-input-file input-file)
  (let ((output-file (concat input-file ".out")))
    (compile (format "make sol && ./sol < %s > %s && head -n100 %s" input-file output-file output-file))))

(defun redo-gcj ()
  "Redo `do-gcj' with last input file provided."
  (interactive)
  (do-gcj last-gcj-input-file))

(defun do-or-redo-gcj (arg)
  "To accept prefix argument (stored as ARG)."
  (interactive "P")
  (if (and arg (listp arg))
      (call-interactively 'do-gcj)
    (redo-gcj)))

(defun sbcl-eval-sol% (sol-name)
  (interactive))

(defvar *cp-last-testcase-name* "sample")

(defun sbcl-compilation-cmd (src-fname test-name)
  (concatenate 'string "time sbcl --noinform --load "
               src-fname " --eval \"(progn (main) (quit))\" < "
               test-name ".in | tee " test-name ".out && diff "
               test-name ".out "
               test-name ".sol"))

(defun sbcl-eval-sol (arg)
  (interactive "P")
  (print arg)
  (let ((fname (buffer-file-name))
        (test-name (if (and arg (listp arg))
                       (read-string "Enter testcase name: ")
                     *cp-last-testcase-name*)))
    (compile (sbcl-compilation-cmd fname test-name))
    (setq *cp-last-testcase-name* test-name)))

(eval-after-load 'cc-mode
  '(define-key c++-mode-map (kbd "C-c g") 'do-or-redo-gcj))

(eval-after-load 'lisp-mode
  '(define-key lisp-mode-map (kbd "C-c g") 'sbcl-eval-sol))

(provide 'cpcfg)
;;; cpcfg.el ends here
