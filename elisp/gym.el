;;; gym.el ---                                       -*- lexical-binding: t; -*-

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

(defvar gym-bodyparts
  '(biceps-r
    forearm-r
    thigh-r
    calf-r
    biceps-l
    forearm-l
    thigh-l
    calf-l
    chest
    belly))

(defvar gym-num-measurements 3)

(defun gym-ask-one (bodypart n)
  "Ask BODYPART measure number N."
  (read-number (format "Enter %S size (measurement %d): " bodypart n)))

(defun gym-ask-for (bodypart)
  "Ask for BODYPART measures and return mean."
  (/ (reduce #'+
             (mapcar (lambda (i) (gym-ask-one bodypart i))
                     ))
     gym-num-measurements))

(defun gym-ask-all* (prev next res)
  "Recursion.  Allows for going back in time."
  (if next
      (let ((curr-v (gym-ask-one (caar next) (cadar next))))
        (if (< curr-v 0)
            (gym-ask-all* (cdr prev)
                          (cons (car prev) next)
                          (cdr res))
          (gym-ask-all* (cons (car next) prev)
                        (cdr next)
                        (cons (list (caar next) curr-v) res))))
    res))

(defun gym-questions-for-bodypart (bodypart)
  (mapcar (lambda (i) (list bodypart i))
          (number-sequence 0 (- gym-num-measurements 1))))

(defun gym-aggregate-results (answers)
  (loop for bodypart in gym-bodyparts
        collect (reduce #'+ answers
                        :key (lambda (tup)
                               (or (and (equal (car tup) bodypart) (cadr tup))
                                   0)))))

(defun gym-ask-all ()
  "Ask all and return list of measures."
  (let* ((questions (reduce #'append gym-bodyparts :key #'gym-questions-for-bodypart))
         (answers (gym-ask-all* nil questions nil)))
    (gym-aggregate-results answers)))


(defun gym-register-day ()
  "Insert day measurements in current file."
  (interactive)
  (org-table-insert-row t)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))
  (loop for res in (gym-ask-all)
        do
        (org-table-next-field)
        (insert (format "%.3f" res))))

(provide 'gym)
;;; gym.el ends here
