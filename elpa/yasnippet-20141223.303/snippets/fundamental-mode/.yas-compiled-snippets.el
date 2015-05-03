;;; Compiled snippets and support files for `fundamental-mode'
;;; .yas-setup.el support file if any:
;;;
(defun ca-all-asscs (asslist query)
  "returns a list of all corresponding values (like rassoc)"
  (cond
   ((null asslist) nil)
   (t
    (if (equal (cdr (car asslist)) query)
        (cons (car (car asslist)) (ca-all-asscs (cdr asslist) query))
      (ca-all-asscs (cdr asslist) query)))))
;;; Do not edit! File generated at Sat Apr 18 21:46:45 2015
