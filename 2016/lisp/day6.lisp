(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun day-6 (lines &optional (comparator #'>))
  (loop with out = nil
        for i below (length (first lines))
        do (loop with occurances = (make-hash-table)
                 for line in lines
                 do (incf (gethash (char line i) occurances 0))
                 finally (push (first (car (sort (alexandria:hash-table-alist occurances)
                                                 (lambda (left right) (funcall comparator (cdr left) (cdr right))))))
                               out))
        finally (return (coerce (nreverse out) 'string))))

(print (time (day-6 (str:lines (str:from-file "../input/day6.txt")))))
(print (time (day-6 (str:lines (str:from-file "../input/day6.txt")) #'<)))
