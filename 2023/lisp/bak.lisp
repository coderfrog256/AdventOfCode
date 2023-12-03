(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defun part-1 (file)
  (let ((claimed (make-hash-table :test #'equal))
        (cleaned (str:replace-all "." " " file)))
    (loop for line in (str:lines file)
          for y from 0
          do (loop for char across line
                   for x from 0
                   when (and (not (equal char #\.)) (not (digit-char-p char))
                             (setf (gethash (list (1- x) y) claimed) t)
                             (setf (gethash (list (1+ x) y) claimed) t)
                             (setf (gethash (list x (1- y)) claimed) t)
                             (setf (gethash (list x (1+ y)) claimed) t)
                             (setf (gethash (list (1+ x) (1+ y)) claimed) t)
                             (setf (gethash (list (1- x) (1+ y)) claimed) t)
                             (setf (gethash (list (1+ x) (1- y)) claimed) t))
                     do (setf (gethash (list (1- x) (1- y)) claimed) t)))
    (loop with valid = (list)
          for line in (str:lines file)
          for y from 0
          do (loop with working-number = (list)
                   with num-valid = nil
                   for char across line
                   for x from 0
                   do (if (digit-char-p char)
                          (progn
                            (push char working-number)
                            (format t "Space valid: (~a, ~a) ~a (was ~a)~%" x y (gethash (list x y) claimed nil) num-valid)
                            (setf num-valid (or num-valid (gethash (list x y) claimed nil))))
                          (progn
                            (when (and num-valid working-number)
                              (push (parse-integer (coerce (nreverse working-number) 'string)) valid))
                            (setf num-valid nil
                                  working-number nil)))
                   finally (when (and num-valid working-number)
                             (push (parse-integer (coerce (nreverse working-number) 'string)) valid)))
          finally (return (reduce #'+ valid)))))

(print (time (part-1
 (frog:get-advent-of-code-input 2023 3))))
