(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel :alexandria))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defun part-1 (input)
  (let* ((lines (str:lines input))
         (columns (list))
         (operations (cl-ppcre:split "\\s*" (car (last lines)))))
    (loop for line in lines
          until (string= line (car (last lines)))
          do (loop for num in (frog:extract-numbers line)
                   for i from 0
                   for column = (nth i columns)
                   do (push num column)
                   if (= 1 (length column))
                     do (setf columns (nconc columns (list column)))
                   else do (setf (nth i columns) column)))
    (loop with out = 0
          for i from 0 below (length operations)
          for column = (nth i columns)
          for operation = (nth i operations)
          if (string= "+" operation) do
            (incf out (reduce #'+ column))
          if (string= "*" operation) do
            (incf out (reduce #'* column))
          finally (return out))))

(frog:report (part-1 (frog:get-advent-of-code-input 2025 6)))

(defun part-2 (input)
  (let* ((lines (str:lines input))
         (matrix (make-array (list (length (first lines)) (length lines))))
         (parsed nil))
    (loop for y from 0 below (length lines) ; Map strings to a rotated char matrix
          for line = (nth y lines)
          do (loop for x from 0 below (length line)
                   for c = (char line x)
                   do (setf (aref matrix x y) c)))
    (setf parsed (loop for i below (array-dimension matrix 0) ; Map to strings
                       collect (loop for j below (array-dimension matrix 1)
                                     collect (aref matrix i j))))
    (loop with working = (list)
          with operation = nil with num = nil with sum = 0
          for entry in parsed
          if (not (char= (car (last entry)) #\ )) ; If last char set, it's an op
            do (setf operation (car (last entry)))
          do (setf num (str:trim (str:substring 0 -1 (coerce entry 'string))))
          if (string= "" num) ; Empty string is our list delim. Apply the op
            do (incf sum (if (char= #\+ operation)
                             (reduce #'+ working)
                             (reduce #'* working)))
               (setf working (list))
          else do (push (parse-integer num) working)
          finally (incf sum (if (char= #\+ operation) ; Wrap up remaining work
                                (reduce #'+ working)
                                (reduce #'* working)))
                  (return sum))))

(frog:report (part-2 (frog:get-advent-of-code-input 2025 6)))
