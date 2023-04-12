(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defparameter *directions* (list (list 0 1) (list 1 0) (list 0 -1) (list -1 0)))
(defun spin (current direction)
  (let ((index (position current *directions* :test #'equal))
        (offset (if (str:starts-with-p "R" direction) 1 -1)))
    (nth (mod (+ index offset) (length *directions*)) *directions*)))

(defun walk (input)
  (loop with position = (list 0 0)
        with direction = (list 0 1)
        for command in (str:split ", " input)
        for amount = (parse-integer (str:substring 1 (length command) command))
        do (setf direction (spin direction command))
           (incf (first position) (* (first direction) amount))
           (incf (second position) (* (second direction) amount))
        finally (return (+ (abs (first position)) (abs (second position))))))

(print (walk (str:from-file "../input/day1.txt")))

;; part 2
(defun walk2 (input)
  (loop with position = (list 0 0)
        with direction = (list 0 1)
        with visited = (list (list 0 0))
        for command in (str:split ", " input)
        for amount = (parse-integer (str:substring 1 (length command) command))
        do (setf direction (spin direction command))
           (loop for i from 1 to amount
                 do (incf (first position) (first direction))
                    (incf (second position) (second direction))
                    (if (member position visited :test #'equal)
                        (return-from walk2 (+ (abs (first position)) (abs (second position))))
                        (push (copy-list position) visited)))))

(print (walk2 (str:from-file "../input/day1.txt")))
