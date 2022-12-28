(ql:quickload '(:str :cl-ppcre :binding-arrows :series :alexandria))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defclass knot ()
  ((x :initarg :start-x :initform 0 :accessor knot-x)
   (y :initarg :start-y :initform 0 :accessor knot-y)
   (next-knot :initform nil :accessor knot-next)
   (visited :accessor knot-visited)))

(defmethod initialize-instance :after ((k knot) &rest args)
  (setf (knot-visited k) (list (list (knot-x k) (knot-y k)))))

(defun move-knot (direction knot)
  (let ((new-x (knot-x knot))
        (new-y (knot-y knot))
        (next (knot-next knot)))
    (setf new-y (cond
                  ((str:starts-with-p "U" direction) (incf (knot-y knot)))
                  ((str:starts-with-p "D" direction) (decf (knot-y knot)))
                  (t new-y)))
    (setf new-x (cond
                  ((str:ends-with-p "L" direction) (decf (knot-x knot)))
                  ((str:ends-with-p "R" direction) (incf (knot-x knot)))
                  (t new-x)))
    (push (list new-x new-y) (knot-visited knot))
    (when next
      (let* ((next-x (knot-x next))
             (next-y (knot-y next))
             (x-diff (abs (- next-x new-x)))
             (y-diff (abs (- next-y new-y))))
        (when (and (< x-diff 2) (< y-diff 2)) (return-from move-knot knot))
        (->
          (cond ((> new-y next-y) "U")
                ((= new-y next-y) "")
                (t "D"))
          (str:concat (cond ((> new-x next-x) "R")
                            ((= new-x next-x) "")
                            (t "L")))
          (move-knot next))))))

(defun run-commands (lines num-knots)
  (let* ((head (make-instance 'knot :start-x 0 :start-y 0))
         (tail head))
    (dotimes (n num-knots)
      (setf (knot-next tail) (make-instance 'knot :start-x 0 :start-y 0))
      (setf tail (knot-next tail)))
    (loop for line in lines
          do (register-groups-bind (direction amount) ("(\\w) (\\d+)" line)
               (dotimes (n (parse-integer amount))
                 (move-knot direction head)))
          finally (return tail))))

;; Part 1
(time (print (-> "../input/day9.txt"
               (str:from-file)
               (str:lines)
               (run-commands 1)
               (knot-visited)
               (remove-duplicates :test #'equal)
               (length))))

;; Part 2
(time (print (-> "../input/day9.txt"
               (str:from-file)
               (str:lines)
               (run-commands 9)
               (knot-visited)
               (remove-duplicates :test #'equal)
               (length))))
