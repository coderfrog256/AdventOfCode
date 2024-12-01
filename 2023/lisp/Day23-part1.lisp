(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(defclass* walker () ((y 0) (x 1) (visited (fset:set (list 0 1)))))

(defun parse-map (file)
  (loop with out = (serapeum:dict)
        with lines = (str:lines file)
        for line in lines
        for y from 0
        maximize (loop for x from 0
                 for c across line
                       do (setf (gethash (list y x) out) c)
                       finally (return x)) into max-x
        finally (return (list out (list y (- max-x 2))))))

(defun walk (walker map)
  (with-slots (y x visited) walker
    (setf visited (fset:with visited (list y x)))
    (case (gethash (list y x) map)
      (#\> (unless (fset:contains? visited (list y (1+ x)))
             (incf x)
             (list walker)))
      (#\< (unless (fset:contains? visited (list y (1- x)))
             (decf x)
             (list walker)))
      (#\^ (unless (fset:contains? visited (list (1- y) x))
             (decf y)
             (list walker)))
      (#\v (unless (fset:contains? visited (list (1+ y) x))
             (incf y)
             (list walker)))
      (#\. (loop for (dy dx) in '((0 1) (0 -1) (1 0) (-1 0))
                 for new-pos = (list (+ y dy) (+ x dx))
                 for char = (gethash new-pos map)
                 if (and char (not (char= char #\#)) (not (fset:contains? visited new-pos)))
                   collect (frog:copy-instance walker :y (first new-pos) :x (second new-pos)))))))

(defun part-1 (file)
  (loop with (map end) = (parse-map file)
        with walkers = (list (make-instance 'walker))
        with max = most-negative-fixnum
        with progress = (cl-tqdm:tqdm 1 "")
        while walkers
        for i from 0
        for walker = (pop walkers)
        if (equal end (list (y-of walker) (x-of walker)))
          do (setf max (max max (fset:size (visited-of walker))))
        else
          do (setf walkers (append walkers (walk walker map)))
        do (cl-tqdm:update progress :total-count (+ i (length walkers)))
        finally (return max)))
(print (time (part-1 (frog:get-advent-of-code-input 2023 23))))
