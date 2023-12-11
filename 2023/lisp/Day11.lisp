(ql:quickload '(:str :hu.dwim.defclass-star :alexandria))
(defpackage :advent (:use :cl :hu.dwim.defclass-star))
(in-package :advent)

(defparameter *drift-distance* 1)
(defclass* galaxy () (id y x))

(defun build-universe (file)
  (let* ((lines (str:lines file))
         (counter 0)
         (max-x 0) (max-y (length lines))
         (galaxies (serapeum:dict)))
    (loop for y from 0 below max-y
          for line = (nth y lines)
          do (loop for x from 0 below (length line)
                   for char = (char line x)
                   when (char= char #\#)
                     do (setf (gethash (list y x) galaxies) (make-instance 'galaxy :id (incf counter) :x x :y y))
                   finally (setf max-x (max max-x x))))
    (loop with adjustments = 0
          for y from 0 below max-y
          for adj-y = (+ y adjustments)
          if (notany (lambda (g) (= adj-y (y-of g))) (alexandria:hash-table-values galaxies))
            do (mapc (lambda (g) (incf (y-of g) *drift-distance*))
                     (remove-if-not (lambda (g) (< adj-y (y-of g))) (alexandria:hash-table-values galaxies)))
               (incf adjustments *drift-distance*))
    (loop with adjustments = 0
          for x from 0 below max-x
          for adj-x = (+ x adjustments)
          if (notany (lambda (g) (= adj-x (x-of g))) (alexandria:hash-table-values galaxies))
            do (mapc (lambda (g) (incf (x-of g) *drift-distance*))
                     (remove-if-not (lambda (g) (< adj-x (x-of g))) (alexandria:hash-table-values galaxies)))
               (incf adjustments *drift-distance*))
    galaxies))

(defun manhattan-distance (a b) (+ (abs (- (x-of a) (x-of b))) (abs (- (y-of a) (y-of b)))))

(defun get-distances (file)
  (loop with universe = (build-universe file)
        for galaxy being the hash-values of universe
        sum (loop for other being the hash-values of universe
                  unless (< (id-of galaxy) (id-of other)) sum (manhattan-distance galaxy other))))

(let ((*drift-distance* 1))
  (print (time (get-distances (str:trim (frog:get-advent-of-code-input 2023 11))))))
(let ((*drift-distance* 999999))
  (print (time (get-distances (str:trim (frog:get-advent-of-code-input 2023 11))))))
