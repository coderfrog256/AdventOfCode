(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defun distance-vector (left right)
  (cons (- (car left) (car right)) (- (cdr left) (cdr right))))

(defun add-vector (position vector)
  (cons (+ (car position) (car vector)) (+ (cdr position) (cdr vector))))

(defun parse-input (input)
  (loop with result = (make-hash-table)
        for line in (str:lines input)
        for y from 0
        maximize (loop for x from 0
                       for c across line
                       when (not (char= c #\.))
                         do (push (cons x y) (gethash c result nil))
                       finally (return x)) into width
        finally (return (list result (1+ y) width))))

(defun part-1 (input)
  (loop with (parsed height width) = (parse-input input)
        with occupied = (make-hash-table :test 'equal)
        for positions being the hash-values of parsed
        do (loop for position in positions
                 do (loop for other in positions
                          for vector = (distance-vector position other)
                          for antinode = (add-vector position vector)
                          when (and (not (equal position antinode))
                                    (>= (car antinode) 0)
                                    (< (car antinode) width)
                                    (>= (cdr antinode) 0)
                                    (< (cdr antinode) height))
                            do (setf (gethash antinode occupied) t)))
        finally (return (hash-table-count occupied))))

(frog:report (part-1 (frog:get-advent-of-code-input 2024 8 :input-suffix "test")))
(frog:report (part-1 (frog:get-advent-of-code-input 2024 8)))

(defun part-2 (input)
  (flet ((reverberate (reverb vector width height occupied)
           (loop for antinode = (add-vector reverb vector)
                 while (and (>= (car antinode) 0) (< (car antinode) width)
                            (>= (cdr antinode) 0) (< (cdr antinode) height))
                 do (setf (gethash antinode occupied) t
                          reverb antinode))))
    (loop with (parsed height width) = (parse-input input)
          with occupied = (make-hash-table :test 'equal)
          for positions being the hash-values of parsed
          do (loop for position in positions
                   do (loop for other in positions
                            for vector = (distance-vector position other)
                            for reverse-vector = (cons (- (car vector)) (- (cdr vector)))
                            if (not (equal vector (cons 0 0)))
                              do (reverberate position vector width height occupied)
                                 (reverberate position reverse-vector width height occupied)))
          finally (return (hash-table-count occupied)))))

(frog:report (part-2 (frog:get-advent-of-code-input 2024 8 :input-suffix "test")))
(frog:report (part-2 (frog:get-advent-of-code-input 2024 8)))
