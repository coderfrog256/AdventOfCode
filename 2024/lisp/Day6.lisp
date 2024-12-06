(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0)))

(defun make-step (position direction)
  (destructuring-bind (y x) position
    (case direction
      (:up (list (1- y) x))
      (:down (list (1+ y) x))
      (:left (list y (1- x)))
      (:right (list y (1+ x))))))

(defun turn-right (direction)
  (case direction (:up :right) (:right :down) (:down :left) (:left :up)))

(defun do-it (input part-1)
  (let* ((visited (make-hash-table :test 'equal))
         (lines (str:lines input))
         (y-len (length (str:lines input)))
         (x-len (length (first (str:lines input))))
         (original-guard '(nil . :up))
         (grid (make-hash-table :test 'equal)))
    (loop for line in lines
          for y from 0
          do (loop for char across line
                   for x from 0
                   if (char= char #\^)
                     do (setf original-guard (cons (list y x) :up))
                   if (char= char #\#) do (setf (gethash (list y x) grid) #\#)))
    (loop with guard = original-guard
          for (pos . dir) = guard
          for next-pos = (make-step pos dir)
          for next = (gethash next-pos grid)
          if (equal next #\#)
            do (setf dir (turn-right dir))
               (setf next-pos pos)
          if (or (> (first next-pos) y-len) (> (second next-pos) x-len)
                 (< (first next-pos) 0) (< (second next-pos) 0))
            do (return (hash-table-count visited))
          do (setf guard (cons next-pos dir))
             (setf (gethash pos visited) t))
    (when part-1 (return-from do-it (hash-table-count visited)))
    (loop for rock being the hash-keys of visited
          for i from 0
          do (setf (gethash rock grid) #\#)
          summing (loop with visited = (make-hash-table :test 'equal)
                        with guard = original-guard
                        for (pos . dir) = guard
                        for next-pos = (make-step pos dir)
                        for next = (gethash next-pos grid)
                        if (gethash guard visited)
                          do (return 1)
                        do (setf (gethash (cons pos dir) visited) t)
                        if (equal next #\#)
                          do (setf dir (turn-right dir))
                             (setf next-pos pos)
                        if (or (> (first next-pos) y-len) (> (second next-pos) x-len)
                               (< (first next-pos) 0) (< (second next-pos) 0))
                          do (return 0)
                        do (setf guard (cons next-pos dir))) into count
          do (remhash rock grid)
          finally (return count))))

(frog:report (do-it (frog:get-advent-of-code-input 2024 6 :input-suffix "test") t))
(frog:report (do-it (frog:get-advent-of-code-input 2024 6) t))
(frog:report (do-it (frog:get-advent-of-code-input 2024 6 :input-suffix "test") nil))
(frog:report (do-it (frog:get-advent-of-code-input 2024 6) nil))
