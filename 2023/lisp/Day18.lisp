;; Grepable keywords: Area, Rectangle, Boxes, Edges
(ql:quickload '(:str))
(defpackage :advent (:use :cl))
(in-package :advent)
(defun parse-line (line)
  (let ((parts (str:split " " line)))
    (list (case (char (first parts) 0) (#\R 0) (#\D 1) (#\L 2) (#\U 3))
          (parse-integer (second parts)))))

(defun decode-hex (line)
  (list (parse-integer (str:substring (- (length line) 2) (- (length line) 1) line))
        (parse-integer (str:substring 2 -2 (third (str:split " " line))) :radix 16)))

(defun dig (file parser)
  (loop with lines = (str:lines file)
        with out = 0 and y = 0 and last-clockwise = nil
        for i from 0 below (1- (length lines))
        for (dir dist) = (funcall parser (nth i lines))
        for (next-dir next-dist) = (funcall parser (nth (1+ i) lines))
        for turning-clockwise = (= next-dir (mod (1+ dir) 4))
        ;; Fixup logic courtesy of http://clb.confined.space/aoc2023/#day18
        if (equal turning-clockwise last-clockwise) do (if turning-clockwise (incf dist) (decf dist))
        do (setf last-clockwise turning-clockwise)
        if (member dir '(1 3)) do (incf y (if (= dir 1) dist (- dist)))
        else do (incf out (* y (if (= dir 2) dist (- dist))))
        finally (return out)))
(print (time (dig (frog:get-advent-of-code-input 2023 18) #'parse-line)))
(print (time (dig (frog:get-advent-of-code-input 2023 18) #'decode-hex)))
