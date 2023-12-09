(ql:quickload '(:str))
(defpackage :advent (:use :cl))
(in-package :advent)

(defun expand-line (line)
  (loop with expansion = (list)
        for i below (1- (length line))
        for l = (nth i line) for r = (nth (1+ i) line)
        do (push (- r l) expansion)
        finally (return (nreverse expansion))))

(defun expand-lines (line)
  (loop with out = (list line)
        until (every #'zerop line)
        do (setf line (expand-line line)) (push line out)
        finally (return (nreverse out))))

(defun extrapolate (lines)
  (loop for i from (- (length lines) 2) downto 0
        for line = (nth i lines) and last = (nth (1+ i) lines)
        for diff = (car (last last))
        for source = (+ diff (car (last line)))
        do (nconc line (list source))
        finally (return source)))

(defun part-1 (input)
  (loop for line in (mapcar #'frog:extract-numbers (str:lines input)) sum (extrapolate (expand-lines line))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 9))))

(defun extrapolate-rev (lines)
  (loop for i from (- (length lines) 2) downto 0
        for line = (nth i lines) and last = (nth (1+ i) lines)
        for diff = (first last)
        for source = (- (first line) diff)
        do (setf (nth i lines) (cons source line))
        finally (return source)))

(defun part-2 (input)
  (loop for line in (mapcar #'frog:extract-numbers (str:lines input)) sum (extrapolate-rev (expand-lines line))))

(print (time (part-2 (frog:get-advent-of-code-input 2023 9))))
