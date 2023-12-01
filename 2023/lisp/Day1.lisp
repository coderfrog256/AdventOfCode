(ql:quickload '(:str))
(defpackage :advent (:use :cl))
(in-package :advent)
;; Two helpers are used, one makes an association list from a list of alternating key value pairs,
;; the other gets the advent of code input via HTTP or the clipboard if a suffix is passed.
(defparameter *int-lookup* (frog:make-alist "1" "1" "2" "2" "3" "3" "4" "4" "5" "5" "6" "6" "7" "7" "8" "8" "9" "9"))

(defun find-number (mapping substring from-end)
  (loop with matches = (list)
        for (key . value) in mapping
        for index = (search key substring :test #'equal :from-end from-end)
        when index do (push (list index value) matches)
        finally (return (second (first (sort matches (if from-end #'> #'<) :key #'car))))))

(defun solve (mapping &optional suffix)
  (loop with input = (frog:get-advent-of-code-input 2023 1 :input-suffix suffix)
        for line in (str:lines input)
        for start = (find-number mapping line nil) and end = (find-number mapping line t)
        sum (parse-integer (str:concat start end))))

(print (time (solve *int-lookup*))) ; Part 1

(defparameter *int-and-english-lookup* (concatenate 'list *int-lookup* (frog:make-alist "one" "1" "two" "2" "three" "3" "four" "4" "five" "5" "six" "6" "seven" "7" "eight" "8" "nine" "9")))

(print (time (solve *int-and-english-lookup*))) ; Part 2
