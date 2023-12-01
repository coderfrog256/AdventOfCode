(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
;; A brute-force solution.
;; A better solution would have been using two regexes and tweaking the greadiness to handle start/end matching.

(defparameter *english-to-int* (serapeum:dict
                                "one"   "1"
                                "two"   "2"
                                "three" "3"
                                "four"  "4"
                                "five"  "5"
                                "six"   "6"
                                "seven" "7"
                                "eight" "8"
                                "nine"  "9"))

;; Part 1
(defun part-one (&optional suffix)
  (loop with input = (frog:get-advent-of-code-input 2023 1 :input-suffix suffix)
        for line in (str:lines input)
        for numbers = (remove-if #'alpha-char-p line)
        for int = (parse-integer (str:concat (str:s-first numbers) (str:s-last numbers)))
        sum int))

(print (time (part-one)))

;; Part 2
(defun matches-english (substring)
  (loop with matches = (list)
        for english being the hash-key of *english-to-int* using (hash-value int)
        do (loop with start = 0 while start
                 for match = (cl-ppcre:scan english substring :start start)
                 if match do (push (list match int) matches) (setf start (1+ match))
                 else do (setf start nil))
        finally (return (list (first (sort matches #'< :key #'car))
                              (first (sort matches #'> :key #'car))))))

(defun matches-digit (substring)
  (loop with matches = (list)
        for char across substring
        for i from 0
        when (digit-char-p char) do (push (list i (string char)) matches)
        finally (return (list (first (sort matches #'< :key #'car))
                              (first (sort matches #'> :key #'car))))))

(defun part-two (&optional suffix)
  (loop with input = (frog:get-advent-of-code-input 2023 1 :input-suffix suffix)
        for line in (str:lines input)
        for english-bounds = (matches-english line) and digit-bounds = (matches-digit line)
        for english-start  = (first english-bounds) and english-end  = (second english-bounds)
        for digit-start    = (first digit-bounds)   and digit-end    = (second digit-bounds)
        if (null english-start) do (setf english-start digit-start   english-end digit-end)
        if (null digit-start)   do (setf digit-start   english-start digit-end   english-end)
        sum (parse-integer (str:concat (if (< (first english-start) (first digit-start))
                                           (second english-start) (second digit-start))
                                       (if (> (first english-end) (first digit-end))
                                           (second english-end) (second digit-end))))))
(print (time (part-two)))
