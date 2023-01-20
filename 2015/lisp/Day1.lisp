(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

;;; Part 1: Each "(" is one, each ")" is -1. Sum the parens in a string
(defun paren-number (char-input)
  (cond
    ((equalp char-input #\( ) 1)
    ((equalp char-input #\) ) -1)
    (t 0)))

(defun day-1-part-1 (file)
  (->> file
    (str:from-file)
    (frog:coerce-r 'list)
    (mapcar #'paren-number)
    (reduce #'+)))

(print (day-1-part-1 "../input/day1.txt"))

;;; Part 2: At what point during iteration did the sum hit -1?
;;; If you want to be cute you can look at the last element to get the sum,
;;; which renders day-1-part-1 unneeded.
(defun day-1-part-2 (file)
  (->> file
    (str:from-file)
    (frog:coerce-r 'list)
    (mapcar #'paren-number)
    (frog:scan-reduce #'+)
    (position -1)
    (1+)))

(print (day-1-part-2 "../input/day1-test.txt"))
(print (day-1-part-2 "../input/day1.txt"))
