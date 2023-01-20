(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun parse-line (line)
  (->> line
    (str:replace-using (list ","  " " "["  "(" "]"  ")" "()" "nil"))
    (read-from-string)))

(defun compare-lists (l1 l2)
  (cond ((and (null l1) (null l2)) 0)
        ((null l1) -1)
        ((null l2) 1)
        ((and (numberp l1) (numberp l2)) (- l1 l2))
        ((and (numberp l1)) (compare-lists (list l1) l2))
        ((and (numberp l2)) (compare-lists l1 (list l2)))
        (t (loop for i from 0
                 for left-item = (nth i l1)
                 for right-item = (nth i l2)
                 for comparison = (compare-lists left-item right-item)
                 do (cond ((< comparison 0) (return -1))
                          ((> comparison 0) (return 1))
                          ((and (null left-item) (null right-item)) (return 0)))))))

;; Part 1
(time (print (loop for pair in (str:split frog:+double-newline+ (str:from-file "../input/day13.txt"))
                   for split = (str:lines pair)
                   for left = (parse-line (first split))
                   for right = (parse-line (second split))
                   for index from 1
                   when (<= (compare-lists left right) 0) summing index)))

;; Part 2
(time (print (let* ((two (parse-line "[[2]]"))
                    (six (parse-line "[[6]]"))
                    (sorted (->> "../input/day13.txt"
                              (str:from-file)
                              (str:lines)
                              (remove-if #'str:blankp)
                              (mapcar #'parse-line)
                              (cons two) (cons six)
                              (frog:sort-r (lambda (l r) (< (compare-lists l r) 0))))))
               (* (1+ (position two sorted))
                  (1+ (position six sorted))))))
