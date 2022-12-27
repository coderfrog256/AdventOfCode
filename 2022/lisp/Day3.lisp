(ql:quickload :str)
(ql:quickload :binding-arrows)

(defpackage :advent (:use :cl :binding-arrows))
(in-package :advent)
(defun to-list (s) (coerce s 'list))

(defun item-priority (item)
  (if (upper-case-p item)
      (+ 27 (- (char-code item) (char-code #\A)))
      (+ 1 (- (char-code item) (char-code #\a)))))

(defun bag-value (bag)
  (let* ((split-point (floor (length bag) 2))
         (first-half (to-list (subseq bag 0 split-point)))
         (second-half (to-list (subseq bag split-point))))
    (item-priority (first (intersection first-half second-half)))))

;; Part 1
(time (print (loop for line in (str:lines (str:from-file "../input/day3.txt"))
                   summing (bag-value line) into sum
                   finally (return sum))))

;; Part 2
(time (print (->> "../input/day3.txt"
               (str:from-file)
               (str:lines)
               (frog:chunk-items 3)
               (mapcar (lambda (group)
                         (->
                           (to-list (first group))
                           (intersection (to-list (second group)))
                           (intersection (to-list (third group)))
                           (first)
                           (item-priority))))
               (reduce #'+))))
