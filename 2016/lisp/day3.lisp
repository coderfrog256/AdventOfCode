(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun part-one (lines)
  (loop for line in (str:lines lines)
        for parts = (str:split #\space (str:trim line) :omit-nulls t)
        for parsed = (mapcar #'parse-integer parts)
        for (a b c) = (sort parsed #'<)
        count (> (+ a b) c)))

(->> "../input/day3.txt"
  (str:from-file)
  (part-one)
  (format t "Part one: ~a~%"))

;; Part two
;; Transpose list of lists (added to common.lisp for future use)
(defun transpose (lists)
  (if (every #'null lists)
      nil
      (cons (mapcar #'first lists)
            (transpose (mapcar #'rest lists)))))

(defun part-two (lines)
  (->> lines
    (str:lines) ;; split into lines
    (mapcar (lambda (line)
              (->> (str:split #\space line :omit-nulls t) ;; split into parts
                (mapcar #'parse-integer)))) ;; parse parts
    (transpose) ;; transpose rows into columns
    (mapcan (lambda (parts) ;; chunk columns into parts of 3, flatten
              (frog:chunk-items 3 parts)))
    (mapcar (lambda (part) (sort part #'<))) ;; sort parts
    (remove-if (lambda (parts) ;; remove invalid triangles
              (<= (+ (first parts) (second parts)) (third parts))))
    (length)))

(->> "../input/day3.txt"
  (str:from-file)
  (part-two)
  (format t "Part two: ~a~%"))
