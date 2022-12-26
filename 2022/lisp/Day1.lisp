(ql:quickload '(:str :cl-ppcre :binding-arrows))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(ql:quickload 'split-sequence)

(defun build-elves (backpack)
  (mapcar
   (lambda (food) (reduce #'+ food :key #'parse-integer))
   (split-sequence:split-sequence "" backpack :test #'equal)))

;; Part 1
(->> "../input/day1.txt"
  (str:from-file)
  (str:lines)
  (build-elves)
  (reduce #'max)
  (print))

;; Part 2
(->> (-> "../input/day1.txt"
       (str:from-file)
       (str:lines)
       (build-elves)
       (sort #'>)
       (subseq 0 3))
  (reduce #'+)
  (print))
