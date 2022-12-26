(ql:quickload '(:str :cl-ppcre :binding-arrows))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun find-first-unique (string window-size)
  (loop for i from 0 to (1- (length string))
        for window = (str:substring i (+ i window-size) string)
        when (= window-size (length (remove-duplicates (coerce window 'list))))
          do (return (+ window-size i))))

;; Part 1
(time (print (-> "../input/day6.txt" (str:from-file) (find-first-unique 4))))
(time (print (-> "../input/day6.txt" (str:from-file) (find-first-unique 14))))
