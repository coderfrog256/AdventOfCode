(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

;;; Part 1
(defun count-houses (input)
  (let ((houses (make-hash-table :test #'equal))
        (x 0)
        (y 0))
    (incf (gethash (list x y) houses 0) 1)
    (loop for direction across input
          do (cond
               ((equal #\> direction) (incf x))
               ((equal #\< direction) (decf x))
               ((equal #\^ direction) (incf y))
               ((equal #\v direction) (decf y)))
          do (incf (gethash (list x y) houses 0) 1)
          do (format t "x=~A y=~A~%" x y))
    (loop for k being the hash-key of houses
          counting t into len
          finally (return len))))

(count-houses ">")
(count-houses "^>v<")
(count-houses "^v^v^v^v^v")

(->> "../input/day3.txt"
  (str:from-file)
  (count-houses))

;;; Part 2
(defun count-houses-robot (input)
  (let ((houses (make-hash-table :test #'equal))
        (santa (list 0 0))
        (robo (list 0 0)))
    (incf (gethash (list (first santa) (second santa)) houses 0) 1)
    (incf (gethash (list (first robo) (second robo)) houses 0) 1)
    (loop for direction across input
          for iter from 1
          for actor = (if (evenp iter) robo santa)
          do (cond
               ((equal #\> direction) (incf (first actor)))
               ((equal #\< direction) (decf (first actor)))
               ((equal #\^ direction) (incf (second actor)))
               ((equal #\v direction) (decf (second actor))))
          do (incf (gethash (list (first actor) (second actor)) houses 0) 1))
    (loop for k being the hash-key of houses
          counting t into len
          finally (return len))))

(print (count-houses-robot "^v"))
(print (count-houses-robot "^>v<"))
(print (count-houses-robot "^v^v^v^v^v"))

(->> "../input/day3.txt"
  (str:from-file)
  (count-houses-robot))
