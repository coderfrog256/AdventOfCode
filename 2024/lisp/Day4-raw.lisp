(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defvar *input* (frog:get-advent-of-code-input 2024 4 :input-suffix "test"))
(setf *input* (str:trim (frog:get-advent-of-code-input 2024 4 :input-suffix "test")))
(setf *input* (str:trim (frog:get-advent-of-code-input 2024 4)))

;; Part1
;; Horizontal
(let* ((count 0)
       (lines (str:lines *input*))
       (x-len (length (first lines)))
       (y-len (length lines))
       (arr (make-array (list x-len y-len) :initial-element nil)))
  (loop for y from 0 below y-len
        for line = (coerce (nth y lines) 'list)
        do (loop for x from 0 below x-len do
              (setf (aref arr y x) (nth x line))))
  ;; Loop horizontally, if the word "XMAS" or "SAMX" is found, increment count
  (loop for y from 0 below (array-dimension arr 0)
        do (loop for x from 0 below (- (array-dimension arr 1) 3)
                do (let ((word (map 'string #'(lambda (x) (aref arr y x)) (list x (+ x 1) (+ x 2) (+ x 3)))))
                     (when (or (string= word "XMAS") (string= word "SAMX"))
                         (incf count 1)))))
  ;; Loop vertically, if the word "XMAS" or "SAMX" is found, increment count
  (loop for x from 0 below (array-dimension arr 1)
        do (loop for y from 0 below (- (array-dimension arr 0) 3)
                do (let ((word (map 'string #'(lambda (y) (aref arr y x)) (list y (+ y 1) (+ y 2) (+ y 3)))))
                     (when (or (string= word "XMAS") (string= word "SAMX"))
                       (incf count 1)))))
  ;; loop diagonally
  (loop for y from 0 below (- (array-dimension arr 0) 3)
        do (loop for x from 0 below (- (array-dimension arr 1) 3)
                do (let ((word (map 'string #'(lambda (i) (aref arr (+ y i) (+ x i))) (list 0 1 2 3))))
                     (when (or (string= word "XMAS") (string= word "SAMX"))
                         (incf count 1)))))
  ;; loop diagonally the other way
  (loop for y from 0 below (- (array-dimension arr 0) 3)
        do (loop for x from (1- (array-dimension arr 1)) downto 3
                 do (let ((word (map 'string #'(lambda (i) (aref arr (+ y i) (- x i))) (list 0 1 2 3))))
                     (when (or (string= word "XMAS") (string= word "SAMX"))
                         (incf count 1)))))
  count)

;; Part 2
(let* ((count 0)
       (lines (str:lines *input*))
       (x-len (length (first lines)))
       (y-len (length lines))
       (arr (make-array (list x-len y-len) :initial-element nil)))
  (loop for y from 0 below y-len
        for line = (coerce (nth y lines) 'list)
        do (loop for x from 0 below x-len do
              (setf (aref arr y x) (nth x line))))
  (loop for y from 0 below (- (array-dimension arr 0) 2)
        do (loop for x from 0 below (- (array-dimension arr 1) 2)
                 for upper-left = (aref arr y x)
                 for upper-right = (aref arr y (+ 2 x))
                 for lower-left = (aref arr (+ 2 y) x)
                 for lower-right = (aref arr (+ 2 y) (+ 2 x))
                 for mid = (aref arr (1+ y) (1+ x))
                 if (and (or (char= #\M upper-left) (char= #\S upper-left))
                         (or (char= #\M upper-right) (char= #\S upper-right))
                         (or (char= #\M lower-left) (char= #\S lower-left))
                         (or (char= #\M lower-right) (char= #\S lower-right))
                         (char= #\A mid)
                         (not (equal upper-left lower-right))
                         (not (equal upper-right lower-left))) do (incf count)
                ))
  count)
