(ql:quickload '(:str :cl-ppcre :binding-arrows :series :alexandria))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun parse-grid (lines)
  (let ((out (make-array (list (length (first lines)) (length lines)))))
    (loop for line in lines
          for y from 0
          do (loop for char across line
                   for x from 0
                   do (setf (aref out x y) (parse-integer (string char))))
          finally (return out))))

(defmacro check-visible-line (var direction)
  "Macro to scan a line, iterating VAR (x or y) in DIRECTION (from or downfrom). Updates OUT"
  (let* ((dimension (if (equal 'x var) 0 1))
         (start (if (equal 'from direction) 0 `(1- (array-dimension grid ,dimension))))
         (end (if (equal 'from direction) `(1- (array-dimension grid ,dimension)) 0)))
    `(loop with max-height = -1
           for ,var ,direction ,start to ,end
           for height = (aref grid x y)
           when (> height max-height)
             do (setf (gethash (list x y) out) t)
                (setf max-height height))))

(defun is-visible (grid)
  (let ((out (make-hash-table :test #'equal)))
    (loop for y from 0 to (1- (array-dimension grid 1))
          do (check-visible-line x from)
          do (check-visible-line x downfrom))
    (loop for x from 0 to (1- (array-dimension grid 0))
          do (check-visible-line y from)
          do (check-visible-line y downfrom)
          finally (return out))))

;; Part 1
(time (print (->> "../input/day8.txt"
               (str:from-file)
               (str:lines)
               (parse-grid)
               (is-visible-macro)
               (alexandria:hash-table-values)
               (length))))

(defmacro check-outward-range (x y direction start end)
  "Iterates a variable named VAR in DIRECTION from START to END.
Replace the X or Y that you want to iterate with VAR"
  `(loop for var ,direction ,start to ,end
         for var-height = (aref grid ,x ,y)
         for visible from 1
         when (<= height var-height)
           do (return visible)
         finally (return visible)))

(defun scenic-score (grid x y)
  (let ((height (aref grid x y)))
    (* (check-outward-range var y downfrom (1- x) 0)
       (check-outward-range var y from (1+ x) (1- (array-dimension grid 0)))
       (check-outward-range x var downfrom (1- y) 0)
       (check-outward-range x var from (1+ y) (1- (array-dimension grid 1)))
       )))

(defun best-scenic-score (grid)
  (loop with best = 0
        for x from 1 to (- (array-dimension grid 0) 2)
        do (loop for y from 1 to (- (array-dimension grid 1) 2)
                 for score = (scenic-score grid x y)
                 when (> score best)
                   do (setf best score))
        finally (return best)))

;; Part 2
(time (print (->> "../input/day8.txt"
               (str:from-file)
               (str:lines)
               (parse-grid)
               (best-scenic-score))))
