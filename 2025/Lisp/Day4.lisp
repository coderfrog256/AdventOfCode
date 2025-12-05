(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel :alexandria))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
;; PAIR ORDERING IS X, Y. ALWAYS. FOREVER. NEVER SHALL Y BE FIRST.

(defun neighbors (pos)
  (mapcar (lambda (d) (mapcar #'+ pos d))
          '((-1 0) (1 0) (1 -1) (1 1)
            (-1 -1) (-1 1) (0 -1) (0 1))))

(defun parse-grid (input)
  (loop with lines = (str:lines (str:trim input))
        with out = (serapeum:dict)
        for y from 0 below (length lines)
        for row = (nth y lines)
        do (loop for x from 0 below (length row)
                 for char = (char row x)
                 if (char= #\@ char) do (setf (gethash (list x y) out) char))
        finally (return out)))

(defun part-1 (grid)
  (->> grid
    (alexandria:hash-table-keys)
    (mapcar (lambda (position)
              (if (> 4 (->> position
                         (neighbors)
                         (mapcar (lambda (n) (if (gethash n grid) 1 0)))
                         (reduce #'+))) position nil)))
    (remove-if #'null)))

(defun part-2 (grid)
  (loop with removed = 0
        for removable = (part-1 grid)
        while removable
        do (loop for position in removable
                 do (incf removed) (remhash position grid))
        finally (return removed)))

(frog:report (length (part-1 (parse-grid (frog:get-advent-of-code-input 2025 4)))))
(frog:report         (part-2 (parse-grid (frog:get-advent-of-code-input 2025 4))))
