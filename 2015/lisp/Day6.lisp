(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun build-light-grid (x y)
  (make-array (list x y) :initial-element nil))

(defun update-grid (command grid init-position end-position)
  (loop for x from (first init-position) to (first end-position)
        do (loop for y from (second init-position) to (second end-position)
                 do (cond
                      ((equal command "on")
                       (setf (aref grid x y) t))
                      ((equal command "off")
                       (setf (aref grid x y) nil))
                      ((equal command "toggle")
                       (setf (aref grid x y)
                             (not (aref grid x y))))
                      (t (error (concatenate 'string "Unexpected input: " command)))))
        finally (return grid)))

(defun count-lit (grid)
  (loop for x from 0 to (1- (array-dimension grid 0))
        summing (loop for y from 0 to (1- (array-dimension grid 1))
                      counting (aref grid x y) into inner-sum
                      finally (return inner-sum))
          into outer-sum
        finally (return outer-sum)))

(defun part-1 (input)
  (loop with grid = (build-light-grid 1000 1000)
        for line in (str:lines (str:from-file input))
        do (register-groups-bind (command start-x start-y end-x end-y)
               ("(?: turn )?(on|off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)" line)
             (update-grid command grid
                          (list (parse-integer start-x) (parse-integer start-y))
                          (list (parse-integer end-x) (parse-integer end-y))))
        finally (return (count-lit grid))))

(time (print (part-1 "../input/day6.txt")))

;;; Part 2

(defun build-int-grid (x y)
  (make-array (list x y) :initial-element 0))

(defun update-int-grid (command grid init-position end-position)
  (loop for x from (first init-position) to (first end-position)
        do (loop for y from (second init-position) to (second end-position)
                 do (cond
                      ((equal command "on")
                       (incf (aref grid x y) 1))
                      ((equal command "off")
                       (when (> 0 (decf (aref grid x y) 1)) (setf (aref grid x y) 0))) ; Never go negative
                      ((equal command "toggle")
                       (incf (aref grid x y) 2))
                      (t (error (concatenate 'string "Unexpected input: " command)))))
        finally (return grid)))

(defun count-brightness (grid)
  (loop for x from 0 to (1- (array-dimension grid 0))
        summing (loop for y from 0 to (1- (array-dimension grid 1))
                      summing (aref grid x y) into inner-sum
                      finally (return inner-sum))
          into outer-sum
        finally (return outer-sum)))

(defun part-2 (input)
  (loop with grid = (build-int-grid 1000 1000)
        for line in (str:lines (str:from-file input))
        do (register-groups-bind (command start-x start-y end-x end-y)
               ("(?: turn )?(on|off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)" line)
             (update-int-grid command grid
                              (list (parse-integer start-x) (parse-integer start-y))
                              (list (parse-integer end-x) (parse-integer end-y))))
        finally (return (count-brightness grid))))

(time (print (part-2 "../input/day6.txt")))
