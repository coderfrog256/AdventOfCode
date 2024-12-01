(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defun part-1 (input)
  (loop with left = nil
        with right = nil
        for pair in (->> input
                      (str:lines)
                      (mapcar (lambda (s) (ppcre:register-groups-bind (left right) ("(\\d+)\\s+(\\d+)" s)
                                            (list (parse-integer left) (parse-integer right))))))
        do (push (first pair) left)
           (push (second pair) right)
        finally (return (reduce #'+
                                (mapcar #'abs
                                        (mapcar #'-
                                                (sort left #'<) (sort right #'<)))))))

(print (time (part-1 (frog:get-advent-of-code-input 2024 1 :input-suffix "test"))))
(print (time (part-1 (frog:get-advent-of-code-input 2024 1))))

(defun part-2 (input)
  (let* ((parsed (->> input
                   (str:lines)
                   (mapcar (lambda (s) (ppcre:register-groups-bind (left right) ("(\\d+)\\s+(\\d+)" s)
                                         (list (parse-integer left) (parse-integer right)))))))
         (second (mapcar #'alexandria:lastcar parsed)))
    (loop for pair in parsed
          for id = (first pair)
          summing (* id (count id second)))))

(print (time (part-2 (frog:get-advent-of-code-input 2024 1 :input-suffix "test"))))
(print (time (part-2 (frog:get-advent-of-code-input 2024 1))))
