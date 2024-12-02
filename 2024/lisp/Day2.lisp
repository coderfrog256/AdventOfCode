(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defun part-1 (line)
  (let ((nums (frog:extract-numbers line)))
    (loop with increasing = t
          with decreasing = t
          with in-range = t
          for i from 0 below (1- (length nums))
          for current = (nth i nums)
          for next = (nth (1+ i) nums)
          do (setf increasing (and increasing (< current next)))
             (setf decreasing (and decreasing (> current next)))
             (setf in-range (and in-range (<= (abs (- current next)) 3)))
          finally (return (and in-range (or increasing decreasing))))))

(frog:report (length (remove-if-not #'part-1 (str:lines
                                              (frog:get-advent-of-code-input 2024 2 :input-suffix "test")))))

(frog:report (length (remove-if-not #'part-1 (str:lines (frog:get-advent-of-code-input 2024 2)))))

;; Hacky, but you can just brute force this.
(defun part-2 (line)
  (let ((nums (frog:extract-numbers line)))
    (loop for i from 0 below (length nums)
          count (part-1 (str:join " " (mapcar #'write-to-string (frog:remove-nth i nums)))) into count
          finally (return (<= 1 count)))))

(frog:report (length (remove-if-not #'part-2
                                    (str:lines (frog:get-advent-of-code-input 2024 2 :input-suffix "test")))))

(frog:report (length (remove-if-not #'part-2
                                    (str:lines (frog:get-advent-of-code-input 2024 2)))))
