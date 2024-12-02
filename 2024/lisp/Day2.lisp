(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defun part-1 (line)
  (let ((nums (frog:extract-numbers line)))
    (loop with sign = nil
          with in-range = t
          for i from 0 below (1- (length nums))
          for current = (nth i nums)
          for next = (nth (1+ i) nums)
          for diff = (- next current)
          for current-sign = (signum diff)
          if (and sign (not (= sign current-sign))) do (return nil)
            do (setf sign current-sign)
               (setf in-range (and in-range (<= (abs diff) 3)))
          finally (return in-range))))

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
