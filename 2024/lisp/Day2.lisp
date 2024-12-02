(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defun part-1 (line)
  (let ((nums (frog:extract-numbers line)))
    (loop with sign = nil
          for i from 0 below (1- (length nums))
          for diff = (- (nth (1+ i) nums) (nth i nums))
          for current-sign = (signum diff)
          if (or (> (abs diff) 3) (and sign (not (= sign current-sign)))) do (return nil)
            do (setf sign current-sign)
          finally (return t))))

(frog:report (length (remove-if-not #'part-1 (str:lines
                                              (frog:get-advent-of-code-input 2024 2 :input-suffix "test")))))
(frog:report (length (remove-if-not #'part-1 (str:lines (frog:get-advent-of-code-input 2024 2)))))

(defun part-2 (line)
  (loop with nums = (frog:extract-numbers line)
        for i from 0 below (length nums)
        when (part-1 (str:join " " (mapcar #'write-to-string (frog:remove-nth i nums))))
          do (return t) finally (return nil)))

(frog:report (length (remove-if-not #'part-2
                                    (str:lines (frog:get-advent-of-code-input 2024 2 :input-suffix "test")))))
(frog:report (length (remove-if-not #'part-2
                                    (str:lines (frog:get-advent-of-code-input 2024 2)))))
