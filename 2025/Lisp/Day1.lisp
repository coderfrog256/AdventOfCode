(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))

(defun spin (current dir)
  (let* ((mult (if (equal #\L (char dir 0)) -1 1))
         (raw (+ current (* mult (parse-integer (str:substring 1 nil dir))))))
    (mod raw 100)))

(defun run (input)
  (loop for dir in (str:lines input)
        with current = 50
        with count = 0
        do (setf current (spin current dir))
        if (= 0 current)
          do (incf count)
        finally (return count)))

(frog:report (run (str:trim (frog:get-advent-of-code-input 2025 1))))

(defun spin-2 (current dir)
  (let* ((n (parse-integer dir :start 1))
         (left? (char= #\L (char dir 0)))
         (new-current (if left?
                          (mod (- current n) 100)
                          (mod (+ current n) 100)))
         (first (if left?
                    (if (zerop current) 100 current)
                    (let ((steps-to-zero (mod (- 100 current) 100)))
                      (if (zerop steps-to-zero) 100 steps-to-zero))))
         (count 0))
    (when (>= n first)
      (incf count (1+ (floor (- n first) 100))))
    (list new-current count)))

(defun run-2 (input)
  (loop for dir in (str:lines input)
        with current = 50
        with count = 0
        for (new-current new-count) = (spin-2 current dir)
        do (setf current new-current)
           (incf count new-count)
        finally (return count)))

(frog:report (run-2 (str:trim (frog:get-advent-of-code-input 2025 1))))
