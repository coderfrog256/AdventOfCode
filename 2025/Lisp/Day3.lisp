(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defun max-char-with-index (char-list)
  (loop for ch in char-list
        for idx from 0
        with best-char = nil
        and best-int = most-negative-fixnum
        and best-idx = -1
        do (let ((ci (char-int ch)))
             (when (> ci best-int)
               (setf best-int ci
                     best-char ch
                     best-idx idx)))
        finally (return (list best-char best-idx))))

(defun max-jolt (n string)
  (loop with chars = (coerce string 'list) with length = (length chars)
        with i = 0 with window-size = (1- n)
        with out = (list) until (= n (length out))
        for window = (subseq chars i (- length (- window-size (length out))))
        for (char index) = (max-char-with-index window)
        do (setf out (cons char out)
                 i (+ 1 i index))
        finally (return (read-from-string (frog:coerce-r 'string (nreverse out))))))

(defun run (n input)
  (->> input (str:lines) (mapcar (lambda (l) (max-jolt n l))) (reduce #'+)))

(frog:report (run 2 (frog:get-advent-of-code-input 2025 3)))
(frog:report (run 12 (frog:get-advent-of-code-input 2025 3)))
