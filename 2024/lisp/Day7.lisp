(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0)))

(defun concat-numbers (left right)
  (parse-integer (concatenate 'string (write-to-string left) (write-to-string right))))

(defun is-valid (target total numbers part-2)
  (if (null numbers) (= total target)
      (or (is-valid target (+ total (car numbers)) (cdr numbers) part-2)
          (is-valid target (* total (car numbers)) (cdr numbers) part-2)
          (and part-2 (is-valid target (concat-numbers total (car numbers)) (cdr numbers) part-2)))))

(defun solve (input part-2)
  (->> input
    (str:lines)
    (mapcar #'frog:extract-numbers)
    (remove-if-not (lambda (row) (is-valid (first row) 0 (rest row) part-2)))
    (mapcar #'first)
    (reduce #'+)))

(frog:report (solve (frog:get-advent-of-code-input 2024 7 :input-suffix "test") nil))
(frog:report (solve (frog:get-advent-of-code-input 2024 7) nil))

(frog:report (solve (frog:get-advent-of-code-input 2024 7 :input-suffix "test") t))
(frog:report (solve (frog:get-advent-of-code-input 2024 7) t))
