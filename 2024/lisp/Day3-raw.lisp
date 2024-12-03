(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

;; Declare *input* variable as a global, I'll clean this up later.
(defvar *input* (frog:get-advent-of-code-input 2024 3 :input-suffix "test"))
(setf *input* (frog:get-advent-of-code-input 2024 3 :input-suffix "test"))
(setf *input* (frog:get-advent-of-code-input 2024 3))

;; Part 1
(let (out)
  (ppcre:do-matches (start end "mul\\((\\d\\d?\\d?),(\\d\\d?\\d?)\\)" *input*)
    (let* ((text (subseq *input* start end))
           (lhs (parse-integer (subseq text 4 (position #\, text))))
           (rhs (parse-integer (subseq text (1+ (position #\, text)) (1- (length text))))))
      (push (* lhs rhs) out)))
  (reduce #'+ out))

;; Part 2
(let ((out nil) (enable-points (list (list 0 t))))
  (ppcre:do-matches (start end "do\\(\\)" *input*) (push (list start t) enable-points))
  (ppcre:do-matches (start end "don't\\(\\)" *input*) (push (list start nil) enable-points))
  (setf enable-points (sort enable-points #'< :key #'car))
  (ppcre:do-matches (start end "mul\\((\\d\\d?\\d?),(\\d\\d?\\d?)\\)" *input*)
    (let* ((text (subseq *input* start end))
           (lhs (parse-integer (subseq text 4 (position #\, text))))
           (rhs (parse-integer (subseq text (1+ (position #\, text)) (1- (length text))))))
      (let ((last-enable-point (find-if (lambda (x) (< (car x) start)) enable-points :from-end t)))
        (if (car (cdr last-enable-point))
            (push (* lhs rhs) out)))))
  (reduce #'+ out))
