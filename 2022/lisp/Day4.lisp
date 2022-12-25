(ql:quickload :str)
(ql:quickload :cl-ppcre)
(ql:quickload :binding-arrows)
(defpackage :advent (:use :cl :ppcre :binding-arrows))
(in-package :advent)

(defun check-complete-overlap (left-start left-end right-start right-end)
  (or (and (<= left-start right-start) (>= left-end right-end))
      (and (<= right-start left-start) (>= right-end left-end))))

(defun check-any-overlap (left-start left-end right-start right-end)
  (and (<= left-start right-end) (<= right-start left-end)))

(defun eval-range (line func)
  (register-groups-bind (left-start left-end right-start right-end) ("(\\d+)-(\\d+),(\\d+)-(\\d+)" line)
    (funcall func (parse-integer left-start) (parse-integer left-end) (parse-integer right-start) (parse-integer right-end))))

(defun eval-range-from-file (func)
  (->> "../input/day4.txt"
    (str:from-file)
    (str:lines)
    (remove-if-not (lambda (line) (eval-range line func)))
    (length)))

(time (format t "Part 1: ~A~%" (eval-range-from-file #'check-complete-overlap)))
(time (format t "Part 2: ~A~%" (eval-range-from-file #'check-any-overlap)))
