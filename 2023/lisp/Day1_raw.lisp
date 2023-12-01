(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defparameter *english-to-int* (serapeum:dict
                                "one" "1"
                                "two" "2"
                                "three" "3"
                                "four" "4"
                                "five" "5"
                                "six" "6"
                                "seven" "7"
                                "eight" "8"
                                "nine" "9"))

(defun line-to-int (line)
(let ((input (remove-if (lambda (x) (alpha-char-p x))
                        (coerce line 'list))))
  (parse-integer (coerce (list (first input) (car (last input))) 'string))))

(print (loop for line in (str:lines (frog:get-advent-of-code-input 2023 1))
      for int = (line-to-int line)
        sum int))

(defun english-line-to-int (line)
  (let (first last)
    (loop for index from 0
          until first
          if (not (alpha-char-p (char line index)))
            do (setf first (string (char line index)))
               (return)
          do (loop for english being the hash-key of *english-to-int* using (hash-value int)
                   when (equal english (str:substring index (+ index (length english)) line))
                     do (setf first int)
                        (return)))
    (loop for index downfrom (length line)
          until last
          if (and (< index (length line))
                      (not (alpha-char-p (char line index))))
            do (setf last (string (char line index)))
               (return)
          do (loop for english being the hash-key of *english-to-int* using (hash-value int)
                   when (equal english (str:substring (- index (length english)) index line))
                     do (setf last int)
                        (return)))
    (print (list first last))
    (parse-integer (coerce (str:concat first last) 'string))))

(print (english-line-to-int "two1nine"))

(print (loop for line in (str:lines (frog:get-advent-of-code-input 2023 1))
      for int = (english-line-to-int line)
        sum int))
