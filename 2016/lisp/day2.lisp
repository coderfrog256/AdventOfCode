(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defparameter *keypad* (make-array '(3 3) :initial-contents '((1 2 3)
                                                              (4 5 6)
                                                              (7 8 9))))
(defun run-command-line (line x y)
  (loop for command across line
        do (cond ((char= command #\U) (setf y (max 0 (1- y))))
                 ((char= command #\D) (setf y (min 2 (1+ y))))
                 ((char= command #\L) (setf x (max 0 (1- x))))
                 ((char= command #\R) (setf x (min 2 (1+ x)))))
        finally (return (values (aref *keypad* y x) x y))))

(defun run-commands (x y line-function commands)
  (loop for line in (str:lines commands)
        with out = '()
        do (multiple-value-bind (key new-x new-y) (funcall line-function line x y)
             (setf x new-x)
             (setf y new-y)
             (push key out))
        finally (return (nreverse out))))

(->>
  (str:from-file "../input/day2.txt")
  (run-commands 1 1 #'run-command-line)
  (format t "Part 1: ~a~%"))

;; Part 2
(defparameter *keypad-2* (make-array '(5 5) :initial-contents '((nil nil  1   nil nil)
                                                                (nil  2   3   4   nil)
                                                                (5    6   7   8   9  )
                                                                (nil "A" "B" "C" nil )
                                                                (nil nil "D" nil nil ))))
(defun run-command-line-2 (line x y)
  (loop for command across line
        for new-x = (cond ((char= command #\L) (max 0 (1- x)))
                          ((char= command #\R) (min 4 (1+ x)))
                          (t x))
        for new-y = (cond ((char= command #\U) (max 0 (1- y)))
                          ((char= command #\D) (min 4 (1+ y)))
                          (t y))
        if (not (null (aref *keypad-2* new-y new-x)))
          do (setf x new-x)
             (setf y new-y)
        finally (return (values (aref *keypad-2* y x) x y))))

(->>
  (str:from-file "../input/day2.txt")
  (run-commands 0 2 #'run-command-line-2)
  (format t "Part 2: ~a~%"))

;; Extra credit: in run-command-line-2 we could use take in the keypad and use (array-dimensions keypad) instead of 4 to make it support any keypad, including the first one.
