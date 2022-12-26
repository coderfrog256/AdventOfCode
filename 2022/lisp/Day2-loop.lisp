(ql:quickload '(:str :cl-ppcre :binding-arrows))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(ql:quickload :trivia)
;; For fun, let's use iterate and loop in addition to plain-old series
(ql:quickload :iterate)

(defconstant +CHOICES+ '(:rock :paper :scissors))

(defun rps-beats (left right)
  (if (equal left right)
      :tie
      (trivia:ematch (list left right)
        ((list :rock :scissors) :win)
        ((list :scissors :paper) :win)
        ((list :paper :rock) :win)
        (_ :lose))))

(defun lhs-to-symbol (lhs)
  (trivia:ematch lhs
    ((or "A" "X") :rock)
    ((or "B" "Y") :paper)
    ((or "C" "Z") :scissors)))

(defun symbol-points (symbol)
  (trivia:ematch symbol
    (:rock 1)
    (:paper 2)
    (:scissors 3)
    (:lose 0)
    (:tie 3)
    (:win 6)))

(defun run-match (line)
  (let* ((parts (str:split " " line))
         (lhs (lhs-to-symbol (first parts)))
         (rhs (lhs-to-symbol (second parts))))
    (+ (symbol-points rhs)
       (symbol-points (rps-beats rhs lhs)))))

;; Loop and iterate approaches
(print (loop for line in (str:lines (str:from-file "../input/day2.txt"))
             summing (run-match line) into total
             finally (return total)))

(print (iterate:iterate
         (iterate:for line in (str:lines (str:from-file "../input/day2.txt")))
         (iterate:sum (run-match line))))

(defun rhs-to-outcome (rhs)
  (trivia:ematch rhs
    ("X" :lose)
    ("Y" :tie)
    ("Z" :win)))

(defun rig-match (line)
  (let* ((parts (str:split " " line))
         (lhs (lhs-to-symbol (first parts)))
         (rhs (rhs-to-outcome (second parts))))
    (+ (symbol-points rhs)
       (symbol-points
        (find-if (lambda (choice)
                   (equal rhs (rps-beats choice lhs)))
                 +CHOICES+)))))

(time
 (print (loop for line in (str:lines (str:from-file "../input/day2.txt"))
              summing (rig-match line) into total
              finally (return total))))
