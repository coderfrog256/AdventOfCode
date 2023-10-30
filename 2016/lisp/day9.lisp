(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq :metabang-bind))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind))
(in-package :advent)

(defrule decompress-rule ()
    (and "(" (frog:integer-rule) "x" (frog:integer-rule) ")" (+ char))
  (:function (lambda (_1 length _2 amount _3 text)
               (list (str:repeat amount (coerce (subseq text 0 length) 'string))
                     (+ length 3 (length (write-to-string length)) (length (write-to-string amount)))))))

(defun decompress (text)
  (loop with out = nil with i = 0
        until (>= i (length text))
        for char = (char text i)
        if (not (equal #\( char)) do (progn (push char out) (incf i))
          else do (bind (((chars advance-amount) (parseq 'decompress-rule (str:substring i nil text))))
                    (loop for char across chars do (push char out))
                    (incf i advance-amount))
        finally (return (coerce (nreverse out) 'string))))

(print (time (length (decompress (str:trim (str:from-file "../input/day9.txt"))))))

;; Part 2
(defrule marker-rule ()
    (and "(" (frog:integer-rule) "x" (frog:integer-rule) ")" (+ char))
  (:function (lambda (_1 length _2 amount _3 text)
               (list amount
                     (coerce (subseq text 0 length) 'string)
                     (+ length 3 (length (write-to-string length)) (length (write-to-string amount)))))))

(defun decompress-len (text)
  (loop with len = 0 with i = 0
        until (>= i (length text))
        for char = (char text i)
        if (not (equal #\( char)) do (progn (incf len) (incf i))
          else do (bind (((repeats chars advance-amount) (parseq 'marker-rule (str:substring i nil text))))
                    (incf len (* repeats (decompress-len chars)))
                    (incf i advance-amount))
        finally (return len)))

(print (time (decompress-len (str:trim (str:from-file "../input/day9.txt")))))
