(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0)))

(defclass* disc () (disc-num positions current))
(defun parse-disc (line)
  (cl-ppcre:register-groups-bind (disc-num positions current)
      ("Disc #(\\d+).*has (\\d+) p.* position (\\d+)." line)
    (make-instance 'disc :disc-num (parse-integer disc-num) :positions (parse-integer positions) :current (parse-integer current))))

(defun position-at (disc time)
  (with-slots (disc-num positions current) disc
    (mod (+ disc-num current time) positions)))

(defun simulate (lines &optional suffix)
  ;; Tiny optimization. check discs in descending size order, they are the least likely to be 0.
  (loop with discs = (sort (append (mapcar #'parse-disc lines) suffix) #'> :key #'positions-of)
        for counter from 0
        until (every (lambda (d) (zerop (position-at d counter))) discs)
        finally (return counter)))

(print (time (simulate (str:lines (str:from-file "../input/day15.txt")))))
(print (time (simulate (str:lines (str:from-file "../input/day15.txt"))
                         (list (make-instance 'disc :disc-num 7 :positions 11 :current 0)))))
(print (time (simulate (str:lines (str:from-file "../input/day15-large.txt"))))) ;; Random reddit post
