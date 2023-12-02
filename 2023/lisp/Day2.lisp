(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :alexandria :parseq))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
;; The input is parsed using parseq, a parser combinator library.
;; One new helper is used here, frog:integer-rule parses an integer.
(defparameter *cube-limits* (frog:make-alist "red" 12 "green" 13 "blue" 14))
(defclass* game-session () (cubes))
(defun session-valid (session)
  (with-slots (cubes) session
    (loop for (count cube) in cubes
          when (> count (cdr (assoc cube *cube-limits* :test #'equal))) do (return nil)
          finally (return t))))

(parseq:defrule game-session-rule ()
    (+ (and (frog:integer-rule) " " (or "blue" "red" "green") (? ", ")))
  (:function (lambda (&rest args) (make-instance 'game-session :cubes
                                                 (mapcar (lambda (g) (list (first g) (third g))) args)))))

(parseq:defrule game-rule ()
    (and "Game " (frog:integer-rule) ": " (* (and (game-session-rule) (? "; "))))
  (:flatten) (:function (lambda (&rest args) (remove-if-not #'game-session-p args))))

(defun part-1 (&optional suffix)
  (loop with valid = (list)
        for i from 0
        for line in (str:lines (frog:get-advent-of-code-input 2023 2 :input-suffix suffix))
        for game = (parseq:parseq 'game-rule line)
        if (->> game
             (mapcar #'session-valid)
             (every #'identity))
          do (push (1+ i) valid)
        finally (return (reduce #'+ valid))))
(print (time (part-1)))

(defun max-cubes-per-session (sessions)
    (loop with max-cubes = (make-hash-table :test #'equal)
          for session in sessions
          for cubes = (cubes-of session)
            do (loop for (count color) in cubes
                     do (let ((max (gethash color max-cubes -1)))
                            (setf (gethash color max-cubes) (max count max))))
            finally (return max-cubes)))

(defun part-2 (&optional suffix)
  (loop for line in (str:lines (frog:get-advent-of-code-input 2023 2 :input-suffix suffix))
        for game = (parseq:parseq 'game-rule line)
        for cubes = (max-cubes-per-session game)
        sum (reduce #'* (alexandria:hash-table-values cubes))))
(print (time (part-2)))
