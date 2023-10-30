(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq :metabang-bind))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind))
(in-package :advent)

(defparameter *board-width* 50)
(defparameter *board-height* 6)

(defstruct pixel x y on)
(defstruct command-rect x y)
(defstruct command-row y amount)
(defstruct command-column x amount)

(defun increment-mod (value amount max) (mod (incf value amount) max))

(defmethod run-command ((command command-rect) board)
  (with-slots (x y) command
    (loop for pixel in board
          if (and (< (pixel-x pixel) x) (< (pixel-y pixel) y))
            do (setf (pixel-on pixel) t))))

(defmethod run-command ((command command-column) board)
  (with-slots (x amount) command
    (loop for pixel in board
          if (= (pixel-x pixel) x)
            do (setf (pixel-y pixel) (increment-mod (pixel-y pixel) amount *board-height*)))))

(defmethod run-command ((command command-row) board)
  (with-slots (y amount) command
    (loop for pixel in board
          if (= (pixel-y pixel) y)
            do (setf (pixel-x pixel) (increment-mod (pixel-x pixel) amount *board-width*)))))

(defun print-board (board)
  (loop with out = nil
        for y below *board-height*
        do (push (str:pad *board-width* "") out)
        finally (loop for pixel in board
                      if (pixel-on pixel)
                        do (setf (char (nth (pixel-y pixel) out) (pixel-x pixel)) #\█)
                      finally (format t "~%~a" (str:join #\newline out)))))

(parseq:defrule board-command-rule ()
    (or (and "rect " (frog:integer-rule) "x" (frog:integer-rule))
        (and "rotate column x=" (frog:integer-rule) " by " (frog:integer-rule))
        (and "rotate row y=" (frog:integer-rule) " by " (frog:integer-rule)))
  (:function (lambda (&rest args)
               (cond ((equal "rotate row y=" (first args)) (make-command-row :y (second args) :amount (fourth args)))
                     ((equal "rotate column x=" (first args)) (make-command-column :x (second args) :amount (fourth args)))
                     ((equal "rect " (first args)) (make-command-rect :x (second args) :y (fourth args)))))))

(defun part-1 (commands)
  (let ((board (loop with out = nil
                     for x below *board-width*
                     do (loop for y below *board-height* do (push (make-pixel :x x :y y) out))
                     finally (return out))))
    (loop for line in commands
          do (run-command (parseq:parseq 'board-command-rule line) board)
          finally (print-board board)
                  (return (count-if #'pixel-on board)))))

(print (time (part-1 (str:lines (str:from-file "../input/day8.txt")))))
