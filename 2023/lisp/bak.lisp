(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
(defun get-matches (line)
  (let ((winning (mapcar #'parse-integer
                         (remove-if (lambda (x) (= 0 (length x)))
                                    (str:split " "
                                               (first (str:split "|" (second (str:split ": " line))))))))
        (hand (mapcar #'parse-integer
                      (remove-if (lambda (x) (= 0 (length x)))
                                 (str:split " "
                                            (second (str:split "|" (second (str:split ": " line)))))))))
    (loop with win-count = 0 and out = 0
          for num in hand
          when (member num winning)
            do (incf win-count)
          finally (return
                    (if (= win-count 0)
                        0
                        (expt 2 (1- win-count)))))))

(defun part-1 (file)
  (loop for line in (str:lines file)
        summing (get-matches line)))

(print (part-1
        (frog:get-advent-of-code-input 2023 4)))

(defun get-match-count (line)
  (let ((winning (mapcar #'parse-integer
                         (remove-if (lambda (x) (= 0 (length x)))
                                    (str:split " "
                                               (first (str:split "|" (second (str:split ": " line))))))))
        (hand (mapcar #'parse-integer
                      (remove-if (lambda (x) (= 0 (length x)))
                                 (str:split " "
                                            (second (str:split "|" (second (str:split ": " line)))))))))
    (loop with win-count = 0 and out = 0
          for num in hand
          when (member num winning)
            do (incf win-count)
          finally (return win-count))))

(defun part-2 (file)
  (let* ((lines (str:lines file))
         (pile (make-hash-table)))
    (loop for i upto (length lines)
          do (setf (gethash i pile) 1))
    (loop with cards-played = 0
          for card from 0 below (length lines)
          for plays = (gethash card pile)
          for wins = (get-match-count (nth card lines))
          do (incf cards-played plays)
             (loop for win below wins
                   do (incf (gethash (+ 1 card win) pile) plays))
          finally (return cards-played))))


(print
(time
(part-2
 (frog:get-advent-of-code-input 2023 4))))
