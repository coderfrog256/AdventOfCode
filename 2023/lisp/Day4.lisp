(ql:quickload '(:str))
(defpackage :advent (:use :cl))
(in-package :advent)

(defun get-matches (line)
  (loop with sides = (str:split "|" (second (str:split ": " line)))
        with winning = (read-from-string (str:concat "(" (first sides) ")"))
        with hand =  (read-from-string (str:concat "(" (second sides) ")"))
        with win-count = 0 and out = 0
        for num in hand
        when (member num winning) do (incf win-count)
          finally (return win-count)))

(defun part-1 (file)
  (loop for line in (str:lines file)
        for matches = (get-matches line)
        when (/= matches 0) summing (expt 2 (1- matches))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 4))))

(defun part-2 (file)
  (loop with lines = (str:lines file) and pile = (make-hash-table) and cards-played = 0
        for card from 0 below (length lines)
        for plays = (gethash card pile 1)
        for wins = (get-matches (nth card lines))
        do (incf cards-played plays)
           (loop for win below wins do (incf (gethash (+ 1 card win) pile 1) plays))
        finally (return cards-played)))
(print (time (part-2 (frog:get-advent-of-code-input 2023 4))))
