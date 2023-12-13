(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defun nth-safe (n list) (if (< n 0) nil (nth n list)))

(defun reflection (lines multiplier)
  (loop for i from 0 below (1- (length lines))
        for line = (nth i lines)
        for next-line = (nth (1+ i) lines)
        when (equal line next-line)
          do (loop for j from 0
                   for line = (nth-safe (- i j) lines)
                   for next-line = (nth-safe (+ i j 1) lines)
                   if (or (< (- i j) 0)
                          (> (+ i j 1) (1- (length lines))))
                     do (return-from reflection (* multiplier (1+ i)))
                   when (not (equal line next-line)) do (return))
        finally (return 0)))

(defun part-1 (file)
  (loop with images = (str:split frog:+double-newline+ file)
        for image in images
        for i from 0
        sum (+ (reflection (str:lines image) 100)
               (reflection (frog:transpose (mapcar (lambda (l) (coerce l 'list)) (str:lines image))) 1))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 13))))

(defun string-diff (l r)
  (loop for i from 0 below (min (length l) (length r))
        for lc = (char l i) and rc = (char r i)
        count (not (equal lc rc))))

(defun reflection-smudge (lines multiplier)
  (loop for i from 0 below (1- (length lines))
        for line = (nth i lines)
        for next-line = (nth (1+ i) lines)
        when (or (equal line next-line) (= 1 (string-diff line next-line)))
          do (loop with smudges = 0
                   for j from 0
                   for line = (nth-safe (- i j) lines)
                   for next-line = (nth-safe (+ i j 1) lines)
                   if (and (= 1 smudges)
                           (or (< (- i j) 0) (> (+ i j 1) (1- (length lines)))))
                     do (return-from reflection-smudge (* multiplier (1+ i)))
                   do (incf smudges (string-diff line next-line))
                   if (> smudges 1) do (return)
                   else if (or (< (- i j) 0) (> (+ i j 1) (1- (length lines))))
                          do (return))
        finally (return 0)))

(defun part-2 (file)
  (loop with images = (str:split frog:+double-newline+ file)
        for image in images
        for i from 0
        sum (+ (reflection-smudge (str:lines image) 100)
               (reflection-smudge (mapcar (lambda (l) (coerce l 'string))
                                          (frog:transpose (mapcar (lambda (l) (coerce l 'list)) (str:lines image))))
                                  1))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 13))))
