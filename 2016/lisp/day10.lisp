(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defparameter *search-ids* (list 17 61))
(defclass* bot () (id low-target high-target (contents nil)))

(defun parse-bot (line bots)
  (cl-ppcre:register-groups-bind (id low high)
      ("(bot \\d+) gives low to ((?:bot|output) \\d+) and high to ((?:bot|output) \\d+)" line)
    (let ((bot (make-instance 'bot :id id :low-target low :high-target high)))
      (setf (gethash id bots) bot)
      (if (str:starts-with-p "output" low) (setf (gethash low bots) (make-instance 'bot :id low)))
      (if (str:starts-with-p "output" high) (setf (gethash high bots) (make-instance 'bot :id high))))))

(defun hand-chip (bots bot-id amount)
  (let ((bot (gethash bot-id bots)))
    (with-slots (contents low-target high-target) bot
      (push amount contents)
      (when (= 2 (length contents))
        (sort contents #'<)
        (when (equal contents *search-ids*) (format t "Bot Found: ~A~%" bot-id))
        (let* ((low  (pop contents))
               (high (pop contents)))
          (hand-chip bots low-target low)
          (hand-chip bots high-target high))))))

(defun parse-value (line bots)
  (cl-ppcre:register-groups-bind (amount bot)  ("value (\\d+) goes to ((?:bot|output) \\d+)" line)
    (hand-chip bots bot (parse-integer amount))))

(defun part-1 (lines)
  (let ((bots (make-hash-table :test #'equal)))
    (loop for line in lines when (str:starts-with-p "bot" line) do (parse-bot line bots))
    (loop for line in lines
          when (str:starts-with-p "value" line) do (parse-value line bots)
          finally (return bots))))

(part-1 (str:lines (str:from-file "../input/day10.txt")))

(defun part-2 (lines)
  (let* ((bots (part-1 lines))
         (out-0 (gethash "output 0" bots)) (out-1 (gethash "output 1" bots)) (out-2 (gethash "output 2" bots)))
    (reduce #'* (concatenate 'list (contents-of out-0) (contents-of out-1) (contents-of out-2)))))

(print (time (part-2 (str:lines (str:from-file "../input/day10.txt")))))
