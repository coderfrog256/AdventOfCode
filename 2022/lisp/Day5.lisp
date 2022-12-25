(ql:quickload '(:str :cl-ppcre :binding-arrows))
(defpackage :advent (:use :cl :binding-arrows))
(in-package :advent)

(defun parse-boxes (lines)
  (->> lines
    (remove-if #'str:blankp)
    (remove-if-not (lambda (line) (equal #\[ (char (str:trim-left line) 0))))
    (mapcar (lambda (line)
              (loop with out = '()
                    for i from 0 to (/ (length line) 4)
                    for char = (char line (1+ (* 4 i)))
                    when (not (equal char #\SPACE))
                      do (setf out (acons (1+ i) char out))
                    finally (return out))))))

(defun build-stacks (boxes)
  (loop with out = (make-hash-table :test #'equal)
        for row in boxes
        do (loop for (index . box) in row
                 for column = (gethash index out '())
                 do (push box column)
                    (setf (gethash index out) column))
        finally (return out)))

(defun flip-stacks (boxes)
  (loop for key being the hash-key of boxes using (hash-value value)
        do (setf (gethash key boxes) (reverse value))
        finally (return boxes)))

(defun parse-commands (line)
  (register-groups-bind (amount from to) ("move (\\d+) from (\\d+) to (\\d+)" line)
    (list (parse-integer amount) (parse-integer from) (parse-integer to))))

(defun execute-commands (command-file phase)
  (let ((boxes (->> command-file (str:from-file) (str:lines)
                 (parse-boxes)
                 (build-stacks)
                 (flip-stacks)))
        (commands (->> command-file (str:from-file) (str:lines)
                    (remove-if-not (lambda (line) (str:starts-with-p "move" line)))
                    (mapcar #'parse-commands))))
    (if (= phase 1)
        (loop for (amount from to) in commands
              do (loop for i from 0 to (1- amount)
                       for box = (pop (gethash from boxes))
                       for to-stack = (gethash to boxes)
                       do (push box to-stack)
                          (setf (gethash to boxes) to-stack)))
        (loop for (amount from to) in commands
              do (loop for i from 0 to (1- amount)
                       with stack = '()
                       do (push (pop (gethash from boxes)) stack)
                       finally (dolist (item stack) (push item (gethash to boxes))))))
    (loop with out = '()
          for key being the hash-key of boxes using (hash-value value)
          do (setf out (acons key (first value) out))
          finally (return (coerce (mapcar #'cdr
                                          (sort out (lambda (l r) (< (car l) (car r))))) 'string)))))

(time (print (execute-commands "../input/day5.txt" 1)))
(time (print (execute-commands "../input/day5.txt" 2)))
