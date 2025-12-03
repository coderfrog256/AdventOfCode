(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
(setf lparallel:*kernel* (lparallel:make-kernel 15))

(defun is-dupe (str)
  (if (zerop (mod (length str) 2))
      (loop with end = (floor (length str) 2)
            for i from 0 upto end
            for j from end upto (1- (length str))
            if (not (char= (char str i) (char str j))) do (return nil)
              finally (return t))))

(defun part-1 (input)
  (loop for part in (str:split "," input)
        for (start end) = (str:split "-" part)
        sum (loop for i from (parse-integer start) to (parse-integer end)
                  when (is-dupe (write-to-string i)) sum i)))

(frog:report (part-1 (str:join "" (str:lines (frog:get-advent-of-code-input 2025 2)))))

(defclass* generator () (input (index 0)))

(defun advance (generator)
  (with-slots (input index) generator
    (if (>= (incf index) (length input)) nil (str:substring 0 index input))))

(defun all-dupes (prefix str)
  (if (zerop (mod (length str) (length prefix)))
      (loop with len = (length prefix)
            for i from 1 upto (1- (floor (length str) len))
            for group = (str:substring (* i len) (+ len (* i len)) str)
            if (not (equal prefix group)) do (return nil)
              finally (return t))))

(defun is-dupe-2 (str)
  (loop with gen = (make-instance 'generator :input str)
        for slice = (advance gen) until (null slice)
        if (all-dupes slice str) do (return slice)
          finally (return nil)))

(defun part-2 (input)
  (->> input
    (str:split ",")
    (lparallel:pmap 'list
                    (lambda (line)
                      (loop with parts = (str:split "-" line)
                            for i from (parse-integer (first parts)) to (parse-integer (second parts))
                            when (is-dupe-2 (write-to-string i)) sum i)))
    (reduce #'+)))

(frog:report (part-2 (str:join "" (str:lines (frog:get-advent-of-code-input 2025 2)))))
