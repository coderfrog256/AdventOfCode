(ql:quickload :str)
(ql:quickload :cl-json)
(ql:quickload :trivia)

(defun get-inner-num (input &key part-2)
  (flet ((alist-item-p (l) (and
                            (listp l)
                            (symbolp (car l)))))
    (or
     (trivia:match input
       ((type fixnum) input)
       ((type list)
        (loop for item in input
              sum (if (alist-item-p item)
                      (if (and part-2 (equal "red" (cdr item)))
                          (return-from get-inner-num 0)
                          (get-inner-num (cdr item) :part-2 part-2))
                      (get-inner-num item :part-2 part-2)) into out
              finally (return out))))
     0)))

(get-inner-num (json:decode-json-from-string (str:from-file "../input/day12-real.txt")))
(get-inner-num (json:decode-json-from-string (str:from-file "../input/day12-real.txt")) :part-2 t)

;; Tests
(loop for line in (str:lines (str:from-file "../input/day12.txt"))
      for parsed = (json:decode-json-from-string line)
      do (format t "~A - ~A~%" line
                 (get-inner-num parsed)))

(loop for line in (str:lines (str:from-file "../input/day12-real.txt"))
      do (format t "~A - ~A~%" line
                 (get-inner-num
                  (json:decode-json-from-string line))))

(loop for line in (str:lines (str:from-file "../input/day12-part2.txt"))
      for parsed = (json:decode-json-from-string line)
      do (format t "~A - ~A~%" line
                 (get-inner-num parsed :part-2 t)))

(loop for line in (str:lines (str:from-file "../input/day12-real.txt"))
      for parsed = (json:decode-json-from-string line)
      do (format t "~A - ~A~%" line
                 (get-inner-num parsed :part-2 t)))
