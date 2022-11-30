(defun get-square-feet (l w h)
  (let ((lw (* 2 l w))
        (wh (* 2 w h))
        (lh (* 2 l h)))
    (let ((min-side (min (* l w) (* w h) (* l h))))
      (+ lw wh lh min-side))))

(get-square-feet 2 3 4)
(get-square-feet 1 1 10)

(destructuring-bind (l w h) (uiop:split-string "1x2x3" :separator "x")
  (format t "~A ~A ~A" l w h))

(print (with-open-file (f "../input/day2.txt")
         (loop for line = (read-line f nil)
               while line
               for (l w h) = (uiop:split-string line :separator "x")
               sum (get-square-feet (parse-integer l)
                                    (parse-integer w)
                                    (parse-integer h)) into result
               finally (return result))))

;; Just part 1.
