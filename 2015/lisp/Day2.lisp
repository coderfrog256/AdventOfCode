(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun get-square-feet (l w h)
  (let* ((lw (* 2 l w))
         (wh (* 2 w h))
         (lh (* 2 l h))
         (min-side (min (* l w) (* w h) (* l h))))
      (+ lw wh lh min-side)))

(get-square-feet 2 3 4)
(get-square-feet 1 1 10)

(->> "../input/day2.txt"
  (str:from-file)
  (str:lines)
  (mapcar (lambda (line) (str:split "x" line)))
  (mapcar (lambda (parts) (get-square-feet (parse-integer (first parts))
                                           (parse-integer (second parts))
                                           (parse-integer (third parts)))))
  (reduce #'+))

;; Just part 1.
