(ql:quickload 'split-sequence)
(ql:quickload 'str)

(defun build-elves (backpack)
  (mapcar
   (lambda (food) (reduce #'+ food :key #'parse-integer))
   (split-sequence:split-sequence "" backpack :test #'equal)))

;; Part 1
(print (reduce #'max
        (build-elves (str:lines (str:from-file "../input/day1.txt")))))

;; Part 2
(print (reduce #'+
        (subseq
         (sort
          (build-elves (str:lines (str:from-file "../input/day1.txt"))) #'>)
         0 3)))
