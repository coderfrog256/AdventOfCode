(ql:quickload 'str)

(defun containerize (total container &optional (start 0))
  (cond ((= 0 total) 1)
        ((< total 0) 0)
        (t (loop with out = 0
                 for i from start upto (1- (length container))
                 for nth = (nth i container)
                 do (incf out (containerize (- total nth) container (1+ i)))
                 finally (return out)))))

(containerize 25 '(20 15 10 5 5))
(time
 (containerize 150 (mapcar #'parse-integer (str:lines (str:from-file "../input/day17.txt")))))

;; Part 2
(defun container-count (total container &optional
                                          (start 0)
                                          (path (list))
                                          (len-count (make-hash-table)))
  (cond ((= 0 total)
         (incf (gethash (length path) len-count 0)))
        ((>= total 0)
         (loop for i from start upto (1- (length container))
               for nth = (nth i container)
               do (container-count (- total nth) container (1+ i) (append '(nth) path) len-count))))
  len-count)

(defun print-hash-entry (key value)
    (format t "~A -> ~A~%" key value))
(maphash #'print-hash-entry (container-count 25 '(20 15 10 5 5)))
(time
 (maphash #'print-hash-entry
          (container-count 150 (mapcar #'parse-integer (str:lines (str:from-file "../input/day17.txt"))))))

;; Alternative way of solving part 1, sum up the values.
(let ((count 0))
  (maphash
   (lambda (k v) (incf count v))
   (container-count 150 (mapcar #'parse-integer (str:lines (str:from-file "../input/day17.txt")))))
  count)
