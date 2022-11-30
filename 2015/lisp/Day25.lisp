(declaim (optimize (speed 3) (safety 0)))

(defun last-cell (array x y)
  (if (< (- x 1) 1)
      (aref array (- y 1) 1)
      (aref array (- x 1) (+ y 1))))

(defun get-array-cell (end-x end-y)
  (let ((array (make-array '(10000 10000))))
    (setf (aref array 1 1) 20151125)
    (loop with x = 1
          with y = 2
          do (format t "~A / ~A~%" x y)
          do (progn
               (setf (aref array x y) (rem (* (last-cell array x y) 252533) 33554393))
               (if (and (= x end-x) (= y end-y))
                   (return (aref array x y)))
               (incf x)
               (decf y)
               (when (< y 1)
                 (setf y x)
                 (setf x 1))))))

(print (get-array-cell 5 5))

(print (get-array-cell 3083 2978))
