(ql:quickload '(:str :access :alexandria :binding-arrows))
(defpackage :advent (:use :cl :binding-arrows))
(in-package :advent)

(defmacro for-each-neighbor ((source-x source-y) (target-x target-y) &body body)
  `(loop for (,target-x ,target-y) in (list (list (1- ,source-x) ,source-y) (list (1+ ,source-x) ,source-y)
                                            (list ,source-x (1- ,source-y)) (list ,source-x (1+ ,source-y))
                                            (list (1- ,source-x) (1- ,source-y)) (list (1- ,source-x) (1+ ,source-y))
                                            (list (1+ ,source-x) (1+ ,source-y)) (list (1+ ,source-x) (1- ,source-y)))
         when (and (>= ,target-x 0) (>= ,target-y 0)) do ,@body))

(defun run-solution (file)
  (let ((input (coerce (str:lines file) 'vector))
        (claimed (make-hash-table :test #'equal))
        (gear-sources (make-hash-table :test #'equal))
        (valid (list)))
    (labels ((handle-number-end (x y word-start working-number num-valid)
               (when (and num-valid working-number)
                 (let ((num (parse-integer (coerce (nreverse working-number) 'string)))
                       (marked (make-hash-table :test #'equal)))
                   (push num valid)
                   (loop for index-x from word-start to x
                         do (for-each-neighbor (index-x y) (nx ny)
                              (let ((old (gethash (list nx ny) gear-sources nil)))
                                (when (and (equal #\* (access:accesses input ny nx)) (not (gethash (list nx ny) marked)))
                                  (push num old)
                                  (setf (gethash (list nx ny) marked) t (gethash (list nx ny) gear-sources) old)))))))))

      (loop for line across input and y from 0
            do (loop for char across line and x from 0
                     when (and (not (equal char #\.)) (not (digit-char-p char)))
                       do (for-each-neighbor (x y) (nx ny)
                            (setf (gethash (list nx ny) claimed) t))))

      (loop for line across input and y from 0
            do (loop with working-number = (list) and num-valid = nil and word-start = nil
                     for char across line and x from 0
                     if (digit-char-p char) do (if (not word-start) (setf word-start x))
                                               (push char working-number)
                                               (setf num-valid (or num-valid (gethash (list x y) claimed nil)))
                     else do (handle-number-end (1- x) y word-start working-number num-valid)
                             (setf num-valid nil working-number nil word-start nil)
                     finally (handle-number-end x y word-start working-number num-valid))
            finally (format t "Solution 1: ~a~%" (reduce #'+ valid)))
      (->> (alexandria:hash-table-values gear-sources)
        (remove-if-not (lambda (x) (= 2 (length x))))
        (mapcar (lambda (x) (apply #'* x)))
        (reduce #'+)))))

(print (time (run-solution (frog:get-advent-of-code-input 2023 3))))
