(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defun part-1 (file)
  (let ((claimed (make-hash-table :test #'equal))
        (cleaned (str:replace-all "." " " file)))
    (loop for line in (str:lines file)
          for y from 0
          do (loop for char across line
                   for x from 0
                   when (and (not (equal char #\.)) (not (digit-char-p char))
                             (setf (gethash (list (1- x) y) claimed) t)
                             (setf (gethash (list (1+ x) y) claimed) t)
                             (setf (gethash (list x (1- y)) claimed) t)
                             (setf (gethash (list x (1+ y)) claimed) t)
                             (setf (gethash (list (1+ x) (1+ y)) claimed) t)
                             (setf (gethash (list (1- x) (1+ y)) claimed) t)
                             (setf (gethash (list (1+ x) (1- y)) claimed) t))
                     do (setf (gethash (list (1- x) (1- y)) claimed) t)))
    (loop with valid = (list)
          for line in (str:lines file)
          for y from 0
          do (loop with working-number = (list)
                   with num-valid = nil
                   for char across line
                   for x from 0
                   do (if (digit-char-p char)
                          (progn
                            (push char working-number)
                            (format t "Space valid: (~a, ~a) ~a (was ~a)~%" x y (gethash (list x y) claimed nil) num-valid)
                            (setf num-valid (or num-valid (gethash (list x y) claimed nil))))
                          (progn
                            (when (and num-valid working-number)
                              (push (parse-integer (coerce (nreverse working-number) 'string)) valid))
                            (setf num-valid nil
                                  working-number nil)))
                   finally (when (and num-valid working-number)
                             (push (parse-integer (coerce (nreverse working-number) 'string)) valid)))
          finally (return (reduce #'+ valid)))))

(print (time (part-1
 (frog:get-advent-of-code-input 2023 3))))

(defun part-2 (file)
  (let ((claimed (make-hash-table :test #'equal))
        (gear-sources (make-hash-table :test #'equal)))
    (loop for line in (str:lines file)
          for y from 0
          do (loop for char across line
                   for x from 0
                   when (and (not (equal char #\.)) (not (digit-char-p char))
                             (setf (gethash (list (1- x) y) claimed) t)
                             (setf (gethash (list (1+ x) y) claimed) t)
                             (setf (gethash (list x (1- y)) claimed) t)
                             (setf (gethash (list x (1+ y)) claimed) t)
                             (setf (gethash (list (1+ x) (1+ y)) claimed) t)
                             (setf (gethash (list (1- x) (1+ y)) claimed) t)
                             (setf (gethash (list (1+ x) (1- y)) claimed) t))
                     do (setf (gethash (list (1- x) (1- y)) claimed) t)))
    (loop with valid = (list)
          for line in (str:lines file)
          for y from 0
          do (loop with working-number = (list)
                   with num-valid = nil
                   with word-start = nil
                   for char across line
                   for x from 0
                   do (if (digit-char-p char)
                          (progn
                            (if (not word-start) (setf word-start x))
                            (push char working-number)
                            (setf num-valid (or num-valid (gethash (list x y) claimed nil))))
                          (progn
                            (when (and num-valid working-number)
                              (let ((x (1- x))
                                    (marked (make-hash-table :test #'equal)))
                                (loop for number-x from word-start to x
                                      do (loop for cells in (list
                                                             (list (1- number-x) y)
                                                             (list (1+ number-x) y)
                                                             (list number-x (1- y))
                                                             (list number-x (1+ y))
                                                             (list (1- number-x) (1- y))
                                                             (list (1+ number-x) (1+ y))
                                                             (list (1- number-x) (1+ y))
                                                             (list (1+ number-x) (1- y)))
                                             do (let ((old (gethash cells gear-sources nil))
                                                      (num (parse-integer (coerce (reverse working-number) 'string))))
                                                  (when (not (gethash cells marked))
                                                    (push num old)
                                                    (format t "Added ~a to ~a @ ~a~%" num old cells)
                                                    (setf (gethash cells marked) t)
                                                    (setf (gethash cells gear-sources) old)))))))
                            (setf num-valid nil
                                  working-number nil
                                  word-start nil)))
                   finally (when (and num-valid working-number)
                             (loop for number-x from word-start to x
                                   with marked = (make-hash-table :test #'equal)
                                   do (loop for cells in (list
                                                          (list (1- number-x) y)
                                                          (list (1+ number-x) y)
                                                          (list number-x (1- y))
                                                          (list number-x (1+ y))
                                                          (list (1- number-x) (1- y))
                                                          (list (1+ number-x) (1+ y))
                                                          (list (1- number-x) (1+ y))
                                                          (list (1+ number-x) (1- y)))
                                            do (let ((old (gethash cells gear-sources nil))
                                                     (num (parse-integer (coerce (reverse working-number) 'string))))
                                                 (when (not (gethash cells marked))
                                                   (push num old)
                                                   (format t "Added ~a to ~a @ ~a~%" num old cells)
                                                   (setf (gethash cells marked) t)
                                                   (setf (gethash cells gear-sources) old))))))))
    (print "Starting gear sources")
    (loop with out = 0
          for line in (str:lines file)
            for y from 0
            do (loop for char across line
                     for x from 0
                     when (equal char #\*)
                       do (let ((sources (gethash (list x y) gear-sources nil)))
                            (format t "Sources (~a ~a): ~a~%" x y sources)
                            (if (= 2 (length sources))
                                (incf out (apply #'* sources)))))
               finally (return out))))

;; Not 62575834
;; Not 32331087
;; Not 62575834, too low
;; Not 62618020
;; (print (time (part-2
;;  (frog:get-advent-of-code-input 2023 3 :input-suffix "test")))

(print (time (part-2
 (frog:get-advent-of-code-input 2023 3))))

;; Check Sources (3 7): (962)
