(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))

(defclass* file () (id amount is-empty))
(defmethod print-object ((file file) stream)
  (loop for i from 1 to (amount-of file)
        do (format stream "~A" (if (is-empty-of file) #\.
                                   (code-char (+ (id-of file) (char-code #\0)))))))

(defun parse-input (line)
  (loop with out = (list)
        with next-id = 0
        with is-empty = nil
        for char across (str:trim line)
        for num = (parse-integer (string char))
        do (push (make-instance 'file :id next-id :amount num :is-empty is-empty) out)
           (if (not is-empty) (setf next-id (1+ next-id)))
           (setf is-empty (not is-empty))
        finally (return (nreverse out))))

(defun part-1 (line)
  (loop with parsed = (parse-input line)
        with last-block = (car (last parsed))
        with max = (length parsed)
        with total = 0
        with index = 0
        for block in parsed
        if (is-empty-of block)
          do (loop while (> (amount-of block) 0)
                   for source = (find-if (lambda (x) (and (not (is-empty-of x))
                                                          (> (amount-of x) 0))) parsed :from-end t)
                   if (not source)
                     do (return total)
                   do (incf total (* index (id-of source)))
                      (incf index)
                      (decf (amount-of block))
                      (decf (amount-of source)))
        else
          do (loop while (> (amount-of block) 0)
                   do (incf total (* index (id-of block)))
                      (incf index)
                      (decf (amount-of block)))
        finally (return total)))

(frog:report (part-1 (frog:get-advent-of-code-input 2024 9 :input-suffix "test")))
(frog:report (part-1 (frog:get-advent-of-code-input 2024 9)))

;; Lmao, this solution sucks. But so does part 2.
;; Did you know . is before 0 in the ascii table?
;; Because of this we can just use the utf-8 value of the id to encode the block without collisions.
;; At the end we just subtract the ascii value of 0 from the utf-8 value of the block to get the number.
;; The string would have emojis in it but if you don't render it, it doesn't matter.
(defun part-2 (line)
  (loop with parsed = (parse-input line)
        with string = (str:join "" (mapcar (lambda (b) (format nil "~A" b)) parsed))
        for block in (nreverse parsed)
        for block-str = (format nil "~A" block)
        for search-string = (str:repeat (amount-of block) ".")
        if (not (is-empty-of block))
          do (let ((my-index (search search-string string))
                   (target-index (search block-str string)))
               (when (and my-index target-index (< my-index target-index))
                 (loop for i from 0 below (length block-str)
                       do (setf (char string (+ my-index i)) (char block-str i)
                                (char string (+ target-index i)) (char search-string i)))))
        finally (return (loop for char across string
                              for i from 0
                              unless (equal #\. char) ;; Safe because . is before 0 in ascii.
                                summing (* i (- (char-code char) (char-code #\0)))))))

(frog:report (part-2 (frog:get-advent-of-code-input 2024 9 :input-suffix "test")))
(frog:report (part-2 (frog:get-advent-of-code-input 2024 9)))
