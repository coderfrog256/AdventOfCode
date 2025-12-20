(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel :alexandria))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defclass* range () (start end))
(defmethod print-object ((obj range) stream) ; For debugging
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A-~A" (start-of obj) (end-of obj))))

(defun merge-range (l r) ;Assumes input is sorted by start (r cannot start BEFORE l)
  (let* ((lstart (start-of l)) (rstart (start-of r))
         (lend (end-of l)) (rend (end-of r)))
    (if (> rstart lend) nil ; return nil if cannot be merged
        (make-instance 'range :start lstart :end (max rend lend)))))

(defun merge-ranges (ranges) ; Repeatedly try to merge ranges
  (loop with sorted = (sort ranges (lambda (l r) (< (start-of l) (start-of r))))
        for i from 0
        for working = (nth i sorted)
        if working do (loop with out = (subseq sorted 0 i)
                            for j from (1+ i) below (length sorted)
                            for rhs = (nth j sorted)
                            for merged = (merge-range working rhs)
                            if merged do (setf working merged)
                              else do (push rhs out)
                            finally (push working out)
                                    (setf sorted (sort out (lambda (l r) (< (start-of l) (start-of r)))))
                                    (setf out (list)))
          else do (return sorted)))

(defun in-range (range i) (and (>= i (start-of range)) (<= i (end-of range))))
(defun in-ranges (ranges i) (find-if (lambda (r) (in-range r i)) ranges))
(defun range-size (range) (1+ (- (end-of range) (start-of range))))

(defun part-1 (input)
  (let* ((parts (str:split frog:+double-newline+ input))
         (ranges (mapcar (lambda (l)
                           (destructuring-bind (start end) (str:split "-" l)
                             (make-instance 'range :start (parse-integer start) :end (parse-integer end))))
                         (str:lines (first parts))))
         (items (mapcar #'parse-integer (str:lines (second parts)))))
    (length (remove-if-not (lambda (i) (in-ranges ranges i)) items))))

(frog:report (part-1 (frog:get-advent-of-code-input 2025 5)))

(defun part-2 (input)
  (let* ((parts (str:split frog:+double-newline+ input))
         (ranges (mapcar (lambda (l)
                           (destructuring-bind (start end) (str:split "-" l)
                             (make-instance 'range :start (parse-integer start) :end (parse-integer end))))
                         (str:lines (first parts))))
         (merged (merge-ranges ranges)))
    (reduce #'+ (mapcar #'range-size merged))))

(frog:report (part-2 (frog:get-advent-of-code-input 2025 5 :input-suffix "test")))
(frog:report (part-2 (frog:get-advent-of-code-input 2025 5)))
