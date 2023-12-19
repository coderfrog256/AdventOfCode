(ql:quickload '(:str :cl-ppcre :hu.dwim.defclass-star))
(defpackage :advent (:use :cl :cl-ppcre :hu.dwim.defclass-star))
(in-package :advent)
(defclass* part () (x m a s))
(defclass* rule () (name rules))
(defun part-value (part) (with-slots (x m a s) part (+ x m a s)))
(defun get-slot-by-name (slot-name) (case slot-name (#\x 'x) (#\m 'm) (#\a 'a) (#\s 's)))
(defun get-comparison (comparison) (case comparison (#\= '=) (#\> '>) (#\< '<)))

(defun run-workflow (rule part rules-by-name)
  (loop for (predicate destination) in (rules-of rule)
        if (funcall predicate part)
          do (cond ((string= destination "R") (return nil))
                   ((string= destination "A") (return t))
                   (t (return (run-workflow (gethash destination rules-by-name) part rules-by-name))))
        finally (return nil)))

(defun parse-rule (line)
  (let* ((parts (str:split "{" (str:substring 0 -1 line)))
         (name (first parts))
         (rules (mapcar (lambda (r) (str:split ":" r)) (str:split "," (second parts)))))
    (loop with parsed-rules = (list)
          for rule in rules
          if (= 1 (length rule)) do (if (not (string= "R" (first rule)))
                                        (push (list (lambda (x)
                                                      (format t "Always true~%") t) (first rule)) parsed-rules))
          else do (register-groups-bind (slot comparison value) ("(\\w+)([=<>])(\\w+)" (first rule))
                    (push (list (lambda (x) (funcall (get-comparison (char comparison 0))
                                                     (slot-value x (get-slot-by-name (char slot 0)))
                                                     (parse-integer value)))
                                (second rule)) parsed-rules))
          finally (return (make-instance 'rule :name name :rules (nreverse parsed-rules))))))

(defun parse-part (line)
  (register-groups-bind (x m a s) ("\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}" line)
    (make-instance 'part :x (parse-integer x) :m (parse-integer m) :a (parse-integer a) :s (parse-integer s))))

(defun part-1 (file)
  (let* ((instructions (str:split frog:+double-newline+ file))
         (rules (mapcar #'parse-rule (str:lines (first instructions))))
         (rules-by-name (serapeum:dict))
         (parts (mapcar #'parse-part (str:lines (second instructions)))))
    (loop for rule in rules do (setf (gethash (name-of rule) rules-by-name) rule))
    (loop for part in parts if (run-workflow (gethash "in" rules-by-name) part rules-by-name)
          summing (part-value part))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 19))))
