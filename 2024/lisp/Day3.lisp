(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(parseq:defrule mul () (and "mul(" (frog:integer-rule) "," (frog:integer-rule) ")")
  (:function (lambda (&rest args) (* (nth 1 args) (nth 3 args)))))

(parseq:defrule do () (and "do()")
  (:function (lambda (&rest args) :do)))

(parseq:defrule don-t () (and "don't()")
  (:function (lambda (&rest args) :don-t)))

(parseq:defrule part-1-parser () (* (or mul char))
  (:function (lambda (&rest args) (remove-if #'characterp args))))

(parseq:defrule part-2-parser () (* (or mul do don-t char))
  (:function (lambda (&rest args) (remove-if #'characterp args))))

(defun part-1 (input) (reduce #'+ (parseq:parseq 'part-1-parser input)))
(frog:report (part-1 (frog:get-advent-of-code-input 2024 3 :input-suffix "test")))
(frog:report (part-1 (frog:get-advent-of-code-input 2024 3)))

(defun part-2 (input)
  (loop with enabled = t
        for i in (parseq:parseq 'part-2-parser input)
        if (eq i :do) do (setf enabled t)
        else if (eq i :don-t) do (setf enabled nil)
        else if enabled sum i))
(frog:report (part-2 (frog:get-advent-of-code-input 2024 3 :input-suffix "test")))
(frog:report (part-2 (frog:get-advent-of-code-input 2024 3)))
