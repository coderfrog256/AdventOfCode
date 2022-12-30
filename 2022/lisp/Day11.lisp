(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defclass monkey ()
  ((id :initarg :id :accessor id)
   (operation :initarg :operation :accessor operation)
   (divisible :initarg :divisible :accessor divisible)
   (true-monkey :initarg :true-monkey :accessor true-monkey)
   (false-monkey :initarg :false-monkey :accessor false-monkey)
   (items :initarg :items :accessor monkey-items)
   (others :initarg :others :accessor others)
   (num-inspected :initform 0 :accessor monkey-inspected)))

(defmethod initialize-instance :after ((m monkey) &rest args)
  (vector-push-extend m (others m)))

(defmethod print-object ((m monkey) out) ; Not relevant, but good for debugging. Keeping as a reference.
  (print-unreadable-object (m out :type t)
    (format out "~A (~A) div=~A true=~A false=~A" (id m) (monkey-items m) (divisible m) (true-monkey m) (false-monkey m))))

(defun run-monkey (monkey worry-divisor lcm)
  (loop for item across (monkey-items monkey)
        do (incf (monkey-inspected monkey))
           (setf item (floor (mod (funcall (operation monkey) item) lcm) worry-divisor))
           (vector-push-extend item (monkey-items (aref (others monkey)
                                                        (if (zerop (mod item (divisible monkey)))
                                                            (true-monkey monkey)
                                                            (false-monkey monkey)))))
           (setf (fill-pointer (monkey-items monkey)) 0)))

(parseq:defrule monkey-rule ()
    (and
     (and "Monkey " (frog:integer-rule) ":" #\newline)
     (and "  Starting items: " (frog:csv-rule (frog:integer-rule)) "" #\newline)
     (and "  Operation: new = old " (or "* old" (and "+ " (frog:integer-rule)) (and "* " (frog:integer-rule))) "" #\newline)
     (and "  Test: divisible by " (frog:integer-rule) "" #\newline)
     (and "    If true: throw to monkey " (frog:integer-rule) "" #\newline)
     (and "    If false: throw to monkey " (frog:integer-rule) (? #\newline)))
  (:choose '(0 1) '(1 1) '(2 1) '(3 1) '(4 1) '(5 1)))

(defun parse-monkey (description monkey-array)
  (let (id items operation divisible true-monkey false-monkey
           (parse-result (parseq:parseq 'monkey-rule description)))
    (destructuring-bind (id-in items-in operation-in divisible-in true-in false-in) parse-result
      (setf id id-in)
      (setf items (make-array (length items-in) :fill-pointer (length items-in) :initial-contents items-in))
      (setf divisible divisible-in)
      (setf true-monkey true-in)
      (setf false-monkey false-in)
      (cond ((equal operation-in "* old") (setf operation (lambda (old) (* old old))))
            ((equal "+ " (first operation-in)) (setf operation (lambda (old) (+ old (second operation-in)))))
            ((equal "* " (first operation-in)) (setf operation (lambda (old) (* old (second operation-in)))))))
    (if (null operation) (error (str:concat "Could not assign operation: " (nth 2 description))))
    (make-instance 'monkey :id id :operation operation :divisible divisible
                           :true-monkey true-monkey :false-monkey false-monkey
                           :items items :others monkey-array)))

(defun build-monkeys (lines)
  (loop with monkey-arr = (make-array 0 :fill-pointer 0)
        for description in (str:split (coerce (list #\newline #\newline) 'string) lines)
        do (print description)
        do (parse-monkey description monkey-arr)
        finally (return monkey-arr)))

(defun play-monkey-ball (loops worry monkeys)
  (let ((lcm 1))
    (loop for m across monkeys do (setf lcm (* lcm (divisible m))))
    (dotimes (n loops) (loop for m across monkeys do (run-monkey m worry lcm))))
  (->>
    (coerce monkeys 'list)
    (mapcar (lambda (m) (monkey-inspected m)))
    (frog:sort-r #'>)
    (frog:subseq-r 0 2)
    (reduce #'*)))

(time (print (->> "../input/day11.txt" (str:from-file) (build-monkeys) (play-monkey-ball 20 3))))
(time (print (->> "../input/day11.txt" (str:from-file) (build-monkeys) (play-monkey-ball 10000 1))))
