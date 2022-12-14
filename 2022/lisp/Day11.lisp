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
     (and "  Starting items: " (frog:csv-rule (frog:integer-rule)) #\newline)
     (and "  Operation: new = old " (or "* old" (and "+ " (frog:integer-rule)) (and "* " (frog:integer-rule))) #\newline)
     (and "  Test: divisible by " (frog:integer-rule) #\newline)
     (and "    If true: throw to monkey " (frog:integer-rule) #\newline)
     (and "    If false: throw to monkey " (frog:integer-rule) (? #\newline)))
  (:choose '(0 1) '(1 1) '(2 1) '(3 1) '(4 1) '(5 1)) ; Grab the second match within each line. This skips the constant prefixes.
  (:function
   (lambda (id items operation divisible true false)
     (make-instance 'monkey
                    :id id :divisible divisible :true-monkey true :false-monkey false
                    :items (make-array (length items) :fill-pointer (length items) :initial-contents items)
                    :operation (cond ((equal operation "* old") (lambda (old) (* old old)))
                                     ((equal "+ " (first operation)) (lambda (old) (+ old (second operation))))
                                     ((equal "* " (first operation)) (lambda (old) (* old (second operation))))
                                     (t (error (str:concat "Could not assign operation: " operation))))))))

(defun build-monkeys (lines)
  (loop with monkey-arr = (make-array 0 :fill-pointer 0)
        for description in (str:split frog:+double-newline+ lines)
        for (monkey success) = (multiple-value-list (parseq:parseq 'monkey-rule description))
        when (not success) do (error (str:concat "Error parsing monkey: " description))
        do (vector-push-extend monkey monkey-arr)
           (setf (others monkey) monkey-arr)
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
