(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :function-cache))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
(defparameter *cave-cache* (make-instance 'function-cache:lru-cache :capacity 1000000))
(defclass* cube () (x y))
(defclass* rock () (x y))

(defun copy-stone (self) (make-instance (if (rockp self) 'rock 'cube) :x (x-of self) :y (y-of self)))
(defun copy-cave (cave)
  (loop with out = (serapeum:dict)
        for pos being the hash-keys of cave using (hash-value rock)
        do (setf (gethash pos out) (copy-stone rock))
        finally (return out)))

(defun roll-vertical (rock cave direction max-y)
  (with-slots (x y) rock
    (setf y (-<>> cave
              (alexandria:hash-table-values)
              (remove-if-not (lambda (rock) (and (= (x-of rock) x)
                                                 (if (equal direction :up)
                                                     (< (y-of rock) y)
                                                     (> (y-of rock) y)))))
              (reduce (if (equal direction :up) #'max #'min) <> :key #'y-of :initial-value (if (equal direction :up) -1 max-y))
              (+ (if (equal direction :up) 1 -1))))))

(defun roll-horizontal (rock cave direction max-x)
  (with-slots (x y) rock
    (setf x (-<>> cave
              (alexandria:hash-table-values)
              (remove-if-not (lambda (rock) (and (= (y-of rock) y)
                                                 (if (equal direction :left)
                                                     (< (x-of rock) x)
                                                     (> (x-of rock) x)))))
              (reduce (if (equal direction :left) #'max #'min) <> :key #'x-of :initial-value (if (equal direction :left) -1 max-x))
              (+ (if (equal direction :left) 1 -1))))))

(defmacro move (outer-iter inner-iter direction)
  (let* ((roll (if (member direction '(:up :down)) 'roll-vertical 'roll-horizontal))
         (max (if (member direction '(:up :down)) 'max-y 'max-x))
         (update (if (member direction '(:up :down)) `(list (,roll rock cave ,direction ,max) x) `(list y (,roll rock cave ,direction ,max)))))
    `(loop ,@outer-iter
           do (loop ,@inner-iter
                    for rock = (gethash (list y x) cave)
                    if (rockp rock) do (remhash (list y x) cave)
                                       (setf (gethash ,update cave) rock)))))

(defun move-up (cave max-x max-y) (move (for y from 0 below max-y) (for x from 0 below max-x) :up))
(defun move-down (cave max-x max-y) (move (for y from (1- max-y) downto 0) (for x from 0 below max-x) :down))
(defun move-left (cave max-x max-y) (move (for x from 0 below max-x) (for y from 0 below max-y) :left))
(defun move-right (cave max-x max-y) (move (for x from (1- max-x) downto 0) (for y from 0 below max-y) :right))

(defun parse-cave (file)
  (loop with lines = (str:lines file)
        with out = (serapeum:dict)
        for y from 0 below (length lines)
        for line = (nth y lines)
        maximize (loop for x from 0 below (length line)
                       for c = (char line x)
                       if (char= c #\O) do (setf (gethash (list y x) out) (make-instance 'rock :x x :y y))
                       else if (char= c #\#) do (setf (gethash (list y x) out) (make-instance 'cube :x x :y y))
                       finally (return x)) into max-x
        finally (return (list out max-x y))))

(defun cave-string (cave max-x max-y)
  (intern (with-output-to-string (sb)
    (loop for y from 0 below max-y
          do (loop for x from 0 below max-x
                   for rock = (gethash (list y x) cave)
                   if (rockp rock) do (format sb "O")
                   else if (cubep rock) do (format sb "#")
                   else do (format sb "."))
             (format sb "~%")
          finally (format sb "~%")))))

(defun part-1 (file)
  (let* ((parsed (parse-cave file))
         (cave (first parsed))
         (max-x (second parsed)) (max-y (third parsed)))
    (move-up cave max-x max-y)
    (reduce #'+ (remove-if-not #'rockp (alexandria:hash-table-values cave))
            :key (lambda (r) (1+ (- max-y (y-of r) 1 ))))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 14))))

(defun cycle (cave key max-x max-y)
  (let* ((cached (function-cache:get-cached-value *cave-cache* key)) new-key)
    (when cached (return-from cycle cached))
    (mapc (lambda (f) (funcall f cave max-x max-y)) '(move-up move-left move-down move-right))
    (setf new-key (cave-string cave max-x max-y)
          (function-cache:get-cached-value *cave-cache* key) (list (copy-cave cave) new-key))
    (list cave new-key)))

(defun part-2 (file)
  (let* ((parsed (parse-cave file))
         (cave (first parsed))
         (max-x (second parsed)) (max-y (third parsed))
         (key (cave-string cave max-x max-y)))
    (dotimes (i 1000000000)
      (when (zerop (mod i 100000)) (format t "~a~%" i)) ; Heartbeat
      (let ((cycled (cycle cave key max-x max-y)))
        (setf cave (first cycled) key (second cycled))))
    (reduce #'+ (remove-if-not #'rockp (alexandria:hash-table-values cave))
            :key (lambda (r) (1+ (- max-y (y-of r) 1 ))))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 14))))
