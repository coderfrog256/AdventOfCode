(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache :lparallel))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(setf lparallel:*kernel* (lparallel:make-kernel 16))
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
(defclass* beam () (y x delta-y delta-x))

(defmethod print-object ((beam beam) stream)
  (with-slots (delta-x delta-y x y) beam
    (format stream "~a ~a ~a ~a" delta-y delta-x y x)))
;; NOTE: Uses the frog:copy-instance helper. This copies an instance, but allows you to override some slots.
(defun handle-tilted-mirror (mirror beam)
  (with-slots (delta-x delta-y x y) beam
    (if (char= mirror #\/)
        (cond
          ((> delta-x 0) (list (frog:copy-instance beam :delta-x 0 :delta-y -1)))
          ((< delta-x 0) (list (frog:copy-instance beam :delta-x 0 :delta-y 1)))
          ((> delta-y 0) (list (frog:copy-instance beam :delta-x -1 :delta-y 0)))
          ((< delta-y 0) (list (frog:copy-instance beam :delta-x 1 :delta-y 0))))
        (cond ; char is #\\
          ((> delta-x 0) (list (frog:copy-instance beam :delta-x 0 :delta-y 1)))
          ((< delta-x 0) (list (frog:copy-instance beam :delta-x 0 :delta-y -1)))
          ((> delta-y 0) (list (frog:copy-instance beam :delta-x 1 :delta-y 0)))
          ((< delta-y 0) (list (frog:copy-instance beam :delta-x -1 :delta-y 0)))))))

(defun handle-mirror (mirror beam)
  (with-slots (delta-x delta-y x y) beam
    (cond
      ((null mirror) (list beam))
      ((and (/= 0 delta-x) (char= mirror #\|)) (list (frog:copy-instance beam :delta-x 0 :delta-y 1)
                                                     (frog:copy-instance beam :delta-x 0 :delta-y -1)))
      ((and (/= 0 delta-y) (char= mirror #\-)) (list (frog:copy-instance beam :delta-x 1 :delta-y 0)
                                                     (frog:copy-instance beam :delta-x -1 :delta-y 0)))
      ((or (char= mirror #\/) (char= mirror #\\)) (handle-tilted-mirror mirror beam))
      (t (list beam)))))

(defun tick (beam grid max-y max-x)
  (with-slots (delta-x delta-y x y) beam
    (if (or (>= x max-x) (>= y max-y) (< x 0) (< y 0)) nil
        (let* ((new-x (+ x delta-x)) (new-y (+ y delta-y))
               (mirror (gethash (list new-y new-x) grid)))
          (setf x new-x)
          (setf y new-y)
          (handle-mirror mirror beam)))))

(defun parse-maze (file)
  (loop with lines = (str:lines file) and out = (serapeum:dict)
        for line in lines and y from 0
        maximize (loop for char across line and x from 0
                       do (setf (gethash (list y x) out) char)
                       finally (return x)) into max-x
        finally (return (list out (1+ y) (1+ max-x)))))

(defun run-maze (maze initial-beam)
  (loop with should-continue = t
        with seen-beams = (list) and lit = (serapeum:dict)
        with mirrors = (first maze) and max-y = (second maze) and max-x = (third maze)
        with beams = (handle-mirror (gethash (list (y-of initial-beam) (x-of initial-beam)) mirrors) initial-beam)
        while should-continue
        do (loop for beam in beams do (setf (gethash (list (y-of beam) (x-of beam)) lit) t))
           (setf beams (remove-if (lambda (beam) (or (< (x-of beam) 0)
                                                     (< (y-of beam) 0)
                                                     (>= (x-of beam) max-x)
                                                     (>= (y-of beam) max-y)))
                                  (->> beams (mapcar (lambda (beam) (tick beam mirrors max-y max-x)))
                                       (alexandria:flatten))))
           (let* ((beam-positions (mapcar (lambda (b) (list b (format nil "~a" b))) beams))
                  (new-beams (remove-if (lambda (b) (member b seen-beams :test #'equal)) beam-positions :key #'second)))
             (setf should-continue (not (null new-beams)))
             (setf seen-beams (nconc seen-beams (mapcar #'second new-beams)))
             (setf beams (mapcar #'first new-beams)))
        finally (return (length (alexandria:hash-table-keys lit)))))

(print (time (run-maze (parse-maze (frog:get-advent-of-code-input 2023 16 :input-suffix "test"))
                       (make-instance 'beam :delta-y 0 :delta-x 1 :x 0 :y 0))))
(print (time (run-maze (parse-maze (frog:get-advent-of-code-input 2023 16))
                       (make-instance 'beam :delta-y 0 :delta-x 1 :x 0 :y 0))))

(defun part-2 (file)
  (let* ((parsed (parse-maze file))
         (maze (first parsed)) (max-y (second parsed)) (max-x (third parsed))
         (rays (list)))
    (loop for y from 0 below max-y
          do (push (make-instance 'beam :delta-y 0 :delta-x 1 :x 0 :y y) rays)
             (push (make-instance 'beam :delta-y 0 :delta-x -1 :x max-x :y y) rays))
    (loop for x from 0 below max-x
          do (push (make-instance 'beam :delta-y 1 :delta-x 0 :x x :y 0) rays)
             (push (make-instance 'beam :delta-y -1 :delta-x 0 :x x :y max-y) rays))
    (reduce #'max (lparallel:pmapcar (lambda (beam) (run-maze (list maze max-y max-x) beam)) rays) :initial-value 0)))

(print (time (part-2 (frog:get-advent-of-code-input 2023 16 :input-suffix "test"))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 16))))
