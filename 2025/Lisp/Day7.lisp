(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel :alexandria))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))

(defun parse-grid (input)
  (loop with lines = (str:lines input) with out = (serapeum:dict)
        with start = nil
        for y from 0 below (length lines)
        for line = (nth y lines)
        do (loop for x from 0 below (length line)
                 for char = (char line x)
                 if (char= char #\S)
                   do (setf start (cons x y))
                 if (char= char #\^)
                   do (setf (gethash (cons x y) out) t))
        finally (return (values out start))))

(defun part-1 (input)
  (multiple-value-bind (grid start) (parse-grid input)
    (let* ((end (reduce #'max (mapcar #'cdr (alexandria:hash-table-keys grid))))
           (splits 0)
           (beams (list start)))
      (loop for y from 0 upto end
            do (loop with new-beams = (list)
                     for beam in beams
                     for (x . y) = beam
                     for splitter = (gethash beam grid)
                     if splitter do (push (cons (1- x) (1+ y)) new-beams)
                                    (push (cons (1+ x) (1+ y)) new-beams)
                                    (incf splits)
                     else if (< y end)
                            do (push (cons x (1+ y)) new-beams)
                     finally (setf beams (remove-duplicates new-beams :test #'equal)))
            finally (return splits)))))
(frog:report (part-1 (frog:get-advent-of-code-input 2025 7)))

(defclass* beam () (x y (paths 1)))
(defun pos-to-beam (pos) (list (make-instance 'beam :x (car pos) :y (cdr pos))))
(defun merge-beams (beams)
  (loop with positions = (serapeum:dict)
        for beam in beams
        for overlap = (gethash (cons (x-of beam) (y-of beam)) positions)
        do (push beam overlap)
           (setf (gethash (cons (x-of beam) (y-of beam)) positions) overlap)
        finally (return (loop with out = (list)
                              for overlap being the hash-value of positions using (hash-key pos)
                              for (x . y) = pos
                              for paths = (reduce #'+ (mapcar #'paths-of overlap))
                              do (push (make-instance 'beam :x x :y y :paths paths) out)
                              finally (return out)))))

(defun part-2 (input)
  (multiple-value-bind (grid start) (parse-grid input)
    (let* ((end (reduce #'max (mapcar #'cdr (alexandria:hash-table-keys grid))))
           (beams (pos-to-beam start)))
      (loop for y from 0 upto end
            do (loop with new-beams = (list)
                     for beam in beams
                     for x = (x-of beam) for y = (y-of beam) for paths = (paths-of beam)
                     for splitter = (gethash (cons x y) grid)
                     if splitter
                       do (push (make-instance 'beam :x (1- x) :y (1+ y) :paths paths) new-beams)
                          (push (make-instance 'beam :x (1+ x) :y (1+ y) :paths paths) new-beams)
                     else do (push (make-instance 'beam :x x :y (1+ y) :paths paths) new-beams)
                     finally (setf beams (merge-beams new-beams)))
            finally (return (reduce #'+ (mapcar #'paths-of beams)))))))
(frog:report (part-2 (frog:get-advent-of-code-input 2025 7)))
