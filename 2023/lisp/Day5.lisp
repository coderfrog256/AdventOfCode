(ql:quickload '(:str :hu.dwim.defclass-star))
(defpackage :advent (:use :cl :hu.dwim.defclass-star))
(in-package :advent)
(defclass* range-map() ((ranges nil) (points-of-interest nil)))

(defun parse-map (map)
  (loop with out = (make-instance 'range-map)
        for line in (rest (str:lines map))
        for parsed = (frog:extract-numbers line)
        do (push parsed (ranges-of out))
        finally (setf (points-of-interest-of out) (mapcar #'second (ranges-of out)))
        finally (return out)))

(defun lookup-in-map (key map reverse)
  (loop for replacement in (ranges-of map)
        for (dest source length) = replacement
        for diff = (- key (if reverse dest source))
        if (and (>= diff 0) (< diff length)) do (return (+ dest diff))
        finally (return key)))

(defun overlay-map (destination source)
  (loop for dest-point in (points-of-interest-of destination)
        for source-point = (lookup-in-map dest-point source t)
        do (push source-point (points-of-interest-of source))
        finally (return source)))

(defun in-seed-map (point map)
  (loop for seed in (ranges-of map)
        for (start _ length) = seed
        when (and (>= point start) (<= point (+ start length))) do (return t)
        finally (return nil)))

(defun part-1-seed-map (seeds)
  (make-instance 'range-map :ranges (mapcar (lambda (seed) (list seed seed 1)) seeds) :points-of-interest seeds))

(defun part-2-seed-map (seeds)
  (loop with out = (make-instance 'range-map)
        for chunk in (frog:chunk-items 2 seeds)
        for (start length) = chunk
        do (push (list start start length) (ranges-of out))
        finally (setf (points-of-interest-of out) seeds)
        finally (return out)))

(defun run-mapping (file part-2)
  (let* ((parts (str:split frog:+double-newline+ file))
         (seeds (frog:extract-numbers (first parts)))
         (maps (mapcar #'parse-map (rest parts))))
    (push (if part-2 (part-2-seed-map seeds) (part-1-seed-map seeds)) maps)
    (reduce (lambda (dest source) (overlay-map dest source)) (reverse maps))
    (loop for point in (points-of-interest-of (first maps))
          when (in-seed-map point (first maps))
          minimize (reduce (lambda (value map) (lookup-in-map value map nil)) maps :initial-value point))))
(print (time (run-mapping (frog:get-advent-of-code-input 2023 5) nil)))
(print (time (run-mapping (frog:get-advent-of-code-input 2023 5) t)))
