(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
(defun parse-map (map)
  (let* ((lines (cdr (str:lines map))))
    (loop for line in lines
          for parsed = (read-from-string (str:concat "(" line ")"))
          for expanded = (list (first parsed) (second parsed) (third parsed))
          collect expanded)))

(declaim (ftype (function (integer list) integer) lookup-in-map))
(defun lookup-in-map (key map)
  (loop for replacement in map
        for (dest source length) = replacement
        for diff = (- key source)
        if (and (>= diff 0) (<= diff length))
          do (return (+ dest diff))
        finally (return key)))

(defun traverse-maps (file)
  (let* ((parts (str:split frog:+double-newline+ file))
         (seeds (read-from-string (str:concat "(" (str:substring 7 nil (first parts)) ")")))
         (maps (mapcar #'parse-map (rest parts))))
    (loop for seed in seeds
          minimize (reduce (lambda (value map) (lookup-in-map value map)) maps :initial-value seed))))
(print (time (traverse-maps (frog:get-advent-of-code-input 2023 5))))

(declaim (ftype (function (integer list) integer) lookup-in-map-reverse))
(defun lookup-in-map-reverse (key map)
  (loop for replacement in map
        for (dest source length) = replacement
        for diff = (- key dest)
        if (and (>= diff 0) (<= diff length)) do (return (+ source diff))
        finally (return key)))

(declaim (ftype (function (integer list) boolean) seed-in-chunk))
(defun seed-in-chunk (seed chunks)
  (loop for chunk in chunks
        for (start length) = chunk
        if (and (>= seed start) (< seed (+ start length))) do (return t)
        finally (return nil)))

(defun part-2 (file)
  (let* ((parts (str:split frog:+double-newline+ file))
         (seeds (read-from-string (str:concat "(" (str:substring 7 nil (first parts)) ")")))
         (chunks (frog:chunk-items 2 seeds))
         (maps (reverse (mapcar #'parse-map (rest parts)))))
    (loop for location from 0
          for seed = (reduce (lambda (value map) (lookup-in-map-reverse value map)) maps :initial-value location)
          if (seed-in-chunk seed chunks) do (return location))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 5))))
