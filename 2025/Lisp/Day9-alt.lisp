(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel :alexandria))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
(setf lparallel:*kernel* (lparallel:make-kernel 16))

(defun parse-line (line)
  (->> line (str:split ",") (mapcar #'parse-integer)))
(defun parse (lines)
  (->> lines (str:trim) (str:lines) (mapcar #'parse-line)))
(defun area (l r)
  (* (1+ (abs (- (first l) (first r))))
     (1+ (abs (- (second l) (second r))))))
(defun part-1 (input)
  (let* ((points (parse input))
         (best nil))
    (dolist (l points)
      (dolist (r points)
        (let ((distance (area l r)))
          (if (or (null best) (> distance best))
              (setf best distance)))))
    best))

;;(frog:report (part-1 (frog:get-advent-of-code-input 2025 9 :input-suffix "test")))
;;(frog:report (part-1 (frog:get-advent-of-code-input 2025 9)))

(defun fill-in (l r out)
  (loop with start-x = (min (first l) (first r))
        with end-x = (max (first l) (first r))
        with start-y = (min (second l) (second r))
        with end-y = (max (second l) (second r))
        for y from start-y upto end-y
        do (loop for x from start-x upto end-x
                 do (setf (gethash (cons x y) out) t))))

(defun shoot-lazers (valid)
  (loop with end-x = (+ 2 (reduce #'max (mapcar #'car  (alexandria:hash-table-keys valid))))
        for (x . y) in (alexandria:hash-table-keys valid)
        for to-add = (list)
        do (block inner (loop for i from (1+ x) upto end-x
                              for point = (cons i y)
                              if (gethash point valid)
                                do (loop for point in to-add do (setf (gethash point valid) t))
                                   (return-from inner) ; break
                              else
                                do (push point to-add)))))

(defun cell-is-valid (x y valid min-x max-x min-y max-y)
  (and
   (loop for j from y downto min-y if (gethash (cons x j) valid) do (return t) finally (return nil))
   (loop for j from y upto max-y if (gethash (cons x j) valid) do (return t) finally (return nil))
   (loop for i from x downto min-x if (gethash (cons i y) valid) do (return t) finally (return nil))
   (loop for i from x upto max-x if (gethash (cons i y) valid) do (return t) finally (return nil))))


(defun is-valid-bad (l r valid min-x max-x min-y max-y)
  (let* ((start-x  (min (first l) (first r)))
         (end-x  (max (first l) (first r)))
         (start-y  (min (second l) (second r)))
         (end-y  (max (second l) (second r))))
    (and (cell-is-valid start-x start-y valid min-x max-x min-y max-y)
         (cell-is-valid end-x start-y valid min-x max-x min-y max-y)
         (cell-is-valid start-x end-y valid min-x max-x min-y max-y)
         (cell-is-valid end-x end-y valid min-x max-x min-y max-y))))

(defun is-valid (l r valid min-x max-x min-y max-y)
  (if (not (is-valid-bad l r valid min-x max-x min-y max-y))
      nil
      (loop with start-x = (min (first l) (first r))
            with end-x = (max (first l) (first r))
            with start-y = (min (second l) (second r))
            with end-y = (max (second l) (second r))
            for x from start-x upto end-x
            do (loop for y from start-y upto end-y
                     if (not (cell-is-valid x y valid min-x max-x min-y max-y))
                       do (return-from is-valid nil))
            finally (return t))))

(defun is-valid-fast (l r valid min-x max-x min-y max-y)
  (if (not (is-valid-bad l r valid min-x max-x min-y max-y))
      nil
  (let* ((start-x  (min (first l) (first r)))
         (end-x  (max (first l) (first r)))
         (start-y  (min (second l) (second r)))
         (end-y  (max (second l) (second r))))
    (and
     (loop for i from start-x upto end-x if (not (cell-is-valid i start-y valid min-x max-x min-y max-y)) do (return nil) finally (return t))
     (loop for i from start-y upto end-y if (not (cell-is-valid end-x i valid min-x max-x min-y max-y)) do (return nil) finally (return t))
     (loop for i from start-x upto end-x if (not (cell-is-valid i end-y valid min-x max-x min-y max-y)) do (return nil) finally (return t))
     (loop for i from start-y upto end-y if (not (cell-is-valid start-x i valid min-x max-x min-y max-y)) do (return nil) finally (return t))))))

(defun parallel-find-first (pred list)
  (let ((futures (mapcar (lambda (item)
                           (lparallel:future
                             (when (funcall pred item) item)))
                         list)))
    (format t "Futures started: ~a~%" (length futures))
    (loop for item in list
          for i from 0
          for fut in futures
          for val = (lparallel:force fut)  ; wait for each future in order
          if val
            do (return val)
          else
            do (format t "Invalid, remaining=~A~%" (- (length futures) i))
          finally (return nil))))

(defun part-2-par (input)
  (let* ((points (parse input))
         (areas (list))
         (valid (serapeum:dict)))
    (print "Set up State")
    (loop with last = (car (last points))
          for current in points
          do (fill-in last current valid)
             (setf last current))
    (print "Filled in spots")
    (dolist (l points)
      (dolist (r points)
        (push (list (area l r) l r) areas)))
    (print "Calculated areas")
    (setf areas (sort areas #'> :key #'first))
    (print "Sorted areas")
    (first
    (parallel-find-first
            (lambda (a)
              (let* ((area (first a))
                     (l (second a))
                     (r (third a))
                     (min-x (reduce #'min (mapcar #'car (alexandria:hash-table-keys valid))))
                     (max-x (reduce #'max (mapcar #'car (alexandria:hash-table-keys valid))))
                     (min-y (reduce #'min (mapcar #'cdr (alexandria:hash-table-keys valid))))
                     (max-y (reduce #'max (mapcar #'cdr (alexandria:hash-table-keys valid)))))
                (is-valid l r valid min-x max-x min-y max-y))) areas))))

(defun part-2 (input)
  (let* ((points (parse input))
         (areas (list))
         (valid (serapeum:dict)))
    (print "Set up State")
    (loop with last = (car (last points))
          for current in points
          do (fill-in last current valid)
             (setf last current))
    (print "Filled in spots")
    (dolist (l points)
      (dolist (r points)
        (push (list (area l r) l r) areas)))
    (print "Calculated areas")
    (setf areas (sort areas #'> :key #'first))
    (print "Sorted areas")
    (first
    (find-if
            (lambda (a)
              (let* ((area (first a))
                     (l (second a))
                     (r (third a))
                     (min-x (reduce #'min (mapcar #'car (alexandria:hash-table-keys valid))))
                     (max-x (reduce #'max (mapcar #'car (alexandria:hash-table-keys valid))))
                     (min-y (reduce #'min (mapcar #'cdr (alexandria:hash-table-keys valid))))
                     (max-y (reduce #'max (mapcar #'cdr (alexandria:hash-table-keys valid)))))
                (is-valid-fast l r valid min-x max-x min-y max-y))) areas))))

(frog:report (part-2 (frog:get-advent-of-code-input 2025 9 :input-suffix "test")))

;; 2364666478 is too high
;; 4615604488 is too high
;;(frog:report (part-2 (frog:get-advent-of-code-input 2025 9)))
;;(frog:report (part-2-par (frog:get-advent-of-code-input 2025 9)))
