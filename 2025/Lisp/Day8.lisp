(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel :alexandria))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
;; Alexandria:flatten cannot be used as it flattens as deep as possible, not just one level
(defun shallow-flatten (list) (mapcan (lambda (x) (if (listp x) x (list x))) list))

(defclass* junction () (id position (circuit nil)))
(defclass* circuit () (junctions))

(defun make-circuit (junction)
  (let ((circuit (make-instance 'circuit :junctions (list junction))))
    (setf (circuit-of junction) circuit)
    circuit))

(defun merge-circuits (l r)
  (if (equal l r) (return-from merge-circuits nil))
  (loop for p in (junctions-of r)
        do (setf (circuit-of p) l)
           (push p (junctions-of l))
        finally (return t)))

(defun parse-line (line)
  (->> line
    (str:split ",")
    (mapcar #'parse-integer)
    (frog:coerce-r 'vector)
    (make-instance 'junction :id line :position)))

(defun 3d-dist (a b)
  (let* ((x0 (- (aref a 0) (aref b 0)))
         (x1 (- (aref a 1) (aref b 1)))
         (x2 (- (aref a 2) (aref b 2))))
    (sqrt (+ (* x0 x0) (* x1 x1) (* x2 x2)))))

(defun get-dists (junctions)
  (remove-duplicates ; I'm sure I can avoid this by not double-looping, but whatever. It is fast enough
   (remove-if (lambda (d) (= 0 (nth 2 d))) ; Remove self-distance
              (shallow-flatten
               (mapcar (lambda (l)
                         (mapcar (lambda (r)
                                   (let* ((l-is-first (string< (id-of l) (id-of r)))
                                          (first (if l-is-first l r))
                                          (second (if l-is-first r l)))
                                     (list first second (3d-dist (position-of l) (position-of r)))))
                                 junctions)) junctions))) :test #'equal))

(defun part-1 (input limit)
  (let* ((distances (->> input
                      (str:lines)
                      (mapcar #'parse-line)
                      (get-dists)
                      (frog:sort-r (lambda (l r) (< (nth 2 l) (nth 2 r))))
                      (mapcar (lambda (d) (subseq d 0 2))))))
    (loop with circuits = (->> distances
                            (alexandria:flatten)
                            (frog:dedupe)
                            (mapcar #'make-circuit))
          for (l r) in (subseq distances 0 limit)
          do (merge-circuits (circuit-of l) (circuit-of r))
          finally (return (->> distances
                              (alexandria:flatten)
                              (mapcar #'circuit-of)
                              (frog:dedupe)
                              (mapcar #'junctions-of)
                              (mapcar #'length)
                              (frog:sort-r #'>)
                              (frog:subseq-r 0 3)
                              (reduce #'*))))))

(frog:report (part-1 (frog:get-advent-of-code-input 2025 8) 1000))

(defun part-2 (input)
  (let* ((distances (->> input
                      (str:lines)
                      (mapcar #'parse-line)
                      (get-dists)
                      (frog:sort-r (lambda (l r) (< (nth 2 l) (nth 2 r))))
                      (mapcar (lambda (d) (subseq d 0 2))))))
    (loop with circuits = (->> distances
                            (alexandria:flatten)
                            (frog:dedupe)
                            (mapcar #'make-circuit))
          for (l r) in distances
          for lc = (circuit-of l) for rc = (circuit-of r)
          if (merge-circuits lc rc)
            do (setf circuits (remove rc circuits :test #'equal))
          until (= 1 (length circuits))
          finally (return (* (aref (position-of l) 0)
                             (aref (position-of r) 0))))))

(frog:report (part-2 (frog:get-advent-of-code-input 2025 8)))
