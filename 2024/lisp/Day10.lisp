(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel :priority-queue))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defclass* node () (value (neighbors (list))))

(defun parse (map)
  (loop with nodes = (serapeum:dict)
        for line in (str:lines map)
        for y from 0
        do (loop for x from 0
                 for c across line
                 when (not (char= c #\.))
                   do (setf (gethash (cons x y) nodes) (make-instance 'node :value (parse-integer (string c)))))
        finally (loop for (x . y) being the hash-keys of nodes using (hash-value node)
                      for value = (value-of node)
                      do (setf (neighbors-of node)
                               (loop for (dx dy) in '((-1 0) (1 0) (0 -1) (0 1))
                                     for nx = (+ x dx) for ny = (+ y dy)
                                     for neighbor = (gethash (cons nx ny) nodes)
                                     when (and neighbor (= 1 (- (value-of neighbor) value)))
                                       collect neighbor))
                      finally (return-from parse (remove-if-not (lambda (n) (zerop (value-of n))) (alexandria:hash-table-values nodes))))))

(defun visit (node seen part-2)
  (when (gethash node seen) (return-from visit 0))
  (setf (gethash node seen) t)
  (if (= 9 (value-of node)) 1
      (loop for neighbor in (neighbors-of node)
            sum (visit neighbor (if part-2 (alexandria:copy-hash-table seen) seen) part-2))))

(defun solve (input part-2)
  (loop for node in (parse input) summing (visit node (serapeum:dict) part-2)))

(frog:report (solve (frog:get-advent-of-code-input 2024 10 :input-suffix "test") nil))
(frog:report (solve (frog:get-advent-of-code-input 2024 10) nil))
(frog:report (solve (frog:get-advent-of-code-input 2024 10 :input-suffix "test") t))
(frog:report (solve (frog:get-advent-of-code-input 2024 10) t))
