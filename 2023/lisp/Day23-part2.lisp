(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache :lparallel))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
(defclass* walker () ((y 0) (x 1) (stepped 0) (visited (fset:set (list 0 1)))))
(defclass* node () (y x (connections (list))))

(defun parse-map (file)
  (loop with out = (serapeum:dict)
        with lines = (str:lines file)
        for line in lines
        for y from 0
        maximize (loop for x from 0
                 for c across line
                       do (setf (gethash (list y x) out) c)
                       finally (return x)) into max-x
        finally (return (list out (list y (- max-x 2))))))

(defun walk (map walker end)
  (with-slots (y x visited stepped) walker
    (setf stepped 1)
    (loop with last = (list y x) and around = (list)
          do (setf around (list))
             (loop for (dy dx) in '((0 1) (0 -1) (1 0) (-1 0))
                   for new-pos = (list (+ y dy) (+ x dx))
                   for char = (gethash new-pos map)
                   if (and char (not (char= char #\#)) (not (equal new-pos last)) (not (fset:contains? visited new-pos)))
                     do (push new-pos around))
             (cond ((null around) (return-from walk
                                    (if (equal end (list y x))
                                        (list walker)
                                        nil)))
                   ((= 1 (length around))
                    (setf last (list y x)
                          y (first (first around))
                          x (second (first around)))
                    (incf stepped)
                    (when (equal around end) (return-from walk (list walker))))
                   (t (return-from walk (loop for pos in around
                                                 collect (frog:copy-instance walker :y (first pos) :x (second pos)
                                                                                    :visited (fset:set (list y x))))))))))

(defun build-grid (map parent-node walker nodes end)
  (when (not (gethash (list (y-of parent-node) (x-of parent-node)) nodes))
    (setf (gethash (list (y-of parent-node) (x-of parent-node)) nodes) parent-node))
  (let ((neighbors (walk map walker end)))
    (when (assoc (list (y-of walker) (x-of walker)) (connections-of parent-node) :test #'equal)
        (return-from build-grid))
    (setf (connections-of parent-node) (acons (list (y-of walker) (x-of walker)) (stepped-of walker)
                                              (connections-of parent-node)))
    (loop for neighbor-walker in neighbors
          for neighbor-node = (gethash (list (y-of walker) (x-of walker)) nodes)
          if (null neighbor-node)
            do (setf neighbor-node (make-instance 'node :y (y-of walker) :x (x-of walker)))
          do (build-grid map neighbor-node neighbor-walker nodes end)
          finally (return parent-node))))

(defun traverse-grid (node nodes visited total-distance end)
  (with-slots (y x connections) node
    (when (fset:contains? visited (list y x)) (return-from traverse-grid -1))
    (if (equal end (list y x))
        total-distance
        (loop for (location . distance) in connections
              for connection = (gethash location nodes)
              maximize (traverse-grid connection nodes
                                      (fset:with visited (list y x))
                                      (+ distance total-distance) end)))))

(defun part-2 (file)
  (bind (((map end) (parse-map file))
         (start (make-instance 'node :y 0 :x 1))
         (nodes (serapeum:dict)))
    (setf *max-value* 0)
    (build-grid map start (make-instance 'walker) nodes end)
    (traverse-grid start nodes (fset:set) 0 end)))

(print (time (part-2 (frog:get-advent-of-code-input 2023 23 :input-suffix "test"))))
