(ql:quickload '(:str :hu.dwim.defclass-star :alexandria :priority-queue))
(defpackage :advent (:use :cl :hu.dwim.defclass-star))
(in-package :advent)
;; Yay for mutability!
(defclass* node () (x y (in-loop nil) (valid t) (potential-neighbors (list)) (neighbors (list))))
;; Given a pipe, what directions can it accept?
(defun accepts-left (char) (member char '(#\- #\J #\7 #\S)))
(defun accepts-right (char) (member char '(#\- #\F #\L #\S)))
(defun accepts-up (char) (member char '(#\| #\J #\L #\S)))
(defun accepts-down (char) (member char '(#\| #\F #\7 #\S)))

;; Given two nodes, what direction is the first from the second?
(defun determine-direction (src dest)
  (with-slots (x y) dest
    (let ((d-x (x-of src))
          (d-y (y-of src)))
      (cond ((and (= x d-x) (< y d-y)) :up)
            ((and (= x d-x) (> y d-y)) :down)
            ((and (< x d-x) (= y d-y)) :left)
            ((and (> x d-x) (= y d-y)) :right)))))

;; Add a potential neighbor to a node if it accepts the given character
(defmacro add-potental-neighbor (y x accept-fn)
  `(if (,accept-fn (char-at ,y ,x)) (push (list ,y ,x) (slot-value node 'potential-neighbors))))

;; Create a network from a file. Returns two values, the start node and the full network
(defun make-network (file)
  (let ((network (serapeum:dict))
        (start nil)
        (lines (str:lines file)))
    (labels ((char-at (y x)
               (if (and (>= y 0) (< y (length lines)) (>= x 0) (< x (length (nth y lines))))
                   (char (nth y lines) x) #\space)))
      ;; Build the network. First pass, just create nodes and list potential neighbors
      (loop for y from 0 below (length lines)
            for line = (nth y lines)
            do (loop for x from 0 below (length line)
                     for node = (setf (gethash (list y x) network) (make-instance 'node :x x :y y))
                     do (case (char line x)
                          (#\|
                           (add-potental-neighbor (1- y) x accepts-down)
                           (add-potental-neighbor (1+ y) x accepts-up))
                          (#\-
                            (add-potental-neighbor y (1- x) accepts-right)
                            (add-potental-neighbor y (1+ x) accepts-left))
                          (#\L
                           (add-potental-neighbor (1- y) x accepts-down)
                           (add-potental-neighbor y (1+ x) accepts-left))
                          (#\J
                            (add-potental-neighbor (1- y) x accepts-down)
                            (add-potental-neighbor y (1- x) accepts-right))
                          (#\7
                            (add-potental-neighbor (1+ y) x accepts-up)
                            (add-potental-neighbor y (1- x) accepts-right))
                          (#\F
                           (add-potental-neighbor (1+ y) x accepts-up)
                           (add-potental-neighbor y (1+ x) accepts-left))
                          (#\S (setf start node)))))
      ;; Second pass, determine actual neighbors based on bi-directional connections
      (loop for node being the hash-values of network
            do (loop for neighbor-coords in (slot-value node 'potential-neighbors)
                     for neighbor = (gethash neighbor-coords network)
                     when neighbor do (progn (push (list neighbor (determine-direction node neighbor))  (slot-value node 'neighbors))
                                             (push (list node (determine-direction neighbor node)) (slot-value neighbor 'neighbors))))
            finally (loop for node being the hash-values of network
                          do (setf (slot-value node 'neighbors) (remove-duplicates (slot-value node 'neighbors) :test #'equal))))
      (values start network))))

;; Use Dijkstra's algorithm to find the shortest path from start to every other node.
(defun get-loop (start)
  (let* ((min-distance (serapeum:dict))
         (best-distance most-positive-fixnum)
         (queue (priority-queue:make-pqueue #'<)))
    (setf (gethash start min-distance) 0)
    (priority-queue:pqueue-push (list 0 start) 0 queue)
    (loop until (priority-queue:pqueue-empty-p queue)
          for next = (priority-queue:pqueue-pop queue)
          for dist = (first next)
          for node = (second next)
          for neighbors = (mapcar #'first (neighbors-of node))
          do (loop with new-distance = (1+ dist)
                   for neighbor in neighbors
                   for old-distance = (gethash neighbor min-distance best-distance)
                   if (< new-distance old-distance)
                     do (setf (gethash neighbor min-distance) new-distance)
                        (priority-queue:pqueue-push (list new-distance neighbor) new-distance queue))
          finally (return min-distance))))

;; Part 1, find the longest path from all the shortest paths.
(defun part-1 (file)
  (let* ((start (make-network file))
         (pipe-loop (get-loop start)))
    (reduce #'max (alexandria:hash-table-values pipe-loop))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 10))))

;; Invalidate a node and all its neighbors that aren't part of the loop.
(defun invalidate-node (node network visited)
  (when (and node (not (in-loop-of node)) (not (gethash node visited)))
    (with-slots (y x valid) node
      (setf valid nil)
      (setf (gethash node visited) t)
      (invalidate-node (gethash (list y (1- x)) network) network visited)
      (invalidate-node (gethash (list y (1+ x)) network) network visited)
      (invalidate-node (gethash (list (1- y) x) network) network visited)
      (invalidate-node (gethash (list (1+ y) x) network) network visited))))

;; Add a linking pipe between two nodes. Expansions are invalid as they are not the original network.
(defmacro expand-direction (y x)
  `(setf (gethash (list ,y ,x) new-network) (make-instance 'node :y ,y :x ,x :valid nil :in-loop (in-loop-of node))))

;; Macro to cut down on repetition. Iterates in a direction until it hits a wall, invalidating nodes along the way.
(defmacro flood-direction ((outer end-outer) (inner start-inner dir-inner end-inner))
  `(loop for ,outer from 0 upto ,end-outer
         do (loop for ,inner from ,start-inner ,dir-inner ,end-inner
                  for node = (gethash (list ,inner ,outer) new-network)
                  if (and node (in-loop-of node)) do (return)
                  else do (invalidate-node node new-network visited))))

(defun part-2 (file)
  (multiple-value-bind (start old-network) (make-network file)
    (let* (max-x max-y (visited (serapeum:dict)) (new-network (serapeum:dict)))
      ;; Mark all nodes that are part of the loop as invalid and in the loop.
      (loop for node in (alexandria:hash-table-keys (get-loop start)) do (setf (valid-of node) nil (in-loop-of node) t))
      ;; Expand the network by adding new nodes between the old ones. Double the x and y dimensions.
      (loop for node being the hash-values of old-network using (hash-key location)
            for x = (second location) and y = (first location)
            do (setf (gethash (list (* 2 y) (* 2 x)) new-network) (frog:copy-instance node :y (* 2 y) :x (* 2 x)))
               (loop for (neighbor direction) in (neighbors-of node)
                     for neighbor-x = (x-of neighbor) and neighbor-y = (y-of neighbor)
                     do (case direction
                          (:left  (expand-direction (* 2 y) (1- (* 2 x))))
                          (:right (expand-direction (* 2 y) (1+ (* 2 x))))
                          (:up    (expand-direction (1- (* 2 y)) (* 2 x)))
                          (:down  (expand-direction (1+ (* 2 y)) (* 2 x))))))
      (setf max-x (reduce #'max (mapcar #'second (alexandria:hash-table-keys new-network)))
            max-y (reduce #'max (mapcar #'first (alexandria:hash-table-keys new-network))))
      ;; Fill in any gaps in the network with empty nodes.
      (loop for x from 0 upto max-x do (loop for y from 0 upto max-y
                                             when (not (gethash (list y x) new-network))
                                               do (setf (gethash (list y x) new-network) (make-instance 'node :y y :x x :valid nil :in-loop nil))))
      ;; Flood fill!
      ;; Run from each edge, invalidate nodes that aren't shielded by the loop.
      (flood-direction (x max-x) (y 0 upto max-y))
      (flood-direction (x max-x) (y max-y downto 0))
      (flood-direction (y max-y) (x 0 upto max-x))
      (flood-direction (y max-y) (x max-x downto 0))
      (count-if #'valid-of (alexandria:hash-table-values new-network)))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 10))))
