(ql:quickload '(:str :alexandria :function-cache))
(defpackage :advent (:use :cl))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun parse-map (input)
  (loop with out = (serapeum:dict)
        with lines = (str:lines input)
        with start = nil
        for y from 0 below (length lines)
        for line = (nth y lines)
        do (loop for x from 0 below (length line)
                 for char = (char line x)
                 if (equal #\S char) do (setf start (list y x))
                                        (setf (gethash (list y x) out) #\.)
                 else do (setf (gethash (list y x) out) char))
        finally (return (list start out))))

(defun valid-directions (pos map)
  (let (out)
    (if (equal #\. (gethash (list (1- (first pos)) (second pos)) map))
        (push (list (1- (first pos)) (second pos)) out))
    (if (equal #\. (gethash (list (1+ (first pos)) (second pos)) map))
        (push (list (1+ (first pos)) (second pos)) out))
    (if (equal #\. (gethash (list (first pos) (1- (second pos))) map))
        (push (list (first pos) (1- (second pos))) out))
    (if (equal #\. (gethash (list (first pos) (1+ (second pos))) map))
        (push (list (first pos) (1+ (second pos))) out))
    out))

;; BFS to find shortest path to each point.
;; Only even numbered distances are tracked as you can't move an odd distance in an even number of steps.
;; If we were instead given an odd number of steps, we'd just track odd numbers (like in part 2)
(defun part-1 (file max-steps)
  (loop with (start map) = (parse-map file)
        with min-distances = (serapeum:dict)
        with queue = (list (list start 0 0)) ;; Position, distance, steps
        while queue
        for (cur-pos cur-dist cur-steps) = (pop queue)
        do (loop with dist = (1+ cur-dist) and steps = (1+ cur-steps)
                 for next in (valid-directions cur-pos map)
                 for old = (gethash next min-distances)
                 if (> steps max-steps) do
                   (return)
                 if (and (evenp dist) (or (not old) (< dist old)))
                   do (setf (gethash next min-distances) dist)
                      (push (list next dist steps) queue)
                 else if (oddp dist)
                        do (push (list next dist steps) queue))
        finally (return (hash-table-count min-distances))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 21) 64)))

;; Theory: Each copy (tile) of the map is surrounded by blank rows and columns.
;; Beyond some distance, the fastest path to a space is to go around the outside.
;; Therefore we only need some subset of the infinite map mapped into distances.
;; We can then repeat the distance of the first/last row/column infinitely, as those tiles must be using the blank rows/columns.
;; When calculating points we can leap over tiles and do some bounds checking to tell if it's incldued.
(defparameter *tile-lookaround* 3) ; How many copies of the map should we look around?

(defun distance-map (file)
  (loop with queue = (list) and counter = 0
        with (start map) = (parse-map file)
        with max-y = (1+ (apply #'max (loop for key being the hash-keys of map collect (first key))))
        with max-x = (1+ (apply #'max (loop for key being the hash-keys of map collect (second key))))
        with min-distances = (serapeum:dict)
        initially (push (list (list 0 0) start 0) queue) ;; Tile-Position, Position, distance
        while queue
        for (tile pos dist) = (pop queue)
        for key = (list tile pos)
        for char = (gethash pos map)
        if (zerop (mod (incf counter) 100000))
          do (format t "~a: ~a ~a~%" counter dist (length queue))
        if (and (char= #\. char) (not (gethash key min-distances))
                (<= (abs (first tile)) *tile-lookaround*) (<= (abs (second tile)) *tile-lookaround*))
          do (setf (gethash key min-distances) dist)
             (loop for (dy dx) in '((0 1) (0 -1) (1 0) (-1 0))
                   for new-y = (+ (first pos) dy) and new-x = (+ (second pos) dx)
                   for new-tile = (list (if (< new-y 0) (1- (first tile)) (if (>= new-y max-y) (1+ (first tile)) (first tile)))
                                        (if (< new-x 0) (1- (second tile)) (if (>= new-x max-x) (1+ (second tile)) (second tile))))
                   for new-pos = (list (mod new-y max-y) (mod new-x max-x))
                   do (setf queue (nconc queue (list (list new-tile new-pos (1+ dist))))))
        finally (return (list min-distances max-y max-x))))

(defparameter *cache* nil) ; Dynamic-scoped cache for handle-tiles. Is set in part-2
(defun handle-tiles (distance corner max-steps max-x) ; We use max-x which works for both directions as tiles are squares.
  (when (function-cache:get-cached-value *cache* (list distance corner max-steps))
    (return-from handle-tiles (function-cache:get-cached-value *cache* (list distance corner max-steps))))
  (loop with out = 0 and tiles-to-consider = (floor (- max-steps distance) max-x)
        for i from 1 upto tiles-to-consider ; Start i at 1, as we already considered the first tile earlier.
        if (and (<= (+ distance (* i max-x)) max-steps) ; If space is reachable within max-steps
                (= (mod max-steps 2) (mod (+ distance (* i max-x)) 2))) ; If both are even or both are odd
          do (incf out (if corner (+ i 1) 1)) ; If in a corner, add one per tile traversed (move left, then up per move), else just add one.
        finally (setf (function-cache:get-cached-value *cache* (list distance corner max-steps)) out)
                (return out)))

(defun part-2 (file max-steps)
  (loop with *cache* = (make-instance 'function-cache:lru-cache :capacity 1000000)
        with (min-distances max-y max-x) = (distance-map file)
        with count = 0
        with lookaround-range = (alexandria:iota (1+ (* 2 *tile-lookaround*)) :start (- *tile-lookaround*))
        for y from 0 below max-y
        do (loop for x from 0 below max-x
                 ;; Don't bother checking the infinite map if it's not even reachable in the relative one.
                 if (gethash (list (list 0 0) (list y x)) min-distances)
                 ;; Loop through the lookarounds
                 do (loop for tile-y in lookaround-range
                          do (loop for tile-x in lookaround-range
                                   for distance = (gethash (list (list tile-y tile-x) (list y x)) min-distances)
                                   ;; If the max steps and distance are both even or odd, and it's reachable in the step limit
                                   if (and (= (mod max-steps 2) (mod distance 2)) (<= distance max-steps))
                                     do (incf count)
                                   ;; If we're at an edge tile, we need to leap through the infinite map..
                                   ;; Corners are handled differently as you advance diagonally (one per tile traversed instead of just one)
                                   if (and (member tile-y (list -3 3)) (member tile-x (list -3 3)))
                                     do (incf count (handle-tiles distance t max-steps max-y))
                                   else if (or (member tile-y (list -3 3)) (member tile-x (list -3 3)))
                                          do (incf count (handle-tiles distance nil max-steps max-x)))))
        finally (return count)))
(print (time (part-2 (frog:get-advent-of-code-input 2023 21) 26501365)))
