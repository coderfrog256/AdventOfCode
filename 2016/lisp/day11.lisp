(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defclass* item () (item-type name))
(defclass* building () (floors (elevator 3)))
(defmethod print-object ((obj item) out) (with-slots (item-type name) obj (format out "~A(~A)" item-type name)))
(defmethod print-object ((building building) out) (format out "~A@~A" (elevator-of building)
                                                          (mapcar (lambda (f) (sort (mapcar (lambda (i) (format nil "~A" i)) (fset:convert 'list f)) #'string-lessp)) (fset:convert 'list (floors-of building)))))
(defun stable-id (building) (format nil "~A" building))

(defun floor-valid (floor)
  (let ((generators (fset:image #'name-of (fset:filter (lambda (i) (equal :generator (item-type-of i))) floor)))
        (chips (fset:image #'name-of (fset:filter (lambda (i) (equal :chip (item-type-of i))) floor))))
    (fset:do-set (chip chips)
      (unless (fset:contains? generators chip)
        (if (not (fset:empty? generators)) (return-from floor-valid nil))))
    t))

(defun building-valid (building) (fset:reduce (lambda (l r) (and l r)) (fset:image #'floor-valid (floors-of building))))

(defun building-move (building to &rest items)
  (loop with out = building
        with elevator = (elevator-of out)
        for item in items
        for floors = (floors-of out)
        for from-floor = (fset:@ floors elevator)
        for to-floor = (fset:@ floors to)
        do (setf out (make-instance 'building
                                    :elevator to
                                    :floors (-> (fset:with floors elevator (fset:less from-floor item))
                                              (fset:with to (fset:with to-floor item)))))
        finally (return out)))

(defun building-finished (building)
  (with-slots (floors) building
    (and (fset:empty? (fset:@ floors 1)) (fset:empty? (fset:@ floors 2)) (fset:empty? (fset:@ floors 3)))))

(defun get-neighbors (b)
  (let* ((e (elevator-of b))
         (floors (floors-of b))
         (floor (fset:@ floors e))
         (out nil))
    (fset:do-set (item floor)
      (when (> e 0)
        (let ((move (building-move b (1- e) item)))
          (when (building-valid move) (push move out)))
        (fset:do-set (item2 floor)
          (unless (equal item item2)
            (let ((move (building-move b (1- e) item item2)))
              (when (building-valid move) (push move out))))))
      (when (< e 3)
        (let ((move (building-move b (1+ e) item)))
          (when (building-valid move) (push move out)))))
    out))

;; Optimization note: The problem space can be reduced by not tracking unique chip+generator pairs on the same floor.
;; It really only matters which chips are without generators and visa-versa. Otherwise the pairs are interchangable.
;; I didn't do this, but it should speed things up a bunch!
(defun dijkstra (start)
  (let* ((min-distance (make-hash-table :test #'equal))
         (best-distance most-positive-fixnum)
         (start-id (stable-id start))
         (queue (priority-queue:make-pqueue #'<)))
    (setf (gethash start-id min-distance) 0)
    (priority-queue:pqueue-push (list 0 start-id start) 0 queue)
    (loop with counter = 0
          until (priority-queue:pqueue-empty-p queue)
          for next = (priority-queue:pqueue-pop queue)
          for dist = (first next)
          for node-id = (second next)
          for node = (third next)
          do (if (= 0 (mod (incf counter) 1000)) (format t "~:d: Best: ~:d, Peek: ~:d Graph Size: ~:d Queue Size: ~:d~%" counter best-distance dist (hash-table-count min-distance) (priority-queue:pqueue-length queue)))
          do (loop with new-distance = (1+ dist)
                   until (< best-distance new-distance) ; Modification: We just care about distance to a single destination. Don't simulate if not better than the current best.
                   for neighbor in (get-neighbors node)
                   for neighbor-id = (stable-id neighbor)
                   for old-distance = (gethash neighbor-id min-distance best-distance)
                   if (< new-distance old-distance)
                     do (setf (gethash neighbor-id min-distance) new-distance)
                        (priority-queue:pqueue-push (list new-distance neighbor-id neighbor) new-distance queue)
                        (when (building-finished neighbor) (setf best-distance (min best-distance new-distance)) ; Modification: Note the best distance when at single destination.
                              (format t "Finished building. New Best: ~:d~%" best-distance)))
          finally (return best-distance))))

;; Parsing
(defrule gen-rule () (and "a " (+ (char "a-zA-Z")) " generator" ) (:function (lambda (_1 element _2) (make-instance 'item :item-type :generator :name (intern (coerce element 'string))))))
(defrule chip-rule () (and "a " (+ (char "a-zA-Z")) "-compatible microchip" ) (:function (lambda (_1 element _2) (make-instance 'item :item-type :chip :name (intern (coerce element 'string))))))
(defrule nothing-rule () (and "nothing relevant" ) (:function (lambda (&rest _) nil)))
(defrule parse-rule () (and "The " (+ (char "a-zA-Z")) " floor contains " (* (and (or gen-rule chip-rule nothing-rule) (? ", ") (? " and ") (? "and ") (? "."))))
  (:flatten) (:function (lambda (_1 _2 _3 &rest contents) (fset:convert 'fset:set (remove-if-not #'itemp contents)))))

(defun shortest-path (file)
  (dijkstra (make-instance 'building :floors
                           (fset:convert 'fset:seq (mapcar (lambda (line) (parseq:parseq 'parse-rule line)) (reverse (str:lines (str:from-file file))))))))

(print (time (shortest-path "../input/day11-test.txt")))
(print (time (shortest-path "../input/day11.txt")))
(print (time (shortest-path "../input/day11-2.txt")))
