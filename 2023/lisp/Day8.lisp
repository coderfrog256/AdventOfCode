(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defun parse-directions (lines)
  (->> lines first (str:replace-all "L" "0 ") (str:replace-all "R" "1 ") (frog:extract-numbers)))

(defun build-network (lines)
  (loop with out = (serapeum:dict)
        for line in lines
        do (register-groups-bind (source l r) ("(\\w+) = \\((\\w+), (\\w+)\\)" line)
             (setf (gethash source out) (list l r)))
        finally (return out)))

(defun part-1 (file)
  (let* ((lines (str:lines file))
         (directions (parse-directions lines))
         (network (build-network (rest lines))))
    (loop with current = "AAA"
          for i from 0
          until (equal current "ZZZ")
          for direction = (nth (mod i (length directions)) directions)
          do (setf current (nth direction (gethash current network)))
          finally (return i))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 8))))

(defclass* walker () (current (loop-start nil) (loop-end nil)))
(defun walk (i walker direction network)
  (with-slots (current loop-start loop-end) walker
    (let ((next (nth direction (gethash current network))))
      (setf current next)
      (if (str:ends-with-p "Z" current)
          (if (null loop-start) (setf loop-start i) (setf loop-end i))))
    loop-end))

(defun part-2 (file)
  (let* ((lines (str:lines file))
         (directions (parse-directions lines))
         (network (build-network (rest lines)))
         (walkers (mapcar (lambda (start) (make-instance 'walker :current start))
                          (remove-if-not (lambda (l) (str:ends-with-p "A" l))
                                         (alexandria:hash-table-keys network)))))
    (loop with remaining = (copy-list walkers)
          for i from 0 and direction = (nth (mod i (length directions)) directions)
          while remaining
          do (setf remaining (remove-if (lambda (walker) (walk i walker direction network)) remaining))
          finally (return (reduce #'lcm (mapcar (lambda (w) (- (loop-end-of w) (loop-start-of w))) walkers))))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 8))))
