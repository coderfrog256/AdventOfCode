(ql:quickload :cl-ppcre)
(ql:quickload :str)
(declaim (optimize (speed 3)))

(defun move (runner run run-duration wait-duration progress at-second)
  (declare (ftype (function (fixnum fixnum fixnum fixnum (array fixnum) fixnum) fixnum) move))
  (flet ((increment-progress (offset increments amount step)
           (dotimes (i increments)
             (setf (aref progress runner (+ offset i))
                   (+ amount (* step (1+ i)))))))
    (loop with distance = 0
          with current-second = 0
          with remaining = at-second
          do (if (< remaining run-duration)
                 (progn
                   (increment-progress (- 0 remaining (- at-second)) remaining distance run)
                   (return (+ distance (* run remaining))))
                 (progn
                   (increment-progress (- 0 remaining (- at-second)) run-duration distance run)
                   (incf distance (* run run-duration))
                   (decf remaining run-duration)))
          do (if (< remaining wait-duration)
                 (progn
                   (increment-progress (- 0 remaining (- at-second)) remaining distance 0)
                   (return distance))
                 (progn
                   (increment-progress (- 0 remaining (- at-second)) wait-duration distance 0)
                   (decf remaining wait-duration))))))

(defun list-winner (progress)
  (declare (ftype (function ((array fixnum)) fixnum) list-winner))
  (let* ((num-racers (array-dimension progress 0))
         (racer-points (make-array (list num-racers) :element-type 'fixnum :initial-element 0)))
    (loop for second from 0 upto (1- (array-dimension progress 1))
          do (let ((winners (list))
                   (max-points 0))
               (dotimes (racer num-racers (dolist (winner winners) (incf (aref racer-points winner))))
                 (when (or (not max-points) (>= (aref progress racer second) max-points))
                   (if (= (aref progress racer second) max-points)
                       (push racer winners)
                       (setf winners (list racer)))
                   (setf max-points (aref progress racer second))))))
    (loop for racer across racer-points
          maximizing racer into out
          finally (return out))))

(defun run-race (file distance)
  (declare (ftype (function (string fixnum) fixnum) run-race))
  (let* ((racers (str:lines (str:from-file file)))
         (progress (make-array (list (length racers) distance) :element-type 'fixnum)))
    (loop for i from 0
          for racer in racers
          maximizing (ppcre:register-groups-bind (run run-distance wait)
                         (".*can fly (\\d+) .* for (\\d+) .* for (\\d+) .*" racer)
                       (move i (parse-integer run) (parse-integer run-distance) (parse-integer wait) progress distance))
            into out
          finally (format t "Part 1: ~D~%" out)
          finally (format t "Part 2: ~D~%" (list-winner progress)))))

(time (run-race "../input/day14-test.txt" 1000))
(time (run-race "../input/day14-real.txt" 2503))
