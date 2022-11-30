(declaim (optimize (speed 3) (safety 0)))
(defvar *puzzle* 33100000)
(defvar *div-puzzle* (/ *puzzle* 10))

(defun get-presents (num)
  (declare (integer num))
  (loop with result = 0
        for i from 1 upto num
        when (= 0 (mod num i)) do (incf result i)
          finally (return result)))

(print (loop with max-presents = 0
             for i from 0 upto *div-puzzle*
             for presents = (get-presents i)
             when (> presents max-presents)
               do (progn
                    (setf max-presents presents)
                    (format t "~A => ~A (Max: ~A, Rem: ~A)~%" i presents max-presents (- *div-puzzle* max-presents)))
             when (>= presents *div-puzzle*)
               do (return i)))

;; Part 2
(defun get-presents-limited (num)
  (declare (integer num))
  (loop with result = 0
        for i from 1 upto num
        for (visits visited) = (multiple-value-list (floor num i))
        when (and
              (= 0 visited)
              (<= visits 50))
          do (incf result (* 11 i))
          finally (return result)))

;; Start from the last run. Since we only have 50 gifts, there's no way it could be before that.
(print (loop with max-presents = 0
             for i from 776160 upto *puzzle*
             for presents = (get-presents-limited i)
             when (> presents max-presents)
               do (progn
                    (setf max-presents presents)
                    (format t "~A => ~A (Max: ~A, Rem: ~A)~%" i presents max-presents (- *puzzle* max-presents)))
             when (>= presents *puzzle*)
               do (return i)))
