(defvar *houses* (make-hash-table :test #'equal))
(setf *houses* (make-hash-table :test #'equal))

(setf (gethash '(1 2) *houses*) 1)
(gethash '(1 2) *houses*)
(incf (gethash '(3 2) *houses* 0) 1)
(gethash '(3 2) *houses*)
(incf (gethash '(3 4) *houses* 0) 1)

(loop for k being the hash-key of *houses* using (hash-value v)
      counting t into len
      finally (return len))

;;; Part 1
(defun count-houses (input)
  (let ((houses (make-hash-table :test #'equal))
        (x 0)
        (y 0))
    (incf (gethash (list x y) houses 0) 1)
    (loop for direction across input
          do (cond
               ((equal #\> direction) (incf x))
               ((equal #\< direction) (decf x))
               ((equal #\^ direction) (incf y))
               ((equal #\v direction) (decf y)))
          do (incf (gethash (list x y) houses 0) 1)
          do (format t "x=~A y=~A~%" x y))
    (loop for k being the hash-key of houses
          counting t into len
          finally (return len))))

(count-houses ">")
(count-houses "^>v<")
(count-houses "^v^v^v^v^v")

(with-open-file (f "../input/day3.txt")
  (let ((directions (read-line f)))
    (print (count-houses directions))))

(defvar *number-list* '(0 0))

(incf (first *number-list*))
(print *number-list*)
(incf (second *number-list*))
(incf (second *number-list*))
(print *number-list*)

;;; Part 2
(defun count-houses-robot (input)
  (let ((houses (make-hash-table :test #'equal))
        (santa (list 0 0))
        (robo (list 0 0)))
    (incf (gethash (list (first santa) (second santa)) houses 0) 1)
    (incf (gethash (list (first robo) (second robo)) houses 0) 1)
    (format t "Initial State: santa=~A robo=~A~%" santa robo)
    (loop for direction across input
          for iter from 1
          for actor = (if (evenp iter) robo santa)
          do (cond
               ((equal #\> direction) (incf (first actor)))
               ((equal #\< direction) (decf (first actor)))
               ((equal #\^ direction) (incf (second actor)))
               ((equal #\v direction) (decf (second actor))))
          do (incf (gethash (list (first actor) (second actor)) houses 0) 1)
          do (format t "Step ~A (~A): santa=~A robo=~A actor=~A~%" iter direction santa robo actor))
    (loop for k being the hash-key of houses
          counting t into len
          finally (return len))))


(print (count-houses-robot "^v"))
(print (count-houses-robot "^>v<"))
(print (count-houses-robot "^v^v^v^v^v"))

(with-open-file (f "../input/day3.txt")
  (let ((directions (read-line f)))
    (print (count-houses-robot directions))))
