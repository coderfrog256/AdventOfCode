(defun build-light-grid (x y)
  (make-array (list x y) :initial-element nil))

(aref (build-light-grid 1000 1000) 999 999)

(not nil)
(not t)

(defun update-grid (command grid init-position end-position)
  (loop for x
          from (first init-position)
            to (first end-position)
        do (loop for y
                   from (second init-position)
                     to (second end-position)
                 do (cond
                      ((equal command "on")
                       (setf (aref grid x y) t))
                      ((equal command "off")
                       (setf (aref grid x y) nil))
                      ((equal command "toggle")
                       (setf (aref grid x y)
                             (not (aref grid x y))))
                      (t (error (concatenate 'string "Unexpected input: " command)))))
        finally (return grid)))

(print (update-grid "on" (build-light-grid 10 10) '(2 2) '(3 5)))

(defun count-lit (grid)
  (loop for x from 0 to (1- (array-dimension grid 0))
        summing (loop for y from 0 to (1- (array-dimension grid 1))
                      counting (aref grid x y) into inner-sum
                      finally (return inner-sum))
          into outer-sum
        finally (return outer-sum)))


(count-lit (build-light-grid 4 4))
(count-lit (update-grid "on" (build-light-grid 10 10) '(2 2) '(3 5)))

(print
 (count-lit
  (update-grid "on"
               (build-light-grid 1000 1000)
               '(499 499)
               '(500 500))))

(defvar *text-input* "turn off 370,39 through 425,839")

(ql:quickload :str)
(ql:quickload :simple-scanf)

(require :simple-scanf)
(str:replace-all "turn" "" "return the slab")

(destructuring-bind (command init-x init-y end-x end-y)
    (simple-scanf:scanf "%s %d,%d through %d,%d" (str:replace-all "turn " "" *text-input*))
  (format t "Running ~A from (~A,~A) to (~A,~A)" command init-x init-y end-x end-y))

;;; Run it!
(with-open-file (f "../input/day6.txt")
  (loop with grid = (build-light-grid 1000 1000)
        for line = (read-line f nil)
        while line
        do (destructuring-bind (command init-x init-y end-x end-y)
               (simple-scanf:scanf "%s %d,%d through %d,%d" (str:replace-all "turn " "" line))
             (update-grid command grid (list init-x init-y) (list end-x end-y)))
        finally (return (count-lit grid))))

;;; Part 2

(defun build-int-light-grid (x y)
  (make-array (list x y) :initial-element 0))

(defun update-int-grid (command grid init-position end-position)
  (loop for x
          from (first init-position)
            to (first end-position)
        do (loop for y
                   from (second init-position)
                     to (second end-position)
                 do (cond
                      ((equal command "on")
                       (incf (aref grid x y) 1))
                      ((equal command "off")
                       (decf (aref grid x y) 1))
                      ((equal command "toggle")
                       (incf (aref grid x y) 2))
                      (t (error (concatenate 'string "Unexpected input: " command)))))
        finally (return grid)))

(defun count-brightness (grid)
  (loop for x from 0 to (1- (array-dimension grid 0))
        summing (loop for y from 0 to (1- (array-dimension grid 1))
                      summing (aref grid x y) into inner-sum
                      finally (return inner-sum))
          into outer-sum
        finally (return outer-sum)))

(print
 (count-brightness
  (update-int-grid
   "on"
   (build-int-light-grid 1000 1000)
   '(0 0)
   '(0 0))))

(print
 (count-brightness
  (update-int-grid
   "toggle"
   (build-int-light-grid 1000 1000)
   '(0 0)
   '(999 999))))

;;; Run it!
(print (with-open-file (f "../input/day6.txt")
         (loop with grid = (build-int-light-grid 1000 1000)
               for line = (read-line f nil)
               while line
               do (destructuring-bind (command init-x init-y end-x end-y)
                      (simple-scanf:scanf "%s %d,%d through %d,%d" (str:replace-all "turn " "" line))
                    (update-int-grid command grid (list init-x init-y) (list end-x end-y)))
               finally (return (count-brightness grid)))))
