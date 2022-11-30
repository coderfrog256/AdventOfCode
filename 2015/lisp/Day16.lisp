(ql:quickload :str)
(ql:quickload :cl-ppcre)

(defparameter *facts* (make-hash-table :test #'equal))
(setf (gethash "children" *facts*) 3)
(setf (gethash "cats" *facts*) 7)
(setf (gethash "samoyeds" *facts*) 2)
(setf (gethash "pomeranians" *facts*) 3)
(setf (gethash "akitas" *facts*) 0)
(setf (gethash "vizslas" *facts*) 0)
(setf (gethash "goldfish" *facts*) 5)
(setf (gethash "trees" *facts*) 3)
(setf (gethash "cars" *facts*) 2)
(setf (gethash "perfumes" *facts*) 1)

(defparameter *sue-regex* ".* (\\d+): (.*): (\\d+), (.*): (\\d+), (.*): (\\d+)$")

(dolist (line (str:lines (str:from-file "../input/day16-sues.txt")))
  (ppcre:register-groups-bind (num f1k f1v f2k f2v f3k f3v) (*sue-regex* line)
    (if (and
         (= (parse-integer f1v) (gethash f1k *facts*))
         (= (parse-integer f2v) (gethash f2k *facts*))
         (= (parse-integer f3v) (gethash f3k *facts*)))
        (print num))))

;; Part 2
(defparameter *comparator* (make-hash-table :test #'equal))
(setf (gethash "cats" *comparator*) #'<)
(setf (gethash "trees" *comparator*) #'<)
(setf (gethash "pomeranians" *comparator*) #'>)
(setf (gethash "goldfish" *comparator*) #'>)

(dolist (line (str:lines (str:from-file "../input/day16-sues.txt")))
  (ppcre:register-groups-bind (num f1k f1v f2k f2v f3k f3v) (*sue-regex* line)
    (if (and
         (funcall (gethash f1k *comparator* #'=) (gethash f1k *facts*) (parse-integer f1v))
         (funcall (gethash f2k *comparator* #'=) (gethash f2k *facts*) (parse-integer f2v))
         (funcall (gethash f3k *comparator* #'=) (gethash f3k *facts*) (parse-integer f3v)))
        (print num))))
