(ql:quickload :cl-ppcre)
(ql:quickload :str)

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for item in list
                 append (mapcar (lambda (l) (cons item l))
                                (all-permutations (remove item list)))))))

(defun build-relation-table (lines)
  (loop for line in lines
        with out = (make-hash-table :test #'equal)
        do (ppcre:register-groups-bind (source action amount target)
               ("(.*) would (.*) (\\d+) happiness.* to (.*).$" line)
             (setf (gethash (list source target) out)
                   (if (equal action "gain")
                       (parse-integer amount)
                       (- (parse-integer amount)))))
        finally (return out)))

(defun get-attendees (table)
  (loop with out = nil
        for (person _) being the hash-keys of table
        if (not (find person out :test #'equal))
          do (push person out)
        finally (return out)))

(defun evaluate-happiness (table arrangement)
  (loop with last-idx = (1- (length arrangement))
        for i from 0 upto last-idx
        for person = (nth i arrangement)
        for person-left = (if (< i 1) (nth last-idx arrangement) (nth (1- i) arrangement))
        for person-right = (if (= i last-idx) (first arrangement) (nth (1+ i) arrangement))
        summing (gethash (list person person-left) table) into happy
        summing (gethash (list person person-right) table) into happy
        finally (return happy)))

(let ((table (build-relation-table (str:lines (str:from-file "../input/day13.txt")))))
  (loop for perm in (all-permutations (get-attendees table))
        maximizing (evaluate-happiness table perm) into max-happy
        finally (return max-happy)))

;; Part 2
(defun add-me (table)
  (loop for person in (get-attendees table)
        do (setf (gethash (list "Frog" person) table) 0)
        do (setf (gethash (list person "Frog") table) 0)
        finally (return table)))

(let ((table (build-relation-table (str:lines (str:from-file "../input/day13.txt")))))
  (add-me table)
  (loop for perm in (all-permutations (get-attendees table))
        maximizing (evaluate-happiness table perm) into max-happy
        finally (return max-happy)))
