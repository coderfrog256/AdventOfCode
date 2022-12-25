(ql:quickload :str)
(defun to-list (s) (coerce s 'list))

(defun item-priority (item)
  (if (upper-case-p item)
      (+ 27 (- (char-code item) (char-code #\A)))
      (+ 1 (- (char-code item) (char-code #\a)))))

(defun bag-value (bag)
  (let* ((split-point (floor (length bag) 2))
         (first-half (to-list (subseq bag 0 split-point)))
         (second-half (to-list (subseq bag split-point))))
    (item-priority (first (intersection first-half second-half)))))

;; Part 1
(loop for line in (str:lines (str:from-file "../input/day3.txt"))
      summing (bag-value line) into sum
      finally (return sum))

(defun group-lists (n lists)
  (loop for list in lists
        with out = '()
        with pending = '()
        do (push list pending)
           (when (= n (length pending))
             (push pending out)
             (setf pending '()))
        finally (return out)))

;; Part 2
(reduce
 #'+ (mapcar
      (lambda (group)
        (item-priority
         (first (intersection
                 (intersection
                  (to-list (first group))
                  (to-list (second group)))
                 (to-list (third group))))))
      (group-lists 3 (str:lines (str:from-file "../input/day3.txt")))))
