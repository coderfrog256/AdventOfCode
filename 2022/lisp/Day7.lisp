(ql:quickload '(:str :cl-ppcre :binding-arrows :series :alexandria))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun parse-directories (lines)
  (loop with stack = '()
        with directories = (make-hash-table :test #'equal)
        for line in lines
        do (register-groups-bind (dir) ("cd (\\S+)" line)
             (cond ((equal dir "/") nil)
                   ((equal dir "..") (pop stack))
                   (t (push dir stack))))
           (register-groups-bind (dir) ("dir (\\S+)" line)
             (setf (gethash (str:join "/" (reverse (cons dir stack))) directories) 0))
           (register-groups-bind (size file) ("(\\d+) (\\S+)" line)
             (setf (gethash (str:join "/" (reverse (cons file stack))) directories) (parse-integer size)))
        finally (return directories)))

(defun directory-size (directory directory-table)
  (loop for path being the hash-key of directory-table using (hash-value size)
        when (str:starts-with-p directory path) summing size))

(defun directory-sizes (directory-table)
  (loop with out = (make-hash-table :test #'equal)
        for path being the hash-key of directory-table using (hash-value size)
        when (zerop size) do (setf (gethash path out) (directory-size path directory-table))
          finally (return out)))

;; Part 1
(time (print
       (->> "../input/day7.txt"
         (str:from-file)
         (str:lines)
         (parse-directories)
         (directory-sizes)
         (alexandria:hash-table-values)
         (remove-if (lambda (size) (> size 100000)))
         (reduce #'+))))

(defun free-memory (directory-sizes)
  (loop with total-used = (directory-size "" directory-sizes)
        with directories = (directory-sizes directory-sizes)
        with unused = (- 70000000 total-used)
        with to-free = (- 30000000 unused)
        for size being the hash-value of directories
        when (>= size to-free) minimizing size))

;; Part 2
(time (print
       (->> "../input/day7.txt"
         (str:from-file)
         (str:lines)
         (parse-directories)
         (free-memory))))
