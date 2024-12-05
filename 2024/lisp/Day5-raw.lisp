(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defparameter *input* (frog:get-advent-of-code-input 2024 5 :input-suffix "test"))
(defparameter *input* (frog:get-advent-of-code-input 2024 5))

(let ((parts (str:split frog:+double-newline+ *input*)))
  (defparameter *ordering* (first parts))
  (defparameter *pages* (car (last parts))))

(defclass* page () (number (dependencies nil)))

(defmethod print-object ((obj page) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "Page(~a, ~a)"
            (number-of obj)
            (dependencies-of obj))))

(defun page-is-valid (page position pages)
  (loop for dependency in (dependencies-of page)
        if (> position (or (position dependency pages :test #'equal) position))
          do (return nil)
        finally (return t)))

(defmethod sort-page (left right)
  (let ((left-number (number-of left))
        (left-dependencies (dependencies-of left))
        (right-number (number-of right))
        (right-dependencies (dependencies-of right)))
    (cond ((member right-number left-dependencies :test #'equal) t)
          ((member left-number right-dependencies :test #'equal) nil)
          (t nil))))

(defun mid (list)
  (nth (floor (length list) 2) list))

(let* ((parts (str:split frog:+double-newline+ *input*))
       (ordering (first parts))
       (pages-input (car (last parts)))
       (pages (make-hash-table :test #'equal)))
  (loop for order in (str:lines ordering)
        for (number dependency) = (str:split "|" order)
        for page = (gethash number pages (make-instance 'page :number number))
        do (push dependency (dependencies-of page))
           (setf (gethash number pages) page))
  (loop for line in (str:lines pages-input)
        for numbers = (str:split "," line)
        if (loop for number in numbers
                 for page = (gethash number pages (make-instance 'page :number number))
                 for i from 0
                 if (not (page-is-valid page i numbers)) do
                   (progn
                     (return nil))
                 finally (return t))
          collect numbers into result
        finally (return (reduce #'+ (mapcar #'parse-integer (mapcar #'mid result)))))
  )

(let* ((parts (str:split frog:+double-newline+ *input*))
       (ordering (first parts))
       (pages-input (car (last parts)))
       (pages (make-hash-table :test #'equal)))
  (loop for order in (str:lines ordering)
        for (number dependency) = (str:split "|" order)
        for page = (gethash number pages (make-instance 'page :number number))
        do (push dependency (dependencies-of page))
           (setf (gethash number pages) page))
  (loop for line in (str:lines pages-input)
        for numbers = (str:split "," line)
        if (not (loop for number in numbers
                      for page = (gethash number pages (make-instance 'page :number number))
                      for i from 0
                      if (not (page-is-valid page i numbers)) do
                        (progn
                          (return nil))
                      finally (return t)))
          collect numbers into result
        ;;finally (return (reduce #'+ (mapcar #'parse-integer (mapcar #'mid (sort (mapcar (lambda (r) (gethash r pages)) result) #'sort-page))))))
        finally (return (reduce #'+
                                (mapcar (lambda (line)
                                          (parse-integer
                                           (number-of
                                            (mid
                                             (sort
                                              (mapcar (lambda (r) (gethash r pages (make-instance 'page :number r))) line) #'sort-page)))))
                                        result))))
  )
