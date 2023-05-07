(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun hash-name (s)
  (let ((counts (make-hash-table :test #'equal)))
    (loop for c across s do (if (alpha-char-p c) (incf (gethash c counts 0))))
    (coerce (mapcar #'car
                    (subseq (sort (alexandria:hash-table-alist counts)
                                  (lambda (a b)
                                    (or (> (cdr a) (cdr b))
                                        (and (= (cdr a) (cdr b))
                                             (char< (car a) (car b))))))
                            0 5)) 'string)))

(defun real-room-value (name)
  (if (->> name
        (str:substring 0 (position #\- name :from-end t))
        (hash-name)
        (equal (str:substring (1+ (position #\[ name)) -1 name)))
      (parse-integer (str:substring (1+ (position #\- name :from-end t)) (position #\[ name :from-end t) name))
      0))

(print (time
        (->> "../input/day4.txt"
          (str:from-file)
          (str:lines)
          (mapcar #'real-room-value)
          (reduce #'+))))

;; Part 2
(defun decrypt-name (name amount)
  (coerce
   (loop for char across name
         collect (if (char= char #\-) #\Space
                     (code-char (+ (char-code #\a)
                                   (mod (+ amount (- (char-code char) (char-code #\a))) 26)))))
   'string))

(print (time (->> "../input/day4.txt"
               (str:from-file)
               (str:lines)
               (mapcar (lambda (line)
                         (let* ((parts (str:split #\- line))
                                (name (str:join "-" (butlast parts)))
                                (amount (parse-integer (str:substring 0 (position #\[ (first (last parts))) (first (last parts)) ))))
                           (list (decrypt-name name amount) amount))))
               (remove-if-not (lambda (x) (search "north" (first x))))
               (first)
               (second))))
