(ql:quickload '(:str :cl-ppcre :hu.dwim.defclass-star))
(defpackage :advent (:use :cl :hu.dwim.defclass-star))
(in-package :advent)
(defclass* lense () (lable num))
(defclass* box () ((lenses (list))))

(defun hash-string (str)
  (loop with value = 0
        for char across str
        do (incf value (char-code char))
           (setf value (* value 17))
           (setf value (mod value 256))
        finally (return value)))

(defun part-1 (line) (loop with parts = (str:split "," line) for part in parts summing (hash-string part)))
(print (time (part-1 (str:trim (frog:get-advent-of-code-input 2023 15)))))

(defun remove-from-box (boxes hash label)
  (let ((box (nth hash boxes)))
    (setf (lenses-of box) (remove-if (lambda (lense) (equal (lable-of lense) label)) (lenses-of box)))))

(defun add-to-box (boxes hash label num)
  (let* ((box (nth hash boxes))
         (lense (find-if (lambda (lense) (equal (lable-of lense) label)) (lenses-of box))))
    (if lense
        (setf (num-of lense) num)
        (setf (lenses-of box) (nconc (lenses-of box) (list (make-instance 'lense :lable label :num num)))))))

(defun focusing-power (boxes)
  (loop for box in boxes and i from 0
        summing (loop for lense in (lenses-of box) and j from 0
                      summing (* (1+ i) (1+ j) (num-of lense)))))

(defun part-2 (line)
  (loop with boxes = (mapcar (lambda (_) (make-instance 'box)) (make-list 256))
        with parts = (str:split "," line)
        for part in parts
        for part-split = (cl-ppcre:split "[-=]" part)
        for label = (first part-split)
        if (= 2 (length part-split))
          do (add-to-box boxes (hash-string label) label (parse-integer (second part-split)))
        else do (remove-from-box boxes (hash-string label) label)
        finally (return (focusing-power boxes))))
(print (time (part-2 (str:trim (frog:get-advent-of-code-input 2023 15)))))
