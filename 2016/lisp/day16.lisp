(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0)))

(defun flip (c) (if (eq :0 c) :1 :0))
(defun expand (s) (nconc s (list :0) (mapcar #'flip (reverse s))))
(defun string-byte (text) (mapcar (lambda (n) (if (eq #\0 n) :0 :1)) (coerce text 'list)))
(defun byte-string (s) (coerce (mapcar (lambda (n) (if (eq :0 n) #\0 #\1)) s) 'string))

(defun expand-to (s len)
  (loop with out = (string-byte s)
        while (< (length out) len)
        do (setf out (expand out))
        finally (return (str:substring 0 len out))))

(defun checksum (s)
  (loop with out = nil
        for i from 0
        for first = (pop s)
        for second = (pop s)
        while second
        do (push (if (eq first second) :1 :0) out)
        finally (return (nreverse out))))

(defun wrapper (len)
  (let ((s (expand-to "10001001100000001" len)))
    (loop while (= 0 (mod (length s) 2)) do (setf s (checksum s)) finally (return s))))

(print (time (byte-string (wrapper 272))))
(print (time (byte-string (wrapper 35651584))))
