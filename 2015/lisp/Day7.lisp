(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun resolve-circuit (label getters)
  (if (not (str:has-letters-p label))
      (mask-field (byte 16 0) (parse-integer label))
      (let ((result (funcall (gethash label getters))))
        (setf (gethash label getters) (lambda () result)) ; For performance, replace the callbacks on our way out to avoid multiple evaluation.
        result)))

(defun circuit-simulator (textfile)
  (let ((getters (make-hash-table :test #'equal)))
    (loop for line in (str:lines (str:from-file textfile))
          do (or (register-groups-bind (lhs rhs val) ("(\\w+) AND (\\w+) -> (\\w+)" line)
                   (setf (gethash val getters) (lambda () (logand (resolve-circuit lhs getters) (resolve-circuit rhs getters)))))
                 (register-groups-bind (lhs rhs val) ("(\\w+) OR (\\w+) -> (\\w+)" line)
                   (setf (gethash val getters) (lambda () (logior (resolve-circuit lhs getters) (resolve-circuit rhs getters)))))
                 (register-groups-bind (lhs rhs val) ("(\\w+) LSHIFT (\\w+) -> (\\w+)" line)
                   (setf (gethash val getters) (lambda () (ash (resolve-circuit lhs getters) (resolve-circuit rhs getters)))))
                 (register-groups-bind (lhs rhs val) ("(\\w+) RSHIFT (\\w+) -> (\\w+)" line)
                   (setf (gethash val getters) (lambda () (ash (resolve-circuit lhs getters) (* -1 (resolve-circuit rhs getters))))))
                 (register-groups-bind (rhs val) ("NOT (\\w+) -> (\\w+)" line)
                   (setf (gethash val getters) (lambda () (lognot (resolve-circuit rhs getters)))))
                 (register-groups-bind (lhs val) ("(\\w+) -> (\\w+)" line)
                   (setf (gethash val getters) (lambda () (resolve-circuit lhs getters)))))
          finally (return getters))))


(funcall (gethash "a" (circuit-simulator "../input/day7.txt")))
(funcall (gethash "a" (circuit-simulator "../input/day7-2.txt")))
