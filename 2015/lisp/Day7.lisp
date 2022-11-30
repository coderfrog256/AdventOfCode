(declaim (sb-ext:muffle-conditions cl:style-warning))
(ql:quickload :simple-scanf)
(require :simple-scanf)
(ql:quickload :str)

(defmacro if-scans ((vars) &body scans)
  (let ((matched (gensym)))
    `(or
      ,@(mapcar (lambda (scan)
                  (destructuring-bind (pattern input &rest body) scan
                    `(multiple-value-bind (,vars ,matched) (simple-scanf:scanf ,pattern ,input)
                       (when ,matched ,@body))))
                scans))))

(setf *test-input* "Hey good friend")

(if-scans (args)
  ("Hi %s!" *test-input* (print 1))
  ("Hey %s %s" *test-input* (print args))
  ("Hey %s" *test-input* (print args))
  ("Howdy %s" *test-input* (print 3)))

(defun resolve-circuit (label getters)
  (if (not (str:has-letters-p label))
      (mask-field (byte 16 0) (parse-integer label))
      (let ((result (funcall (gethash label getters))))
        (setf (gethash label getters) (lambda () result))
        result)))


(defun circuit-simulator (textfile)
  (let ((getters (make-hash-table :test #'equal)))
    (with-open-file (f textfile)
      (loop for line = (read-line f nil)
            while line
            do (if-scans (args)
                 ("%s AND %s -> %s" line (setf (gethash (third args) getters)
                                               (lambda ()
                                                 (logand (resolve-circuit (first args) getters)
                                                         (resolve-circuit (second args) getters)))))
                 ("%s OR %s -> %s" line (setf (gethash (third args) getters)
                                              (lambda ()
                                                (logior (resolve-circuit (first args) getters)
                                                        (resolve-circuit (second args) getters)))))
                 ("%s LSHIFT %s -> %s" line (setf (gethash (third args) getters)
                                                  (lambda ()
                                                    (ash (resolve-circuit (first args) getters)
                                                         (resolve-circuit (second args) getters)))))
                 ("%s RSHIFT %s -> %s" line (setf (gethash (third args) getters)
                                                  (lambda ()
                                                    (ash (resolve-circuit (first args) getters)
                                                         (* -1 (resolve-circuit (second args) getters))))))
                 ("NOT %s -> %s" line (setf (gethash (second args) getters)
                                            (lambda ()
                                              (lognot (resolve-circuit (first args) getters)))))
                 ("%s -> %s" line (setf (gethash (second args) getters)
                                        (lambda ()
                                          (resolve-circuit (first args) getters)))))))
    getters))

(funcall (gethash "a" (circuit-simulator "../input/day7.txt")))

; Part 2 - Replace the "-> b" set with the value you got for a
(funcall (gethash "a" (circuit-simulator "../input/day7-2.txt")))
