(ql:quickload :str)
(defvar *registers* (make-hash-table))
(defvar *instruction-pointer* 0)

(defun reset-state ()
  (setf *instruction-pointer* 0)
  (loop for char from (char-code #\a) upto (char-code #\z)
        do (setf (gethash (code-char char) *registers*) 0)))

(defun get-register (register)
  (gethash register *registers*))

(defun hlf (register)
  (setf (gethash register *registers*) (/ (gethash register *registers*) 2))
  (format t " -> ~A~%" (get-register register))
  (incf *instruction-pointer*))

(defun tpl (register)
  (setf (gethash register *registers*) (* (gethash register *registers*) 3))
  (format t " -> ~A~%" (get-register register))
  (incf *instruction-pointer*))

(defun inc (register)
  (incf (gethash register *registers*))
  (format t " -> ~A~%" (get-register register))
  (incf *instruction-pointer*))

(defun jmp (offset)
  (incf *instruction-pointer* offset)
  (format t " -> ~A~%" *instruction-pointer*))

(defun jie (register offset)
  (if (evenp (gethash register *registers*))
      (incf *instruction-pointer* offset)
      (incf *instruction-pointer*))
  (format t " -> ~A~%" *instruction-pointer*))

(defun jio (register offset)
  (if (= 1 (gethash register *registers*))
      (incf *instruction-pointer* offset)
      (incf *instruction-pointer*))
  (format t " -> ~A~%" *instruction-pointer*))

(defun eval-line (line)
  (let* ((args (str:split " " line))
         (raw-second-arg (read-from-string (second args)))
         (second-arg (if (numberp raw-second-arg) raw-second-arg (char (second args) 0))))
    (if (= 3 (length args))
        (funcall (read-from-string (first args)) second-arg (read-from-string (third args)))
        (funcall (read-from-string (first args)) second-arg))))

(defun run-script (script)
  (let ((lines (str:lines (str:from-file script))))
    (loop until (>= *instruction-pointer* (length lines))
          for command = (nth *instruction-pointer* lines)
          do (format t "~A Eval: ~A" *instruction-pointer* command)
          do (eval-line command))))

;; Execution
(reset-state)
(run-script "../input/day23-test.txt")
(get-register #\a)

(reset-state)
(run-script "../input/day23.txt")
(get-register #\a)
(get-register #\b)

(reset-state)
(setf (gethash #\a *registers*) 1)
(run-script "../input/day23.txt")
(get-register #\b)
