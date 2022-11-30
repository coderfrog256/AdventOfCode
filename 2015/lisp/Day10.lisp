(defun look-and-say (text)
  (declare (optimize (speed 3) (safety 0)))
  (let (last
        (count 0)
        (out (list)))
    (flet ((new-number (i)
             (when last
               (dolist (char (coerce
                              (reverse (write-to-string count)) 'list))
                 (push char out))
               (push last out))
             (setf count 1)
             (setf last i)))
      (loop for i across text
            do (if (equal last i)
                   (incf count)
                   (new-number i))
            finally (new-number nil))
      (coerce (reverse out) 'string))))

;; Test Input
(loop with text = "1"
      for i from 1 upto 5
      do (setf text (look-and-say text))
      do (format t "~D) ~A~%" i text)
      finally (print (length text)))

;; Part 1
(loop with text = "3113322113"
      for i from 1 upto 40
      do (setf text (look-and-say text))
      finally (print (length text)))

;; Part 2
(loop with text = "3113322113"
      for i from 1 upto 50
      do (setf text (look-and-say text))
      finally (print (length text)))
