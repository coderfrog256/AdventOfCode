(subseq "000001dbbfa" 0 5)

(declaim (inline is-valid))
(defun is-valid (hash)
  (declare (type string hash))
  (equal (subseq hash 0 5) "00000"))

(is-valid "000001dbbfa")
(is-valid "001001dbbfa")

(ql:quickload 'ironclad)
(ql:quickload 'md5)

(format t "~a~%" (ironclad:byte-array-to-hex-string (md5:md5sum-string "abcdef609043")))
(is-valid (ironclad:byte-array-to-hex-string (md5:md5sum-string "abcdef609043")))

(concatenate 'string "ab" "cd")
(concatenate 'string "ab" (write-to-string 1))

(defun is-valid-hash (prefix int)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type string prefix))
  (declare (type fixnum int))
  (is-valid
   (ironclad:byte-array-to-hex-string
    (md5:md5sum-string
     (concatenate 'string prefix
                  (write-to-string int))))))

(is-valid-hash "abcdef" 609043)

(disassemble 'is-valid-hash)

(defun discover-hash (prefix)
  (loop for i from 1
        until (is-valid-hash prefix i)
        finally (return i)))

(discover-hash "abcdef")
(discover-hash "pqrstuv")
(print (discover-hash "yzbqklnj"))

;;; Part 2
(declaim (inline is-valid-six))
(defun is-valid-six (hash)
  (declare (type string hash))
  (equal (subseq hash 0 6) "000000"))

(defun is-valid-hash-six (prefix int)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type string prefix))
  (declare (type fixnum int))
  (is-valid-six
   (ironclad:byte-array-to-hex-string
    (md5:md5sum-string
     (concatenate 'string prefix
                  (write-to-string int))))))

(defun discover-hash-six (prefix)
  (loop for i from 1
        until (is-valid-hash-six prefix i)
        finally (return i)))

(print (discover-hash-six "yzbqklnj"))
