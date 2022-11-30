(declaim (optimize (speed 3) (safety 0) (debug 0)))
(ql:quickload :str)
(ql:quickload :cl-ppcre)

(defvar *test-output*
  "ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF")

(defun build-mapping (file-name)
  (let ((mapping (make-hash-table :test #'equal)))
    (dolist (line (str:lines (str:from-file file-name)) mapping)
      (ppcre:register-groups-bind (source dest) ("(.*) => (.*)" line)
        (let* ((new-mapping (gethash source mapping (list))))
          (push dest new-mapping)
          (setf (gethash source mapping) new-mapping))))))

(defun build-reverse-mapping (file-name)
  (let ((mapping (make-hash-table :test #'equal)))
    (dolist (line (str:lines (str:from-file file-name)) mapping)
      (ppcre:register-groups-bind (dest source) ("(.*) => (.*)" line)
        (let* ((new-mapping (gethash source mapping (list))))
          (push dest new-mapping)
          (setf (gethash source mapping) new-mapping))))))

(defun expand-chars (mapping input &optional (seen (make-hash-table :test #'equal)))
  (loop with expanded = (list)
        for i from 0 upto (1- (length input))
        for prefix = (str:substring 0 i input)
        do (loop for regex being the hash-key in mapping using (hash-value replacements)
                 do (loop for replacement in replacements
                          for (suffix replaced) = (multiple-value-list (ppcre:regex-replace regex input replacement :start i))
                          for combined = (str:concat prefix suffix)
                          when (and replaced (not (gethash combined seen nil)))
                            do (push combined expanded)))
        finally (return expanded)))

(length (remove-duplicates (expand-chars (build-mapping "../input/day19-test.txt") "HOHOHO") :test #'equal))

;;; Part 2
(defun expand-to (mapping input output &optional (iterations 0) (seen (make-hash-table :test #'equal)))
  (format t "(~A) #~A ~A ~A~%" (hash-table-count seen) iterations (length output) output)
  (setf (gethash output seen) t)
  (when (equal input output)
    (return-from expand-to iterations))
  (loop for candidate in (sort (remove-duplicates (expand-chars mapping output seen)) (lambda (l r) (< (length l) (length r))))
        for result = (if (gethash candidate seen nil)
                         (format t "(~A) Skipping ~A~%" (hash-table-count seen) candidate)
                         (expand-to mapping input candidate (1+ iterations) seen))
        when result do (return result)))

(print (expand-to (build-reverse-mapping "../input/day19-input.txt") "e" *test-output*))
