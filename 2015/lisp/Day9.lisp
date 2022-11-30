(ql:quickload :str)
(ql:quickload :cl-ppcre)

(defun read-routes (file)
  (loop with out = (list)
        for line in (str:lines (str:from-file file))
        do (ppcre:register-groups-bind (source dest dist) ("(.*) to (.*) = (\\d+)" line)
             (push (list source dest (parse-integer dist)) out)
             (push (list dest source (parse-integer dist)) out))
        finally (return out)))

(defun build-full-routes (direct-routes)
  (let* ((out (list))
         (unique-cities (remove-duplicates
                         (mapcar (lambda (route)
                                   (first route)) direct-routes)
                         :test #'equal))
         (max-route-len (length unique-cities)))
    (labels ((handle-city (cur-source visited running-dist)
               (when (find cur-source visited :test #'equal)
                 (return-from handle-city))
               (push cur-source visited)
               (unwind-protect
                    (progn
                      (when (= (length visited) max-route-len)
                        (push (append (reverse visited) (list running-dist)) out)
                        (return-from handle-city))
                      (loop for (source dest dist) in direct-routes
                            for new-dist = (+ running-dist dist)
                            do (when (equal source cur-source)
                                 (handle-city dest visited new-dist)))
                      (pop visited)))))
      (loop for city in unique-cities
            do (handle-city city (list) 0)
            finally (return out)))))

(loop for full-route in (build-full-routes (read-routes "../input/day9.txt"))
      for dist = (car (last full-route))
      minimizing dist into min-dist
      finally (print min-dist))
