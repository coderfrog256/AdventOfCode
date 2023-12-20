(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)

(defclass* module () (name (loop-start nil) (loop-length nil) (connections (list)) (inbound (list))))
(defmethod send ((module module) sender value modules send-queue i) send-queue)

(defclass* flip-flop (module) ((enabled nil)))
(defmethod send ((module flip-flop) sender value modules send-queue i)
  (with-slots (name connections enabled loop-start loop-length) module
    (when (equal value :low)
      (setf enabled (not enabled))
      (if enabled
          (if (not loop-start) (setf loop-start i)
              (if (not loop-length) (setf loop-length (- i loop-start)))))
      (loop for connection in connections
            do (setf send-queue (nconc send-queue (list (list name connection (if enabled :high :low)))))))
    send-queue))

(defclass* conjunction (module) ((most-recent (list))))
(defmethod send ((module conjunction) sender value modules send-queue i)
  (with-slots (name connections most-recent loop-start loop-length) module
    (setf (cdr (assoc sender most-recent :test #'equal)) value)
    (if (every (lambda (x) (equal x :high)) (mapcar #'cdr most-recent))
        (progn
          (loop for connection in connections
                do (setf send-queue (nconc send-queue (list (list name connection :low))))
                finally (return send-queue)))
        (progn
          (if (not loop-start) (setf loop-start i)
              (if (not loop-length) (setf loop-length (- i loop-start))))
          (loop for connection in connections
                do (setf send-queue (nconc send-queue (list (list name connection :high))))
                finally (return send-queue))))))

(defclass* broadcast (module) ())
(defmethod send ((module broadcast) sender value modules send-queue i)
  (with-slots (name connections) module
    (loop for connection in connections
          do (setf send-queue (nconc send-queue (list (list name connection value))))
          finally (return send-queue))))

(defun parse (file)
  (loop with out = (serapeum:dict)
        for line in (str:lines file)
        for (source dest-csv) = (str:split " -> " line)
        for unprefixed = (str:substring 1 nil source)
        for dests = (str:split ", " dest-csv)
        if (string= "broadcaster" source)
          do (setf (gethash source out) (make-instance 'broadcast :name source :connections dests))
        if (str:starts-with-p "%" source)
          do (setf (gethash unprefixed out) (make-instance 'flip-flop :name unprefixed :connections dests))
        if (str:starts-with-p "&" source)
          do (setf (gethash unprefixed out) (make-instance 'conjunction :name unprefixed :connections dests))
        finally (loop for source being the hash-values of out
                      for source-name = (name-of source)
                      for connections = (connections-of source)
                      do (loop for conection in connections if (not (gethash conection out))
                               do (setf (gethash conection out) (make-instance 'module :name conection)))
                         (loop for dest being the hash-values of out
                               for dest-name = (name-of dest)
                               if (member dest-name connections :test #'equal)
                                 do (push source-name (inbound-of dest))
                                    (if (conjunctionp dest)
                                        (setf (most-recent-of dest) (acons source-name :low (most-recent-of dest))))))
        finally (return out)))

(defun part-1 (file)
  (let* ((modules (parse file))
         (send-queue (list))
         (high 0) (low 0))
    (dotimes (i 1000 (* high low))
      (incf low)
      (setf send-queue (send (gethash "broadcaster" modules) nil :low modules send-queue i))
      (loop while send-queue
            for (sender dest value) = (pop send-queue)
            do (if (equal :high value) (incf high) (incf low))
            do (setf send-queue (send (gethash dest modules) sender value modules send-queue i))))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 20))))

(defun part-2 (file)
  (let* ((modules (parse file))
         (send-queue (list))
         (rx-sources (mapcar (lambda (x) (gethash x modules)) (-> "rx" (gethash modules) inbound-of first
                                                                (gethash modules) inbound-of))))
    (dotimes (i most-positive-fixnum)
      (setf send-queue (send (gethash "broadcaster" modules) nil :low modules send-queue i))
      (loop while send-queue
            for (sender dest value) = (pop send-queue)
            do (setf send-queue (send (gethash dest modules) sender value modules send-queue i)))
      (when (every #'loop-length-of rx-sources)
        (return-from part-2 (reduce #'lcm (mapcar #'loop-length-of rx-sources)))))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 20))))
