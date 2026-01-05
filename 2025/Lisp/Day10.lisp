(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel :alexandria))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
(setf lparallel:*kernel* (lparallel:make-kernel 16))

(defparameter *line* "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

(defun parse-target (target)
  (->> target (frog:coerce-r 'list) (mapcar (lambda (c)
                                              (if (char= c #\.) nil t)))))
(defun parse-buttons (buttons)
  (->> buttons (str:substring 1 -1) (str:split "(") (mapcar #'frog:extract-numbers)))
(defun parse-line (line)
  (cl-ppcre:register-groups-bind (target buttons garbage)
      ("\\[(.*)\\] (.*) \\{(.*)\\}" line)
    (list (parse-target target) (parse-buttons buttons) garbage)))
(parse-line *line*)

(defun click-button (state button)
  (loop with new-state = (copy-list state)
        for index in button
        do (setf (nth index new-state) (not (nth index new-state)))
        finally (return new-state)))

(defun simulate (target buttons state steps state-steps)
  (if (equal target state) steps
      (loop for button in buttons
            for clicked = (click-button state button)
            for new = (1+ steps)
            for old = (gethash clicked state-steps)
            if (or (null old) (< new old))
              do (setf (gethash clicked state-steps) new)
                 (simulate target buttons clicked new state-steps)))
  (if (zerop steps) (gethash target state-steps)))

(defun part-1 (input)
  (loop with lines = (str:lines (str:trim input))
        for line in lines
        for parsed = (parse-line line)
        summing (simulate (first parsed) (second parsed)
                          (mapcar (lambda (n) nil)
                                  (first parsed)) 0 (serapeum:dict))))

(frog:report (part-1 (frog:get-advent-of-code-input 2025 10 :input-suffix "test")))
;;(frog:report (part-1 (frog:get-advent-of-code-input 2025 10)))

(defun parse-line-2 (line)
  (cl-ppcre:register-groups-bind (garbage buttons target)
      ("\\[(.*)\\] (.*) \\{(.*)\\}" line)
    (list garbage (parse-buttons buttons) (mapcar #'parse-integer (str:split "," target)))))

(defun valid (state target)
  (dotimes (i (length target))
    (when (> (nth i state) (nth i target))
      (return-from valid nil))) t)

(defclass* button () (id strength indices))
;; Print method for button, showing strength and indices
(defmethod print-object ((button button) stream)
  (format stream "Button[~a, ~a, ~a]"
          (id-of button)
          (strength-of button)
          (indices-of button)))

(defun click-button-2 (state button)
  (loop with new-state = (copy-list state)
        with strength = (strength-of button)
        for index in (indices-of button)
        do (incf (nth index new-state) strength)
        finally (return new-state)))

(defparameter *button-id* 0)
(defun expand-button (target button)
  (loop with zeros = (mapcar (lambda (n) 0) target)
        with out = (list)
        with id = (incf *button-id*)
        for i from 1
        for b = (make-instance 'button :id id :strength i :indices button)
        while (valid (click-button-2 zeros b) target)
        do (push b out)
        finally (return out)))

(defun contribution (target state button)
  (loop with clicked = (click-button-2 state button)
        for i from 0 below (length state)
        for l = (nth i clicked) for r = (nth i target)
        summing (abs (* r (- r l)))))

(defun claim-button (target state button buttons)
  (remove-if (lambda (b) (= (id-of b) (id-of button))) buttons))

(defparameter *best* 100)
(defun visit (temp target buttons state steps state-steps)
  (cond ;; ((= -1 (gethash state state-steps *best*)) -1)
        ;; ((or (> steps (gethash state state-steps *best*)) (not (valid state target)))
        ((or (> steps *best*) (not (valid state target))) nil)
        ((equal target state) (when (< steps *best*)
                                (format t "~A: Found new best! ~A vs ~A~%" temp steps *best*)
                                (setf *best* steps)) 0)
        (t (loop with local-best = nil
                 ;;with sorted-buttons = buttons
                 for button in buttons
                 for strength = (strength-of button)
                 for next-state = (click-button-2 state button)
                 for result = (visit temp target (claim-button target next-state button buttons) next-state (+ strength steps) state-steps)
                 if (and result (> result -1))
                   do (setf local-best (min (or local-best *best*) (+ strength result)))
                 finally (if nil ; local-best
                             (setf (gethash state state-steps) local-best)
                             )
                             ;;(setf (gethash state state-steps) -1)))
                         (return local-best)))))

(defun part-2 (input)
  (let ((counter 0))
  (reduce #'+
          (lparallel:pmapcar
           (lambda (input)
             (let* ((line (second input))
                    (parsed (parse-line-2 line))
                    (zeros (mapcar (lambda (n) 0) (third parsed)))
                    (expanded (alexandria:flatten (mapcar (lambda (l) (expand-button (third parsed) l)) (second parsed))))
                    (buttons (sort expanded #'< :key (lambda (l) (contribution (third parsed) zeros l))))
                    (*best* most-positive-fixnum)
                    (result (visit (first input) (third parsed) buttons zeros 0 (serapeum:dict))))
               (format t "~A: Got Result: ~A~%" (first input) result)
               result))
           (->> input (str:trim) (str:lines) (mapcar (lambda (l) (list (incf counter) l))))))))

;;(frog:report (part-2 (frog:get-advent-of-code-input 2025 10 :input-suffix "test")))
;;(frog:report (part-2 (frog:get-advent-of-code-input 2025 10)))
