(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache :lparallel))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(setf lparallel:*kernel* (lparallel:make-kernel 16))
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))

(defclass* brick () (xs ys zs xe ye ze id))
(defparameter *id-counter* -1)
(defun parse-brick (line)
  (register-groups-bind (xs ys zs xe ye ze) ("(\\d+),(\\d+),(\\d+)~(\\d+),(\\d+),(\\d+)" line)
    (make-instance 'brick :xs (parse-integer xs) :ys (parse-integer ys) :zs (parse-integer zs)
                          :xe (parse-integer xe) :ye (parse-integer ye) :ze (parse-integer ze)
                          :id (incf *id-counter*))))

(defun drop-brick (brick ground &optional (preview nil))
  (with-slots (xs ys zs xe ye ze id) brick
    (if (loop for x from xs to xe
              always (loop for y from ys to ye
                           always (every #'null (loop for z from zs to ze
                                                      collect (= 1 (- z (aref ground x y)))))))
        (progn (unless preview (decf zs) (decf ze))
               nil)
        t)))

(defun adjust-ground (brick ground)
  (with-slots (xs ys xe ye ze) brick
    (loop for x from xs to xe
          do (loop for y from ys to ye
                   do (setf (aref ground x y) (max (aref ground x y) ze)))
             finally (return ground))))

(defun build-ground (brick bricks)
  (with-slots (xs ys zs xe ye ze) brick
    (loop with ground = (make-array (list (1+ (apply #'max (mapcar #'xe-of bricks)))
                                          (1+ (apply #'max (mapcar #'ye-of bricks)))) :initial-element 0)
          for brick in bricks
          if (or (< (zs-of brick) zs) (< (ze-of brick) zs))
            do (setf ground (adjust-ground brick ground))
          finally (return ground))))

(defun part-1 (file)
  (loop with bricks = (sort (mapcar #'parse-brick (str:lines file)) #'< :key #'zs-of)
        with floating = bricks
        with ground = (make-array (list (1+ (apply #'max (mapcar #'xe-of bricks)))
                                        (1+ (apply #'max (mapcar #'ye-of bricks)))) :initial-element 0)
        with progress = (cl-tqdm:tqdm (length bricks) "")
        with last = nil
        while floating
        for brick = (first floating)
        if (drop-brick brick ground)
          do (setf floating (rest floating))
             (cl-tqdm:update progress)
             (setf ground (adjust-ground brick ground))
        finally (format t "Testing removal~%")
                (return (loop with progress = (cl-tqdm:tqdm (length bricks) "")
                              for test in bricks
                              for others = (remove test bricks)
                              do (cl-tqdm:update progress)
                              count (loop for other in others
                                          for ground = (build-ground other others)
                                          always (drop-brick other ground t))))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 22))))

(defun chain-reaction (others)
  (loop with out = (list)
        with to-check = others
        for other in others
        for ground = (build-ground other others)
        if (not (drop-brick other ground t))
          do (setf to-check (remove other to-check))
             (push (id-of other) out)
        finally (return (if out (append out (chain-reaction to-check)) out))))

(defun part-2 (file)
  (loop with bricks = (sort (mapcar #'parse-brick (str:lines file)) #'< :key #'zs-of)
        with floating = bricks
        with ground = (make-array (list (1+ (apply #'max (mapcar #'xe-of bricks)))
                                        (1+ (apply #'max (mapcar #'ye-of bricks)))) :initial-element 0)
        with progress = (cl-tqdm:tqdm (length bricks) "")
        with last = nil
        while floating
        for brick = (first floating)
        if (drop-brick brick ground)
          do (setf floating (rest floating))
             (cl-tqdm:update progress)
             (setf ground (adjust-ground brick ground))
        finally (format t "Testing removal~%")
                (setf progress (cl-tqdm:tqdm (length bricks) ""))
                (return (reduce #'+
                                (lparallel:pmapcar (lambda (test)
                                                     (cl-tqdm:update-locked progress)
                                                     (length (remove-duplicates (chain-reaction (remove test bricks)))))
                                                   bricks)))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 22))))
