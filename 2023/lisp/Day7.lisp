(ql:quickload '(:str :binding-arrows :alexandria :hu.dwim.defclass-star))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(defconstant +joker+ #\J)
(defparameter *joker-world* nil)   ; Differs based on part
(defparameter *card-ordering* '())
(defclass* hand () (cards bid (classification nil)))
(defun count-chars (list)
  (loop with counts = (make-hash-table)
        for char in (if *joker-world* (remove +joker+ list) list)
        do (incf (gethash char counts 0))
        finally (return counts)))

(defun five-of-a-kind (hand)
  (if (and *joker-world* (member +joker+ hand))
      (if (= 5 (count +joker+ hand)) t
          (= 5 (+ (count +joker+ hand) (first (sort (alexandria:hash-table-values (count-chars hand)) #'>)))))
      (every (lambda (card) (equal card (first hand))) hand)))

(defun four-of-a-kind (hand)
  (if (and *joker-world* (member +joker+ hand))
      (= 4 (+ (count +joker+ hand) (first (sort (alexandria:hash-table-values (count-chars hand)) #'>))))
      (= 4 (first (sort (alexandria:hash-table-values (count-chars hand)) #'>)))))

(defun full-house (hand)
  (if (and *joker-world* (member +joker+ hand))
      (let* ((counts (alexandria:hash-table-values (count-chars hand)))
             (sorted (sort counts #'>))
             (first (first sorted))
             (second (second sorted)))
        (case (count +joker+ hand)
          (2 (= 2 first))
          (1 (or (= 3 first)
                 (and (= 2 first) (= first second))))
          (t t)))
      (let* ((counts (alexandria:hash-table-values (count-chars hand)))
             (sorted (sort counts #'>)))
        (and (= 3 (first sorted)) (= 2 (second sorted))))))

(defun three-of-a-kind (hand)
  (if (and *joker-world* (member +joker+ hand))
      (= 3 (+ (count +joker+ hand) (first (sort (alexandria:hash-table-values (count-chars hand)) #'>))))
      (= 3 (first (sort (alexandria:hash-table-values (count-chars hand)) #'>)))))

(defun two-pair (hand)
  (if (and *joker-world* (member +joker+ hand))
      (let ((counts (alexandria:hash-table-values (count-chars hand))))
        (if (= 1 (count +joker+ hand)) ; If only one joker, then we need an existing pair
            (= 2 (first (sort counts #'>)))
            t)) ; If two or more, they can make their own pairs.
      (let* ((counts (alexandria:hash-table-values (count-chars hand)))
             (sorted (sort counts #'>)))
        (and (= 2 (first sorted))
             (= 2 (second sorted))))))

(defun one-pair (hand)
  (if (and *joker-world* (member +joker+ hand)) t ; A joke can make anything a pair.
      (= 2 (first (sort (alexandria:hash-table-values (count-chars hand)) #'>)))))

(defun high-card (hand) (every (lambda (count) (= 1 count)) (alexandria:hash-table-values (count-chars hand))))

(defun classify-hand (hand)
  (cond ((five-of-a-kind hand) 7)
        ((four-of-a-kind hand) 6)
        ((full-house hand) 5)
        ((three-of-a-kind hand) 4)
        ((two-pair hand) 3)
        ((one-pair hand) 2)
        ((high-card hand) 1)))

(defun right-first (left right)
  (loop for i from 0 below (min (length left) (length right))
        for left-card = (position (nth i left) *card-ordering*)
        for right-card = (position (nth i right) *card-ordering*)
        when (< left-card right-card) return nil
          when (> left-card right-card) return t))

(defun compare-hands (left right)
  (cond ((< (classification-of left) (classification-of right)) t)
         ((> (classification-of left) (classification-of right)) nil)
         (t (right-first (cards-of left) (cards-of right)))))

(defun parse-hand (line)
  (let* ((parts (str:split #\Space line))
         (hand (make-instance 'hand :cards (coerce (first parts) 'list) :bid (parse-integer (second parts)))))
    (setf (classification-of hand) (classify-hand (cards-of hand)))
    hand))

(defun run-solution (file)
  (let ((sorted (-<>> file (str:lines) (mapcar #'parse-hand) (sort <> #'compare-hands) (mapcar #'bid-of))))
    (loop for i from 0 below (length sorted) sum (* (1+ i) (nth i sorted)))))

(let ((*joker-world* nil) ; Not in joker world, jokers are regular cards.
      (*card-ordering* '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2))) ; Regular order
  (print (time (run-solution (frog:get-advent-of-code-input 2023 7))))) ; Part 1

(let ((*joker-world* t) ; Joker world, enable shape shifting.
      (*card-ordering* '(#\A #\K #\Q #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\J))) ; Jokers are lowest.
  (print (time (run-solution (frog:get-advent-of-code-input 2023 7))))) ; Part 2
