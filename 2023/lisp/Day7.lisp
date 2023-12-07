(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defparameter *joker-world* nil)
(defparameter *card-ordering* '()) ; Set by solve method

(defun count-chars (list)
  (loop with counts = (make-hash-table)
        for char in (if *joker-world* (remove #\J list) list)
        do (incf (gethash char counts 0))
        finally (return counts)))

(defun five-of-a-kind (hand)
  (if (and *joker-world* (member #\J hand))
      (if (= 5 (count #\J hand)) t
          (let ((counts (count-chars hand)))
            (= 5 (+ (count #\J hand) (first (sort (alexandria:hash-table-values counts) #'>))))))
      (every (lambda (card) (equal card (first hand))) hand)))

(let ((*joker-world* t)) (five-of-a-kind (coerce "AAAAJ" 'list)))

(defun four-of-a-kind (hand)
  (if (and *joker-world* (member #\J hand))
      (= 4 (+ (count #\J hand) (first (sort (alexandria:hash-table-values (count-chars hand)) #'>))))
      (= 4 (first (sort (alexandria:hash-table-values (count-chars hand)) #'>)))))

(defun full-house (hand)
  (if (and *joker-world* (member #\J hand))
      (let ((counts (alexandria:hash-table-values (count-chars hand))))
        (ecase (count #\J hand)
          (4 t)
          (3 t)
          (2 (<= 2 (first (sort counts #'>))))
          (1 (or (= 3 (first (sort counts #'>)))
                 (and (= 2 (first (sort counts #'>)))
                      (= (first (sort counts #'>)) (second (sort counts #'>))))))))
      (let ((counts (alexandria:hash-table-values (count-chars hand))))
        (and (= 3 (first (sort counts #'>)))
             (= 2 (second (sort counts #'>)))))))

(let ((*joker-world* t)) (full-house (coerce "26J395" 'list)))

(defun three-of-a-kind (hand)
  (if (and *joker-world* (member #\J hand))
      (<= 3 (+ (count #\J hand) (first (sort (alexandria:hash-table-values (count-chars hand)) #'>))))
      (= 3 (first (sort (alexandria:hash-table-values (count-chars hand)) #'>)))))

(let ((*joker-world* t)) (three-of-a-kind (coerce "ACBJJ" 'list)))

(defun two-pair (hand)
  (if (and *joker-world* (member #\J hand))
      (let ((counts (alexandria:hash-table-values (count-chars hand))))
        (ecase (count #\J hand)
          (4 t)
          (3 t)
          (2 t)
          (1 (= 2 (first (sort counts #'>))))))
      (let ((counts (alexandria:hash-table-values (count-chars hand))))
        (and (= 2 (first (sort counts #'>)))
             (= 2 (second (sort counts #'>)))))))

(let ((*joker-world* t)) (two-pair (coerce "ABBJA" 'list)))


(defun one-pair (hand)
  (if (and *joker-world* (member #\J hand)) t
      (let ((counts (alexandria:hash-table-values (count-chars hand))))
        (= 2 (first (sort counts #'>))))))

(defun high-card (hand)
  (every (lambda (count) (= 1 count)) (alexandria:hash-table-values (count-chars hand))))

(defun classify-hand (hand)
  (cond ((five-of-a-kind hand) 7)
        ((four-of-a-kind hand) 6)
        ((full-house hand) 5)
        ((three-of-a-kind hand) 4)
        ((two-pair hand) 3)
        ((one-pair hand) 2)
        ((high-card hand) 1)))

(defun left-first (left right)
  (loop for i from 0 below (min (length left) (length right))
        for left-card = (position (nth i left) *card-ordering*)
        for right-card = (position (nth i right) *card-ordering*)
        when (< left-card right-card) return nil
          when (> left-card right-card) return t))

(defun compare-hands (left-pair right-pair)
  (let* ((left-hand (first left-pair))
         (right-hand (first right-pair))
         (left-class (classify-hand left-hand))
         (right-class (classify-hand right-hand)))
    (cond ((< left-class right-class) t)
          ((> left-class right-class) nil)
          (t (left-first left-hand right-hand)))))

(defun parse-hand (line)
  (let ((parts (str:split #\Space line)))
    (list (coerce (first parts) 'list)
          (parse-integer (second parts)))))

(defun part-1 (file)
  (let* ((*joker-world* nil)
         (*card-ordering* '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2))
         (sorted (-<>> file
                   (str:lines)
                   (mapcar #'parse-hand)
                   (sort <> #'compare-hands))))
    (loop for i from 0 below (length sorted)
          for hand = (nth i sorted)
          sum (* (1+ i) (second hand)))))

(print (time (part-1 (frog:get-advent-of-code-input 2023 7 :input-suffix "test"))))
(print (time (part-1 (frog:get-advent-of-code-input 2023 7))))

(defun part-2 (file)
  (let* ((*joker-world* t)
         (*card-ordering* '(#\A #\K #\Q #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\J))
         (sorted (-<>> file
                   (str:lines)
                   (mapcar #'parse-hand)
                   (sort <> #'compare-hands))))
    (loop for i from 0 below (length sorted)
          for hand = (nth i sorted)
          sum (* (1+ i) (second hand)))))

(print (time (part-2 (frog:get-advent-of-code-input 2023 7 :input-suffix "test"))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 7))))
