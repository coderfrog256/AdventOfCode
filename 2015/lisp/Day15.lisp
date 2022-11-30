(ql:quickload 'cl-ppcre)
(ql:quickload 'str)

(defconstant +input-regex+ ".* (\\S+),.* (\\S+),.* (\\S+),.* (\\S+),.* (\\S+)")

(defun read-ingredient (input ingredients)
  (ppcre:register-groups-bind (capacity durability flavor texture calories)
      (+input-regex+ input)
    (cons
     (cons (mapcar #'parse-integer (list capacity durability flavor texture))
           (parse-integer calories))
     ingredients)))

(defun parse-ingredients (file)
  (loop for line in (reverse (str:lines (str:from-file file)))
        with out = nil
        do (setf out (read-ingredient line out))
        finally (return out)))

(defun mix (ratios ingredients calories)
  (when calories
    (if (not (= calories (reduce #'+
                                 (mapcar (lambda (ratio ingredient) (* ratio (cdr ingredient)))
                                         ratios ingredients))))
        (return-from mix -1)))
  (flet ((no-neg (i) (if (< i 0) 0 i)))
    (let ((out 1)
          (ratioed (mapcar (lambda (ratio ingredient)
                             (mapcar (lambda (quality) (* ratio quality)) (car ingredient)))
                           ratios ingredients)))
      (dotimes (i (length (car ratioed)) out)
        (setf out
              (no-neg
               (* out
                  (reduce #'+
                          (mapcar (lambda (ingredient) (nth i ingredient))
                                  ratioed)))))))))

(defun gen-ratios (len total &optional (running-sum 0))
  (if (= 1 len)
      (list (list (- total running-sum)))
      (let (out)
        (dotimes (i (- (1+ total) running-sum) out)
          (dolist (tail (gen-ratios (1- len) total (+ i running-sum)))
            (push (cons i tail) out))))))

(defun max-mix (file &optional (calories nil))
  (let* ((ingredients (parse-ingredients file))
         (ratios (gen-ratios (length ingredients) 100)))
    (loop for ratio in ratios
          maximizing (mix ratio ingredients calories))))

(print (max-mix "../input/day15.txt"))
(time (print (max-mix "../input/day15-real.txt")))
(print (max-mix "../input/day15.txt" 500))
(time (print (max-mix "../input/day15-real.txt" 500)))
