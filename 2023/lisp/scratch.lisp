;; Sliding window function. Invokes a function on each overlapping window of size n in a list.
;; The last invocations will have less than n elements.
(defun sliding-window (length list fn)
  (loop for i from 0 to (- (length list) length) collect (funcall fn (subseq list i (+ i length)))))

(sliding-window 3 '(1 2 3 4 5 6 7 8 9) #'print)
