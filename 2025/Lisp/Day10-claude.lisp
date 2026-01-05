(ql:quickload '(:str :binding-arrows :hu.dwim.defclass-star :parseq :lparallel :alexandria))
(defpackage :advent (:use :cl :binding-arrows :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
(setf lparallel:*kernel* (lparallel:make-kernel 4))

;;; ============ PARSING ============

(defun parse-buttons (buttons)
  (->> buttons (str:substring 1 -1) (str:split "(") (mapcar #'frog:extract-numbers)))

(defun parse-line-2 (line)
  (cl-ppcre:register-groups-bind (garbage buttons target)
      ("\\[(.*)\\] (.*) \\{(.*)\\}" line)
    (declare (ignore garbage))
    (list (parse-buttons buttons)
          (mapcar #'parse-integer (str:split "," target)))))

;;; ============ CORE DATA STRUCTURES ============
;;; Use vectors instead of lists for speed

(defun list-to-vec (lst)
  (coerce lst '(simple-array fixnum (*))))

(defun vec-copy (vec)
  (let ((new (make-array (length vec) :element-type 'fixnum)))
    (dotimes (i (length vec))
      (setf (aref new i) (aref vec i)))
    new))

(defun vec-add! (vec indices amount)
  "Add amount to vec at each index in indices (destructive)"
  (dolist (i indices)
    (incf (aref vec i) amount))
  vec)

(defun vec-equal (v1 v2)
  (and (= (length v1) (length v2))
       (dotimes (i (length v1) t)
         (unless (= (aref v1 i) (aref v2 i))
           (return nil)))))

(defun vec-subtract (target state)
  "Return target - state as a new vector"
  (let ((result (make-array (length target) :element-type 'fixnum)))
    (dotimes (i (length target))
      (setf (aref result i) (- (aref target i) (aref state i))))
    result))

;;; ============ OPTIMIZATION 1: LOWER BOUND ============

(defun lower-bound (remaining target-vec n-counters button-max-per-counter)
  "Compute lower bound on presses needed.
   For each counter, we need at least ceil(remaining[i] / max-contribution[i]) presses.
   The overall lower bound is the max across all counters."
  (let ((lb 0))
    (dotimes (i n-counters)
      (let ((rem (aref remaining i)))
        (when (> rem 0)
          (let ((max-contrib (aref button-max-per-counter i)))
            (if (zerop max-contrib)
                (return-from lower-bound most-positive-fixnum)  ; impossible
                (setf lb (max lb (ceiling rem max-contrib))))))))
    lb))

(defun compute-max-per-counter (buttons n-counters)
  "For each counter, what's the max contribution from a single button press?"
  (let ((result (make-array n-counters :element-type 'fixnum :initial-element 0)))
    (dolist (btn buttons)
      (dolist (i btn)
        (when (< i n-counters)
          (setf (aref result i) (max (aref result i) 1)))))
    ;; Actually, each button contributes 1 per press to each index it covers
    ;; So max-per-counter[i] = 1 if any button covers i, else 0
    ;; But for better bounds, count how many buttons cover each counter
    (let ((counts (make-array n-counters :element-type 'fixnum :initial-element 0)))
      (dolist (btn buttons)
        (dolist (i btn)
          (when (< i n-counters)
            (incf (aref counts i)))))
      counts)))

;;; ============ OPTIMIZATION 2: CONSTRAINT PROPAGATION ============

(defun find-unique-coverage (buttons n-counters remaining)
  "Find counters that are covered by exactly one button.
   Returns (counter-idx button presses-needed) or nil."
  (dotimes (i n-counters)
    (when (> (aref remaining i) 0)
      (let ((covering nil)
            (count 0))
        (dolist (btn buttons)
          (when (member i btn)
            (incf count)
            (setf covering btn)
            (when (> count 1) (return))))  ; early exit
        (when (= count 1)
          ;; This counter is only covered by one button
          ;; We MUST press it exactly remaining[i] times
          (return-from find-unique-coverage
            (list i covering (aref remaining i)))))))
  nil)

(defun apply-forced-moves (buttons remaining n-counters)
  "Apply all forced moves. Returns (total-presses remaining-buttons remaining-target) or nil if impossible."
  (let ((total-presses 0)
        (current-remaining (vec-copy remaining))
        (current-buttons (copy-list buttons)))
    (loop
      (let ((forced (find-unique-coverage current-buttons n-counters current-remaining)))
        (if (null forced)
            (return (list total-presses current-buttons current-remaining))
            (destructuring-bind (counter-idx btn presses) forced
              (declare (ignore counter-idx))
              ;; Check if this would make any counter go negative
              (dolist (i btn)
                (when (< i n-counters)
                  (decf (aref current-remaining i) presses)
                  (when (< (aref current-remaining i) 0)
                    (return-from apply-forced-moves nil))))
              (incf total-presses presses)
              ;; Remove this button from consideration
              (setf current-buttons (remove btn current-buttons :test #'equal))))))))

;;; ============ OPTIMIZATION 3: BETTER SEARCH ============

(defun max-useful-presses (btn remaining n-counters)
  "Maximum useful presses for this button = min of remaining[i] for i in btn"
  (let ((max-p most-positive-fixnum))
    (dolist (i btn)
      (when (< i n-counters)
        (setf max-p (min max-p (aref remaining i)))))
    (if (= max-p most-positive-fixnum) 0 max-p)))

(defun all-zero-p (vec)
  (dotimes (i (length vec) t)
    (unless (zerop (aref vec i))
      (return nil))))

(defun any-negative-p (vec)
  (dotimes (i (length vec) nil)
    (when (< (aref vec i) 0)
      (return t))))

;;; ============ MAIN SOLVER: BRANCH AND BOUND WITH IDA* ============

(defvar *best* most-positive-fixnum)
(defvar *n-counters* 0)
(defvar *button-coverage* nil)

(defun solve-recursive (buttons remaining presses)
  "Recursively solve using branch and bound with constraint propagation."
  ;; Prune if we can't possibly beat best
  (when (>= presses *best*)
    (return-from solve-recursive nil))

  ;; Check if solved
  (when (all-zero-p remaining)
    (setf *best* (min *best* presses))
    (return-from solve-recursive presses))

  ;; Check for negative (invalid state)
  (when (any-negative-p remaining)
    (return-from solve-recursive nil))

  ;; Apply forced moves first
  (let ((forced-result (apply-forced-moves buttons remaining *n-counters*)))
    (when (null forced-result)
      (return-from solve-recursive nil))  ; impossible

    (destructuring-bind (forced-presses rem-buttons rem-remaining) forced-result
      (let ((new-presses (+ presses forced-presses)))
        ;; Prune again after forced moves
        (when (>= new-presses *best*)
          (return-from solve-recursive nil))

        ;; Check if solved after forced moves
        (when (all-zero-p rem-remaining)
          (setf *best* (min *best* new-presses))
          (return-from solve-recursive new-presses))

        ;; No buttons left but not solved
        (when (null rem-buttons)
          (return-from solve-recursive nil))

        ;; Compute lower bound
        (let ((lb (lower-bound rem-remaining rem-remaining *n-counters* *button-coverage*)))
          (when (>= (+ new-presses lb) *best*)
            (return-from solve-recursive nil)))

        ;; Branch on first button: try different number of presses
        (let* ((btn (first rem-buttons))
               (rest-buttons (rest rem-buttons))
               (max-p (max-useful-presses btn rem-remaining *n-counters*))
               (best-result nil))

          ;; Try from max down to 0 (often better to try larger values first)
          (loop for p from max-p downto 0 do
            (let ((new-remaining (vec-copy rem-remaining)))
              (dolist (i btn)
                (when (< i *n-counters*)
                  (decf (aref new-remaining i) p)))
              (unless (any-negative-p new-remaining)
                (let ((result (solve-recursive rest-buttons new-remaining (+ new-presses p))))
                  (when result
                    (setf best-result (if best-result (min best-result result) result)))))))

          best-result)))))

(defun solve-machine (buttons target)
  "Solve a single machine. Returns minimum presses needed."
  (let* ((*n-counters* (length target))
         (*best* most-positive-fixnum)
         (*button-coverage* (compute-max-per-counter buttons *n-counters*))
         (target-vec (list-to-vec target)))

    ;; Check if any counter has no covering buttons
    (dotimes (i *n-counters*)
      (when (and (> (aref target-vec i) 0)
                 (zerop (aref *button-coverage* i)))
        (return-from solve-machine nil)))  ; impossible

    (let ((result (solve-recursive buttons target-vec 0)))
      (or result most-positive-fixnum))))

;;; ============ MEET IN THE MIDDLE FOR HARD CASES ============

(defun enumerate-half (buttons remaining n max-presses)
  "Enumerate all reachable states from this set of buttons.
   Returns hash table: state-as-list -> min-presses"
  (let ((states (make-hash-table :test #'equal))
        (initial (make-list n :initial-element 0)))
    (setf (gethash initial states) 0)

    (labels ((explore (btns state presses)
               (when (<= presses max-presses)
                 (let ((state-list (coerce state 'list)))
                   (let ((old (gethash state-list states)))
                     (when (or (null old) (< presses old))
                       (setf (gethash state-list states) presses))))
                 (when btns
                   (let* ((btn (first btns))
                          (rest (rest btns))
                          (max-p (loop for i in btn
                                       when (< i n)
                                       minimize (- (aref remaining i) (aref state i)))))
                     (loop for p from 0 to (min max-p (- max-presses presses)) do
                       (let ((new-state (vec-copy state)))
                         (dolist (i btn)
                           (when (< i n)
                             (incf (aref new-state i) p)))
                         (explore rest new-state (+ presses p)))))))))
      (let ((init-vec (make-array n :element-type 'fixnum :initial-element 0)))
        (explore buttons init-vec 0)))
    states))

(defun solve-with-mitm (buttons target)
  "Solve using meet-in-the-middle when there are many buttons."
  (let* ((n (length target))
         (target-vec (list-to-vec target))
         (n-buttons (length buttons))
         (half (floor n-buttons 2))
         (first-half (subseq buttons 0 half))
         (second-half (subseq buttons half))
         (max-per-half (apply #'max target))  ; rough upper bound
         (best most-positive-fixnum))

    ;; Enumerate states reachable by first half
    (let ((first-states (enumerate-half first-half target-vec n max-per-half)))

      ;; For each state reachable by second half, check if complement exists
      (labels ((explore2 (btns state presses)
                 (when (< presses best)
                   ;; Check if first half can reach the complement
                   (let* ((complement (loop for i below n
                                            collect (- (aref target-vec i) (aref state i))))
                          (valid (every #'>= complement (make-list n :initial-element 0)))
                          (first-presses (and valid (gethash complement first-states))))
                     (when first-presses
                       (setf best (min best (+ presses first-presses)))))
                   (when btns
                     (let* ((btn (first btns))
                            (rest (rest btns))
                            (max-p (loop for i in btn
                                         when (< i n)
                                         minimize (- (aref target-vec i) (aref state i)))))
                       (loop for p from 0 to max-p do
                         (let ((new-state (vec-copy state)))
                           (dolist (i btn)
                             (when (< i n)
                               (incf (aref new-state i) p)))
                           (explore2 rest new-state (+ presses p)))))))))
        (let ((init-vec (make-array n :element-type 'fixnum :initial-element 0)))
          (explore2 second-half init-vec 0))))

    (if (= best most-positive-fixnum) nil best)))

;;; ============ HYBRID SOLVER ============

(defun solve-machine-hybrid (buttons target)
  "Choose best algorithm based on problem size."
  (let ((n-buttons (length buttons))
        (max-target (apply #'max target)))
    ;; Use MITM for problems with many buttons and moderate target values
    ;; Use branch-and-bound for others
    (if (and (> n-buttons 8) (< max-target 50))
        (or (solve-with-mitm buttons target)
            (solve-machine buttons target))
        (solve-machine buttons target))))

;;; ============ PART 2 MAIN ============

(defun part-2 (input)
  (let ((counter 0)
        (lines (->> input (str:trim) (str:lines))))
    (reduce #'+
            (lparallel:pmapcar
             (lambda (input)
               (let* ((idx (first input))
                      (line (second input))
                      (parsed (parse-line-2 line))
                      (buttons (first parsed))
                      (target (second parsed))
                      (result (solve-machine-hybrid buttons target)))
                 (format t "~A: Result = ~A~%" idx result)
                 result))
             (mapcar (lambda (l) (list (incf counter) l)) lines)))))

;;; ============ RUN ============

(frog:report (part-2 (frog:get-advent-of-code-input 2025 10)))
