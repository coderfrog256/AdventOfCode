(defvar *weapons* (list
                   (list 8 4 0)
                   (list 10 5 0)
                   (list 25 6 0)
                   (list 40 7 0)
                   (list 74 8 0)
                   ))

(defvar *armors* (list
                  (list 0 0 0)
                  (list 13 0 1)
                  (list 31 0 2)
                  (list 53 0 3)
                  (list 75 0 4)
                  (list 102 0 5)
                  ))

(defvar *rings* (list
                 (list 0 0 0)
                 (list 0 0 0)
                 (list 25 1 0)
                 (list 50 2 0)
                 (list 100 3 0)
                 (list 20 0 1)
                 (list 40 0 2)
                 (list 80 0 3)
                 ))

(defclass actor ()
  ((hp :initarg :hp :accessor hp)
   (cost :initform -1 :initarg :cost :accessor cost)
   (armor :initarg :armor :accessor armor)
   (damage :initarg :damage :accessor damage)))

(defvar *boss* (make-instance 'actor
                              :hp 104
                              :armor 1
                              :damage 8))

(defun can-beat (player boss)
  (let* ((player-damage (max 1 (- (damage player) (armor boss))))
         (boss-damage (max 1 (- (damage boss) (armor player))))
         (player-moves (ceiling (/ (hp boss) player-damage)))
         (boss-moves (ceiling (/ (hp player) boss-damage))))
    (<= player-moves boss-moves)))

(defun build-players ()
  (loop with out = (list)
        for weapon in *weapons*
        do (loop for armor in *armors*
                 do (loop for left-ring in *rings*
                          for i from 0
                          do (loop for right-ring in *rings*
                                   for j from 0
                                   when (/= i j)
                                     do (push (make-instance
                                               'actor
                                               :hp 100
                                               :cost (+
                                                      (first weapon)
                                                      (first armor)
                                                      (first left-ring)
                                                      (first right-ring))
                                               :damage (+
                                                        (second weapon)
                                                        (second armor)
                                                        (second left-ring)
                                                        (second right-ring))
                                               :armor (+
                                                       (third weapon)
                                                       (third armor)
                                                       (third left-ring)
                                                       (third right-ring)))
                                              out))))
        finally (return out)))

(loop for player in (build-players)
      when (can-beat player *boss*)
        minimizing (cost player))

;; Part w
(loop for player in (build-players)
      when (not (can-beat player *boss*))
        maximizing (cost player))
