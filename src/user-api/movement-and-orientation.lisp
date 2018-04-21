(in-package :daft)

;;------------------------------------------------------------

(defun strafe (distance)
  (let ((distance (float distance 0f0)))
    (v3:incf (%pos *self*)
             (m3:*v (m3:rotation-z (+ (%rot *self*)
                                      (radians -90f0)))
                    (v! 0 (float distance 1f0) 0)))))



(defun move-forward (distance)
  (setf (%pos *self*)
        (v3:+ (%pos *self*)
              (m3:*v (m3:rotation-z (%rot *self*))
                     (v! 0 (float distance 1f0) 0))))
  nil)


(defun angle-between (from-actor to-actor)
  (degrees
   (v2:angle-from (v2:from-angle (%rot from-actor))
                  (v2:- (s~ (%pos to-actor) :xy)
                        (s~ (%pos from-actor) :xy)))))

(defun angle-to (actor)
  (angle-between *self* actor))

(defun turn-left (angle)
  (incf (%rot *self*) (radians angle))
  nil)

(defun turn-right (angle)
  (incf (%rot *self*) (radians (- angle)))
  nil)

(defun turn-towards (actor angle)
  (let ((angle-to (angle-to actor)))
    (if (< angle-to 0)
        (turn-right angle)
        (turn-left angle))
    nil))

(defun strafe-towards (actor distance)
  (let* ((dir2-to (direction-to actor))
         (strafe-vec (v2:from-angle
                      (+ (%rot *self*) (radians -90f0))))
         (ang-to (v2:angle-from dir2-to strafe-vec)))
    (if (< ang-to 0)
        (strafe (- distance))
        (strafe distance))
    nil))

(defun direction-to (actor)
  (v2:normalize (v2:- (s~ (%pos actor) :xy)
                      (s~ (%pos *self*) :xy))))

(defun distance-to (actor)
  (let ((from (%pos actor))
        (to (%pos *self*)))
    (v2:length
     (v2:make (- (x to) (x from))
              (- (y to) (y from))))))

(defun move-towards (actor distance)
  (let* ((off (v2:*s (direction-to actor)
                     (float distance 0f0)))
         (pos (%pos *self*)))
    (incf (x pos) (x off))
    (incf (y pos) (y off))
    nil))

(defun move-away-from (actor distance)
  (move-towards actor (- distance))
  nil)

(defun compass-angle ()
  (degrees (%rot *self*)))

(defun compass-dir (&optional (distance 1f0))
  (let ((ang (%rot *self*)))
    (v2-n:*s (v2:from-angle ang)
             (float distance 0f0))))

(defun compass-dir-move (direction)
  ;; this kinda feels like the api breaking
  (let ((pos (%pos *self*)))
    (incf (x pos) (x direction))
    (incf (y pos) (y direction))
    nil))

(defun compass-angle-move (angle distance)
  (let ((pos (%pos *self*))
        (direction (v2:from-angle (radians angle))))
    (incf (x pos) (* (x direction) distance))
    (incf (y pos) (* (y direction) distance))
    nil))

(defun compass-angle-dir (compass-angle &optional (distance 1f0))
  (v2-n:*s (v2:from-angle (radians compass-angle))
           distance))

(defun snap-position (position grid-size)
  (let* ((grid-size (etypecase grid-size
                      (number (v! grid-size grid-size))
                      (vec2 grid-size)))
         (offset (v2:rotate position (%rot *self*)))
         (pos3 (%pos *self*))
         (p2 (v! (+ (x offset) (x pos3))
                 (+ (y offset) (y pos3)))))
    (v! (- (* (ceiling (- (x p2) (/ (x grid-size) 2)) (x grid-size))
              (x grid-size))
           (x p2))
        (- (* (ceiling (y p2) (y grid-size)) (y grid-size))
           (y p2)))))

;;------------------------------------------------------------
