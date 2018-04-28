(in-package :daft)

;;------------------------------------------------------------

(defun+ strafe (distance)
  (declare (profile t))
  (let ((distance (float distance 0f0)))
    (v3:incf (%pos *self*)
             (m3:*v (m3:rotation-z (+ (%rot *self*)
                                      (radians -90f0)))
                    (v! 0 (float distance 1f0) 0)))))



(defun+ move-forward (distance)
  (declare (profile t))
  (setf (%pos *self*)
        (v3:+ (%pos *self*)
              (m3:*v (m3:rotation-z (%rot *self*))
                     (v! 0 (float distance 1f0) 0))))
  nil)


(defun+ angle-between (from-actor to-actor)
  (declare (profile t))
  (degrees
   (v2:angle-from (v2:from-angle (%rot from-actor))
                  (v2:- (s~ (%pos to-actor) :xy)
                        (s~ (%pos from-actor) :xy)))))

(defun+ angle-to (actor)
  (declare (profile t))
  (angle-between *self* actor))

(defun+ turn-left (angle)
  (declare (profile t))
  (incf (%rot *self*) (radians angle))
  nil)

(defun+ turn-right (angle)
  (declare (profile t))
  (incf (%rot *self*) (radians (- angle)))
  nil)

(defun+ turn-towards (actor angle)
  (declare (profile t))
  (let ((angle-to (angle-to actor)))
    (if (< angle-to 0)
        (turn-right angle)
        (turn-left angle))
    nil))

(defun+ strafe-towards (actor distance)
  (declare (profile t))
  (let* ((dir2-to (direction-to actor))
         (strafe-vec (v2:from-angle
                      (+ (%rot *self*) (radians -90f0))))
         (ang-to (v2:angle-from dir2-to strafe-vec)))
    (if (< ang-to 0)
        (strafe (- distance))
        (strafe distance))
    nil))

(defun+ direction-to (actor)
  (declare (profile t))
  (v2:normalize (v2:- (s~ (%pos actor) :xy)
                      (s~ (%pos *self*) :xy))))

(defun+ distance-to (actor)
  (declare (profile t))
  (let ((from (%pos actor))
        (to (%pos *self*)))
    (v2:length
     (v2:make (- (x to) (x from))
              (- (y to) (y from))))))

(defun+ move-towards (actor distance)
  (declare (profile t))
  (let* ((off (v2:*s (direction-to actor)
                     (float distance 0f0)))
         (pos (%pos *self*)))
    (incf (x pos) (x off))
    (incf (y pos) (y off))
    nil))

(defun+ move-away-from (actor distance)
  (declare (profile t))
  (move-towards actor (- distance))
  nil)

(defun+ compass-angle ()
  (declare (profile t))
  (degrees (%rot *self*)))

(defun+ compass-dir (&optional (distance 1f0))
  (declare (profile t))
  (let ((ang (%rot *self*)))
    (v2-n:*s (v2:from-angle ang)
             (float distance 0f0))))

(defun+ compass-dir-move (direction)
  (declare (profile t))
  ;; this kinda feels like the api breaking
  (let ((pos (%pos *self*)))
    (incf (x pos) (x direction))
    (incf (y pos) (y direction))
    nil))

(defun+ compass-angle-move (angle distance)
  (declare (profile t))
  (let ((pos (%pos *self*))
        (direction (v2:from-angle (radians angle))))
    (incf (x pos) (* (x direction) distance))
    (incf (y pos) (* (y direction) distance))
    nil))

(defun+ compass-angle-dir (compass-angle &optional (distance 1f0))
  (declare (profile t))
  (v2-n:*s (v2:from-angle (radians compass-angle))
           distance))

(defun point-at (dir)
  (incf (%rot *self*)
        (v2:angle-from (v! 0 1) dir)))

(defun+ snap-position (position grid-size)
  (declare (profile t))
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

(defun set-position-relative-to (actor offset2)
  (let ((apos (%pos actor)))
    (setf (%pos *self*)
          (v2:+ apos offset2))
    nil))

;;------------------------------------------------------------

(defn-inline per-frame ((val real)) single-float
  (* (float val 0f0) *per-frame-mult*))
