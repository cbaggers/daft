(in-package :daft)

;;------------------------------------------------------------

(defun+ strafe (distance)
  (declare (profile t))
  (let ((distance (float distance 0f0)))
    (v3:incf (next-pos *self*)
             (m3:*v (m3:rotation-z (+ (next-rot *self*)
                                      (radians -90f0)))
                    (v3:make 0f0 (float distance 0f0) 0f0)))))



(defun+ move-forward (distance)
  (declare (profile t)
           (optimize (speed 3) (safety 1) (debug 1)))
  (v3:incf (next-pos *self*)
           (m3-n:*v (m3:rotation-z (next-rot *self*))
                    (v3:make 0f0 (float distance 0f0) 0f0)))
  nil)


(defun+ angle-between (from-actor to-actor)
  (declare (profile t))
  (degrees
   (v2:angle-from (v2:from-angle (%rot from-actor))
                  (v2:- (s~ (%pos to-actor) :xy)
                        (s~ (%pos from-actor) :xy)))))

(defun+ angle-to (actor)
  (declare (profile t))
  (degrees
   (v2:angle-from (v2:from-angle (next-rot *self*))
                  (v2:- (s~ (current-pos actor) :xy)
                        (s~ (next-pos *self*) :xy)))))

(defun+ turn-left (angle)
  (declare (profile t))
  (incf (next-rot *self*) (radians angle))
  nil)

(defun+ turn-right (angle)
  (declare (profile t))
  (decf (next-rot *self*) (radians angle))
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
                      (+ (next-rot *self*) (radians -90f0))))
         (ang-to (v2:angle-from dir2-to strafe-vec)))
    (if (< ang-to 0)
        (strafe (- distance))
        (strafe distance))
    nil))

(defun+ direction-to (actor)
  (declare (profile t))
  (v2:normalize (v2:- (s~ (current-pos actor) :xy)
                      (s~ (next-pos *self*) :xy))))

(defun+ offset-to (actor)
  (declare (profile t))
  (v2:normalize (v2:- (s~ (current-pos actor) :xy)
                      (s~ (next-pos *self*) :xy))))

(defun+ distance-to (actor)
  (declare (profile t))
  (let ((from (current-pos actor))
        (to (next-pos *self*)))
    (v2:length
     (v2:make (- (x to) (x from))
              (- (y to) (y from))))))

(defun+ move-towards (actor distance)
  (declare (profile t))
  (let* ((off (v2:*s (direction-to actor)
                     (float distance 0f0)))
         (pos (next-pos *self*)))
    (incf (x pos) (x off))
    (incf (y pos) (y off))
    nil))

(defun+ move-away-from (actor distance)
  (declare (profile t))
  (move-towards actor (- distance))
  nil)

(defun+ compass-angle ()
  (declare (profile t))
  (degrees (next-rot *self*)))

(defun+ compass-dir (&optional (distance 1f0))
  (declare (profile t))
  (let ((ang (next-rot *self*)))
    (v2-n:*s (v2:from-angle ang)
             (float distance 0f0))))

(defun+ compass-dir-move (direction)
  (declare (profile t))
  ;; this kinda feels like the api breaking
  (let ((pos (next-pos *self*)))
    (incf (x pos) (x direction))
    (incf (y pos) (y direction))
    nil))

(defun+ compass-angle-move (angle distance)
  (declare (profile t))
  (let ((pos (next-pos *self*))
        (direction (v2:from-angle (radians angle))))
    (incf (x pos) (* (x direction) distance))
    (incf (y pos) (* (y direction) distance))
    nil))

(defun+ compass-angle-dir (compass-angle &optional (distance 1f0))
  (declare (profile t))
  (v2-n:*s (v2:from-angle (radians compass-angle))
           distance))

(defun+ point-at (dir)
  (incf (next-rot *self*)
        (v2:angle-from (v! 0 1) dir)))

(defun+ snap-position (position grid-size)
  (declare (profile t))
  (let* ((grid-size (etypecase grid-size
                      (number (v2:make (float grid-size 0f0)
                                       (float grid-size 0f0)))
                      (vec2 grid-size)))
         (offset (v2:rotate position (next-rot *self*)))
         (pos3 (next-pos *self*))
         (p2 (v2:make (+ (x offset) (x pos3))
                      (+ (y offset) (y pos3)))))
    (v2:make (- (* (ceiling (- (x p2) (/ (x grid-size) 2)) (x grid-size))
                   (x grid-size))
                (x p2))
             (- (* (ceiling (y p2) (y grid-size)) (y grid-size))
                (y p2)))))

(defun+ set-position-relative-to (actor offset2)
  (let ((apos (current-pos actor)))
    (setf (next-pos *self*)
          (v3:make (+ (x apos) (x offset2))
                   (+ (y apos) (y offset2))
                   (z (next-pos *self*))))
    nil))

;;------------------------------------------------------------

(defun+ position-between (actor-a actor-b zero-to-one)
  (let* ((val (clamp 0f0 1f0 (float zero-to-one 0f0)))
         (a (current-pos actor-a))
         (b (current-pos actor-b))
         (res (v3:lerp a b val)))
    (setf (z res) (z (next-pos *self*)))
    (setf (next-pos *self*) res)))

;;------------------------------------------------------------

(defn-inline per-second ((val real)) single-float
  (declare (profile t))
  (* (float val 0f0) *per-frame-mult*))
