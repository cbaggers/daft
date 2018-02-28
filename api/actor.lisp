(in-package :daft)

(defvar *noisy-spawn* t)

;;------------------------------------------------------------

(defun spawn (actor-kind-name pos
              &rest args &key &allow-other-keys)
  (%spawn actor-kind-name
          (%pos *self*)
          (%rot *self*)
          pos args
          *spawn-into*))

(defun strafe (distance)
  (let ((distance (float distance 0f0)))
    (v3:incf (%pos *self*)
             (m3:*v (m3:rotation-z (+ (%rot *self*)
                                      (radians -90f0)))
                    (v! 0 (float distance 1f0) 0)))))

(defun die ()
  (with-slots (dead) *self*
    (setf dead t))
  nil)

(defun move-forward (distance)
  (setf (%pos *self*)
        (v3:+ (%pos *self*)
              (m3:*v (m3:rotation-z (%rot *self*))
                     (v! 0 (float distance 1f0) 0))))
  nil)


(defun is-dead (actor)
  (slot-value actor 'dead))

(defun is-alive (actor)
  (not (is-dead actor)))

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
  (let* ((dir-to (direction-to actor))
         (strafe-vec
          (m3:*v (m3:rotation-z (+ (%rot *self*)
                                   (radians -90f0)))
                 (v! 0 1f0 0)))
         (dp (v3:dot dir-to strafe-vec)))
    (if (< dp 0)
        (strafe (- distance))
        (strafe distance))
    nil))

(defun direction-to (actor)
  (v3:normalize (v3:- (%pos actor) (%pos *self*))))

(defun move-towards (actor distance)
  (let ((dir (direction-to actor)))
    (v3:incf (%pos *self*)
             (v3:*s dir (float distance 0f0)))
    nil))

(defun move-away-from (actor distance)
  (move-towards actor (- distance))
  nil)

(defun set-angle-from-analog (analog-id
                              &optional (gamepad (gamepad 0)))
  (setf (%rot *self*)
        (v2:angle-from (v! 0 1)
                       (gamepad-2d gamepad analog-id)))
  nil)

(defun next-frame ()
  (with-slots (anim-frame anim-length) *self*
    (setf anim-frame (mod (+ anim-frame 1) anim-length))))

;;------------------------------------------------------------

(defun play-sound (sound-name)
  (declare (ignore sound-name))
  nil)

(defun mouse-x ()
  0f0)



(defun actors-in-range (distance &optional actor-kind)
  (declare (ignore distance actor-kind)))

(defun offscreen-p (&optional (actor *self*))
  ;; TODO: WAT! ↓↓↓↓↓↓
  (> (y (%pos actor)) 300))


;;------------------------------------------------------------
