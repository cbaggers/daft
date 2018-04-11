(in-package :daft)

;;------------------------------------------------------------

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
                              &optional
                                (dead-zone 0.1)
                                (gamepad (gamepad 0)))
  (let ((val (gamepad-2d gamepad analog-id)))
    (when (> (v2:length val) dead-zone)
      (setf (%rot *self*)
            (v2:angle-from (v! 0 1) val))))
  nil)

(defun pad-button (button-id
                   &optional (gamepad (gamepad 0)))
  (gamepad-button gamepad button-id))

(defun pad-1d (1d-id
               &optional (gamepad (gamepad 0)))
  (gamepad-1d gamepad 1d-id))

(defun next-frame (&optional range)
  (let ((start-frame (if range
                         (first range)
                         0))
        (anim-length (if range
                         (second range)
                         (slot-value *self* 'anim-length))))
    (with-slots (anim-frame) *self*
      (let ((frame (- anim-frame start-frame)))
        (setf anim-frame
              (float (+ (mod (+ (floor frame) 1)
                             anim-length)
                        start-frame)
                     0f0))))))

(defun advance-frame (amount &optional range)
  (let ((start-frame (if range
                         (first range)
                         0))
        (anim-length (if range
                         (second range)
                         (slot-value *self* 'anim-length))))
    (with-slots (anim-frame) *self*
      (let ((frame (- anim-frame start-frame)))
        (setf anim-frame
              (float (+ (mod (+ frame amount)
                             anim-length)
                        start-frame)
                     0f0))))))

(defun compass-angle ()
  (degrees (%rot *self*)))

(defun compass-dir (&optional (distance 1f0))
  (let ((ang (%rot *self*)))
    (v2-n:*s (v2:from-angle ang)
             (float distance 0f0))))

(defun compass-dir-move (direction)
  (let ((pos (%pos *self*)))
    (incf (x pos) (x direction))
    (incf (y pos) (y direction))
    nil))

(defun coll-with (actor-kind)
  (with-slots (kind id) *self*
    (with-slots (coll-with) kind
      (setf (gethash actor-kind coll-with) t))
    (let ((results
           (gethash actor-kind
                    (actors-coll-results kind))))
      (when (and id results)
        (aref results id)))))

;;------------------------------------------------------------

(defun play-sound (sound-name)
  (declare (ignore sound-name))
  nil)

(defun mouse-x ()
  0f0)

(defun actors-in-range (distance &optional actor-kind)
  (declare (ignore distance actor-kind)))

(defun in-screen-p (&optional (actor *self*))
  ;; TODO: WAT! ↓↓↓↓↓↓
  (and (< (y (%pos actor)) 400)
       (> (y (%pos actor)) -400)
       (< (x (%pos actor)) 400)
       (> (x (%pos actor)) -400)))

(defun in-world-p (&optional (actor *self*))
  ;; TODO: WAT! ↓↓↓↓↓↓
  (and (< (y (%pos actor)) 1024)
       (> (y (%pos actor)) -1024)
       (< (x (%pos actor)) 1024)
       (> (x (%pos actor)) -1024)))

;;------------------------------------------------------------
