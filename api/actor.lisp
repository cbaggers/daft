(in-package :daft)

(defvar *noisy-spawn* t)

;;------------------------------------------------------------

(defun spawn (actor-kind-name pos
              &rest args &key &allow-other-keys)
  (with-slots ((parent-pos pos)
               (parent-rot rot))
      *self*
    (%spawn actor-kind-name
            parent-pos
            parent-rot
            pos args
            *spawn-into*)))

(defun %spawn (actor-kind-name parent-pos parent-rot
               pos args into)
  (let* ((hack-name (intern (symbol-name actor-kind-name)
                            :daft))
         (human-name (get-name))
         (actor (init-actor
                 (apply #'make-instance hack-name
                        args)
                 args))
         (next (init-actor
                (apply #'make-instance hack-name
                       args)
                args)))
    (setf (slot-value actor 'next) next)
    (setf (slot-value next 'next) actor)
    (setf (slot-value actor 'debug-name) human-name)
    (setf (slot-value next 'debug-name) human-name)
    (when *noisy-spawn*
      (format t "~%; ~a has spawned!" human-name))

    (setf (slot-value actor 'pos)
          (v3:+ parent-pos
                (m3:*v (m3:rotation-z parent-rot)
                       (v! (x pos) (y pos) 0))))
    (setf (slot-value actor 'rot)
          parent-rot)

    (vector-push-extend actor into)
    actor))

(defun strafe (distance)
  (let ((distance (float distance 0f0)))
    (with-slots (pos rot) *self*
      (v3:incf pos
               (m3:*v (m3:rotation-z (+ rot (radians -90f0)))
                      (v! 0 (float distance 1f0) 0))))))

(defun die ()
  (with-slots (dead next) *self*
    (setf dead t)
    (with-slots (dead) next
      (setf dead t)))
  nil)

(defun play-sound (sound-name)
  (declare (ignore sound-name))
  nil)

(defun mouse-x ()
  0f0)

(defun move-forward (distance)
  (with-slots (pos rot) *self*
    (setf pos
          (v3:+ pos
                (m3:*v (m3:rotation-z rot)
                       (v! 0 (float distance 1f0) 0))))))

(defun gamepad-button-a ())

(defun actors-in-range (distance &optional actor-kind)
  (declare (ignore distance actor-kind)))

(defun offscreen-p ()
  nil)

(defun is-dead (actor)
  (slot-value actor 'dead))

(defun is-alive (actor)
  (not (is-dead actor)))

(defun angle-between (from-actor to-actor)
  (with-slots (pos rot) from-actor
    (degrees
     (v2:angle-from (v2:from-angle rot)
                    (v2:- (s~ (slot-value to-actor 'pos) :xy)
                          (s~ pos :xy))))))

(defun angle-to (actor)
  (angle-between *self* actor))

(defun turn-left (angle)
  (with-slots (rot) *self*
    (incf rot (radians angle))))

(defun turn-right (angle)
  (with-slots (rot) *self*
    (incf rot (radians (- angle)))))

(defun turn-towards (actor angle)
  (let ((angle-to (angle-to actor)))
    (if (< angle-to 0)
        (turn-right angle)
        (turn-left angle))))

(defun strafe-towards (actor distance)
  (let* ((dir-to (direction-to actor))
         (strafe-vec
          (m3:*v (m3:rotation-z (+ (%rot *self*)
                                   (radians -90f0)))
                 (v! 0 1f0 0)))
         (dp (v3:dot dir-to strafe-vec)))
    (if (< dp 0)
        (strafe (- distance))
        (strafe distance))))

(defun direction-to (actor)
  (v3:normalize (v3:- (%pos actor) (%pos *self*))))

(defun move-towards (actor distance)
  (let ((dir (direction-to actor)))
    (v3:incf (%pos *self*)
             (v3:*s dir (float distance 0f0)))))

(defun move-away-from (actor distance)
  (move-towards actor (- distance)))

;;------------------------------------------------------------
