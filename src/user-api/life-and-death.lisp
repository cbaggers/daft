(in-package :daft)

;;------------------------------------------------------------

(defun+ die ()
  (declare (profile t))
  (unless (eq *self* *god*)
    (with-slots (dead kind) *self*
      (setf dead t)
      (setf (dirty-p kind) t)))
  nil)

(defun+ is-dead (actor)
  (declare (profile t))
  (slot-value actor 'dead))

(defun+ is-alive (actor)
  (declare (profile t))
  (not (is-dead actor)))

(defun+ kill (actor)
  (declare (profile t))
  (unless (eq actor *god*)
    (if (or (eq *self* *god*)
            (null *self*))
        (with-slots (dead) actor
          (setf dead t))
        (warn "Only *god* can strike an actor dead"))))

;;------------------------------------------------------------
