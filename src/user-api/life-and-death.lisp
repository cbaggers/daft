(in-package :daft)

;;------------------------------------------------------------

(defun die ()
  (unless (eq *self* *god*)
    (with-slots (dead kind) *self*
      (setf dead t)
      (setf (dirty-p kind) t)))
  nil)

(defun is-dead (actor)
  (slot-value actor 'dead))

(defun is-alive (actor)
  (not (is-dead actor)))

(defun kill (actor)
  (unless (eq actor *god*)
    (if (or (eq *self* *god*)
            (null *self*))
        (with-slots (dead) actor
          (setf dead t))
        (warn "Only *god* can strike an actor dead"))))

;;------------------------------------------------------------
