(in-package :daft)

;;------------------------------------------------------------

(defun die ()
  (with-slots (dead) *self*
    (setf dead t))
  nil)

(defun is-dead (actor)
  (slot-value actor 'dead))

(defun is-alive (actor)
  (not (is-dead actor)))

;;------------------------------------------------------------
