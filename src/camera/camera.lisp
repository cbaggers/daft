(in-package #:daft)

;;------------------------------------------------------------

(defclass camera ()
  ((pos
    :initarg :pos
    :initform (v! 0 0)
    :accessor pos)))

(defun make-camera (pos)
  (check-type pos vec2)
  (make-instance 'camera :pos pos))

;;------------------------------------------------------------
