(in-package :daft)

;;------------------------------------------------------------

(defun depth ()
  (let ((pos (%pos *self*)))
    (z pos)))

(defun (setf depth) (value)
  (let ((pos (%pos *self*))
        (val (clamp 0f0 100f0 (float value 0f0))))
    (setf (z pos) val)))

;;------------------------------------------------------------
