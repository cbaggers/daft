(in-package :daft)

;;------------------------------------------------------------

(defun scale ()
  (slot-value *self* 'scale))

(defun (setf scale) (value)
  (setf (slot-value *self* 'scale)
        (clamp 0.1f0 1f0 (float value 0f0))))

;;------------------------------------------------------------
