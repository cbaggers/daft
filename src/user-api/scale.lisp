(in-package :daft)

;;------------------------------------------------------------

(defun+ scale ()
  (declare (profile t))
  (slot-value *self* 'scale))

(defun+ (setf scale) (value)
  (declare (profile t))
  (setf (slot-value *self* 'scale)
        (clamp 0.1f0 1f0 (float value 0f0))))

;;------------------------------------------------------------
