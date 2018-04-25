(in-package #:daft)

;;------------------------------------------------------------

(defun+ focus-camera (&optional (offset (v! 0 0)))
  (declare (profile t))
  (let* ((actor *self*)
         (offset (v2:rotate (etypecase offset
                              (number (v! 0 offset))
                              (vec2 offset))
                            (%rot actor)))
         (pos (v2:+ (%pos actor) offset))
         (camera (camera *current-scene*)))
    (setf (pos camera) pos)))

;;------------------------------------------------------------
