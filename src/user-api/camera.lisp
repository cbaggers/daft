(in-package #:daft)

;;------------------------------------------------------------

(defun+ focus-camera (&optional (offset (v! 0 0)))
  (declare (profile t))
  (let* ((actor *self*)
         (offset (v2:rotate (etypecase offset
                              (number (v2:make 0f0 offset))
                              (vec2 offset))
                            (next-rot actor)))
         (apos (next-pos actor))
         (camera (camera *current-scene*))
         (cam-pos (pos camera)))
    (incf (x cam-pos) (+ (x apos) (x offset)))
    (incf (y cam-pos) (+ (y apos) (y offset)))
    nil))

;;------------------------------------------------------------
