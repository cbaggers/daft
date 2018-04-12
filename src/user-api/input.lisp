(in-package :daft)

;;------------------------------------------------------------

(defun set-angle-from-analog (analog-id
                              &optional
                                (dead-zone 0.1)
                                (gamepad (gamepad 0)))
  (let ((val (gamepad-2d gamepad analog-id)))
    (when (> (v2:length val) dead-zone)
      (setf (%rot *self*)
            (v2:angle-from (v! 0 1) val))))
  nil)

(defun pad-button (button-id
                   &optional (gamepad (gamepad 0)))
  (gamepad-button gamepad button-id))

(defun pad-1d (1d-id
               &optional (gamepad (gamepad 0)))
  (gamepad-1d gamepad 1d-id))

;;------------------------------------------------------------
