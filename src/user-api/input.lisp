(in-package :daft)

;;------------------------------------------------------------

(defun+ set-angle-from-analog (analog-id
                              &optional
                                (dead-zone 0.1)
                                (gamepad (gamepad *default-gamepad-id*)))
  (declare (profile t))
  (let ((val (gamepad-2d gamepad analog-id)))
    (when (> (v2:length val) dead-zone)
      (setf (%rot *self*)
            (v2:angle-from (v! 0 1) val))))
  nil)

(defun+ compass-angle-from-analog (analog-id
                                  &optional
                                    (dead-zone 0.1)
                                    (gamepad (gamepad *default-gamepad-id*)))
  (declare (profile t))
  (let ((val (gamepad-2d gamepad analog-id)))
    (when (> (v2:length val) dead-zone)
      (degrees (v2:angle-from (v! 0 1) val)))))

(defun+ pad-button (button-id
                   &optional (gamepad (gamepad *default-gamepad-id*)))
  (declare (profile t))
  (gamepad-button gamepad button-id))

(defun+ pad-1d (1d-id
               &optional (gamepad (gamepad *default-gamepad-id*)))
  (declare (profile t))
  (gamepad-1d gamepad 1d-id))

;;------------------------------------------------------------
