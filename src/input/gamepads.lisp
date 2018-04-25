(in-package #:daft)

;;------------------------------------------------------------

(defun+ init-pads ()
  (declare (profile t))
  (unless *sdl2-pads*
    (let ((ids '(0)))
      (setf *sdl2-pads*  (make-array 10 :initial-element nil))
      (sdl2-game-controller-db:load-db)
      (loop :for id :in ids :do
         (unless (aref *sdl2-pads* id)
           (setf (aref *sdl2-pads* id)
                 (sdl2:game-controller-open id))))
      (skitter.sdl2:enable-background-joystick-events))))

;;------------------------------------------------------------
