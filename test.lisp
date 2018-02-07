(in-package :daft)

(define-actor bullet ((:visual "bullet.png")
                      (speed 1))
  (:main
   (when (or (touching-p 'ship) (> y 300))
     (die))
   (move-forward 2)))

(define-actor ship ((:visual "shuttle2.png")
                    (start-time (now) t)
                    (fire (make-stepper (seconds 0.1)
                                        (seconds 0.1))))
  (:main
   (incf x (* 2 (x (mouse-move (mouse)))))
   (when (and (mouse-button (mouse) mouse.left)
              (funcall fire))
     (spawn 'bullet (v! 0 40)))))

(defun refresh-actors ()
  (setf *current-actors*
        (make-array 0 :element-type 'actor :adjustable t
                    :fill-pointer 0))
  (spawn! 'ship (v! 0 140))
  (spawn! 'ship (v! 0 40))
  (spawn! 'ship (v! 0 -140)))
