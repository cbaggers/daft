(in-package :daft)

(define-god ((spawn-counter (make-stepper (seconds 40)
                                          (seconds 40))))
  (:game-starting
   (spawn 'ship (v! 0 -140))
   (change-state :game-running))
  (:game-running
   (when (funcall spawn-counter)
     (spawn 'alien (v! 0 200)))))

(define-actor bullet ((:visual "bullet.png")
                      (fired-by nil)
                      (speed 1))
  (:main
   (let ((touching (touching-p 'alien)))
     (when (or touching
               ;; TODO: WAT! ↓↓↓↓↓↓
               (> (y (slot-value *self* 'pos)) 300))
       (die)))
   (move-forward 2)))

(define-actor alien ((:visual "alien.png")
                     (health 10))
  (:main
   (strafe (* (sin (now)) 2))
   (when (touching-p 'bullet)
     (decf health)
     (when (<= health 0)
       (die)))))

(define-actor ship ((:visual "shuttle2.png")
                    (start-time (now) t)
                    (fire (make-stepper (seconds 0.1)
                                        (seconds 0.1))))
  (:main
   (strafe (* 2 (x (mouse-move (mouse)))))
   (when (and (mouse-button (mouse) mouse.left)
              (funcall fire))
     (spawn 'bullet (v! 0 40)
            :fired-by *self*))))
