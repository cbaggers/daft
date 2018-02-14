(in-package :daft)

(define-god ()
  (:game-starting
   (spawn 'ship (v! 0 -140))
   (spawn 'alien (v! 0 200))
   (change-state :game-running))
  (:game-running
   nil))

(define-actor bullet ((:visual "bullet.png")
                      (speed 1))
  (:main
   (when (or (touching-p 'alien)
             ;; TODO: WAT! ↓↓↓↓↓↓
             (> (y (pos *self*)) 300))
     (die))
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
     (spawn 'bullet (v! 0 40)))))
