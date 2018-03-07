(in-package :daft)

(define-god ((spawn-counter (make-stepper (seconds 400)
                                          (seconds 400))))
  (:game-starting
   (spawn 'ship (v! 0 -140))
   (change-state :game-running))
  (:game-running
   ;; (when (funcall spawn-counter)
   ;;   (spawn 'alien (v! 0 200)))
   ))

(define-actor bullet ((:visual "bullet.png")
                      (fired-by nil)
                      (speed 1))
  (:main
   (let ((touching (touching-p 'alien)))
     (when (or touching (offscreen-p))
       (die)))
   (move-forward 2)))

(define-actor dead-shuttle ((:visual "arfy-walking6.png")
                            (:tile-count (4 4)))
  (:main
   (advance-frame 0.1 '(0 4))
   ;;(next-frame)
   ))

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
                    (speed 0f0)
                    (max-speed 10f0)
                    (fire (make-stepper (seconds 0.1)
                                        (seconds 0.1))))
  (:main
   (set-angle-from-analog 0)
   (when (and (or (mouse-button (mouse) mouse.left)
                  (pad-button 0))
              (funcall fire))
     (spawn 'bullet (v! 0 40)
            :fired-by *self*))
   (setf speed
         (clamp 0f0 max-speed
                (+ (* speed 0.99)
                   (* (pad-1d 1) 0.2))))
   (move-forward speed)))
