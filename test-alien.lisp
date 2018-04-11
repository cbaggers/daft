(in-package :daft)

(define-god ((spawn-counter (make-stepper (seconds 10)
                                          (seconds 10))))
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
   (when (or (coll-with 'alien)
             (not (in-world-p)))
     (die))
   (move-forward 4)))

(define-actor alien ((:visual "alien.png")
                     (health 10)
                     (center 0.0))
  (:main
   (strafe (* (sin (now)) 2))
   (move-forward -0.2)
   (when (or (coll-with 'bullet)
             (not (in-screen-p)))
     (when (<= (decf health) 0)
       (die)))))

(define-actor new-thing ((:visual "alien.png")
                         (health 10))
  (:main))

(define-actor ship ((:visual "shuttle2.png")
                    (start-time (now) t)
                    (speed (v! 0 0))
                    (max-speed 10f0)
                    (fire (make-stepper (seconds 0.1)
                                        (seconds 0.1))))
  (:main
   (set-angle-from-analog 0)
   ;; (when (coll-with 'alien)
   ;;   (print "blerp"))
   (when (and (or (mouse-button (mouse) mouse.left)
                  (pad-button 0))
              (funcall fire))
     (spawn 'bullet (v! 0 40)
            :fired-by *self*))
   (v2:incf speed (v2:*s (compass-dir)
                         (float (* (pad-1d 1) 0.2) 1f0)))
   (setf speed (v2:*s speed 0.99))
   (compass-dir-move speed)))
