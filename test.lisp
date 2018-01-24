(in-package :daft)

(define-actor bullet ((:visual "bullet.png")
                      (speed 1))
  (move-forward 2))

(define-actor ship ((:visual "shuttle2.png")
                    (start-time (now))
                    (fire (make-stepper (seconds 1.3))))
  (let ((time (+ (mod start-time 2pi-f)
                 (mod (now) 2pi-f))))
    (setf x (* (sin time) 200))

    (when (funcall fire)
      (spawn 'bullet (v! 0 30)))))
