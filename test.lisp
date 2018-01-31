(in-package :daft)

(define-actor bullet ((:visual "bullet.png")
                      (speed 1))
  (when (or (touching-p) (> y 300))
    (die))
  (move-forward 2))

(define-actor ship ((:visual "shuttle2.png")
                    (start-time (now))
                    (fire (make-stepper (seconds 1.3))))
  (let ((time (+ (mod start-time 2pi-f)
                 (mod (now) 2pi-f))))
    (setf x (* (sin time) 200))

    (when (funcall fire)
      (spawn 'bullet (v! 0 30)))))

(defun refresh-actors ()
  (setf *current-actors*
        (make-array 0 :element-type 'actor :adjustable t
                    :fill-pointer 0))
  (spawn! 'ship (v! 0 140))
  (spawn! 'ship (v! 0 40))
  (spawn! 'ship (v! 0 -140)))
