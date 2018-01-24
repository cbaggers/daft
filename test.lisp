(in-package :daft)

(define-actor bullet ((:visual "bullet.png")
                      (:sprite-size (20 20))
                      (speed 1))
  (move-forward 1)
  (setf x (mouse-x))
  (let ((a (actors-in-range 10)))
    (when (touching-p a)
      (play-sound :bang)
      (die))))

(define-actor ship ((:visual "shuttle2.png"))
  )
