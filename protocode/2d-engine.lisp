#|| ENGINE NAME HERE

2d engine where everything has its own process and
all functions etc are relative to self.

||#

(define-sound bang "sounds/bang.wav")

(define-god ()
  )

(define-actor bullet ((:visual "bullet.png")
                      (:sprite-size (20 20))
                      (speed 1))
  (move-forward 1)
  (setf x (mouse-x))
  (let ((a (actors-in-range 10)))
    (when (touching-p a)
      (play-sound :bang)
      (die))))

(distance-to (nearest :ship))

;; - querying within a range
;; - predicates that work over sets
;; - implicit self (this in c#)
;; - by setting sprite-size we make the visual
;;   and animation

(define-actor ship ((:visual "ship.png")
                    (health 10)
                    (bullet nil))
  (:normal-mode
   (setf x (mouse-x))
   (when (gamepad-button-a)
     (setf bullet (spawn :bullet (v! 4 7) :speed 2))
     (change-mode :bullet-mode))
   (when (touching-p (actors-in-range 10 :alien-bullet))
     (die)))

  (:bullet-mode
   (when (touching-p (actors-in-range 10 alien-bullets))
     (die))
   (when (dead bullet)
     (change-mode :normal-mode))))
