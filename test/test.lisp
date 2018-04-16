(in-package :daft)

(defparameter *screen-height-in-game-units* 2000.0)

(define-god () (:main))

(define-actor foo ((:visual "test/shuttle.png")
                   (dir nil t))
  (:main
   (setf (%pos *self*)
         (v3-n:*s (v! dir 0f0)
                  (* (sin (* (x dir) (now) 0.2))
                     1100)))))

(defun hacky-test ()
  (loop :for i :below 3000 :do
     (let* ((pos (v! (- (random 300f0) 150f0)
                  (- (random 300f0) 150f0)))
            (dir (v2:normalize (v2:negate pos))))
       (spawn! 'foo pos :dir dir))))

(defun hacky-kill ()
  (clrhash *actor-kinds*))
