(in-package :daft)

(defvar *stepper*
  (make-stepper (seconds 1f0)))
(defvar *wip* 0)
(defvar *fps* 0)


(define-god ()
  (:main
   (incf *wip*)
   (when (funcall *stepper*)
     (setf *fps* *wip*
           *wip* 0))))


(define-actor foo ((:visual "shuttle.png")
                   (dir nil t))
  (:main
   (setf (%pos *self*)
         (v3:*s (v! dir 0f0)
                (* (sin (* (now) 0.1)) 300)))))


(defun hacky-test ()
  (loop :for i :below 3000 :do
     (let* ((pos (v! (- (random 300f0) 150f0)
                  (- (random 300f0) 150f0)))
            (dir (v2:normalize (v2:negate pos))))
       (spawn! 'foo pos :dir dir))))

(defun hacky-kill ()
  (clrhash *actors*))
