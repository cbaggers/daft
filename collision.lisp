(in-package :daft)

(defun touching-p (&optional set-of-actors)
  (declare (ignore set-of-actors))
  (let ((self *self*))
    (loop :for actor :across *current-actors*
       :when (and (not (eq (slot-value actor 'next)
                           self))
                  (%touching-p self actor))
       :collect actor)))

(defun %touching-p (a b)
  (let ((r-a (radius a))
        (r-b (radius b)))
    (< (v2:distance (s~ (pos a) :xy)
                    (s~ (pos b) :xy))
       (+ r-a r-b))))
