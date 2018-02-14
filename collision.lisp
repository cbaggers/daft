(in-package :daft)

(defun touching-p (&optional set-of-actors/actor-kind)
  (let ((self *self*)
        (target (or set-of-actors/actor-kind
                    *current-actors*)))
    (if (symbolp target)
        (%touching-kind-p self target)
        (%touching-set-p self target))))

(defun %touching-kind-p (self target)
  (loop :for actor :across *current-actors*
     :when (and (typep actor target)
                (%touching-p self actor))
     :collect actor))

(defun %touching-set-p (self set)
  (loop :for actor :across set
     :when (and (not (eq (slot-value actor 'next)
                         self))
                (%touching-p self actor))
     :collect actor))

(defun %touching-p (a b)
  (let ((r-a (radius a))
        (r-b (radius b)))
    (< (v2:distance (s~ (slot-value a 'pos) :xy)
                    (s~ (slot-value b 'pos) :xy))
       (+ r-a r-b))))
