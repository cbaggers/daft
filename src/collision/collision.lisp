(in-package :daft)

;;------------------------------------------------------------

(defun touching-p (&optional set-of-actors/actor-kind)
  (let ((self *self*)
        (scene *current-scene*)
        (target set-of-actors/actor-kind))
    (if (and target (symbolp target))
        (%touching-kind-p scene self target)
        (%touching-set-p scene self target))))

(defun %touching-kind-p (scene self target)
  (let ((actors (get-actor-kind-by-name scene target)))
    (loop :for actor :across (this-frames-actors actors)
       :when (and (typep actor target)
                  (%touching-p self actor))
       :collect actor)))

(defun %touching-set-p (scene self sets)
  (let* ((sets (uiop:ensure-list sets))
         (sets (or sets
                   (mapcar
                    #'this-frames-actors
                    (alexandria:hash-table-values
                     (kinds scene))))))
    (loop :for set :in sets :append
       (loop :for actor :across set
          :when (and (not (eq (slot-value actor 'next)
                              self))
                     (%touching-p self actor))
          :collect actor))))

(defun %touching-p (a b)
  (let ((r-a (radius a))
        (r-b (radius b)))
    (< (v2:distance (s~ (%pos a) :xy)
                    (s~ (%pos b) :xy))
       (+ r-a r-b))))

;;------------------------------------------------------------

(defun coll-with (actor-kind)
  (with-slots (kind id) *self*
    (setf (gethash actor-kind (kinds-to-test-collision-with kind)) t)
    (let ((results (gethash actor-kind (collision-results kind))))
      (when (and id results)
        (aref results id)))))

;;------------------------------------------------------------
