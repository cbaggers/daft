(in-package :daft)

;;------------------------------------------------------------

(defun+ touching-p (&optional set-of-actors/actor-kind)
  (declare (profile t))
  (let ((self *self*)
        (scene *current-scene*)
        (target set-of-actors/actor-kind))
    (if (and target (symbolp target))
        (%touching-kind-p scene self target)
        (%touching-set-p scene self target))))

;;------------------------------------------------------------

(defun+ coll-with (actor-kind)
  (declare (profile t))
  (with-slots (kind id) *self*
    (setf (gethash actor-kind (kinds-to-test-collision-with kind)) t)
    (let ((results (gethash actor-kind (collision-results kind))))
      (when (and id results)
        (aref results id)))))

;;------------------------------------------------------------
