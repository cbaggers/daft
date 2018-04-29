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
    (let ((target-kind (%get-actor-kind-by-name *current-scene* actor-kind)))
      (if target-kind
          (progn
            (setf (gethash target-kind (kinds-to-test-collision-with kind)) t)
            (let ((results (gethash target-kind (collision-results kind))))
              (when (and id results)
                (> (aref-c results id) 0))))
          (warn "Cant check for collision with ~s.~%Not a known kind of actor"
                actor-kind)))))

;;------------------------------------------------------------
