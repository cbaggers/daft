(in-package :daft)

;;------------------------------------------------------------

(defun rotate-actor-state (actor)
  (with-slots (current-public-state next-public-state)
      actor
    (rotatef current-public-state next-public-state)))

;;------------------------------------------------------------

(defun write-actor-data (actor c-array index)
  (let ((c-actor (aref-c c-array index)))
    (with-slots (current-public-state anim-frame) actor
      (with-slots (pos rot) current-public-state
        (setf (per-actor-data-pos c-actor) pos)
        (setf (per-actor-data-rot c-actor) rot)
        (setf (per-actor-data-anim-frame c-actor) anim-frame)))))

;;------------------------------------------------------------
