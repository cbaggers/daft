(in-package :daft)

;;------------------------------------------------------------

(defun rotate-actor-state (actor)
  (with-slots (current-public-state next-public-state)
      actor
    (rotatef current-public-state next-public-state)))

;;------------------------------------------------------------

(defun write-actor-data (actor c-array index)
  (let* ((c-actor (aref-c c-array index))
         (kind (kind actor))
         (origin (slot-value kind 'origin)))
    (with-slots (current-public-state anim-frame scale) actor
      (with-slots (pos rot) current-public-state
        (setf (per-actor-data-pos c-actor) (v3:- pos origin))
        (setf (per-actor-data-rot c-actor) rot)
        (setf (per-actor-data-scale c-actor) scale)
        (setf (per-actor-data-anim-frame c-actor) anim-frame)))))

;;------------------------------------------------------------
