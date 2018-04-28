(in-package #:daft)

;;------------------------------------------------------------

(defstruct-g per-actor-data
  (pos :vec3)
  (rot :float)
  (scale :float)
  (anim-frame :float))

(defstruct-g (collision-info :layout std-430)
  (ids (:int #.+max-actor-count+)))

;;------------------------------------------------------------
