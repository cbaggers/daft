(in-package #:daft)

;;------------------------------------------------------------

(defun init ()
  (init-audio)
  (init-pads)
  (init-render)
  (do-hash-vals scene *scenes*
    (ensure-initialized scene)))

;;------------------------------------------------------------
