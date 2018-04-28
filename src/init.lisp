(in-package #:daft)

;;------------------------------------------------------------

(defun+ init ()
  (declare (profile t))
  (init-audio)
  (init-pads)
  (init-render)
  (init-collision)
  (init-window-listener)
  (do-hash-vals scene *scenes*
    (ensure-initialized scene))
  (on-resize (surface-resolution (current-surface))))

;;------------------------------------------------------------
