(in-package #:daft)

;;------------------------------------------------------------

(defun init ()
  (init-audio)
  (init-pads)
  (init-render)
  (init-window-listener)
  (do-hash-vals scene *scenes*
    (ensure-initialized scene)))

;;------------------------------------------------------------
