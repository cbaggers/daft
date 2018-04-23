(in-package #:daft)

;;------------------------------------------------------------

(defun init ()
  (init-audio)
  (init-pads)
  ;;(init-text)
  (unless *ssbo*
    (setf *ssbo* (make-ssbo nil 'collision-info)))
  (do-hash-vals scene *scenes*
    (ensure-initialized scene)))

;;------------------------------------------------------------
