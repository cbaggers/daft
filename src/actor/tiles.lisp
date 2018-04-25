(in-package :daft)

;;------------------------------------------------------------

(defun+ tile-size (sampler tile-count)
  (declare (profile t))
  (if sampler
      (v2:/ (resolution (sampler-texture sampler))
        (v! tile-count))
      (v! 0 0)))

;;------------------------------------------------------------
