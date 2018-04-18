(in-package #:daft)

;;------------------------------------------------------------

(defun init-actor-data ()
  (unless *per-actor-data*
    (setf *per-actor-data*
          (make-gpu-array nil :element-type 'per-actor-data
                          :dimensions +max-actor-count+))
    (setf *per-actor-c-data*
          (make-c-array nil :element-type 'per-actor-data
                        :dimensions +max-actor-count+))))

;;------------------------------------------------------------

(defun init ()
  (init-audio)
  (init-pads)
  (unless *ssbo*
    (setf *ssbo* (make-ssbo nil 'collision-info)))
  (unless *instanced-cube-stream*
    (init-actor-data)
    (destructuring-bind (vert-arr index-arr)
        (nineveh.mesh.data.primitives:cube-gpu-arrays)
      (setf *instanced-cube-stream*
            (make-buffer-stream (list vert-arr
                                      (cons *per-actor-data* 1))
                                :index-array index-arr))))
  (do-hash-vals scene *scenes*
    (ensure-initialized scene)))

;;------------------------------------------------------------
