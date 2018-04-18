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
  (unless *sdl2-pads*
    (init-pads '(0)))
  (unless *ssbo*
    (setf *ssbo* (make-ssbo nil 'collision-info)))
  (unless *instanced-cube-stream*
    (init-actor-data)
    (destructuring-bind (vert-arr index-arr)
        (nineveh.mesh.data.primitives:cube-gpu-arrays)
      (setf *instanced-cube-stream*
            (make-buffer-stream (list vert-arr
                                      (cons *per-actor-data* 1))
                                :index-array index-arr)))))

(defun init-pads (ids)
  (setf *sdl2-pads*  (make-array 10 :initial-element nil))
  (sdl2-game-controller-db:load-db)
  (loop :for id :in ids :do
     (unless (aref *sdl2-pads* id)
       (setf (aref *sdl2-pads* id)
             (sdl2:game-controller-open id))))
  (skitter.sdl2:enable-background-joystick-events))

;;------------------------------------------------------------
