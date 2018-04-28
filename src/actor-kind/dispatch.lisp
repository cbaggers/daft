(in-package :daft)

;;------------------------------------------------------------

(defun+ draw-opaque-parts-of-actors (scene actor-kind count res)
  (declare (profile t))
  (with-slots (visual per-actor-gpu-stream tile-count size) actor-kind
    (destructuring-bind (tx ty) tile-count
      (with-instances count
        (map-g #'draw-actors-opaque
               per-actor-gpu-stream
               :offset (v2:+ (pos (camera scene))
                             (focus-offset))
               :screen-height *screen-height-in-game-units*
               :screen-ratio (/ (x res) (y res))
               :size size
               :sam visual
               :tile-count-x tx
               :tile-count-y ty)))))

(defun+ clear-oi-accum-fbo (accum-fbo)
  (declare (profile t))
  (with-fbo-bound (accum-fbo :with-blending nil)
    (clear-fbo accum-fbo) ;; could just clear depth
    (map-g #'clear-oit-pline (get-quad-stream-v2))))

(defun+ accumulate-transparent-parts-of-actors (scene actor-kind
                                                      count res)
  (declare (profile t))
  (with-slots (visual per-actor-gpu-stream tile-count size) actor-kind
    (destructuring-bind (tx ty) tile-count
      (with-instances count
        (map-g #'accum-actors-transparent
               per-actor-gpu-stream
               :offset (v2:+ (pos (camera scene))
                             (focus-offset))
               :screen-height *screen-height-in-game-units*
               :screen-ratio (/ (x res) (y res))
               :size size
               :sam visual
               :tile-count-x tx
               :tile-count-y ty)))))

(defun+ composite-opaque-and-transparent-parts-of-actors (opaque-sampler
                                                         trans-color-sampler
                                                         revealage-sampler)
  (declare (profile t))
  (map-g #'composite-actors (get-quad-stream-v2)
         :solid-sam opaque-sampler
         :accum-sam trans-color-sampler
         :revealage-sam revealage-sampler))

;;------------------------------------------------------------
