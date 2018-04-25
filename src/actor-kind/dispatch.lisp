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

(defun+ draw-actors-collision-mask (scene actor-kind count res)
  (declare (profile t))
  (declare (ignore res))
  (with-slots (collision-fbo
               collision-mask
               per-actor-gpu-stream
               tile-count
               size)
      actor-kind
    (with-fbo-bound ((collision-fbo actor-kind))
      (destructuring-bind (tx ty) tile-count
        (with-blending *blend-params*
          (with-instances count
            (map-g #'draw-actor-collision-mask
                   per-actor-gpu-stream
                   :offset (v! 0 0)
                   :screen-height (y (size scene))
                   :screen-ratio 1f0
                   :size size
                   :sam collision-mask
                   :tile-count-x tx
                   :tile-count-y ty)))))))

(defun+ run-collision-checks (scene
                             actor-kind
                             count
                             res)
  (declare (profile t))
  (declare (ignore res))
  (with-fbo-bound ((empty-fbo scene)
                   :attachment-for-size t)
    (do-hash-keys kind-name (kinds-to-test-collision-with actor-kind)
       (let* ((kind (get-actor-kind-by-name scene kind-name))
              (coll-mask (collision-sampler kind)))
         (with-slots (visual
                      per-actor-gpu-stream
                      (actor-coll-mask collision-mask)
                      tile-count size)
             actor-kind
           (destructuring-bind (tx ty) tile-count
             (with-instances count
               (map-g #'check-collisions-with
                      per-actor-gpu-stream
                      :offset-v2 (v! 0 0)
                      :size size
                      :sam actor-coll-mask
                      :tile-count-x tx
                      :tile-count-y ty
                      :coll-mask coll-mask
                      :world-size (size scene)
                      :collision *ssbo*))))
         (let ((results (gethash kind-name (collision-results
                                            actor-kind))))
           (if results
               (ensure-array-size results count)
               (let ((arr (make-array count
                                      :adjustable t
                                      :fill-pointer t
                                      :initial-element nil
                                      :element-type 'boolean)))
                 (setf (gethash kind-name
                                (collision-results actor-kind))
                       arr)
                 (setf results arr)))
           (with-gpu-array-as-c-array (tmp (ssbo-data *ssbo*))
             (let ((cols (collision-info-ids (aref-c tmp 0))))
               (loop :for i :below count :do
                  (setf (aref results i)
                        (> (aref-c cols i) 0)))))
           nil)))))

;;------------------------------------------------------------
