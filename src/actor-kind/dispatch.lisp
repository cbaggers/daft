(in-package :daft)

;;------------------------------------------------------------

(defun draw-actors-common (actor-kind count height ratio
                           sampler offset-v2)
  (with-slots (per-actor-gpu-stream tile-count size) actor-kind
    (destructuring-bind (tx ty) tile-count
      (with-blending *blend-params*
        (with-instances count
          (map-g #'draw-actor-pline
                 per-actor-gpu-stream
                 :offset offset-v2
                 :screen-height height
                 :screen-ratio ratio
                 :size size
                 :sam sampler
                 :tile-count-x tx
                 :tile-count-y ty))))))

(defun draw-actors-to-screen (scene actor-kind count res)
  (with-slots (visual) actor-kind
    (draw-actors-common actor-kind
                        count
                        *screen-height-in-game-units*
                        (/ (x res) (y res))
                        visual
                        (v2:+ (pos (camera scene))
                              (focus-offset)))))

(defun draw-actors-collision-mask (scene actor-kind count res)
  (declare (ignore res))
  (with-slots (collision-fbo collision-mask) actor-kind
    (with-fbo-bound ((collision-fbo actor-kind))
      (draw-actors-common actor-kind
                          count
                          (y (size scene))
                          1f0
                          collision-mask
                          (v! 0 0)))))

(defun run-collision-checks (scene
                             actor-kind
                             count
                             res)
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
