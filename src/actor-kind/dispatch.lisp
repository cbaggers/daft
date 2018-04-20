(in-package :daft)

;;------------------------------------------------------------

(defun draw-actors-common (actor-kind count height ratio
                           &optional (offset-v2 (v! 0 0)))
  (with-slots (visual tile-count size) actor-kind
    (destructuring-bind (tx ty) tile-count
      (with-blending *blend-params*
        (with-instances count
          (map-g #'instanced-cube *instanced-cube-stream*
                 :offset offset-v2
                 :screen-height height
                 :screen-ratio ratio
                 :size size
                 :sam visual
                 :tile-count-x tx
                 :tile-count-y ty))))))

(defun draw-actors-to-screen (scene actor-kind count res)
  (draw-actors-common actor-kind
                      count
                      *screen-height-in-game-units*
                      (/ (x res) (y res))
                      (pos (camera scene))))

(defun draw-actors-collision-mask (scene actor-kind count res)
  (declare (ignore res))
  (with-fbo-bound ((collision-fbo actor-kind))
    (draw-actors-common actor-kind
                        count
                        (y (size scene))
                        1f0)))

(defun run-collision-checks (scene
                             actor-kind
                             count
                             instanced-cube-stream
                             res)
  (declare (ignore res))
  (with-fbo-bound ((empty-fbo scene)
                   :attachment-for-size t)
    (do-hash-keys kind-name (kinds-to-test-collision-with actor-kind)
       (let* ((kind (get-actor-kind-by-name scene kind-name))
              (coll-mask (collision-sampler kind)))
         (with-slots (visual tile-count size) actor-kind
           (destructuring-bind (tx ty) tile-count
             (with-instances count
               (map-g #'check-collisions-with
                      instanced-cube-stream
                      :size size
                      :sam visual
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
                        (> (aref-c cols i) 0))))))))))

;;------------------------------------------------------------
