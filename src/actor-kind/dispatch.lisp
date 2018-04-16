(in-package :daft)

;;------------------------------------------------------------

(defun draw-actors-common (count actor height ratio)
  (with-slots (visual tile-count size) actor
    (destructuring-bind (tx ty) tile-count
      (with-blending *blend-params*
        (with-instances count
          (map-g #'instanced-cube *instanced-cube-stream*
                 :screen-height height
                 :screen-ratio ratio
                 :size size
                 :sam visual
                 :tile-count-x tx
                 :tile-count-y ty))))))

(defun draw-actors-to-screen (count actor res)
  (draw-actors-common
   count actor *screen-height-in-game-units* (/ (x res) (y res))))

(defun draw-actors-collision-mask (count actor res)
  (declare (ignore res))
  (with-fbo-bound ((collision-fbo (slot-value actor 'kind)))
    (draw-actors-common count actor (y *world-size*) 1f0)))

(defun run-collision-checks (instanced-cube-stream count actor res actor-kind)
  (declare (ignore res))
  (with-fbo-bound (*world-empty-fbo* :attachment-for-size t)
    (loop
       :for kind-name :being :the
       :hash-keys :of (actors-coll-with actor-kind)
       :do
       (let* ((kind (get-actor-kind-by-name kind-name))
              (coll-mask (collision-sampler kind)))
         (with-slots (visual tile-count size) actor
           (destructuring-bind (tx ty) tile-count
             (with-instances count
               (map-g #'check-collisions-with
                      instanced-cube-stream
                      :size size
                      :sam visual
                      :tile-count-x tx
                      :tile-count-y ty
                      :coll-mask coll-mask
                      :world-size *world-size*
                      :collision *ssbo*))))
         (let ((results (gethash kind-name (collision-results actor-kind))))
           (if results
               (ensure-array-size results count)
               (let ((arr (make-array count :adjustable t :fill-pointer t
                                      :initial-element nil
                                      :element-type 'boolean)))
                 (setf (gethash kind-name (collision-results actor-kind)) arr
                       results arr)))
           (with-gpu-array-as-c-array (tmp (ssbo-data *ssbo*))
             (let ((cols (collision-info-actors (aref-c tmp 0))))
               (loop :for i :below count :do
                  (setf (aref results i)
                        (> (aref-c cols i) 0))))))))))

;;------------------------------------------------------------
