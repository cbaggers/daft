(in-package :daft)

;;------------------------------------------------------------

(defun draw-instanced-actors (count actor res)
  (with-slots (visual tile-count size) actor
    (destructuring-bind (tx ty) tile-count
      (with-blending *blend-params*
        (with-instances count
          (map-g #'instanced-cube *instanced-cube-stream*
                 :screen-height *screen-height-in-game-units*
                 :screen-ratio (/ (x res) (y res))
                 :size size
                 :sam visual
                 :tile-count-x tx
                 :tile-count-y ty))))))

(defun draw-actors-collision-mask (count actor res)
  (declare (ignore res))
  (with-fbo-bound (*actors-fbo*)
    (with-slots (visual tile-count size) actor
      (destructuring-bind (tx ty) tile-count
        (with-blending *blend-params*
          (with-instances count
            (map-g #'write-collision-map *instanced-cube-stream*
                   :screen-height (y *world-size*)
                   :screen-ratio 1f0
                   :size size
                   :sam visual
                   :tile-count-x tx
                   :tile-count-y ty)))))))

(defun run-collision-checks (instanced-cube-stream count actor res actors)
  (declare (ignore res))
  (with-fbo-bound (*world-empty-fbo* :attachment-for-size t)
    (loop
       :for kind-name :being :the
       :hash-keys :of (slot-value actors 'coll-with)
       :do
       (let* ((kind (gethash kind-name *actors*))
              (coll-mask (actors-coll-sampler kind)))
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
         (let ((results (gethash kind-name (actors-coll-results actors))))
           (if results
               (ensure-array-size results count)
               (let ((arr (make-array count :adjustable t :fill-pointer t
                                      :initial-element nil
                                      :element-type 'boolean)))
                 (setf (gethash kind-name (actors-coll-results actors)) arr
                       results arr)))
           (with-gpu-array-as-c-array (tmp (ssbo-data *ssbo*))
             (let ((cols (collision-info-actors (aref-c tmp 0))))
               (loop :for i :below count :do
                  (setf (aref results i)
                        (> (aref-c cols i) 0))))))))))

;;------------------------------------------------------------
