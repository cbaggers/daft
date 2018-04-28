(in-package :daft)

;;------------------------------------------------------------

(defun+ %touching-kind-p (scene self target)
  (declare (profile t))
  (let ((actors (get-actor-kind-by-name scene target)))
    (loop :for actor :across (this-frames-actors actors)
       :when (and (typep actor target)
                  (%touching-p self actor))
       :collect actor)))

(defun+ %touching-set-p (scene self sets)
  (declare (profile t))
  (let* ((sets (uiop:ensure-list sets))
         (sets (or sets
                   (mapcar
                    #'this-frames-actors
                    (alexandria:hash-table-values
                     (kinds scene))))))
    (loop :for set :in sets :append
       (loop :for actor :across set
          :when (and (not (eq (slot-value actor 'next)
                              self))
                     (%touching-p self actor))
          :collect actor))))

(defun+ %touching-p (a b)
  (declare (profile t))
  (let ((r-a (radius a))
        (r-b (radius b)))
    (< (v2:distance (s~ (%pos a) :xy)
                    (s~ (%pos b) :xy))
       (+ r-a r-b))))

(defun+ %touching-any-of-kind-p (scene self kind)
  (declare (profile t))
  (let ((actor-kind (get-actor-kind-by-name scene kind)))
    (loop :for actor :across (this-frames-actors actor-kind)
       :when (%touching-p self actor)
       :return t)))

;;------------------------------------------------------------

(defun+ draw-actors-collision-mask (scene actor-kind count res)
  (declare (profile t))
  (declare (ignore res))
  (with-slots (collision-mask
               per-actor-gpu-stream
               tile-count
               size)
      actor-kind
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
                 :tile-count-y ty))))))

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
