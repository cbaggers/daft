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

(defun+ coll-result-array-size (count)
  (* (ceiling count 5000) 5000))

(defun+ make-col-result-array (count)
  (make-c-array
   nil
   :dimensions (coll-result-array-size count)
   :element-type :int))

(defun+ ensure-coll-array-size (arr count)
  (declare (profile t))
  (when (< (c-array-total-size arr) count)
    (make-col-result-array count)))

(defun+ get-coll-result-array (actor-kind target-kind count)
  (let ((results
         (gethash target-kind (collision-results actor-kind))))
    (if results
        (let ((new (ensure-coll-array-size results count)))
          (if new
              (progn
                (free-c-array results)
                (setf (gethash target-kind (collision-results actor-kind))
                      new))
              results))
        (let ((arr (make-col-result-array count)))
          (setf (gethash target-kind (collision-results actor-kind)) arr)))))

(defun+ run-collision-checks (scene
                              actor-kind
                              count
                              res)
  (declare (profile t))
  (declare (ignore res))
  (do-hash-keys target-kind (kinds-to-test-collision-with actor-kind)
    (let* ((coll-mask (collision-sampler target-kind)))
      (with-slots (per-actor-gpu-stream
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
      (let ((results (get-coll-result-array actor-kind target-kind count)))
        (pull-into results (ssbo-data *ssbo*) count)
        nil))))

;; should be in cepl in proper form
(defn pull-into ((c-arr c-array)
                 (g-arr %cepl.types:gpu-array-bb)
                 (len unsigned-byte))
    c-array
  (let ((buffer (gpu-array-buffer g-arr)))
    (setf (cepl.context:gpu-buffer-bound (cepl-context) :array-buffer)
          buffer)
    (%gl:get-buffer-sub-data
     #.(cepl-utils:gl-enum :array-buffer)
     (%cepl.types:gpu-array-bb-offset-in-bytes-into-buffer g-arr)
     (* #.(cffi:foreign-type-size :int) len)
     (c-array-pointer c-arr)))
  c-arr)

(defun+ run-all-kind-collision-checks (scene res)
  (with-setf* ((clear-color) (v! 0 0 0 0))
    (do-hash-vals actor-kind (kinds scene)
      (with-slots (collision-fbo
                   per-actor-c-len
                   dirty-p)
          actor-kind
        (when dirty-p
          (with-fbo-bound (collision-fbo)
            (clear-fbo collision-fbo)
            (when (> per-actor-c-len 0)
              (draw-actors-collision-mask scene
                                          actor-kind
                                          per-actor-c-len
                                          res))))))
    (with-fbo-bound ((empty-fbo scene)
                     :attachment-for-size t)
      (do-hash-vals actor-kind (kinds scene)
        (with-slots (per-actor-c-len static-p)
            actor-kind
          (when (> per-actor-c-len 0)
            (unless static-p
              (run-collision-checks scene
                                    actor-kind
                                    per-actor-c-len
                                    res))))))))

;;------------------------------------------------------------
