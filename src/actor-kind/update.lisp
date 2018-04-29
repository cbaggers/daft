(in-package :daft)

;;------------------------------------------------------------

(defun+ update-actor-kinds (scene)
  (declare (profile t))
  (with-slots (kinds) scene
    ;;
    (do-hash-vals actor-kind kinds
      (reset-surviving-actor-array actor-kind))
    ;;
    (do-hash-vals actor-kind kinds
      (unless (static-p actor-kind)
        (clrhash (kinds-to-test-collision-with actor-kind))
        (loop
           :for actor :across (this-frames-actors actor-kind)
           :do
           (copy-actor-state actor)
           (update actor)
           (if (slot-value actor 'dead)
               (free-actor actor)
               (enqueue-actor-for-next-frame actor)))))
    ;;
    (do-hash-vals actor-kind (kinds scene)
      (when (dirty-p actor-kind)
        (write-per-actor-data actor-kind)))))

(defun+ reset-surviving-actor-array (actor-kind)
  (declare (profile t))
  (setf (fill-pointer (next-frames-actors actor-kind)) 0))

(defun+ enqueue-actor-for-next-frame (actor)
  (declare (profile t))
  (with-slots (kind) actor
    (vector-push-extend actor (next-frames-actors kind))))

(defun+ write-per-actor-data (actor-kind)
  (declare (profile t)
           (optimize speed))
  (with-slots (per-actor-c-data
               per-actor-gpu-data
               per-actor-c-len)
      actor-kind
    (let* ((count 0)
           (datum (aref-c per-actor-c-data 0)))
      (declare (type (unsigned-byte 16) count))
      (when (slot-value actor-kind 'visual)
        (loop
           :for actor :across (this-frames-actors actor-kind)
           :do
           (when (not (slot-value actor 'dead))
             (write-actor-data actor datum)
             (setf (id actor) count)
             (cffi:incf-pointer
              (per-actor-data-pointer datum)
              #.(cffi:foreign-type-size 'per-actor-data))
             (incf count)))
        (when (> count 0)
          (push-g (subseq-c per-actor-c-data 0 count)
                  (subseq-g per-actor-gpu-data 0 count))))
      (setf per-actor-c-len count)
      actor-kind)))

(defun+ draw-actor-kinds (scene res)
  (declare (profile t))

  (with-setf (depth-test-function) nil
    (clear-oi-accum-fbo
     *transparent-actor-fbo*))

  ;; seperate from collision loop to avoid unbinding/rebinding fbo
  (with-fbo-bound (*opaque-actor-fbo*)
    (clear-fbo *opaque-actor-fbo*)
    (do-hash-vals actor-kind (kinds scene)
      (with-slots (per-actor-c-data
                   per-actor-gpu-data
                   per-actor-c-len
                   dirty-p)
          actor-kind
        (when (> per-actor-c-len 0)
          (draw-opaque-parts-of-actors scene
                                       actor-kind
                                       per-actor-c-len
                                       res)))))

  (with-setf (depth-mask) nil
    (with-fbo-bound (*transparent-actor-fbo*)
      (do-hash-vals actor-kind (kinds scene)
        (with-slots (per-actor-c-len) actor-kind
          (when (> per-actor-c-len 0)
            (accumulate-transparent-parts-of-actors
             scene
             actor-kind
             per-actor-c-len
             res))))))

  (with-setf (depth-test-function) nil
    (composite-opaque-and-transparent-parts-of-actors
     *opaque-actor-sampler*
     *transparent-color-sampler*
     *transparent-revealage-sampler*))
  nil)

(defun+ mark-actors-clean (scene)
  (declare (profile t))
  (do-hash-vals actor-kind (kinds scene)
    (when (and (dirty-p actor-kind)
               (static-p actor-kind))
      (setf (dirty-p actor-kind) nil))))

(defun+ rotate-actor-kind-state (scene)
  (declare (profile t))
  (do-hash-vals actor-kind (kinds scene)
    (unless (static-p actor-kind)
      (loop :for actor :across (this-frames-actors actor-kind) :do
         (rotate-actor-state actor))
      (rotatef (this-frames-actors actor-kind) (next-frames-actors actor-kind)))))

;;------------------------------------------------------------
