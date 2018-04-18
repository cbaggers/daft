(in-package :daft)

;;------------------------------------------------------------

(defun update-actor-kinds (scene)
  (with-slots (kinds) scene
    ;;
    (do-hash-vals actor-kind kinds
      (reset-surviving-actor-array actor-kind))
    ;;
    (do-hash-vals actor-kind kinds
      (clrhash (kinds-to-test-collision-with actor-kind))
      (loop
         :for actor :across (this-frames-actors actor-kind)
         :do
         (copy-actor-state actor)
         (update actor)
         (if (slot-value actor 'dead)
             (free-actor actor)
             (enqueue-actor-for-next-frame actor))))))

(defun reset-surviving-actor-array (actor-kind)
  (setf (fill-pointer (next-frames-actors actor-kind)) 0))

(defun enqueue-actor-for-next-frame (actor)
  (with-slots (kind) actor
    (vector-push-extend actor (next-frames-actors kind))))

(defun write-per-actor-data (actor-kind per-actor-c-data)
  (let ((this-frames-actors (this-frames-actors actor-kind))
        (c-arr per-actor-c-data)
        (count 0))
    (when (slot-value actor-kind 'visual)
      (loop
         :for actor :across this-frames-actors
         :do
         (when (not (slot-value actor 'dead))
           (write-actor-data actor c-arr count)
           (setf (id actor) count)
           (incf count))))
    count))

(defun draw-actor-kinds (scene res instanced-cube-stream)
  (with-setf* ((depth-test-function) nil
               (clear-color) (v! 0 0 0 0))
    (do-hash-vals actor-kind (kinds scene)
      (let ((kind-collision-fbo (collision-fbo actor-kind)))
        (clear-fbo kind-collision-fbo)
        (let* ((c-arr *per-actor-c-data*)
               (count (write-per-actor-data actor-kind c-arr)))
          (when (> count 0)
            (push-g (subseq-c c-arr 0 count)
                    (subseq-g *per-actor-data* 0 count))
            (draw-actors-to-screen scene
                                   actor-kind
                                   count
                                   res)
            (draw-actors-collision-mask scene
                                        actor-kind
                                        count
                                        res)
            (run-collision-checks scene
                                  actor-kind
                                  count
                                  instanced-cube-stream
                                  res)))))))

(defun rotate-actor-kind-state (scene)
  (do-hash-vals actor-kind (kinds scene)
    (loop :for actor :across (this-frames-actors actor-kind) :do
       (rotate-actor-state actor))
    (rotatef (this-frames-actors actor-kind) (next-frames-actors actor-kind))))

;;------------------------------------------------------------

(defun ensure-god ()
  (unless *god*
    (let ((*self* nil))
      (setf *god* (spawn 'god (v! 0 0))))))
