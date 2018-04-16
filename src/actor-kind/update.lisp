(in-package :daft)

;;------------------------------------------------------------

(defun update-actor-kinds ()
  ;;
  (do-hash-vals actor-kind *actor-kinds*
    (reset-surviving-actor-array actor-kind))
  ;;
  (do-hash-vals actor-kind *actor-kinds*
    (clrhash (actors-coll-with actor-kind))
    (loop
       :for actor :across (this-frames-actors actor-kind)
       :do
       (copy-actor-state actor)
       (update actor)
       (if (slot-value actor 'dead)
           (free-actor actor)
           (enqueue-actor-for-next-frame actor)))))

(defun reset-surviving-actor-array (actor-kind)
  (setf (fill-pointer (next-frames-actors actor-kind)) 0))

(defun enqueue-actor-for-next-frame (actor)
  (with-slots (kind) actor
    (vector-push-extend actor (next-frames-actors kind))))

(defun write-per-actor-data (actor-kind per-actor-c-data)
  (let ((cur-actors (this-frames-actors actor-kind))
        (c-arr per-actor-c-data)
        (count 0))
    (loop
       :for actor :across cur-actors
       :do
       (when (and (not (slot-value actor 'dead))
                  (slot-value actor 'visual))
         (write-actor-data actor c-arr count)
         (setf (id actor) count)
         (incf count)))
    count))

(defun draw-actor-kinds (res instanced-cube-stream)
  (with-setf* ((depth-test-function) nil
               (clear-color) (v! 0 0 0 0))
    (do-hash-vals actor-kind *actor-kinds*
      (let ((kind-collision-fbo (collision-fbo actor-kind)))
        (clear-fbo kind-collision-fbo)
        (let* ((cur-actors (this-frames-actors actor-kind))
               (c-arr *per-actor-c-data*)
               (count (write-per-actor-data actor-kind c-arr)))
          (when (> count 0)
            (push-g (subseq-c c-arr 0 count)
                    (subseq-g *per-actor-data* 0 count))
            (draw-actors-to-screen count
                                   (aref cur-actors 0)
                                   res)
            (draw-actors-collision-mask count
                                        (aref cur-actors 0)
                                        res)
            (run-collision-checks instanced-cube-stream
                                  count
                                  (aref cur-actors 0)
                                  res
                                  actor-kind)))))))

(defun rotate-actor-kind-state ()
  (do-hash-vals actor-kind *actor-kinds*
    (loop :for actor :across (this-frames-actors actor-kind) :do
       (rotate-actor-state actor))
    (rotatef (this-frames-actors actor-kind) (next-frames-actors actor-kind))))

;;------------------------------------------------------------

(defun ensure-god ()
  (unless *god*
    (let ((*self* nil))
      (setf *god* (spawn 'god (v! 0 0))))))
