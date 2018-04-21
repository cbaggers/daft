(in-package :daft)

;;------------------------------------------------------------

(defun update-actor-kinds (scene)
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

(defun reset-surviving-actor-array (actor-kind)
  (setf (fill-pointer (next-frames-actors actor-kind)) 0))

(defun enqueue-actor-for-next-frame (actor)
  (with-slots (kind) actor
    (vector-push-extend actor (next-frames-actors kind))))

(defun write-per-actor-data (actor-kind)
  (let ((this-frames-actors (this-frames-actors actor-kind))
        (c-arr (per-actor-c-data actor-kind))
        (count 0))
    (when (slot-value actor-kind 'visual)
      (loop
         :for actor :across this-frames-actors
         :do
         (when (not (slot-value actor 'dead))
           (write-actor-data actor c-arr count)
           (setf (id actor) count)
           (incf count))))
    (setf (per-actor-c-len actor-kind) count)
    actor-kind))

(defun draw-actor-kinds (scene res instanced-cube-stream)
  (with-setf* ((clear-color) (v! 0 0 0 0))
    (do-hash-vals actor-kind (kinds scene)
      (with-slots (collision-fbo per-actor-c-data per-actor-c-len dirty-p)
          actor-kind
        (let* ((kind-collision-fbo (collision-fbo actor-kind))
               (count per-actor-c-len))
          (when dirty-p
            (clear-fbo kind-collision-fbo))
          (when (> count 0)
            (push-g (subseq-c per-actor-c-data 0 count)
                    (subseq-g *per-actor-data* 0 count))
            (draw-actors-to-screen scene
                                   actor-kind
                                   count
                                   res)
            (when dirty-p
              (draw-actors-collision-mask scene
                                          actor-kind
                                          count
                                          res)
              (run-collision-checks scene
                                    actor-kind
                                    count
                                    instanced-cube-stream
                                    res))))))))

(defun mark-actors-clean (scene)
  (do-hash-vals actor-kind (kinds scene)
    (when (and (dirty-p actor-kind)
               (static-p actor-kind))
      (setf (dirty-p actor-kind) nil))))

(defun rotate-actor-kind-state (scene)
  (do-hash-vals actor-kind (kinds scene)
    (unless (static-p actor-kind)
      (loop :for actor :across (this-frames-actors actor-kind) :do
         (rotate-actor-state actor))
      (rotatef (this-frames-actors actor-kind) (next-frames-actors actor-kind)))))

;;------------------------------------------------------------

(defun ensure-god ()
  (unless *god*
    (let ((*self* nil))
      (setf *god* (spawn 'god (v! 0 0))))))
