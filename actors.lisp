(in-package :daft)

;;------------------------------------------------------------

(defclass public-state ()
  ((pos :initform (v! 0 0 0) :initarg :pos)
   (rot :initform 0f0 :initarg :rot)))

(defun %pos (actor)
  (slot-value
   (if (eq *self* actor)
       (slot-value actor 'next-public-state)
       (slot-value actor 'current-public-state))
   'pos))

(defun (setf %pos) (value actor)
  (setf (slot-value
         (if (eq *self* actor)
             (slot-value actor 'next-public-state)
             (slot-value actor 'current-public-state))
         'pos)
        value))

(defun %rot (actor)
  (slot-value
   (if (eq *self* actor)
       (slot-value actor 'next-public-state)
       (slot-value actor 'current-public-state))
   'rot))

(defun (setf %rot) (value actor)
  (setf (slot-value
         (if (eq *self* actor)
             (slot-value actor 'next-public-state)
             (slot-value actor 'current-public-state))
         'rot)
        value))

;;------------------------------------------------------------

(defclass actor ()
  ((debug-name :initform (get-name) :reader debug-name)
   (current-public-state)
   (next-public-state)
   (visual :initarg :visual)
   (dead :initform nil)
   (tile-count :initform '(1 1))
   (anim-length :initform 1)
   (anim-frame :initform 0)
   (size :initform (v! 0 0))
   kind))

(defmethod print-object ((actor actor) stream)
  (format stream "#<~a ~a>" (type-of actor)
          (slot-value actor 'debug-name)))

(defun radius (actor)
  (with-slots (visual) actor
    (/ (x (resolution (sampler-texture visual))) 2f0)))

;;------------------------------------------------------------

(defclass actors ()
  ((current :initform (make-array 0 :adjustable t :fill-pointer 0)
            :initarg :current
            :type (array t (*))
            :accessor actors-current)
   (next :initform (make-array 0 :adjustable t :fill-pointer 0)
         :initarg :next
         :type (array t (*))
         :accessor actors-next)
   (collision-texture :initform nil
                      :initarg :collision-texture
                      :type (or null texture)
                      :accessor actors-collision-texture)
   (coll-sampler :initform nil
                 :initarg :coll-sampler
                 :type (or null sampler)
                 :accessor actors-coll-sampler)
   (coll-with :initform (make-hash-table)
              :accessor actors-coll-with)))

(defun make-actors (&key current next collision-texture coll-sampler)
  (make-instance
   'actors
   :current (or current (make-array 0 :adjustable t :fill-pointer 0))
   :next (or next (make-array 0 :adjustable t :fill-pointer 0))
   :collision-texture collision-texture
   :coll-sampler coll-sampler))

(defun get-actor-arrays (type)
  (or (gethash type *actors*)
      (let ((col (gen-collision-texture)))
        (setf (gethash type *actors*)
              (make-actors
               :collision-texture col
               :coll-sampler (sample col))))))

(defun gen-collision-texture ()
  (make-texture nil :dimensions '(2048 2048)
                :element-type :uint8-vec4))

(defvar *actors-fbo* nil)

;;------------------------------------------------------------

(defun copy-actor-state (actor)
  (let ((src (slot-value actor 'current-public-state))
        (next (slot-value actor 'next-public-state)))
    (setf (slot-value next 'pos) (slot-value src 'pos))
    (setf (slot-value next 'rot) (slot-value src 'rot))))

(defgeneric update (actor))
(defgeneric init-actor (actor spawn-args))

(defgeneric %change-state (actor new-state))

(defun change-state (new-state)
  (%change-state *self* new-state))

(defvar *stepper*
  (make-stepper (seconds 1f0)))
(defvar *wip* 0)
(defvar *fps* 0)

(defun update-actors ()
  (let ((res (viewport-resolution (current-viewport))))
    (with-setf (depth-test-function) nil
      (incf *wip*)
      (when (funcall *stepper*)
        (setf *fps* *wip*
              *wip* 0))
      (loop :for actors being the hash-values of *actors* :do
         (setf (fill-pointer (actors-next actors)) 0))
      (loop :for actors being the hash-values of *actors* :do
         (clrhash (actors-coll-with actors))
         (let* ((cur-actors (actors-current actors))
                (c-arr *per-actor-c-data*)
                (count 0))
           (loop
              :for actor :across cur-actors
              :do
              (copy-actor-state actor)
              (update actor)
              (if (slot-value actor 'dead)
                  (when (symbol-package (debug-name actor))
                    (push (debug-name actor) *freed-names*))
                  (vector-push-extend
                   actor (actors-next actors))))
           ;; --

           (loop
              :for actor :across cur-actors
              :do
              (unless (slot-value actor 'dead)
                (write-actor-data actor c-arr count)
                (incf count)))
           (when (> count 0)
             ;; =====================
             ;; this isnt the problem
             ;; =====================
             (push-g (subseq-c c-arr 0 count)
                     (subseq-g *per-actor-data* 0 count))
             ;; draw 'count' instances of actor
             (draw-instanced-actors count
                                    (aref cur-actors 0)
                                    res)
             (run-collision-checks count
                                   (aref cur-actors 0)
                                   res
                                   actors))
           (with-setf (clear-color) (v! 0 0 0 0)
             (setf (attachment *actors-fbo* 0)
                   (texref (actors-collision-texture actors)))
             (with-fbo-bound (*actors-fbo*)
               (clear-fbo *actors-fbo*)
               (when (> count 0)
                 (let ((*screen-height-in-game-units* 2048f0))
                   (draw-actors-collision-mask count
                                               (aref cur-actors 0)
                                               res)))))))
      ;; (nineveh:draw-tex-bl
      ;;  (actors-coll-sampler
      ;;   (gethash 'alien *actors*)))
      (livesupport:continuable
        (livesupport:update-repl-link))
      ;; --
      (unless *god*
        (setf *god* (spawn! 'god (v! 0 0))))
      (loop :for task :in *tasks-for-next-frame* :do
         (restart-case (funcall task)
           (continue () :report "Daft: Skip Task")))
      (setf *tasks-for-next-frame* nil)
      (loop :for actors being the hash-values of *actors* :do
         (loop :for actor :across (actors-current actors) :do
            (with-slots (current-public-state
                         next-public-state)
                actor
              (rotatef current-public-state
                       next-public-state)))
         (rotatef (actors-current actors)
                  (actors-next actors))))))

(defun run-collision-checks (count actor res actors)
  (with-viewport (make-viewport '(2048 2048))
    (loop
       :for kind-name :being :the
       :hash-keys :of (slot-value actors 'coll-with)
       :do
       (let* ((kind (gethash kind-name *actors*))
              (coll-mask (actors-coll-sampler kind)))
         (with-slots (visual tile-count size) actor
           (destructuring-bind (tx ty) tile-count
             ;;(with-instances count)
             (map-g #'check-collisions-with
                    *instanced-cube-stream*
                    :screen-height *screen-height-in-game-units*
                    :screen-ratio (/ (x res) (y res))
                    :size size
                    :sam visual
                    :tile-count-x tx
                    :tile-count-y ty
                    :coll-mask coll-mask
                    :world-size (v! 2048 2048)
                    :collision *ssbo*
                    )))))))

(defun write-actor-data (actor c-array index)
  (let ((c-actor (aref-c c-array index)))
    (with-slots (current-public-state anim-frame) actor
      (with-slots (pos rot) current-public-state
        (setf (per-actor-data-pos c-actor) pos)
        (setf (per-actor-data-rot c-actor) rot)
        (setf (per-actor-data-anim-frame c-actor) anim-frame)))))

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
  (with-slots (visual tile-count size) actor
    (destructuring-bind (tx ty) tile-count
      (with-blending *blend-params*
        (with-instances count
          (map-g #'write-collision-map *instanced-cube-stream*
                 :screen-height *screen-height-in-game-units*
                 :screen-ratio (/ (x res) (y res))
                 :size size
                 :sam visual
                 :tile-count-x tx
                 :tile-count-y ty))))))

(defun update-all-existing-actors (type-name
                                   new-visual
                                   new-tile-count
                                   new-valid-states
                                   gen-vars)
  (let ((new-len (reduce #'* new-tile-count))
        (actors (get-actor-arrays type-name)))
    (loop :for a :across (actors-current actors) :do
       (with-slots (visual
                    state
                    tile-count
                    anim-length
                    anim-frame
                    size)
           a
         (loop :for (slot-name val)
            :in (funcall gen-vars a)
            :do (setf (slot-value a slot-name) val))
         (setf visual (when new-visual
                        (load-tex new-visual)))
         (setf size
               (if visual
                   (v2:/ (resolution
                          (sampler-texture visual))
                         (v! tile-count))
                   (v! 0 0)))
         (setf tile-count new-tile-count
               anim-length new-len
               anim-frame (if (< anim-frame new-len)
                              anim-frame
                              0))
         (when (not (find state new-valid-states))
           (setf state (first new-valid-states)))))))

;;------------------------------------------------------------

(defun %spawn (actor-kind-name parent-pos parent-rot
               pos args)
  (let* ((hack-name (intern (symbol-name actor-kind-name)
                            :daft))
         (actor (init-actor
                 (apply #'make-instance hack-name
                        args)
                 args)))
    (with-slots (current-public-state
                 next-public-state
                 debug-name
                 kind)
        actor
      (setf kind (gethash actor-kind-name *actors*))
      (setf current-public-state
            (make-instance 'public-state
                           :pos (v3:+ parent-pos
                                      (m3:*v (m3:rotation-z parent-rot)
                                             (v! (x pos) (y pos) 0)))
                           :rot parent-rot))
      (setf next-public-state
            (make-instance 'public-state))
      (when *noisy-spawn*
        (format t "~%; ~a has spawned!" debug-name)))

    (let ((actors (get-actor-arrays actor-kind-name)))
      (vector-push-extend actor (actors-next actors)))
    actor))

;;------------------------------------------------------------
