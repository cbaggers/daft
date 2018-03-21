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
   (size :initform (v! 0 0))))

(defmethod print-object ((actor actor) stream)
  (format stream "#<~a ~a>" (type-of actor)
          (slot-value actor 'debug-name)))

(defun radius (actor)
  (with-slots (visual) actor
    (/ (x (resolution (sampler-texture visual))) 2f0)))

;;------------------------------------------------------------

(defvar *self*)
(defvar *actors* (make-hash-table))

(defstruct actors
  (current (make-array 0 :adjustable t :fill-pointer 0)
           :type (array t (*)))
  (next (make-array 0 :adjustable t :fill-pointer 0)
        :type (array t (*))))

(defun get-actor-arrays (type)
  (or (gethash type *actors*)
      (setf (gethash type *actors*)
            (make-actors))))

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

(defmacro define-god (values &body states)
  `(define-actor god ,values ,@states))

(defun update-actors ()
  (let ((res (viewport-resolution (current-viewport))))
    (with-setf (depth-test-function) nil

      (loop :for actors being the hash-values of *actors* :do
         (setf (fill-pointer (actors-next actors)) 0)
         (loop
            :for actor :across (actors-current actors)
            :do
            (copy-actor-state actor)
            (update actor)
            (unless (slot-value actor 'dead)
              ;; (when (slot-value actor 'visual)
              ;;   (draw-actor actor res))
              (vector-push-extend
               actor (actors-next actors))))

         (let ((cur-actors (actors-current actors))
               (count 0))
           (when (> (length cur-actors) 0)
             ;; copy actor data to gpu-array
             (with-gpu-array-as-c-array
                 (c-arr *per-actor-data* :access-type :write-only)
               (loop
                  :for actor :across cur-actors
                  :do
                  (unless (slot-value actor 'dead)
                    (write-actor-data actor c-arr count)
                    (incf count))))
             ;; draw 'count' instances of actor
             (draw-instanced-actors count (aref cur-actors 0) res))))
      (livesupport:continuable
        (livesupport:update-repl-link))
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

(defvar *blend-params* (make-blending-params))

(defun calc-uv-mod (actor)
  (with-slots (tile-count anim-frame) actor
    (let ((anim-frame (floor anim-frame)))
      (destructuring-bind (tx ty) tile-count
        (let* ((uv-scale (v! (/ 1f0 tx) (/ 1f0 ty)))
               (uv-offset
                (v! (* (mod anim-frame tx) (x uv-scale))
                    (* (floor (/ anim-frame tx))
                       (y uv-scale)))))
          (values uv-scale uv-offset))))))

(defun write-actor-data (actor c-array index)
  (multiple-value-bind (uv-scale uv-offset) (calc-uv-mod actor)
    (with-slots (visual current-public-state size) actor
      (with-slots (pos rot) current-public-state
        (let ((c-actor (aref-c c-array index)))
          (setf (per-actor-data-transform c-actor)
                (m4:* (m4:translation pos)
                      (m4:rotation-z rot)))
          (setf (per-actor-data-size c-actor)
                size)
          (setf (per-actor-data-uv-scale c-actor)
                uv-scale)
          (setf (per-actor-data-uv-offset c-actor)
                uv-offset))))))

(defun draw-instanced-actors (count actor res)
  (with-slots (visual) actor
    (with-blending *blend-params*
      (with-instances count
        (map-g #'instanced-cube *instanced-cube-stream*
               :screen-height *screen-height-in-game-units*
               :screen-ratio (/ (x res) (y res))

               :sam visual)))))

(defun draw-actor (actor res)
  (multiple-value-bind (uv-scale uv-offset)
      (calc-uv-mod actor)
    (with-slots (visual current-public-state size) actor
      (with-slots (pos rot) current-public-state
        (with-blending *blend-params*
          (map-g #'simple-cube *cube-stream*
                 :screen-height *screen-height-in-game-units*
                 :screen-ratio (/ (x res) (y res))

                 :sam visual
                 :transform (m4:* (m4:translation pos)
                                  (m4:rotation-z rot))
                 :size size
                 :uv-scale uv-scale
                 :uv-offset uv-offset))))))

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
                 debug-name)
        actor
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
