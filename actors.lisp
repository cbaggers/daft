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
   (dead :initform nil)))

(defmethod print-object ((actor actor) stream)
  (format stream "#<~a ~a>" (type-of actor)
          (slot-value actor 'debug-name)))

(defun radius (actor)
  (with-slots (visual) actor
    (/ (x (resolution (sampler-texture visual))) 2f0)))

;;------------------------------------------------------------

(defvar *self*)

(defvar *current-actors*
  (make-array 0 :adjustable t :fill-pointer 0))

(defvar *next-actors*
  (make-array 0 :adjustable t :fill-pointer 0))

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

(defun zero-out-next-actors ()
  (setf (fill-pointer *next-actors*) 0))

(defvar *spawn-into* nil)

(defun update-actors ()
  (let ((res (viewport-resolution (current-viewport)))
        (*spawn-into* *next-actors*))
    (with-setf (depth-test-function) nil
      (setf (fill-pointer *next-actors*) 0)
      (loop :for actor :across *current-actors* :do
         (copy-actor-state actor)
         (update actor)
         (unless (slot-value actor 'dead)
           (when (slot-value actor 'visual)
             (draw-actor actor res))
           (vector-push-extend actor *next-actors*)))
      (livesupport:continuable
        (livesupport:update-repl-link))
      (loop :for actor :across *current-actors* :do
         (with-slots (current-public-state
                      next-public-state)
             actor
           (rotatef current-public-state
                    next-public-state))))
    (rotatef *current-actors* *next-actors*)))

(defvar *blend-params* (make-blending-params))

(defun draw-actor (actor res)
  (with-slots (visual current-public-state) actor
    (with-slots (pos rot) current-public-state
      (let ((size (resolution
                   (sampler-texture visual))))
        (with-blending *blend-params*
          (map-g #'simple-cube *cube-stream*
                 :screen-height *screen-height-in-game-units*
                 :screen-ratio (/ (x res) (y res))
                 :transform (m4:* (m4:translation pos)
                                  (m4:rotation-z rot))
                 :sam visual
                 :size size))))))

(defun update-all-existing-actors (type-name
                                   new-visual
                                   new-valid-states
                                   gen-vars)
  (loop :for a :across *current-actors* :do
     (with-slots (visual state) a
       (when (typep a type-name)
         (loop :for (slot-name val)
            :in (funcall gen-vars a)
            :do (setf (slot-value a slot-name) val))
         (setf visual (when new-visual
                        (load-tex new-visual)))
         (when (not (find state new-valid-states))
           (setf state (first new-valid-states)))))))

;;------------------------------------------------------------

(defun %spawn (actor-kind-name parent-pos parent-rot
               pos args into)
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

    (vector-push-extend actor into)
    actor))

;;------------------------------------------------------------
