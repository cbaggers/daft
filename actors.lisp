(in-package :daft)

(defclass actor ()
  ((pos :initform (v! 0 0 0) :initarg :pos
        :accessor pos)
   (rot :initform 0f0 :initarg :rot
        :accessor rot)
   (visual :initarg :visual)))

(defvar *current-actor-state*
  (make-array 0 :element-type 'actor :adjustable t
              :fill-pointer 0))
(defvar *next-actor-state*
  (make-array 0 :element-type 'actor :adjustable t
              :fill-pointer 0))

(defgeneric update (actor))

(defmacro define-actor (name values &body body)
  (let* ((local-vars (remove-if #'keywordp values
                                :key #'first))
         (keyword-vars (remove-if-not #'keywordp values
                                      :key #'first))
         (local-var-names (mapcar #'first local-vars)))
    (destructuring-bind (&key visual sprite-size)
        (reduce #'append keyword-vars)
      (declare (ignore sprite-size))
      `(progn
         (defclass ,name (actor)
           ((visual :initform (load-tex ,visual))
            ,@(loop :for (var-name var-val) :in local-vars :collect
                 `(,var-name
                   :initform ,var-val
                   :initarg ,(intern (symbol-name var-name)
                                     :keyword)))))
         (defmethod update ((self ,name))
           (symbol-macrolet ((x (x (slot-value self 'pos)))
                             (y (y (slot-value self 'pos))))
             (with-slots ,local-var-names self
               (let ((*self* self))
                 ,@body))))
         (push
          (lambda ()
            (update-all-existing-actors ',name ,visual))
          *tasks-for-next-frame*)))))

(defun update-actors ()
  (let ((res (viewport-resolution (current-viewport))))
    (loop :for actor :across *current-actor-state* :do
       (update actor)
       (draw-actor actor res))))

(defun draw-actor (actor res)
  (let ((size (resolution
               (sampler-texture
                (slot-value tmp0 'visual)))))
    (map-g #'simple-cube *cube-stream*
           :screen-height *screen-height-in-game-units*
           :screen-ratio (/ (x res) (y res))
           :transform (m4:translation (pos actor))
           :sam (slot-value actor 'visual)
           :size size)))

(defun update-all-existing-actors (type-name visual)
  (loop :for a :across *current-actor-state* :do
     (when (typep a type-name)
       (setf (slot-value a 'visual)
             (load-tex visual)))))

;;------------------------------------------------------------

(defvar *self*)

(defun spawn (actor-kind-name pos
              &rest args &key &allow-other-keys)
  (%spawn actor-kind-name (pos *self*) pos args))

(defun spawn! (actor-kind-name pos
               &rest args &key &allow-other-keys)
  (%spawn actor-kind-name (v! 0 0 0) pos args))

(defun %spawn (actor-kind-name parent-pos pos args)
  (let* ((hack-name (intern (symbol-name actor-kind-name)
                            :daft))
         (actor (apply #'make-instance hack-name
                       args)))
    (setf (pos actor)
          (v3:+ parent-pos (v! (x pos) (y pos) 0)))
    (vector-push-extend actor *current-actor-state*)
    actor))

(defun die ()
  nil)

(defun play-sound (sound-name)
  (declare (ignore sound-name))
  nil)

(defun touching-p (actor/s)
  (declare (ignore actor/s))
  nil)

(defun mouse-x ()
  0f0)

(defun move-forward (distance)
  (with-slots (pos rot) *self*
    (setf pos
          (v3:+ pos
                (v3:*s (v! (sin (radians rot)) (cos (radians rot)) 0)
                       (float distance 1f0))))))

(defun gamepad-button-a ())

(defun actors-in-range (distance &optional actor-kind)
  (declare (ignore distance actor-kind)))

;;------------------------------------------------------------
