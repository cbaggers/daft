(in-package :daft)

(defclass actor ()
  ((pos :initform (v! 0 0 0) :initarg :pos
        :accessor pos)
   (rot :initform 0f0 :initarg :rot
        :accessor rot)))

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
         (local-var-names (mapcar #'first local-vars)))
    `(progn
       (defclass ,name (actor)
         ,(loop :for (var-name var-val) :in local-vars :collect
             `(,var-name :initform ,var-val
                         :initarg ,(intern (symbol-name var-name)
                                           :keyword))))
       (defmethod update ((self ,name))
         (symbol-macrolet ((x (x (slot-value self 'pos)))
                           (y (y (slot-value self 'pos))))
           (with-slots ,local-var-names self
             (let ((*self* self))
               ,@body)))))))

(defun update-actors ()
  (let ((res (viewport-resolution (current-viewport))))
    (loop :for actor :across *current-actor-state* :do
       (update actor)
       (draw-actor actor res))))

(defun draw-actor (actor res)
  (map-g #'simple-cube *cube-stream*
         :screen-height *screen-height-in-game-units*
         :screen-ratio (/ (x res) (y res))
         :transform (m4:translation (pos actor))
         :sam (gethash "shuttle2.png" *samplers*)))

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
  (declare (ignore distance))
  nil)

(defun gamepad-button-a ())

(defun actors-in-range (distance &optional actor-kind)
  (declare (ignore distance actor-kind)))

;;------------------------------------------------------------
