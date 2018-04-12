(in-package :daft)

;;------------------------------------------------------------

(defclass actor ()
  ((id :initform nil :accessor id)
   (debug-name :initform (get-name) :reader debug-name)
   (dead :initform nil)
   (anim-frame :initform 0f0)
   current-public-state
   next-public-state
   state
   visual
   tile-count
   anim-length
   size
   kind))

(defmethod print-object ((actor actor) stream)
  (format stream "#<~a ~a>" (type-of actor)
          (slot-value actor 'debug-name)))

(defun radius (actor)
  (with-slots (visual) actor
    (/ (x (resolution (sampler-texture visual))) 2f0)))

;;------------------------------------------------------------

(defgeneric spawn (actor-kind-name pos &key))
(defgeneric reinit-system-state (actor))
(defgeneric reinit-private-state (actor))

;;------------------------------------------------------------

(defun free-actor (actor)
  (when (symbol-package (debug-name actor))
    (push (debug-name actor) *freed-names*)))

;;------------------------------------------------------------

(defgeneric update (actor))

;;------------------------------------------------------------

(defgeneric %change-state (actor new-state))

(defun change-state (new-state)
  (%change-state *self* new-state))

;;------------------------------------------------------------

(defun update-all-existing-actors (type-name)
  (let ((actors (get-actor-kind type-name)))
    (loop :for actor :across (actors-current actors) :do
       (reinit-private-state actor)
       (reinit-system-state actor))))

;;------------------------------------------------------------
