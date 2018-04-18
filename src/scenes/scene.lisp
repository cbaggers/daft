(in-package :daft)

(defclass scene ()
  ((name
    :initarg :name
    :accessor name)
   (size
    :initarg :size
    :accessor size)
   (viewport
    :initarg :viewport
    :accessor viewport)
   (empty-fbo
    :initarg :empty-fbo
    :accessor empty-fbo)
   (camera-position
    :initarg :camera-position
    :accessor camera-position)
   (kinds
    :initform (make-hash-table)
    :accessor kinds)))

(defun make-scene (name size camera-position)
  (make-instance
   'scene
   :name name
   :size (v! size)
   :viewport (make-viewport size)
   :empty-fbo (make-fbo (list nil :dimensions size))
   :camera-position camera-position))

(defvar *scenes* (make-hash-table))

(defun register-scene (scene)
  (check-type scene scene)
  (let ((existing (gethash (name scene) *scenes*)))
    (when existing
      (setf (kinds scene) (kinds existing)))
    (setf (gethash (name scene) *scenes*) scene))
  scene)

(defmacro define-scene (name
                        &body body
                        &key size camera-position)
  (declare (ignore body))
  `(register-scene
    (make-scene ',name ',size ',camera-position)))

#+nil
(define-scene main-game
  :size (2048 2048)
  :camera-position (0 0))
