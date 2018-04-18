(in-package :daft)

;;------------------------------------------------------------

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
   (background-color
    :initarg :background-color
    :initform (v! 0.03 0.03 0.05 0)
    :accessor background-color)
   (kinds
    :initform (make-hash-table)
    :accessor kinds)))

(defun make-scene (name size camera-position background-color)
  (make-instance
   'scene
   :name name
   :size (if size (v! size) (v! 2048 2048))
   :viewport (make-viewport size)
   :empty-fbo nil
   :camera-position camera-position
   :background-color (if background-color
                         (v! background-color 0)
                         (v! 0.03 0.03 0.05 0))))

(defvar *scenes* (make-hash-table))

(defun register-scene (scene)
  (check-type scene scene)
  (let ((existing (gethash (name scene) *scenes*)))
    (if existing
        (setf (kinds scene) (kinds existing))
        (when (= (hash-table-count *scenes*) 0)
          ;; first scene gets made current, this should be :scratch
          (setf *current-scene* scene)))
    (setf (gethash (name scene) *scenes*) scene))
  scene)

(defmacro define-scene (name
                        &body body
                        &key size camera-position background-color)
  (declare (ignore body))
  (flet ((chk (name x len)
           (when x
             (assert (and (listp x) (= (length x) len)) ()
                     "define-scene: ~a should be a list of ~a numbers"
                     name len))))
    (chk :size size 2)
    (chk :camera-position camera-position 2)
    (chk :background-color background-color 3)
    `(register-scene
      (make-scene ',name ',size ',camera-position ',background-color))))

(defmethod ensure-initialized ((obj scene))
  (with-slots (empty-fbo viewport) obj
    (unless empty-fbo
      (let ((dims (viewport-dimensions viewport)))
        (setf empty-fbo
              (make-fbo (list nil :dimensions dims))))))
  obj)

(defun scene (scene-name)
  (ensure-initialized
   (gethash scene-name *scenes*)))

(defun change-scene (scene-name)
  (let ((scene (gethash scene-name *scenes*)))
    (assert scene)
    (setf *current-scene* scene)))

;;------------------------------------------------------------
