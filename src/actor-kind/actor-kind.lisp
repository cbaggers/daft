(in-package :daft)

;;------------------------------------------------------------

(defclass actor-kind ()
  ((name
    :initarg :name
    :reader name)
   (current
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :initarg :current
    :accessor this-frames-actors)
   (next
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :initarg :next
    :accessor next-frames-actors)
   (collision-fbo
    :initform nil
    :initarg :collision-fbo
    :accessor collision-fbo)
   (collision-sampler
    :initform nil
    :initarg :collision-sampler
    :accessor collision-sampler)
   (kinds-to-test-collision-with
    :initform (make-hash-table)
    :accessor kinds-to-test-collision-with)
   (collision-results
    :initform (make-hash-table)
    :accessor collision-results)
   (per-actor-c-data
    :initarg :per-actor-c-data
    :accessor per-actor-c-data)
   (per-actor-gpu-data
    :initarg :per-actor-gpu-data
    :accessor per-actor-gpu-data)
   (per-actor-gpu-stream
    :initarg :per-actor-gpu-stream
    :accessor per-actor-gpu-stream)
   (per-actor-c-len
    :initform 0
    :accessor per-actor-c-len)
   (static-p
    :initarg :static-p
    :initform nil
    :accessor static-p)
   (dirty-p
    :initarg :dirty-p
    :initform t
    :accessor dirty-p)
   visual
   collision-mask
   tile-count
   anim-length
   size
   origin))

(defmethod print-object ((obj actor-kind) stream)
  (with-slots (name) obj
    (format stream "#<ACTOR-KIND :NAME ~a>" name)))

(defun make-actor-kind (scene name)
  (destructuring-bind (vert-arr index-arr)
      (nineveh.mesh.data.primitives:cube-gpu-arrays)
    (let* ((tex (gen-collision-texture scene))
           (c-arr (make-c-array nil :element-type 'per-actor-data
                                :dimensions +max-actor-count+))
           (g-arr (make-gpu-array nil :element-type 'per-actor-data
                                  :dimensions +max-actor-count+))
           (strem (make-buffer-stream (list vert-arr
                                            (cons g-arr 1))
                                      :index-array index-arr)))
      (reinit-kind
       (make-instance
        (kind-class-name name)
        :current (make-array 0 :adjustable t :fill-pointer 0)
        :next (make-array 0 :adjustable t :fill-pointer 0)
        :collision-fbo (make-fbo (list 0 tex))
        :collision-sampler (sample tex)
        :per-actor-c-data c-arr
        :per-actor-gpu-data g-arr
        :per-actor-gpu-stream strem)))))

(defun get-actor-kind-by-name (scene type)
  (with-slots (kinds) scene
    (or (gethash type kinds)
        (setf (gethash type kinds)
              (make-actor-kind scene type)))))

(defun gen-collision-texture (scene)
  (let ((vp (viewport scene)))
    (make-texture
     nil
     :dimensions (viewport-dimensions vp)
     :element-type :uint8-vec4)))

(defun kill-actor-kind! (kind-name)
  (let* ((scene *current-scene*)
         (kind (get-actor-kind-by-name scene kind-name)))
    (assert kind)
    (remhash kind-name (kinds scene))
    scene))

;;------------------------------------------------------------
